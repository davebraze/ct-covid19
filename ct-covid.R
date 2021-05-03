library(httr)
library(XML)
library(here)
library(fs)
library(RCurl)
library(readr)
library(sf)
library(RSocrata)

library(tabulizer)
library(stringr)
library(lubridate)
library(wordstonumbers)
library(forcats)
library(dplyr)

library(ggplot2)
library(ggpmisc)
library(ggrepel)
library(cowplot)
library(kableExtra)

source(here::here("locals.R"))

#######################
## load up map files ##
#######################

##### GIS data for CT is available here:
## http://magic.lib.uconn.edu/connecticut_data.html
##
## Must first download the relevant shape files by hand, unzip them, and then load them up.
## URL for town shape files is here: http://magic.lib.uconn.edu/magic_2/vector/37800/townct_37800_0000_2010_s100_census_1_shp.zip
ct.shp <-
    sf::st_read(here::here("02-shapefiles/CT/townct_37800_0000_2010_s100_census_1_shp/townct_37800_0000_2010_s100_census_1_shp/nad83",
                           "townct_37800_0000_2010_s100_census_1_shp_nad83_feet.shp")) %>%
    filter(NAME10 != "County subdivisions not defined") %>%
    mutate(LAT = as.numeric(INTPTLAT10),
           LON = as.numeric(INTPTLON10))

#####################################
## download CTDPH daily reports    ##
#####################################

## Manually download DPH daily reports at: https://portal.ct.gov/Coronavirus

## TODO: look into writing routine to automate downloads.
## example url for daily COVID-19 update from CT DPH looks like this
## https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en
## Maybe use httr::modify_url to pass in different file names, as needed.

## httr::parse_url("https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en")

#######################################
## Extract info from CTDPH pdf files ##
#######################################

## get list of ctdph covid reports at hand
covid.fnames <- fs::dir_ls(here::here("01-ctdph-daily-reports")) %>%
    str_subset("CTDPHCOVID19summary[0-9]+.pdf")

##### read available covid reports and scrape data from them
##### this is necessary to get numbers for early part of the pandemic
scrape.reports = FALSE
town.tab <- TRUE
if(scrape.reports) {
    covid <- purrr::map_dfr(covid.fnames, read_ctcovid_pdf, town.tab=town.tab)
    saveRDS(covid, file=here::here("03-other-source-data", "pdf-reports.rds"))
} else {
    covid <- readRDS(file=here::here("03-other-source-data", "pdf-reports.rds"))
}

##################################################
## Use the Socrata API to access state DPH data ##
##################################################

## David Lucey points out that the data seem to be available more directly on the state's data
## portal at: https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc/#data-library

## State data is accessed using the Socrata API. The R package
## RSocrata:: package facilitates this.
##
## initial set up involves
## 1. registering with https://opendata.socrata.com/login
## 2. create an app_token to access the api via read.socrata()

socrata.app.token <- Sys.getenv("SOCRATA_APP_TOKEN_CTCOVID19")

if(FALSE) {
    url <- httr::parse_url("https://data.ct.gov/resource/28fr-iqnx.json")
    url$path <- NULL
    url <- httr::build_url(url)
    D.ct <- RSocrata::ls.socrata(url)

    D.covid <- D.ct %>%
        filter(str_detect(.$title, "COVID"))

}

##### cases and deaths by town

## Upon reviewing the data provided by CT on Nov. 1 2020, for the first time since mid-May, there
## have been some changes to the variables in the Town data file. Need to sort that out before
## pushing new data to the web. Metadata for the Town dataset can be found here
## https://data.ct.gov/Health-and-Human-Services/COVID-19-Tests-Cases-and-Deaths-By-Town-/28fr-iqnx
## OR https://data.ct.gov/resource/28fr-iqnx

covid.api <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                          app_token=socrata.app.token) %>%
    rename(Town = town,
           town.cases = towntotalcases) %>%
    mutate(across(starts_with("town", ignore.case=FALSE), as.integer),
           across(starts_with("people", ignore.case=FALSE), as.integer),
           across(starts_with("number", ignore.case=FALSE), as.integer),
           Date = as.Date(lastupdatedate))

if(FALSE) {

    covid.api %>%
        ggplot(aes(x=Date, color=Town)) +
##        geom_line(aes(y=peopletested), legend=FALSE) +
        geom_line(aes(y=numberoftests), legend=FALSE)

}


##### scrape town/county data from wikipedia

########### FIXME: STASH THIS TABLE LOCALLY AND USE THAT UNLESS WP PAGE IS UPDATED #########
url <- "https://en.wikipedia.org/wiki/List_of_towns_in_Connecticut"
town.info <- GET(url) %>%
    htmlParse() %>%
    readHTMLTable(header=TRUE, which=3, skip=170) %>%
    janitor::clean_names() %>%
    select(-c(number, form_ofgovernment, native_americanname)) %>%
    rename(year.est = dateestablished,
           land.area.sq.miles = land_area_square_miles,
           pop.2010 = population_in_2010,
           council.of.governments = council_of_governments) %>%
    mutate(pop.2010 = as.integer(str_remove(pop.2010, ",")),
           land.area.sq.miles = as.numeric(land.area.sq.miles),
           county = str_replace(county, "County", "Co."),
           pop.2010.bin = cut(pop.2010,
                              breaks=c(0, 5000, 15000, 35000, 75000, Inf),
                              labels=c("less than 5,000", "5k, <15k", "15k, <35k", "35k, <75k", "75,000 or more"),
                              ordered_result=TRUE))

## Merge shapes covid data
ct.covid <-
    covid.api %>%
    left_join(town.info, by=c("Town" = "town")) %>%
    mutate(town.cases.10k = (10000/pop.2010)*town.cases,
           town.deaths.10k = (10000/pop.2010)*towntotaldeaths) %>%
    left_join(ct.shp, by=c("Town" = "NAME10"))

##### state wide counts
## tests.complete info does not seem to be anywhere in any of the covid-19 data sets provided by the
## state up until late May. The only way to get it is to extract it from their daily reports (pdf
## files).

ct.summary.wide <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                           app_token=socrata.app.token) %>%
    rename(Date = date,
           Cases.0 = totalcases,
           Hospitalized.0 = hospitalizedcases,
           Deaths.0 = totaldeaths,
           `Tests.0` = covid_19_tests_reported) %>%
    mutate(Date = as.Date(Date),
           `Tests.0` = as.integer(`Tests.0`),
           Cases.0 = as.integer(Cases.0),
           Deaths.0 = as.integer(Deaths.0),
           Hospitalized.0 = as.integer(Hospitalized.0),
           confirmedcasescum = as.integer(confirmedcases),
           probablecasescum = as.integer(probablecases),
           confirmeddeathscum = as.integer(confirmeddeaths),
           probabledeathscum = as.integer(probabledeaths)) %>%
    select(-c(state, confirmeddeaths, probabledeaths, confirmedcases, probablecases), -starts_with("cases_"))

## FIXME: this line should be redundant.
## The pdfs have already been scanned at line 66, although with town.tab=TRUE.
## tmp <- purrr::map_dfr(covid.fnames, read_ctcovid_pdf, town.tab=FALSE) %>%
##     select(Date, tests.complete)

tmp <- covid %>%
    select(Date, tests.complete) %>%
    distinct()

ct.summary.wide <- left_join(ct.summary.wide, tmp) %>%
    arrange(Date) %>%
    mutate(`Tests.0` = if_else(is.na(`Tests.0`), tests.complete, `Tests.0`)) %>%
    select(-tests.complete) %>%
    mutate(Cases = c(NA, diff(Cases.0)),
           Deaths = c(NA, diff(Deaths.0)),
           `Tests Reported` = c(NA, diff(`Tests.0`)),
           Hospitalized = Hospitalized.0,
           `Test Positivity (percent)` = if_else(Date < as.Date("2020-07-01"),
                                       as.numeric(NA),
                                       Cases/`Tests Reported`*100)
           ) ## Handful of x<0 after this. Let it be.


##### Get what limited school data the state makes available

ct.schools <- read.socrata("https://data.ct.gov/resource/vvjf-9vkr.json",
                                app_token=socrata.app.token) %>%
    mutate_at(vars(ends_with("date")), as.Date) %>%
    mutate_at(vars(starts_with("number")), as.integer) %>%
    select(-starts_with("difference"), -starts_with("percent")) %>%
    tidyr::pivot_longer(cols=starts_with("number"),
                        names_to = "Group",
                        values_to = "Count") %>%
    mutate(Group = as_factor(str_remove_all(Group, "^number_of_")))



#########################
## constants for plots ##
#########################
caption.ctdph <- paste("Data Source: https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc/#data-library.",
                       "Figure by David Braze (davebraze@gmail.com) using R statistical software,",
                       "Released under the Creative Commons v4.0 CC-by license.",
                       sep="\n")

## file names/types
today <- strftime(today(), "%Y%m%d-")
ftype <- "png"
fig.path <- here::here("figures")

## layout
font.size  <- 10
width <- 7
height <- 7
units <- "in"
dpi <- 300

##########################
## school related cases ##
##########################

ct.students.cap <- paste("Weekly Covid19 case count in Connecticut schools broken down",
                         "by learning model (in person; remote; hybrid).",
                         "DPH provides no baselines for these numbers. So, it is unknown",
                         "how many students are in each learning model, and unknown what",
                         "proportion of students in each group were actually tested.")

ct.students.plt <-
    ct.schools %>%
    filter(Group != "student_cases",
           Group != "staff_cases") %>%
    mutate(Group = fct_relabel(Group, function(x) str_remove(x, "_student_cases|_learning_model_student_cases")),
           Group = fct_relevel(Group, rev)) %>%  ## glimpse()
    ggplot(aes(y=Count, x=report_period_start_date)) +
    geom_col(aes(fill=Group)) +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 week",
                 name="Week Starting Date") +
    scale_fill_brewer(type="qual",
                      palette="Dark2",
                      name = "Student Group") +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1)) +
    labs(title="Weekly Covid19 Cases in Connecticut Public School Students",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.schools$report_period_end_date), "%B %d, %Y")),
         caption=caption.ctdph)

ggsave(filename=fs::path_ext_set(paste0(today, "ct-students"), ftype),
       plot=ct.students.plt,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

################################################
## map 10 day average Test Positivity by Town ##
################################################

ct.covid.positivity.0 <-
    ct.covid %>%
    group_by(Town) %>%
    slice_max(Date, n=11) %>% ## use 11 days instead of 10 so as to catch test count on 1st of 10 days of interest
    mutate(ending.date = max(Date),
           positive.sum = diff(range(numberofpositives)),
           tests.sum = diff(range(numberoftests)),
           tests.10k = tests.sum*(10000/pop.2010),
           town.positivity = positive.sum/tests.sum*100) %>%
    filter(Date == ending.date)

if(FALSE) {

    ct.covid.positivity.0 %>%
        ggplot(aes(x=fct_reorder(Town, town.positivity), y=town.positivity)) +
        geom_point(aes(size=tests.10k)) +
        theme_fdbplot(font_size=font.size*.7) +
        background_grid(major="xy") +
        theme(legend.position="top",
              plot.margin = unit(c(1,1,1,1), "lines"),
              axis.text.x = element_text(angle=45, hjust=1))


}

breaks.0 <- c(0,2,4,6,8,10,12,14,16,18,20)
shade.0 <- max(ct.covid.positivity.0$town.positivity)*.5

map.positivity.cap <- paste("Ten Day Average Covid-19 Test Positivity for each Connecticut Town.",
                            "Test Positivity is the percentage of tests administered in a town",
                            "that had a positive result.")

map.positivity <-
    ggplot(ct.covid.positivity.0) +
    geom_sf(aes(fill=town.positivity,
                geometry=geometry),
            color="white", size=.33) +
    geom_sf_text(aes(label=formatC(town.positivity, format="f", digits=2),
                     geometry=geometry,
                     color=town.positivity<shade.0,
                     ## 'text' is a dummy aes used for plotly tooltip
                     text=paste0(Town,
                                 "\nTest Pos: ", formatC(town.positivity, format="f", digits=2), "%",
                                 "\nPopulation: ", formatC(pop.2010, format="d"),
                                 "\nTests/10k/day: ", formatC(tests.10k/10, format="f", digits=2))),
                 size=2, show.legend=FALSE) +
    scale_color_manual(values=c("black", "white")) +
    viridis::scale_fill_viridis(option="magma",
                                breaks=breaks.0,
                                labels=breaks.0) +
    guides(fill=guide_colorbar(barwidth=10,
                               title="Test Positivity (%)",
                               title.vjust=1)) +
    labs(title=paste("10 Day Average Covid-19 Test Positivity in Connecticut Towns\nfor period ending",
                     format(max(ct.covid$Date), "%b %d, %Y")),
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    xlab(NULL) + ylab(NULL) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

if(FALSE) {

    library(plotly)

    map.positivity.plotly <- ggplotly(map.positivity,
                                      layerData=2, ## default = 1
                                      tooltip=c("text")) %>%
        hide_legend() ## also hide_colorbar() & hide_guides()

    map.positivity.plotly

}

ggsave(filename=fs::path_ext_set(paste0(today, "map-positivity"), ftype),
       plot=map.positivity,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

#################################################################
## map cumulative confirmed case count by Town most recent day ##
#################################################################

ct.covid.cumcases <-
    ct.covid %>%
    filter(Date==max(Date))

breaks.1 <- c(1, 3, 6, 12, 25, 50, 100, 200, 400, 800, 1600, 3200, 6400, 12800)
shade.1 <- exp(log(max(ct.covid.cumcases$town.cases))*.75)

map.cumcases <-
    ggplot(ct.covid.cumcases) +
    geom_sf(aes(fill=town.cases, geometry=geometry), color="white", size=.33) +
    geom_sf_text(aes(label=town.cases,
                     geometry=geometry,
                     color=town.cases<shade.1),
                 size=2, show.legend=FALSE) +
    scale_color_manual(values=c("black", "white")) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks.1,
                          labels=breaks.1) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    ## facet_wrap(~Date,
    ##            ncol=4,
    ##            labeller=label_date) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    xlab(NULL) + ylab(NULL) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

ggsave(filename=fs::path_ext_set(paste0(today, "map-cumcases"), ftype),
       plot=map.cumcases,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

####################################
## rate plot by Town, raw counts  ##
####################################

label.cut.towns <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, town.cases, Date) %>%
    arrange(desc(town.cases)) %>%
    pull(town.cases)
label.count.towns <- 17

highlight <- ct.covid %>%
    filter(Town %in% c(""))

town.rate.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases, group=Town),
              size=1, alpha=1/2, color="blue") +
    geom_line(data=highlight, aes(x=Date, y=town.cases, group=Town),
              size=1/2, color="darkorange", alpha=1) +
    ggrepel::geom_text_repel(data = subset(ct.covid,
                                           Date == max(Date) & town.cases >= label.cut.towns[label.count.towns]),
                             aes(label = Town, x = Date, y = town.cases),
                             segment.size=.25,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_color_brewer(type="qual", palette="Dark2") +
    scale_x_date(date_labels="%b %d",
                 date_breaks = "1 month",
                 expand = expansion(mult=c(.01,.15)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

  town.rate.cap <- paste0("Total Covid-19 Cases for Connecticut's 169 Towns.",
                        "The top ", label.count.towns, " towns are labeled\n",
                        "(those with at least ", formatC(label.cut.towns[label.count.towns], format="d", big.mark=","), " cases).")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-rate"), ftype),
       plot=town.rate.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)


####################################
## rate plot by Town, per 10k pop  ##
####################################

label.cut.towns <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, town.cases.10k, Date) %>%
    arrange(desc(town.cases.10k)) %>%
    pull(town.cases.10k)
label.count.towns <- 17

highlight <- ct.covid %>%
    filter(Town %in% c(""))

town.rate.10k.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases.10k, group=Town),
              size=1, alpha=1/2, color="blue") +
    geom_line(data=highlight, aes(x=Date, y=town.cases, group=Town),
              size=1/2, color="darkorange", alpha=1) +
    ggrepel::geom_text_repel(data = subset(ct.covid,
                                           Date == max(Date) & town.cases.10k >= label.cut.towns[label.count.towns]),
                             aes(label = Town, x = Date, y = town.cases.10k),
                             segment.size=.25,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_color_brewer(type="qual", palette="Dark2") +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 month",
                 expand = expansion(mult=c(.01,.15)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut Towns (per 10k pop.)",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Number of Cases per 10,000 population") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

town.rate.10k.note <- paste("Cases per 10,000 population for each town.",
                             "The top ", label.count.towns, " towns are labeled",
                             "(those with at least", formatC(trunc(label.cut.towns[label.count.towns]), digits=2, format="f", big.mark=","),
                             "cases per 10k pop.).")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-rate-10k"), ftype),
       plot=town.rate.10k.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)


########################################
## rate plot by town, facet by county ##
########################################

label.count <- 6
label.cut <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, county, town.cases, Date) %>%
    group_by(county) %>%
    arrange(town.cases, .by_group=TRUE) %>%
    top_n(label.count, town.cases)

town.by.county.rate.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases, group=Town, color=county),
              size=1, alpha=1/2) +
    facet_wrap(~county, nrow=4, scales="free_y") +
    ggrepel::geom_text_repel(data = label.cut,
                             aes(label = Town, x = Date, y = town.cases),
                             segment.size=.25,
                             min.segment.length = 0,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_color_brewer(type="qual", palette="Dark2", guide=FALSE) +
    scale_y_continuous(limits=my_limits) +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 month",
                 expand = expansion(mult=c(.01,.25)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns, split by county",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ct.town.by.county.rate.cap <- paste(
    "Cumulative Covid-19 case counts by town, split by county.",
    "The top ", label.count, " towns in each county are labeled.",
    "Note differing y scales for each county.")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-by-county-rate"), ftype),
       plot=town.by.county.rate.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)


#############################################################
## rate plot by town per 10k pop., facet by population bin ##
#############################################################

label.count <- 7
label.cut <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, county, pop.2010.bin, town.cases.10k, Date) %>%
    group_by(pop.2010.bin) %>%
    arrange(town.cases.10k, .by_group=TRUE) %>%
    top_n(label.count, town.cases.10k)

town.by.pop.rate10k.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases.10k, group=Town, color=county),
              size=1, alpha=1/2) +
    facet_wrap(~pop.2010.bin, nrow=5, scales="free_y") +
    ggrepel::geom_text_repel(data = label.cut,
                             aes(label = Town, x = Date, y = town.cases.10k, color=county),
                             alpha=1,
                             segment.size=.25,
                             min.segment.length = 0,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5,
                             show.legend=FALSE) +
    scale_color_brewer(type="qual", palette="Dark2") +
    scale_y_continuous(limits=my_limits) +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 month",
                 expand = expansion(mult=c(.01,.15)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases per 10k population for 169 Connecticut Towns\nsplit by population bin",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Number of Cases per 10,000 Population") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    guides(color=guide_legend(title="County", title.hjust=0.5, ncol=1,
                              override.aes=list(size=3, alpha=1),
                              shape=16)) +
    theme(legend.position="right",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ct.town.by.pop.rate10k.cap <- paste(
    "Cumulative Covid-19 case counts per capita for each town, split by population bin.",
    "The top ", label.count, " towns in each population group are labeled.",
    "Note differing y scales for each group.",
    "Counties are color coded as before.")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-by-pop-rate10k"), ftype),
       plot=town.by.pop.rate10k.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)


################################################
## rate plot by town, facet by population bin ##
################################################

label.count <- 7
label.cut <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, county, pop.2010.bin, town.cases, Date) %>%
    group_by(pop.2010.bin) %>%
    arrange(town.cases, .by_group=TRUE) %>%
    top_n(label.count, town.cases)

town.by.pop.rate.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases, group=Town, color=county),
              size=1, alpha=1/2) +
    facet_wrap(~pop.2010.bin, nrow=5, scales="free_y") +
    ggrepel::geom_text_repel(data = label.cut,
                             aes(label = Town, x = Date, y = town.cases, color=county),
                             alpha=1,
                             segment.size=.25,
                             min.segment.length = 0,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5,
                             show.legend=FALSE) +
    scale_color_brewer(type="qual", palette="Dark2") +
    scale_y_continuous(limits=my_limits) +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 month",
                 expand = expansion(mult=c(.01, .15)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns, split by population",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    guides(color=guide_legend(title="County", title.hjust=0.5, ncol=1,
                              override.aes=list(size=3, alpha=1),
                              shape=16)) +
    theme(legend.position="right",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))


ct.town.by.pop.rate.cap <- paste(
    "Cumulative Covid-19 case counts by town, split by population bin.",
    "The top ", label.count, " towns in each population group are labeled.",
    "Note differing y scales for each group.",
    "Counties are color coded as in the previous plot.")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-by-pop-rate"), ftype),
       plot=town.by.pop.rate.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)


########################################################
## line plot for daily change in core statistics.     ##
## Includes 7 day running average                     ##
########################################################

## FIXME:
## o add "(confirmed + probable)" to Cases & Deaths labels

ct.summary.1 <-
    ct.summary.wide %>%
    select(- c("confirmeddeathscum",
               "probabledeathscum",
               "confirmedcasescum",
               "probablecasescum")) %>%
    mutate(`Tests Reported.7mn` = TTR::runMean(`Tests Reported`,7),
           Cases.7mn = TTR::runMean(Cases,7),
           Hospitalized.7mn = TTR::runMean(Hospitalized,7),
           Deaths.7mn = TTR::runMean(Deaths,7),
           `Test Positivity (percent).7mn` = TTR::runMean(`Test Positivity (percent)`,7))

ct.summary.long <-
    ct.summary.1 %>%
    tidyr::pivot_longer(cols=-c(Date)) %>%
    ## tidyr::separate() might be useful for the following
    mutate(type = if_else(str_detect(name, "7mn"), "7mn", "daily"),
           name = forcats::fct_collapse(name,
                                        "Tests Reported" = c("Tests Reported", "Tests Reported.7mn"),
                                        "Cases" = c("Cases", "Cases.7mn"),
                                        "Test Positivity (percent)" = c("Test Positivity (percent)", "Test Positivity (percent).7mn"),
                                        "Hospitalized" = c("Hospitalized", "Hospitalized.7mn"),
                                        "Deaths" = c("Deaths", "Deaths.7mn"))) %>%
    mutate(name = forcats::fct_relevel(name, "Tests Reported",
                                       "Cases", "Test Positivity (percent)",
                                       "Hospitalized", "Deaths"),
           type = forcats::fct_relevel(type, "daily", "7mn"))

ct.stats.label <-
    ct.summary.long %>%
    filter(! name %in% c("Tests.0", "Cases.0", "Hospitalized.0", "Deaths.0")) %>%
    filter(Date == max(Date)) %>%
    mutate(label = formatC(value, digits=2, format="f", big.mark=","),
           label = if_else(type == "7mn",
                           label,
                           str_replace(label, "\\.00", "")))

ct.stat.daily.change.plt  <-
    ct.summary.long %>%
    filter(! name %in% c("Tests.0", "Cases.0", "Hospitalized.0", "Deaths.0")) %>% ## glimpse()
    ggplot(aes(x=Date)) +
    geom_line(aes(y=value, color=name, linetype=type, size=type, alpha=type), show.legend=FALSE) +
    scale_size_manual(values=c(1,.75)) +
    scale_alpha_manual(values=c(1/3,1)) +
    scale_linetype_manual(values=c("solid", "F2")) +
    scale_x_date(date_labels="%b %d",
                 date_breaks="1 month",
                 expand = expansion(mult=c(.01,.1)),
                 name=NULL) +
    scale_y_continuous(limits=my_limits) +
    scale_color_manual(values=RColorBrewer::brewer.pal(5,"Dark2")[c(1,2,5,3,4)]) +
    facet_wrap(~name, nrow=5, scales="free_y") +
    labs(title="Daily Values for Covid-19 Statistics in Connecticut",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Daily Value (non-cumulative)") + xlab(NULL) +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    ggrepel::geom_text_repel(data = ct.stats.label,
                             aes(label = label, x = Date, y = value),
                             segment.size=.25,
                             min.segment.length = 0,
                             size=2.5,
                             hjust = 0,
                             direction="y",
                             force=1/4,
                             nudge_x=10)

ct.stat.daily.change.cap <- paste(
    "Daily Values (non-cumulative) for Covid-19 Statistics.",
    "Pale solid lines = raw counts.",
    "Dark dashed lines = 7 day running averages.",
    "Tests before July 1 were in short supply and largely limited to those suspected of being infected.",
    "So, 'Test Positivity' for the period before July 1 is not shown.",
    "Note different y scales on subplots.")

ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary-rate"), ftype),
       plot=ct.stat.daily.change.plt,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

if(FALSE){

    plotly::ggplotly(ct.stat.daily.change.plt)

    tmp <-
        ct.summary.wide %>%
        filter(Date >= "2020-10-01")

    Hmisc::rcorr(tmp$`Tests Reported`, tmp$`Test Positivity (percent)`)

}

#####################################
## Drill down into test positivity ##
#####################################

if (FALSE) {

    ct.tpos.label <-
        ct.summary.long %>%
        filter(name == "Test Positivity (percent)") %>%
        filter(Date == max(Date)) %>%
        mutate(label = formatC(value, digits=2, format="f", big.mark=","),
               label = if_else(type == "7mn",
                               label,
                               str_replace(label, "\\.00", "")))

    holidays <- tibble(name = c("Independence Day", "Labor Day", "Columbus Day", "Thanksgiving"),
                       start = as.Date(c("2020-07-03", "2020-09-05", "2020-10-10", "2020-11-26")),
                       end = as.Date(c("2020-07-05", "2020-09-07", "2020-10-12", "2020-11-29")),
                       ymin = 0,
                       ymax = 7)

    ct.tpos.daily.change.plt  <-
        ct.summary.long %>%
        filter(name == "Test Positivity (percent)") %>% ## glimpse()
        ggplot(aes(x=Date)) +
        geom_line(aes(y=value, color=name, linetype=type, size=type, alpha=type), show.legend=FALSE) +
        scale_size_manual(values=c(1,.75)) +
        scale_alpha_manual(values=c(1/3,1)) +
        scale_linetype_manual(values=c("solid", "F2")) +
        scale_x_date(date_labels="%b %d",
                     date_breaks="1 month",
                     expand = expansion(add=c(2,30)),
                     name=NULL) +
        scale_y_continuous(limits=my_limits) +
        scale_color_manual(values=RColorBrewer::brewer.pal(5,"Dark2")[c(1,2,5,3,4)]) +
        geom_rect(data=holidays, inherit.aes=FALSE,
                  aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax),
                  fill="orange", color=NA, alpha=1/4) +
        labs(title="Covid-19 Test Positivity in Connecticut",
             subtitle=paste("Data compiled by CT Dept. of Public Health through",
                            format(max(ct.covid$Date), "%B %d, %Y")),
             caption=caption.ctdph) +
        ylab("Test Positivity Value (percent)") + xlab(NULL) +
        theme_fdbplot(font_size=font.size) +
        background_grid(major="xy") +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        ggrepel::geom_text_repel(data = ct.tpos.label,
                                 aes(label = label, x = Date, y = value),
                                 segment.size=.25,
                                 min.segment.length = 0,
                                 size=2.5,
                                 hjust = 0,
                                 direction="y",
                                 force=1/4,
                                 nudge_x=10)

    ct.tpos.daily.change.cap <- paste(
        "Daily Values for Covid-19 Test Positivity.",
        "Pale solid line = raw percent.",
        "Dark dashed line = 7 day running average.",
        "Tests before July 1 were in short supply and largely limited to those suspected of being infected.",
        "So, 'Test Positivity' for the period before July 1 is not shown.")

    ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary-tpos"), ftype),
           plot=ct.tpos.daily.change.plt,
           path=fig.path,
           device=ftype,
           width=width, height=height,
           units=units,
           dpi=dpi)
}
####################################
## bar plot 2 for state-wide data ##
####################################

ct.stats.label.2 <-
    ct.summary.long %>%
    filter(name %in% c("Tests.0", "Cases.0", "Hospitalized.0", "Deaths.0")) %>%
    filter(Date == max(Date)) %>%
    mutate(name = fct_recode(name,
                             "Cases, cumulative" = "Cases.0",
                             "Tests, cumulative" = "Tests.0",
                             "Hospitalized, daily" = "Hospitalized.0",
                             "Deaths, cumulative" = "Deaths.0")) %>%
    mutate(label = formatC(value, format="d", big.mark=","))

ct.summary.2.plt <-
    ct.summary.long %>%
    filter(name %in% c("Cases.0", "Tests.0", "Hospitalized.0", "Deaths.0" )) %>% ## glimpse()
    mutate(name = fct_recode(name,
                             "Cases, cumulative" = "Cases.0",
                             "Tests, cumulative" = "Tests.0",
                             "Hospitalized, daily" = "Hospitalized.0",
                             "Deaths, cumulative" = "Deaths.0")) %>%
    mutate(name = forcats::fct_relevel(name,
                                       "Tests, cumulative",
                                       "Cases, cumulative",
                                       "Hospitalized, daily",
                                       "Deaths, cumulative")) %>%
    ggplot(aes(y=value, x=Date)) +
    geom_bar(aes(fill=name), alpha=1/2, position="dodge", stat="identity", show.legend=FALSE) +
    facet_wrap(~name, nrow=4, scales="free_y") +
    scale_fill_brewer(type="qual", palette="Dark2") +
    scale_x_date(expand = expansion(add=c(2, 30)),
                 date_labels="%b %d",
                 date_breaks="1 month",
                 name=NULL) +
    scale_color_brewer(type="qual", palette="Dark2") +
    ylim(-150, NA) +
    guides(fill=guide_legend(title=NULL)) +
    labs(title="Covid-19 Cases, Hospitalizations, Deaths, & Tests for Connecticut",
         subtitle=paste("Data compiled by CT Dept. of Public Health through",
                        format(max(ct.covid$Date), "%B %d, %Y")),
         caption=caption.ctdph) +
    ylab("Count") + xlab(NULL) +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(legend.position=c(.05, .90),
          axis.text.x = element_text(angle=45, hjust=1)) +
    ggrepel::geom_text_repel(data = ct.stats.label.2,
                             aes(label = label, x = Date, y = value),
                             segment.size=.25,
                             min.segment.length = 0,
                             size=2.5,
                             hjust = 0,
                             direction="y",
                             force=1/4,
                             nudge_x=10)



ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary"), ftype),
       plot=ct.summary.2.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)

####################################
## NYT state-by-state corona data ##
####################################

## NY Times maintains a github repo with "a series of data files with
## cumulative counts of coronavirus cases in the United States, at the
## state and county level, over time. We are compiling this time series
## data from state and local governments and health departments in an
## attempt to provide a complete record of the ongoing outbreak." The repo
## is here: https://github.com/nytimes/covid-19-data. It is updated
## regularly.
##
## State csv file: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv
##
## County csv file: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv

##### Get the data

## ##  This worked up until January 13 2021
## usa.state.corona <-
##     getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv?accessType=DOWNLOAD") %>%
##     readr::read_csv()

## The workaround is here
downloader::download("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv?accessType=DOWNLOAD",
                     here::here("03-other-source-data", "nyt-us-states.csv"))

usa.state.corona <- readr::read_csv(here::here("03-other-source-data", "nyt-us-states.csv"))


##### Get state meta data
state.meta <-
    readr::read_csv(here::here(fs::path("03-other-source-data", "state_table.csv")), skip=1) %>%
    select(-c(country, sort, status, occupied, id, fips_state, notes))

usa.state.corona <-
    usa.state.corona %>%
    left_join(state.meta, by = c("state" = "name")) %>%
    filter(date>ymd("2020-03-15"))

highlight <- usa.state.corona %>%
    filter(state %in% c("Connecticut"))

label.cut.states <-
    usa.state.corona %>%
    filter(date == max(date)) %>%
    select(state, cases, date) %>%
    arrange(desc(cases)) %>%
    pull(cases)
label.count.states <- 17 ## label the top third of states

caption.nyt <- paste("Data Source: https://github.com/nytimes/covid-19-data.",
                     "Figure by David Braze (davebraze@gmail.com) using R statistical software,",
                     "Released under the Creative Commons v4.0 CC-by license.", sep="\n")

## FIXME: need a version of this plot that shows cases per 100k population
usa.state.corona.plt <-
    ggplot(usa.state.corona) +
    geom_line(aes(x=date, y=cases, group=state),
              size=1, color="orange", alpha=1/3) +
    geom_line(data=highlight, aes(x=date, y=cases, group=state), ## highlight CT
              size=1, color="blue", alpha=.8) +
    ggrepel::geom_text_repel(data = subset(usa.state.corona,
                                           date == max(date) & cases >= label.cut.states[label.count.states]),
                             aes(label = assoc_press, x = date, y = cases),
                             segment.size=.25,
                             size=2.5,
                             hjust = 0,
                             direction="y",
                             force=1/4,
                             nudge_x=15) +
    scale_x_date(expand = expansion(mult=c(.01, .15)),
                 date_breaks = "1 month",
                 date_labels = "%b %d",
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases per U.S. State",
         subtitle=paste("Data compiled by the New York Times through",
                        format(max(usa.state.corona$date), "%B %d, %Y")),
         caption=caption.nyt) +
    geom_text_npc(aes(npcx=.1, npcy=.8, label="Connecticut in Blue"), color="blue", size=2.5) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    background_grid(major="xy") +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

usa.state.corona.cap <- paste("Cumulative Covid-19 Cases per U.S. State",
                              "The top", label.count.states, "states are labeled",
                              "(those with at least", formatC(label.cut.states[label.count.states], format="d", big.mark=","), "cases).")

ggsave(filename=fs::path_ext_set(paste0(today, "usa-rate"), ftype),
       plot=usa.state.corona.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)

####################################################
## Uni of Washington IMHE modeling data           ##
## http://www.healthdata.org/covid/data-downloads ##
####################################################

## from U of Washington
## https://covid19.healthdata.org/


#############################################
## Other data sets that may be of interest ##
#############################################

## from the CDC
## https://data.cdc.gov/browse?category=NCHS&sortBy=last_modified

## from the Johns Hopkins project
## https://github.com/CSSEGISandData/COVID-19

## for help with JHU data see
## https://github.com/strengejacke/Corona-19-shiny
## https://github.com/lorindavies/R_Downloadtoolkit_CSSEGISandData

## covid tracking project (USA)
## https://covidtracking.com/data

## Need Population data for CT Towns, CT Counties, nearby counties in
## adjacent states (plus maybe NJ)

