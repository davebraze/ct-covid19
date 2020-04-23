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

## ##### read all available covid pdfs
## town.tab <- TRUE
## covid <- purrr::map_dfr(covid.fnames[1:19], read_ctcovid_pdf, town.tab=town.tab)

## if (town.tab == TRUE){
##     covid[779, "Town"] <- "North Stonington" ## repair bad line from one input file
##     covid <- covid[-778,]
## }

## covid  <-
##     covid %>%
##     arrange(Date) %>%
##     mutate(date.string = as_factor(strftime(Date, "%b %d")))

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

##### cases and deaths by town
covid.api <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                          app_token=socrata.app.token) %>%
    rename(Town = town,
           case.rate = caserate) %>%
    mutate(town.cases = as.integer(confirmedcases), ## cumulative confirmed cases
           town.deaths = as.integer(deaths),  ## cumulative deaths
           case.rate = as.integer(case.rate), ## don't know what this means
           Date = as.Date(lastupdatedate)) %>%
    select(-c(town_no, lastupdatedate, confirmedcases, deaths))

##### scrape town/county data from wikipedia

url <- "https://en.wikipedia.org/wiki/List_of_towns_in_Connecticut"
town.info <- GET(url) %>%
    htmlParse() %>%
    readHTMLTable(header=TRUE, which=2, skip=170) %>%
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
           town.deaths.10k = (10000/pop.2010)*town.deaths) %>%
    left_join(ct.shp, by=c("Town" = "NAME10"))

##### state wide counts
## tests.complete info does not seem to be anywhere in any of the
## covid-19 data sets provided by the state. The only way to get it is
## to extract it from their daily reports (pdf files).
ct.summary <- read.socrata("https://data.ct.gov/resource/rf3k-f8fg.json",
                           app_token=socrata.app.token) %>%
    rename(Date = date,
           Cases = cases,
           Hospitalized = hospitalizations,
           Deaths = deaths) %>%
    mutate(Date = as.Date(Date)) %>%
    select(-state, -starts_with("cases_")) %>%
    mutate(Cases = as.integer(Cases),
           Hospitalized = as.integer(Hospitalized),
           Deaths = as.integer(Deaths))

tmp <- purrr::map_dfr(covid.fnames, read_ctcovid_pdf, town.tab=FALSE) %>%
    select(Date, tests.complete)

ct.summary <- left_join(ct.summary, tmp)%>%
    tidyr::pivot_longer(cols=-c(Date)) %>%
    mutate(name = forcats::fct_relevel(name, "Cases", "Hospitalized", "Deaths"))

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

#########################################################
## map cumulative confirmed case count by Town and Day ##
#########################################################

breaks <- c(1, 3, 6, 12, 25, 50, 100, 200, 400, 800, 1600)

map.days <-
    ct.covid %>%
    ggplot() +
    geom_sf(aes(fill=town.cases, geometry=geometry), color="lightblue", size=.33) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~Date,
               ncol=5,
               labeller=label_date) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

ggsave(filename=fs::path_ext_set(paste0(today, "map-days"), ftype),
       plot=map.days,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

#################################################################
## map cumulative confirmed case count by Town most recent day ##
#################################################################

map.today <-
    ct.covid %>%
    filter(Date==max(Date)) %>%
    ggplot() +
    geom_sf(aes(fill=town.cases, geometry=geometry), color="lightblue", size=.33) +
    geom_sf_text(aes(label=town.cases, geometry=geometry), color="white", size=2) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~Date,
               ncol=4,
               labeller=label_date) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    xlab(NULL) + ylab(NULL) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

ggsave(filename=fs::path_ext_set(paste0(today, "map-today"), ftype),
       plot=map.today,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

#######################
## rate plot by Town ##
#######################

label.cut <-
    ct.covid %>%
    filter(Date == max(Date)) %>%
    select(Town, town.cases, Date) %>%
    arrange(desc(town.cases)) %>%
    pull(town.cases)
label.count <- 17

highlight <- ct.covid %>%
    filter(Town %in% c(""))

town.rate.plt <-
    ct.covid %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases, group=Town),
              size=4/3, alpha=1/2, color="blue") +
    geom_line(data=highlight, aes(x=Date, y=town.cases, group=Town),
              size=1/2, color="darkorange", alpha=1) +
    ggrepel::geom_text_repel(data = subset(ct.covid,
                                           Date == max(Date) & town.cases >= label.cut[label.count]),
                             aes(label = Town, x = Date, y = town.cases),
                             segment.size=.25,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_color_brewer(type="qual", palette="Dark2") +
    scale_x_date(labels=strftime(unique(ct.covid$Date), "%b %d"),
                 breaks=unique(ct.covid$Date),
                 expand = expansion(add=c(1/4,3)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    geom_text_npc(aes(npcx=.1, npcy=.9,
                      label=paste0("The top ", label.count, " towns are labeled\n",
                                   "(those with at least ", label.cut[label.count], " cases)")),
                  size=2.5) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-rate"), ftype),
       plot=town.rate.plt,
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
              size=4/3, alpha=1/2) +
    facet_wrap(~county, nrow=4, scales="free_y") +
    ggrepel::geom_text_repel(data = label.cut,
                             aes(label = Town, x = Date, y = town.cases),
                             segment.size=.25,
                             min.segment.length = .01,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_color_brewer(type="qual", palette="Dark2", guide=FALSE) +
    scale_y_continuous(limits=my_limits) +
    scale_x_date(labels=strftime(unique(ct.covid$Date), "%b %d"),
                 breaks=unique(ct.covid$Date),
                 expand = expansion(add=c(1/4,3)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns, split by county",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    geom_text_npc(aes(npcx=.1, npcy=.9,
                      label=paste0("The top ", label.count, " towns/county are labeled.",
                                  "\nNote differing y scales for each county.")),
                  size=2.5) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "ct-town-by-county-rate"), ftype),
       plot=town.by.county.rate.plt,
       path=fig.path,
       device=ftype,
       width=width*16/9, height=height,
       units=units,
       dpi=dpi)

########################################################
## line plot for daily change in core statistics.     ##
## Includes 3 day running average                     ##
########################################################

ct.hosp.rate.plt  <-
    ct.summary %>%
    group_by(name) %>%
    mutate(value.diff = c(NA, diff(value)),
           value.diff.3mn = TTR::runMean(value.diff,3)) %>%
    ggplot(aes(x=Date)) +
    geom_line(aes(y=value.diff, group=name, color=name), size=2, alpha=.50) +
    geom_line(aes(y=value.diff.3mn, group=name, color=name), size=1, alpha=1, linetype="52") +
    geom_text(aes(y=value.diff, group=name, label=value.diff), size=2) +
    scale_x_date(labels=strftime(unique(ct.summary$Date), "%b %d"),
                 breaks=unique(ct.summary$Date),
                 name=NULL) +
    scale_y_continuous(limits=my_limits) +
    scale_color_brewer(type="qual", palette="Dark2", guide=FALSE) +
    facet_wrap(~name, scales="free_y") +
    geom_text_npc(aes(npcx=.067, npcy=.9,
                  label=paste("Heavy line = raw counts.",
                               "Dashed line = 3 day average.",
                               "\nNote different y scales.",
                               sep="\n")),
                  size=2.5) +
    labs(title="Daily Change in Covid-19 Statistics for Connecticut",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    ylab("Count") + xlab(NULL) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position=c(.05, .90),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary-rate"), ftype),
       plot=ct.hosp.rate.plt,
       path=fig.path,
       device=ftype,
       width=width*(16/9), height=height,
       units=units,
       dpi=dpi)

##################################
## bar plot for state-wide data ##
##################################

ct.summary.plt <-
    ct.summary %>%
    filter(!name %in% c("tests.complete")) %>%
    mutate(name = fct_recode(name, "Cases, cumulative" = "Cases",
                             "Hospitalized, daily" = "Hospitalized",
                             "Deaths, cumulative" = "Deaths")) %>%
    ggplot(aes(y=value, x=Date)) +
    geom_bar(aes(fill=name), position="dodge", stat="identity") +
    scale_fill_brewer(type="qual", palette="Dark2") +
    scale_x_date(expand = expansion(add=c(1/4, 1/4)),
                 labels=strftime(unique(ct.summary$Date), "%b %d"),
                 breaks=unique(ct.summary$Date),
                 name=NULL) +
    geom_text(aes(color=name, label=value),
              size=2,
              position=position_dodge(width=1),
              vjust=.5, hjust=-.1,
              angle=90,
              show.legend=FALSE) +
    scale_color_brewer(type="qual", palette="Dark2") +
    geom_text(data=filter(ct.summary, name=="tests.complete"),
              aes(label=value, x=Date, y=-150), color="grey70", size=2.25) +
    geom_text_npc(aes(npcx=.05, npcy=.85, label="Cumulative No. of tests administered"),
                  size=3,
                  color="grey70") +
    ylim(-150, NA) +
    guides(fill=guide_legend(title=NULL)) +
    labs(title="Covid-19 Cases, Hospitalizations, & Deaths for Connecticut",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption.ctdph) +
    ylab("Count") + xlab(NULL) +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position=c(.05, .90),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary"), ftype),
       plot=ct.summary.plt,
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
usa.state.corona <-
    getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv?accessType=DOWNLOAD") %>%
    readr::read_csv()

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

label.cut <-
    usa.state.corona %>%
    filter(date == max(date)) %>%
    select(state, cases, date) %>%
    arrange(desc(cases)) %>%
    pull(cases)
label.count <- 17 ## label the top third of states

caption.nyt <- paste("Data Source: https://github.com/nytimes/covid-19-data.",
                     "Figure by David Braze (davebraze@gmail.com) using R statistical software,",
                     "Released under the Creative Commons v4.0 CC-by license.", sep="\n")

usa.state.corona.plt <-
    ggplot(usa.state.corona) +
    geom_line(aes(x=date, y=cases, group=state),
              size=4/3, color="orange", alpha=1/3) +
    geom_line(data=highlight, aes(x=date, y=cases, group=state), ## highlight CT
              size=1, color="blue", alpha=.8) +
    ggrepel::geom_text_repel(data = subset(usa.state.corona,
                                           date == max(date) & cases >= label.cut[label.count]),
                             aes(label = assoc_press, x = date, y = cases),
                             segment.size=.25,
                             size=2,
                             hjust = 0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_x_date(expand = expansion(add=c(1/4, 4)),
                 breaks = unique(usa.state.corona$date),
                 labels = strftime(unique(usa.state.corona$date), "%b %d"),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases per U.S. State",
         subtitle="Data compiled by the New York Times",
         caption=caption.nyt) +
    geom_text_npc(aes(npcx=.1, npcy=.9,
                      label=paste0("The top ", label.count, " states are labeled\n",
                                   "(those with at least ", label.cut[label.count], " cases)")),
                  size=2.5) +
    geom_text_npc(aes(npcx=.1, npcy=.8, label="Connecticut in Blue"), color="blue", size=2.5) +
    ylab("Number of Cases") +
    theme_fdbplot(font_size=font.size) +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

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

