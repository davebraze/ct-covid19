## library(httr)
library(here)
library(fs)
library(RCurl)
library(readr)
library(sf)

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

#######################
## load up map files ##
#######################

##### GIS data for CT is available here:
## http://magic.lib.uconn.edu/connecticut_data.html
##
## Must first download the relevant shape files by hand, unzip them, and then load them up.
## URL for town shape files is here: http://magic.lib.uconn.edu/magic_2/vector/37800/townct_37800_0000_2010_s100_census_1_shp.zip
ct.shp <-
    sf::st_read(here::here("02-shapefiles/townct_37800_0000_2010_s100_census_1_shp/townct_37800_0000_2010_s100_census_1_shp/nad83",
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

httr::parse_url("https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en")

#######################################
## Extract info from CTDPH pdf files ##
#######################################

## get list of ctdph covid reports at hand
covid.fnames <- fs::dir_ls(here::here("01-ctdph-daily-reports")) %>%
    str_subset("CTDPHCOVID19summary[0-9]+.pdf")

read_ctcovid_data <- function(covid.fname) {
    ## extract date from file metadata
    meta <- tabulizer::extract_metadata(covid.fname)

    ## Using file creation date for this purpose is dodgy. Will fail if there is a delay in building the daily report
    date <- as.Date(lubridate::parse_date_time(meta$created, "a b! d! T* Y!", tz="EST"))

    covid.text <- tabulizer::extract_text(covid.fname,
                                          pages=1:meta$pages)

    ## several variable values are given in prose on page 1.
    ## Clean it up before extracting them
    page1 <- words_to_numbers(str_remove_all(covid.text[[1]], "\r\n"))
    page1 <- str_remove_all(page1, "COVID-19")

    ## get total lab-confirmed cases (cumulative) state wide from first page of report
    state.cases <- str_extract(page1, "[0-9,]+ laboratory-confirmed cases")
    state.cases <- as.integer(str_remove_all(state.cases[!is.na(state.cases)], "[^0-9]"))
    ## get total number of fatalities (cumulative) state wide from first page of report
    fatalities <- str_extract(page1, "[0-9]+[^0-9]*(died|deaths)")   ## "[0-9]+[^0-9]*died")
    fatalities <- as.integer(str_remove_all(fatalities[!is.na(fatalities)], "[^0-9]"))
    ## get current number hospitalized state-wide from first page of report
    hospitalized <- str_extract(page1, "[0-9]+[^0-9]*hospitalized")
    hospitalized <- as.integer(str_remove_all(hospitalized[!is.na(hospitalized)], "[^0-9]"))
    ## get approx Number of lab tests completed (cumulative)
    tests.complete <- str_extract(covid.text, "more than [0-9,]+")
    tests.complete <- as.integer(str_remove_all(tests.complete[!is.na(tests.complete)], "[^0-9]"))

    ##### get cases-by-town data
    ## manually get page areas for town data table
    ## tabulizer::locate_areas(covid.fnames[18], pages=rep(10, 3))

    ## find page for cases-by-town table, assume town names occur only there
    page.tab <- which(str_detect(covid.text, "West Haven"))

    ## data split across 3 "areas" on the page
    ## note table format change starting Apr. 5
    if(date<ymd("2020-04-05")){
        area <- list(c(80,67,730,205),
                     c(80,234,730,368),
                     c(80,400,720,540))
    } else if(date<ymd("2020-04-07")) {
        area <- list(c(105,82,730,225),
                     c(105,230,730,380),
                     c(105,385,720,500))
    } else {
        area <- list(c(105,90,700,235),
                     c(105,236,700,379),
                     c(105,378,680,523))
    }

    tab <- tabulizer::extract_tables(covid.fname,
                                     pages=rep(page.tab, 3),
                                     area=area,
                                     guess=FALSE,
                                     output="data.frame")
    retval <- do.call(rbind,tab)  %>%
        rename(town.cases = Cases) %>%
        mutate(town.cases = as.numeric(town.cases),
               Date = date,
               tests.complete = tests.complete,
               state.cases = state.cases,
               fatalities = fatalities,
               hospitalized = hospitalized)
    retval
}

##### read all available covid reports
covid <- purrr::map_dfr(covid.fnames, read_ctcovid_data)
covid[779, "Town"] <- "North Stonington" ## repair bad line from one input file
covid <- covid[-778,]

covid  <-
    covid %>%
    arrange(Date) %>%
    mutate(date.string = as_factor(strftime(Date, "%b %d")))

## Merge shapes covid data
ct.covid <-
    right_join(ct.shp, covid, by=c("NAME10" = "Town"))

#########################
## constants for plots ##
#########################
caption <- paste("Data Source: https://portal.ct.gov/Coronavirus.",
                 "Figure by David Braze (davebraze@gmail.com)",
                 "using R statistical software.", sep="\n")

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

breaks <- c(1, 3, 6, 12, 25, 50, 100, 200, 400, 800)
map.days <-
    ct.covid %>%
    ggplot() +
    geom_sf(aes(fill=town.cases), color="lightblue", size=.33) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~date.string, ncol=5) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption) +
    theme_cowplot(font_size=font.size) +
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
    geom_sf(aes(fill=town.cases), color="lightblue", size=.33) +
    geom_sf_text(aes(label=town.cases), color="white", size=2) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~date.string, ncol=4) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption) +
    xlab(NULL) + ylab(NULL) +
    theme_cowplot(font_size=font.size) +
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

x.labs <- unique(ct.covid$date.string)
rate.plt <-
    ct.covid %>%
    filter(town.cases > 3) %>%
    ggplot() +
    geom_line(aes(x=Date, y=town.cases, group=NAME10),
              size=4/3, color="blue", alpha=1/3) +
    ggrepel::geom_text_repel(data = subset(ct.covid,
                                           Date == max(Date) & town.cases > 85),
                             aes(label = NAME10, x = Date, y = town.cases),
                             segment.size=.25,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_x_date(labels=x.labs,
                 breaks=unique(ct.covid$Date),
                 expand = expansion(add=c(1/4,3)),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases for Connecticut's 169 Towns",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption) +
    ylab("Number of Cases") +
    coord_equal(ratio =.02) +
    theme_cowplot(font_size=font.size) +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "rate"), ftype),
       plot=rate.plt,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

##################################
## bar plot for state-wide data ##
##################################

## reorganize the data
ct.summary <-
    covid %>%
    select(-c(town.cases, Town)) %>%
    group_by(Date) %>%
    summarize(date=unique(Date),
              date.string=unique(date.string),
              tests.complete=unique(tests.complete),
              Cases=unique(state.cases),
              Hospitalized=unique(hospitalized),
              Deaths=unique(fatalities)) %>%
    select(-Date) %>%
    tidyr::pivot_longer(cols=-c(date, date.string)) %>%
    mutate(name = as_factor(name))

ct.summary.plt <-
    ct.summary %>%
    filter(!name=="tests.complete") %>%
    ggplot(aes(y=value, x=date.string)) +
    geom_bar(aes(fill=name),
             position="dodge", stat="identity") +
    geom_text(aes(color=name, label=value),
              size=2,
              position=position_dodge(width=1),
              vjust=.5, hjust=-.1,
              angle=90,
              show.legend=FALSE) +
    geom_text(data=filter(ct.summary, name=="tests.complete"),
              aes(label=value, x=date.string, y=-150), color="grey70", size=2.25) +
    geom_text_npc(aes(npcx=.05, npcy=.85, label="Cumulative No. of tests each day"),
                  size=3,
                  color="grey70") +
    ylim(-150, NA) +
    guides(fill=guide_legend(title=NULL)) +
    labs(title="Covid-19 Cases, Hospitalizations, & Deaths for Connecticut",
         subtitle="Data compiled by CT Dept. of Public Health",
         caption=caption) +
    ylab("Count") + xlab(NULL) +
    theme_cowplot(font_size=font.size) +
    theme(legend.position=c(.05, .90),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "ct-summary"), ftype),
       plot=ct.summary.plt,
       path=fig.path,
       device=ftype,
       width=width, height=height,
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

usa.state.corona <- dplyr::left_join(usa.state.corona, state.meta, by=c("state" = "name")) %>%
    mutate(state = fct_relevel(as_factor(state), "Connecticut"),
           date.string = as_factor(strftime(date, "%b %d")))

caption <- paste("Data Source: https://github.com/nytimes/covid-19-data.",
                 "Figure by David Braze (davebraze@gmail.com)",
                 "using R statistical software.", sep="\n")

tmp <-
    usa.state.corona %>%
    filter(date>ymd("2020-03-15"))

tmp.ct <- tmp %>%
    filter(state=="Connecticut")

usa.state.corona.plt <-
    ggplot(tmp) +
    geom_line(aes(x=date, y=cases, group=state),
              size=4/3, color="orange", alpha=1/3) +
    geom_line(data=tmp.ct, aes(x=date, y=cases, group=state), ## highlight CT
              size=1, color="blue", alpha=.8) +
    ggrepel::geom_text_repel(data = subset(usa.state.corona,
                                           date == max(date) & cases > 3500),
                             aes(label = assoc_press, x = date, y = cases),
                             segment.size=.25,
                             size=2,
                             hjust = -0,
                             direction="y",
                             force=1/4,
                             nudge_x=5) +
    scale_x_date(expand = expansion(add=c(1/4,6)),
                 breaks= unique(tmp$date),
                 labels = unique(tmp$date.string),
                 name=NULL) +
    labs(title="Cumulative Covid-19 Cases per U.S. State",
         subtitle="Data compiled by the New York Times",
         caption=caption) +
    geom_text_npc(aes(npcx=.1, npcy=.9, label="Connecticut in Blue"), color="blue") +
    ylab("Number of Cases") +
    theme_cowplot(font_size=font.size) +
    theme(legend.position="top",
          plot.margin = unit(c(1,1,1,1), "lines"),
          axis.text.x = element_text(angle=45, hjust=1))

ggsave(filename=fs::path_ext_set(paste0(today, "usa-rate"), ftype),
       plot=usa.state.corona.plt,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

####################################################
## Uni of Washington IMHE modeling data           ##
## http://www.healthdata.org/covid/data-downloads ##
####################################################


