library(httr)
library(here)
library(tabulizer)
library(stringr)
library(lubridate)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)

## example of url for daily COVID-19 update from CT DPH, with breakdown by town.
##
## https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en

httr::parse_url("https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en")
## Use modify_url to pass in different file names to get new daily reports.

##### GIS data for CT is available here:
## http://magic.lib.uconn.edu/connecticut_data.html
##
## Must first download the relevant shape files by hand, unzip them, and then load them up.
## URL for town shape files is here: http://magic.lib.uconn.edu/magic_2/vector/37800/townct_37800_0000_2010_s100_census_1_shp.zip
ct.shp <-
    sf::st_read(here::here("shapefiles/townct_37800_0000_2010_s100_census_1_shp/townct_37800_0000_2010_s100_census_1_shp/nad83",
                           "townct_37800_0000_2010_s100_census_1_shp_nad83_feet.shp")) %>%
    filter(NAME10 != "County subdivisions not defined") %>%
    mutate(LAT = as.numeric(INTPTLAT10),
           LON = as.numeric(INTPTLON10))

##### extract info from file

## get list of ctdph covid reports at hand
covid.fnames <- fs::dir_ls(".") %>%
    str_subset("CTDPHCOVID19summary[0-9]+.pdf")

## filename for most recent covid-19 report
## covid.fname <- covid.fnames[length(covid.fnames)]

read_covid_data <- function(covid.fname){
    ## extract date from file metadata
    meta <- tabulizer::extract_metadata(covid.fname)
    date <- lubridate::parse_date_time(meta$created, "a b! d! T* Y!")

    ##### find page with town data
    covid.text <- tabulizer::extract_text(covid.fname,
                                          pages=1:meta$pages)
    ## find page for cases-by-town table, assume town names occur only there
    page <- which(str_detect(covid.text, "West Haven"))

    ## find approx Number of lab tests completed
    tests.complete <- str_extract(covid.text, "more than [0-9,]+")
    tests.complete <- as.integer(str_remove_all(tests.complete[!is.na(tests.complete)], "[^0-9]"))

    ##### get cases-by-town data
    ## manually get page areas for town data table
    ## tabulizer::locate_areas(covid.fname.pdf, pages=rep(pages, 3))

    tmp <- tabulizer::extract_tables(covid.fname,
                                     pages=rep(page, 3),
                                     area=list(c(80,67,740,205), ## data split across 3 "areas" on the page
                                               c(80,234,740,368),
                                               c(80,400,740,540)),
                                     guess=FALSE,
                                     output="data.frame")
    covid <- do.call(rbind,tmp)  %>%
        rename(`N Cases` = Cases) %>%
        mutate(`N Cases` = as.numeric(`N Cases`),
               `Log N Cases` = (log(`N Cases`)),
               Date = date,
               date.string = strftime(Date, "%B %d, %Y"),
               tests.complete = tests.complete)

    return(covid)
}

##### read all available covid reports
covid <- purrr::map_dfr(covid.fnames, read_covid_data)
covid[779, "Town"] <- "North Stonington" ## repair bad line from one input file
covid <- covid[-778,]

## Merge shapes covid data
ct.covid <-
    right_join(ct.shp, covid, by=c("NAME10" = "Town"))

ct.covid %>%
##    filter(strftime(.$Date, "%B %d, %Y") == "March 21, 2020") %>%
    ggplot() +
    geom_sf(aes(fill=`Log N Cases`), color="lightblue", size=.33) +
    scale_fill_continuous(type="viridis") +
    ## geom_sf_text(aes(label=NAME10),
    ##              color="white",
    ##              size=3) +
    facet_wrap(~date.string, nrow=3) +
    labs(title="Lab Confirmed Covid-19 Cases per Connecticut Town",
##         subtitle=strftime(Date, "%B %d, %Y"),
         caption="Data Source: https://portal.ct.gov/Coronavirus") +
    ##    ylab("latitude") + xlab("longitude") +
    theme_cowplot() +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
