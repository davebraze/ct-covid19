library(httr)
library(here)
library(tabulizer)
library(stringr)
library(lubridate)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)


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



#####################################
## download CTDPH daily reports    ##
#####################################

## Manually download DPH daily reports at: https://portal.ct.gov/Coronavirus

## OR, look into writing routine to automate downloads.
## example url for daily COVID-19 update from CT DPH looks like this
## https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en
## Maybe use httr::modify_url to pass in different file names, as needed.

httr::parse_url("https://portal.ct.gov/-/media/Coronavirus/CTDPHCOVID19summary3282020.pdf?la=en")

#######################################
## Extract info from CTDPH pdf files ##
#######################################

## get list of ctdph covid reports at hand
covid.fnames <- fs::dir_ls(here::here("ctdph-daily-reports")) %>%
    str_subset("CTDPHCOVID19summary[0-9]+.pdf")

## filename for most recent covid-19 report
## covid.fname <- covid.fnames[length(covid.fnames)]

read_covid_data <- function(covid.fname) {
    ## extract date from file metadata
    meta <- tabulizer::extract_metadata(covid.fname)

    ## Using file creation date for this purpose is dodgy. Will fail if there is a delay in building the daily report
    date <- as.Date(lubridate::parse_date_time(meta$created, "a b! d! T* Y!", tz="EST"))

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
    ## tabulizer::locate_areas(covid.fnames[12], pages=rep(10, 3))

    area <- list(c(80,67,730,205), ## data split across 3 "areas" on the page
                 c(80,234,730,368),
                 c(80,400,720,540))

    tmp <- tabulizer::extract_tables(covid.fname,
                                     pages=rep(page, 3),
                                     area=area,
                                     guess=FALSE,
                                     output="data.frame")

    covid <- do.call(rbind,tmp)  %>%
        rename(`N Cases` = Cases) %>%
        mutate(`N Cases` = as.numeric(`N Cases`),
               `Log N Cases` = (log(`N Cases`)),
               Date = date,
               tests.complete = tests.complete)

    return(covid)
}



##### read all available covid reports

covid <- purrr::map_dfr(covid.fnames, read_covid_data)
covid[779, "Town"] <- "North Stonington" ## repair bad line from one input file
covid <- covid[-778,]

covid  <-
    covid %>%
    arrange(Date) %>%
    mutate(date.string = forcats::as_factor(strftime(Date, "%b %d")))

## Merge shapes covid data
ct.covid <-
    right_join(ct.shp, covid, by=c("NAME10" = "Town"))

#########################################################
## map cumulative confirmed case count by Town and Day ##
#########################################################

caption <- paste("Data Source: https://portal.ct.gov/Coronavirus.",
                 "Figure by David Braze (davebraze@gmail.com)",
                 "using R statistical software.", sep="\n")
breaks <- c(1, 3, 6, 12, 25, 50, 100, 200, 400)
ct.covid %>%
    ggplot() +
    geom_sf(aes(fill=`N Cases`), color="lightblue", size=.33) +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~date.string, ncol=4) +
    labs(title="Cumulative Lab Confirmed Covid-19 Cases per Connecticut Town",
         caption=caption) +
    theme_cowplot() +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())


#################################################################
## map cumulative confirmed case count by Town most recent day ##
#################################################################

breaks <- c(1, 3, 6, 12, 25, 50, 100, 200, 400)
ct.covid %>%
    filter(Date==max(Date)) %>%
    ggplot() +
    geom_sf(aes(fill=`N Cases`), color="lightblue", size=.33) +
    geom_sf_text(aes(label=`N Cases`), color="white") +
    scale_fill_continuous(type="viridis",
                          trans="log",
                          breaks=breaks,
                          labels=breaks) +
    guides(fill=guide_colorbar(barwidth=20,
                               title="Number of Cases",
                               title.vjust=1)) +
    facet_wrap(~date.string, ncol=4) +
    labs(title="Cumulative Lab Confirmed Covid-19 Cases per Connecticut Town",
         caption=caption) +
    xlab(NULL) + ylab(NULL) +
    theme_cowplot() +
    theme(legend.position="top",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

#######################
## rate plot by Town ##
#######################

x.labs <- unique(ct.covid$date.string)
ct.covid %>%
    filter(`N Cases` > 3) %>%
    ggplot() +
    geom_line(aes(x=Date, y=`N Cases`, group=NAME10),
              size=4/3, color="blue", alpha=1/3) +
    geom_text(data = subset(ct.covid, date.string == "Apr 01" & `N Cases` > 75),
              aes(label = NAME10, x = Date, y = `N Cases`),
              hjust = -.1) +
    scale_x_date(labels=x.labs,
                 breaks=unique(ct.covid$Date),
                 expand = expansion(add=c(0,4/3)),
                 name=NULL) +
    ## scale_y_continuous(trans="log",
    ##                    breaks=c(3,6,12, 25,50,100,200,400)) +
    labs(title="Cumulative Lab Confirmed Covid-19 Cases per Connecticut Town",
         caption=caption) +
   ylab("Number of Cases") +
   coord_equal(ratio =.02) +
    theme_cowplot() +
    theme(legend.position="top",
          plot.margin = unit(c(1,6,1,1), "lines"))

