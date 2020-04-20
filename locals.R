theme_fdbplot <- function (font_size = 14,
                           font_family = "",
                           line_size = 0.5,
                           rel_small = 12/14,
                           rel_tiny = 11/14,
                           rel_large = 16/14) {
    ## custom ggplot theme using theme_cowplot() as a starting point

    cowplot::theme_cowplot(font_size = font_size,
                           font_family = font_family,
                           line_size = line_size,
                           rel_small = rel_small,
                           rel_tiny = rel_tiny,
                           rel_large = rel_large) +
        theme(plot.caption = element_text(size = rel(rel_tiny*.75), color = "grey50"))
}

my_limits <- function(limits) {
    ## force y range to include 0
    if(limits[1] > 0) return(c(0, limits[2]))
    else return(limits)
}

label_date <- function(variable, value) {
    ## formatted dates for use with ggplot2 facet_* functions
    strftime(unique(value), "%b %d")
}

read_ctcovid_pdf <- function(covid.fname, town.table=TRUE) {
    ## extract data from CT COVID-19 daily reports (PDF files).

    if(FALSE) {
        covid.fname <- covid.fnames[20]
        town.table=TRUE
    }

    ## extract date from file metadata
    meta <- tabulizer::extract_metadata(covid.fname)

    ## Using file creation date for this purpose is dodgy. Will fail if there is a delay in building the daily report
    date <- as.Date(lubridate::parse_date_time(meta$created, "a b! d! T* Y!", tz="EST"))

    covid.text <-
        tabulizer::extract_text(covid.fname,
                                pages=1:meta$pages) %>%
        str_remove_all("\r\n")
    names(covid.text) <- 1:length(covid.text)
    covid.text <- purrr::map_dfr(covid.text, words_to_numbers) %>%
        purrr::map_dfr(str_remove_all, "COVID-19")

    ## several variable values are given in prose on page 1.
    ## Clean it up before extracting them
    ## page1 <- words_to_numbers(str_remove_all(covid.text[[1]], "\r\n"))
    ## page1 <- str_remove_all(page1, "COVID-19")
    page1 <- covid.text[[1]]

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
    tests.complete <- str_extract(covid.text, "(more than|Patients tested for) +[0-9,]+")
    tests.complete <- max(as.integer(str_remove_all(tests.complete[!is.na(tests.complete)], "[^0-9]")))

    ct.tab <- tibble(
        Date = date,
        tests.complete = tests.complete,
        state.cases = state.cases,
        fatalities = fatalities,
        hospitalized = hospitalized
    )

    retval <- ct.tab

    if (town.table == TRUE) {
    ##### get cases-by-town data
        ## manually get page areas for town data table
        ## tabulizer::locate_areas(covid.fnames[19], pages=rep(10, 3))

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
        } else if(date<ymd("2020-04-08")) {
            area <- list(c(105,90,700,235),
                         c(105,236,700,379),
                         c(105,378,680,523))
        } else {
            area <- list(c(101,90,730,235),
                         c(101,236,730,379),
                         c(101,378,710,523))
        }

        town.tab <- tabulizer::extract_tables(covid.fname,
                                              pages=rep(page.tab, 3),
                                              area=area,
                                              guess=FALSE,
                                              output="data.frame")

        town.tab <- do.call(rbind,town.tab)  %>%
            rename(town.cases = Cases) %>%
            mutate(town.cases = as.numeric(town.cases),
                   Date = date) %>%
            left_join(ct.tab)
        retval <- town.tab
    }

    retval
}

