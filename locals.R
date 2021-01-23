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
    ## force axis range to include 0
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
        covid.fname <- covid.fnames[29]
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
    covid.text <- purrr::map(covid.text, words_to_numbers) %>%
        purrr::map(str_remove_all, "COVID-19")

    page1 <- covid.text[[1]]

    ## get total lab-confirmed cases (cumulative) state wide from first page of report
    ## NOTE: On April 21, DPH changed reporting to include "probable" covid19 cases, per CDC recommendation
    state.cases <- str_extract(page1, "([0-9,]+ laboratory-confirmed cases|a total of [0-9,]+ cases of)")
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
        ## tabulizer::locate_areas(covid.fnames[28], pages=rep(7, 3))

        ## find page for cases-by-town table, assume town names occur only there
        page.tab <- which(str_detect(covid.text, "West Haven"))

        ## Data split across 3 "areas" on the page
        ## Handle several table format changes
        if(date < ymd("2020-04-05")){
            area <- list(c(80,67,730,205),
                         c(80,234,730,368),
                         c(80,400,720,540))
        } else if(date < ymd("2020-04-07")) {
            area <- list(c(105,82,730,225),
                         c(105,230,730,380),
                         c(105,385,720,500))
        } else if(date < ymd("2020-04-08")) {
            area <- list(c(105,90,700,235),
                         c(105,236,700,379),
                         c(105,378,680,523))
        } else if(date < ymd("2020-04-13")) {
            area <- list(c(94,88,722,235),
                         c(94,234,722,379),
                         c(94,376,700,523))
        } else if(date %in% ymd("2020-04-17")) {
            area <- list(c(86,91,692,234),
                         c(86,233,692,378),
                         c(86,377,672,523))
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


## format_table() cribbed from work for intelexia, llc
format_table <- function (content, out_type="html", digits=2, caption=" ", ## args to kable()
                          group_labels=NULL, group_starts=NULL, group_ends=NULL, ## args to pack_rows() via subtables()
                          ## args to kable_styling
                          bootstrap_options=c("striped", "condensed"),
                          latex_options=c("hold_position"),
                          full_width=FALSE,
                          font_size=10) {

    if(length(group_labels) != length(group_starts) ||
       length(group_starts) != (length(group_ends))) {
        stop("group_labels and group_starts and group_ends must all be the same length.")
    }

    subtables <- function(kable_table, group_labels, group_starts, group_ends) {
        if(is.null(group_labels)) return(kable_table)
        retval <- kable_table
        for (i in 1:length(group_labels)) {
            retval <- pack_rows(retval,
                                group_labels[i],
                                group_starts[i],
                                group_ends[i])
        }
        retval
    }

    if(out_type=="html") {
        ## format a table for html output.
        content %>%
            kable(format="html",
                  digits=digits,
                  caption=caption) %>%
            kable_styling(bootstrap_options=bootstrap_options,
                          full_width=full_width,
                          font_size=font_size)    %>%
            column_spec(1, bold=TRUE) %>%
            subtables (group_labels, group_starts, group_ends)
    } else if (out_type=="pdf") {
        ## format a table for pdf output via latex
        content %>%
            kable(format="latex",
                  digits=digits,
                  caption=caption,
                  booktabs=TRUE) %>%
            kable_styling(latex_options=latex_options,
                          full_width=full_width,
                          font_size=font_size)    %>%
            column_spec(1, bold=TRUE) %>%
            subtables (group_labels, group_starts, group_ends)
    } else if (out_type=="docx") {
        ## Mostly to get at least some output to docx files. NB. kableExtra::
        ## functions work only for html & latex.
        content %>%
            kable(format="pandoc", ## also consider "markdown"
                  digits=digits,
                  caption=caption)
    } else {
        ## used to suppress table printing.
        cat("format_table(): TABLE NOT PRINTED.")
    }
}

