I've built some visualizations of Connecticut COVID-19 data, partly because I wasn't happy with others that I'd seen. You'll find a webpage with some of my graphics at [davebraze.github.io/ct-covid19](https://davebraze.github.io/ct-covid19/). A GitHub repo with R code used to make the graphs is at [github.com/davebraze/ct-covid19](https://github.com/davebraze/ct-covid19). The data are (mostly) pulled from the state's [open data portal](https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc/#data-library) using the Socrata API and the RSocrata R package.

I built these graphs for a couple of reasons. On the one hand, they are an excuse to play with several R features that I wanted to become more familiar with: scraping data from PDF files ([tabulizer](https://docs.ropensci.org/tabulizer/)), using an API for data access ([RSocrata](https://github.com/Chicago/RSocrata)), working with geo-linked data and building choropleths ([sf](https://r-spatial.github.io/sf/)), and animated dataviz. Haven't got around to the animations yet, but you'll find examples of the other stuff in this repo. Another reason for making these graphs, as already mentioned, is that I was not satisfied with other visualizations of Connecticut's COVID-19 data that are available. There are links to some of those below. All are based on the same Connecticut DPH data as my own dataviz.

## Other Takes on CT COVID-19 Data
* The State of Connecticut's own portal for COVID-19 information is at [data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc](https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc). At the bottom of that page, you'll find links to the state's COVID-19 datasets, which are the source of the numbers for my own visualizations.
* Connecticut's [DataHaven](https://www.ctdatahaven.org) has created a "live" report of Connecticut COVID-19 data. It can be found at [www.ctdatahaven.org/reports/covid-19-connecticut-data-analysis](https://www.ctdatahaven.org/reports/covid-19-connecticut-data-analysis).
* The [Connecticut Data Collaborative](https://www.ctdata.org) has created an interactive dashboard of Connecticut COVID-19 data. It is available at [www.ctdata.org/covid19](https://www.ctdata.org/covid19).

## WARNING 

I am not even close to being an epidemiologist or infectious disease expert. If you're looking for an expert take on anything to do with COVID-19, then you're in the wrong place.
