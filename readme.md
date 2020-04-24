# CT-COVID19

This is code for visualizations of Connecticut COVID-19 data. The data are (mostly) pulled from the state's [open data portal](https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc/#data-library) using the Socrata API and the RSocrata R package.

I built these visualizations for a couple of reasons. On the one hand, they are an excuse to play with several R features that I wanted to get more familiar with: scraping data from PDF files ([tabulizer](https://docs.ropensci.org/tabulizer/)), using an API for data access ([RSocrata](https://github.com/Chicago/RSocrata)), working with geo-linked data and building choropleths ([sf](https://r-spatial.github.io/sf/)), and animated dataviz. Haven't got around to the animations yet, but you'll find examples of the other stuff in this repo. Another reason for making these graphs is that I was not satisfied with other vizualizations of Connecticut's COVID-19 data that I had seen. There are links to some below. All are based on the same state DPH data, as my own dataviz.

## Other Takes on Connecticut COVID-19 Data
* The State of Connecticut's own portal for COVID-19 data is [here](https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc). At the bottom of that page, you'll find links to the state's COVID-19 datasets, which are the source of the numbers for my own visualizations. 
* The [Connecticut Data Collaborative](https://www.ctdata.org) has created an interactive dashboard of Connecticut COVID-19 data. It is available [here](https://www.ctdata.org/covid19). 
* Connecticut's [DataHaven](https://www.ctdatahaven.org) has created a "live" report of Connecticut COVID-19 data. It can be found [here](https://www.ctdatahaven.org/reports/covid-19-connecticut-data-analysis).

## WARNING 

I am not even close to being an epidemiologist or contagious disease expert. If you're looking for an expert take on anything to do with COVID-19, then you're in the wrong place.


