library(tidyverse)
library(tidytext)

lines <- read_lines("ct-covid.Rmd")

text <- tibble(line = lines) %>%


    unnest_tokens(word, line) %>%
    anti_join(stop_words)

## need to exclude yaml block and R code chunks

text %>%
    count(word, sort=TRUE) %>%
    head(20)

