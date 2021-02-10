library(tidyverse)
library(tidytext)
library(stringr)

lines <- read_lines("ct-covid.Rmd")

text <- tibble(line = lines) %>%
    mutate (line = str_replace_all(line, "[Cc]ovid-19", "covid19")) %>%
    unnest_tokens(word, line) %>%
    anti_join(stop_words)

## need to exclude yaml block, R code chunks, inline code, links, & references

text %>%
    count(word, sort=TRUE) %>%
    head(20)

