# https://trends.google.com/trends/explore?date=2015-01-01%202016-12-01&geo=US&q=Lord%20of%20the%20Rings,%2Fm%2F0fdv3&hl=en
# https://trends.google.com/trends/explore?date=2015-09-01%202016-01-02&geo=US&q=Lord%20of%20the%20Rings,%2Fm%2F0fdv3&hl=en
library(data.table)
library(ggplot2)
library(magrittr)
library(lubridate)

data <- fread('chapter 11/multiTimeline.csv') %>% 
  melt(id.vars='Week', variable.name ='keyword', value.name = 'hits') %>% 
  .[, month := floor_date(Week, 'month')] %>% 
  .[, .(hits = mean(hits)), .(keyword, month)]
ggplot(data, aes(month, hits, group=keyword, color=keyword)) +
  geom_line()

