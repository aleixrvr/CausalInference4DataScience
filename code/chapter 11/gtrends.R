library(gtrendsR)
library(ggplot2)
library(data.table)
library(magrittr)
library(lubridate)

dates <- "2015-10-01 2016-02-01"

star_wars <- gtrends(
  keyword="Star Wars",
  time=dates,
  onlyInterest = TRUE
)$interest_over_time %>% as.data.table

control <- gtrends(
  keyword="Lord of the Rings",
  time=dates,
  onlyInterest = TRUE,
  geo = 'US'
)$interest_over_time %>% as.data.table

T_week <- week(as.Date("2015-12-18"))

dt <- rbind(star_wars, control) %>% 
  .[, week := week(date)] %>% 
  .[, week := ifelse(year(date) == "2016", week + 53, week) - T_week] %>% 
  .[, .(hits = mean(hits)), .(keyword, week)]

ggplot(dt,
       aes(week, hits, group=keyword, color=keyword)) +
  geom_line()


