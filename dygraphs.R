library(dygraphs)

load('rankHistoryTop100.RData')

library(magrittr); library(dplyr)

best = rankHistoryAll %>%
  # filter(Singles<=50) %>%
  na.omit %>%
  droplevels %>%
  group_by(Player) %>%
  arrange(Date) # b/c layer paths is stupid

library(xts)
djo = filter(best, Player == 'Novak Djokovic') %>% data.frame %>%  select(Date, Singles) %$% xts(Singles, Date)
fed = filter(best, Player == 'Roger Federer')%>% data.frame  %>% select(Date, Singles) %$% xts(Singles, Date)
nad = filter(best, Player == 'Rafael Nadal') %>% data.frame %>% select(Date, Singles) %$% xts(Singles, Date)
murr = filter(best, Player == 'Andy Murray') %>% data.frame %>% select(Date, Singles) %$% xts(Singles, Date)

big4wide = merge(djo, nad, fed, murr)

dygraph(big4wide) %>%
  dyRangeSelector() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", drawGrid = FALSE, valueRange=c(max(big4wide, na.rm=T), -10)) %>%
  dyOptions(stepPlot = TRUE,stackedGraph = F, colors = RColorBrewer::brewer.pal(4, "Set2")) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)