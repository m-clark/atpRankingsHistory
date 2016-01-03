library(dygraphs)

load('rankHistoryTop100.RData')

library(magrittr); library(dplyr)

big4 = rankHistoryAll %>%
  na.omit %>%
  droplevels %>%
  group_by(Player) %>%
  filter(Player %in% c('Novak Djokovic', 'Andy Murray', 'Roger Federer', 'Rafael Nadal')) %>%
  arrange(Date) # b/c layer paths is stupid

library(xts)
djo = filter(big4, Player == 'Novak Djokovic') %>% data.frame %>%  select(Date, Singles) %$% xts(Singles, Date)
fed = filter(big4, Player == 'Roger Federer')%>% data.frame  %>% select(Date, Singles) %$% xts(Singles, Date)
nad = filter(big4, Player == 'Rafael Nadal') %>% data.frame %>% select(Date, Singles) %$% xts(Singles, Date)
murr = filter(big4, Player == 'Andy Murray') %>% data.frame %>% select(Date, Singles) %$% xts(Singles, Date)

big4wide = merge(djo, nad, fed, murr)

dygraph(big4wide) %>%
  dyRangeSelector() %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", drawGrid = FALSE, valueRange=c(max(big4wide, na.rm=T), -10)) %>%
  dyOptions(stepPlot = TRUE, stackedGraph = F, colors = RColorBrewer::brewer.pal(4, "Set2")) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)
