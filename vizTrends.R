load('rankHistory.RData')

library(magrittr); library(ggvis);library(dplyr)

big4 = dplyr::filter(rankHistoryAll, Player %in% c('Novak Djokovic', 'Rafael Nadal',
                                                   'Roger Federer', 'Andy Murray')) %>%
  na.omit %>%
  droplevels %>%
  group_by(Player) %>%
  arrange(Date) # b/c layer paths is stupid

load('rankHistoryNo1s.RData')
rankHistoryNo1s %<>%
  na.omit %>%
  droplevels %>%
  group_by(Player) %>%
  arrange(Date)


rankHistoryAll %>%
  filter() %>%
  ggvis(~Date, ~Singles) %>%
  group_by(Player) %>%
  layer_paths(strokeOpacity:=.05) %>%
  layer_paths(stroke=~Player, strokeOpacity:=1, data=big4) %>%
  scale_numeric("y", reverse=T) %>%
  scale_nominal("stroke", range=RColorBrewer::brewer.pal(4, 'Set2')) %>%
  add_axis('x', grid=F,
           properties = axis_props(
             labels = list(
               fill = "black",
               fillOpacity = .5,
               angle = 45,
               fontSize = 14,
               align = "left",
               baseline = "middle",
               dx = 3),
             title = list(fill = "black",
                          fillOpacity = .75,
                          dy=20),
             axis = list(stroke = "black",
                         strokeOpacity = .5)
  )) %>%
  add_axis('y', grid=F, title='Singles Ranking', title_offset=50, format='####',
           properties = axis_props(
             labels = list(
               fill = "black",
               fillOpacity = .5,
               fontSize = 14,
               dx = 3),
             title = list(fill = "black",
                          fillOpacity = .75),
             axis = list(stroke = "black",
                strokeOpacity = .5)
  ))


# all of them; fairly ugly
# rankHistoryNo1s %>%
#   filter(Singles<=100) %>%
#   ggvis(~Date, ~Singles) %>%
#   group_by(Player) %>%
#   layer_paths(stroke=~Player, strokeOpacity:=.8) %>%
#   scale_numeric("y", reverse=T) %>%
#   add_axis('x', grid=F, properties = axis_props(
#     labels = list(
#       fill = "black",
#       fillOpacity = .5,
#       angle = 45,
#       fontSize = 14,
#       align = "left",
#       baseline = "middle",
#       dx = 3),
#     title = list(fill = "black",
#                  fillOpacity = .75,
#                  dy=20),
#     axis = list(stroke = "black",
#                 strokeOpacity = .5)
#   )) %>%
#   add_axis('y', grid=F, properties = axis_props(
#     labels = list(
#       fill = "black",
#       fillOpacity = .5,
#       fontSize = 14,
#       dx = 3),
#     title = list(fill = "black",
#                  fillOpacity = .75),
#     axis = list(stroke = "black",
#                 strokeOpacity = .5)
#   ))



no1Ranks %>%
  ggvis(~`Total Weeks At No. 1`, ~`Max Consecutive Weeks At No. 1`) %>%
  layer_text(text:=~Player, stroke:=NA, fill=~Player, fillOpacity=~Slams, fontSize=~`Year-End No. 1s`, align:='center') %>%
  add_axis('x', grid=F, properties = axis_props(
    labels = list(
      fill = "black",
      fillOpacity = .5,
      angle = 45,
      fontSize = 14,
      align = "left",
      baseline = "middle",
      dx = 3),
    title = list(fill = "black",
                 fillOpacity = .75,
                 dy=20),
    axis = list(stroke = "black",
                strokeOpacity = .5)
  )) %>%
  add_axis('y', grid=F, properties = axis_props(
    labels = list(
      fill = "black",
      fillOpacity = .5,
      fontSize = 14,
      dx = 3),
    title = list(fill = "black",
                 fillOpacity = .75),
    axis = list(stroke = "black",
                strokeOpacity = .5)
  )) %>%
  hide_legend('fill')


