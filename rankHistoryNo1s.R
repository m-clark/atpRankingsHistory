## grab rankings history for top 100 from atp site

library(dplyr); library(rvest)

# grab current ranks if desired
atp <- read_html("http://www.atpworldtour.com/en/rankings/former-no-1s")
no1Ranks = atp %>%
  html_table(header = T) %>%
  `[[`(1)


# grab all links, results in several per person
allLinks = atp %>%
  html_nodes('a') %>%
  html_attr("href")


gs0 = read_html('https://en.wikipedia.org/wiki/List_of_Grand_Slam_men%27s_singles_champions') %>%
  html_nodes('table') %>%
  `[[`(1) %>%
  html_table(header = T, fill=T) %>%
  filter(Year>=1968)      # open era

library(tidyr)
gs = gs0 %>%
  gather(key='Tournament', value='Player', -Year) %>%
  mutate(Player = repair_encoding(Player),
         Player = stringr::str_replace_all(Player, pattern='\\(|[0-9]|\\/|\\)|', ''), #\\(|[0-9]|\\/|\\)|†
         Player = stringr::str_replace_all(Player, pattern='(.)*:', ''),
         Player = stringr::str_replace_all(Player, pattern='(.)*\n', ''),
         Player = stringr::str_replace_all(Player, pattern='  (.)*', ''),  # gets rid of †††† for almost all, but not sure how to deal with the symbol itself
         Player = stringr::str_trim(Player),
         Year = as.numeric(Year)) %>%
  group_by(Player) %>%
  summarise(Slams=length(Player))
# glimpse(gs)
gs$Player[gs$Player=='Carlos Moyá'] = 'Carlos Moya'
gs$Player[gs$Player=='Ilie N\032stase'] = 'Ilie Nastase'
gs$Player[gs$Player=='Björn Borg'] = 'Bjorn Borg'
gs$Player[gs$Player==''] = NA


gs = rbind(gs, c("Marcelo Rios", 0)) %>% na.omit
# while gs is still not pretty, it contains all of who were number 1 in a clean fashion
# no1Ranks$Player[-which(no1Ranks$Player %in% gs$Player)]

no1Ranks = left_join(no1Ranks[,-c(1:2)], gs, by='Player')
no1Ranks$Slams = as.numeric(no1Ranks$Slams)

library(stringr)
# grab 'overview' links, change to hisory. Note the issue is that each link is first-last/uniqueID/...
mainLinks = allLinks[str_detect(allLinks, pattern='/en/players/')]
mainLinks = grep(mainLinks, pattern='/overview', value=T)
rankhistLinks = paste0('http://www.atpworldtour.com', str_replace_all(mainLinks, 'overview', 'rankings-history'))

# get the rankings history pages, and the second table on each is the history, also class = 'mega-table'
rankhistPages = sapply(rankhistLinks, function(x) html_table(read_html(x), header=T), simplify=F) # sometimes throws a 500 error depending on their website
rankHistory = lapply(rankhistPages,  function(x) x[[2]]) # first or second table is the one of interest sometimes depending on their website

# add names as columns
rankHistory2 = mapply(function(x,y) cbind(x, Player=y),
                      x=rankHistory, y=as.list(no1Ranks$Player), SIMPLIFY=F)

head(rankHistory2[[1]])
#
rankHistoryNo1s = do.call('rbind', rankHistory2)
head(rankHistoryNo1s)

# Take out T for ties convert to numeric, convert date
library(magrittr); library(dplyr); library(lubridate)
rankHistoryNo1s %<>%
  mutate(Singles = as.integer(str_replace_all(Singles, 'T', '')),
         Doubles = as.integer(str_replace_all(Doubles, 'T', '')),
         Date = ymd(Date))

rankHistoryNo1s[rankHistoryNo1s$Singles==0, 'Singles'] = NA  # ranked in doubles, but not singles
rankHistoryNo1s = na.omit(rankHistoryNo1s)  # for whatever reason, ATP has dates for non-pro years

summary(rankHistoryNo1s)
save(no1Ranks, rankHistoryNo1s, file='rankHistoryNo1s.RData')

