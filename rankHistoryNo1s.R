## grab rankings history for top 100 from atp site

library(rvest)

# grab current ranks if desired
atp <- html("http://www.atpworldtour.com/en/rankings/former-no-1s")
no1Ranks = atp %>%
  html_table(header = T) %>%
  `[[`(1)


# grab all links, results in several per person
allLinks = atp %>%
  html_nodes('a') %>%
  html_attr("href")


library(stringr)
# grab 'overview' links, change to hisory. Note the issue is that each link is first-last/uniqueID/...
mainLinks = allLinks[str_detect(allLinks, pattern='/en/players/')]
mainLinks = grep(mainLinks, pattern='/overview', value=T)
rankhistLinks = paste0('http://www.atpworldtour.com', str_replace_all(mainLinks, 'overview', 'rankings-history'))

# get the rankings history pages, and the second table on each is the history, also class = 'mega-table'
rankhistPages = sapply(rankhistLinks, function(x) html_table(html(x), header=T), simplify=F)
rankHistory = lapply(rankhistPages,  function(x) x[[2]]) # second table is the one of interest

# add names as columns
rankHistory2 = mapply(function(x,y) cbind(x, Player=y),
                      x=rankHistory, y=as.list(curranks$Player), SIMPLIFY=F)

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

