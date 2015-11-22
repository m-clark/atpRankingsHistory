## grab rankings history for top 100 from atp site

library(rvest)

# grab current ranks if desired
atp <- html("http://www.atpworldtour.com/en/rankings/singles")
curranks = atp %>%
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
library(parallel)
cl = makeCluster(6)
clusterExport(cl, 'rankhistLinks')
clusterEvalQ(cl, library(rvest))

rankhistPages = parSapply(cl, rankhistLinks, function(x) html_table(html(x), header=T), simplify=F)

rankHistory = lapply(rankhistPages,  function(x) x[[2]])

# add names as columns
rankHistory2 = mapply(function(x,y) cbind(x, Player=y),
                      x=rankHistory, y=as.list(curranks$Player), SIMPLIFY=F)

head(rankHistory2[[1]])
#
rankHistoryAll = do.call('rbind', rankHistory2)
head(rankHistoryAll)

# Take out T for ties convert to numeric, convert date
library(magrittr); library(dplyr); library(lubridate)
rankHistoryAll %<>%
  mutate(Singles = as.integer(str_replace_all(Singles, 'T', '')),
         Doubles = as.integer(str_replace_all(Doubles, 'T', '')),
         Date = ymd(Date))

rankHistoryAll[rankHistoryAll$Singles==0, 'Singles'] = NA  # ranked in doubles, but not singles
rankHistoryAll = na.omit(rankHistoryAll)  # for whatever reason, ATP has dates for non-pro years

summary(rankHistoryAll)
save(rankHistoryAll, file='rankHistory.RData')

