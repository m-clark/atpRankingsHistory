## grab rankings history for top 100 from atp site

library(rvest)

# grab current ranks if desired
atp <- read_html("http://www.atpworldtour.com/en/rankings/singles")
curranks = atp %>%
  html_table(header = T) %>%
  `[[`(1)


# Note the issue is that each link is first-last/uniqueID/...
# grab all links, results in several per person, extract only overview
library(stringr)
rankhistLinks = atp %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  grep(pattern='.*overview', value =T) %>%
  str_replace_all('overview', 'rankings-history') %>%
  paste0('http://www.atpworldtour.com', .)


# get the rankings history pages, and the second table on each is the history,
# also class = 'mega-table'
# atp has a broken link for Daniel Muñoz de la Nava that hasn't been fixed since
# October of 2015, probably because of the ñ coupled with typical web designer
# carelessness; at end of 2015 he was ranked 75th

library(parallel)
cl = makeCluster(6)
clusterExport(cl, 'rankhistLinks')
clusterEvalQ(cl, library(rvest))

rankHistory = parSapply(cl, rankhistLinks[-75], function(x) html_table(html_node(read_html(x), 'table.mega-table'), header=T), simplify=F)

stopCluster(cl)

# add names as column
rankHistory2 = mapply(function(x, y) cbind(x, Player=y),
                      x=rankHistory, y=as.list(curranks$Player)[-75], SIMPLIFY=F)

# check
head(rankHistory2[[1]])

# b/c dplyr 'bind_rows' won't combine different classes, and some ranks are tied like 50T
rankHistoryAll = do.call('rbind', rankHistory2)

# check
head(rankHistoryAll)

# Take out T for ties convert to numeric, convert date
library(magrittr); library(dplyr); library(lubridate)
rankHistoryAll %<>%
  mutate(Singles = as.integer(str_replace_all(Singles, 'T', '')),
         Doubles = as.integer(str_replace_all(Doubles, 'T', '')),
         Date = ymd(Date))

rankHistoryAll[rankHistoryAll$Singles==0, 'Singles'] = NA  # ranked in doubles, but not singles
rankHistoryAll = na.omit(rankHistoryAll)  # don't care about doubles only rankings

summary(rankHistoryAll)
save(curranks, rankHistoryAll, file='rankHistoryTop100.RData')

