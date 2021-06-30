source('helpers.R')

datadir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'
outdir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/TSDshiny/data'

# library(DBI)
# library(RSQLite)
# 
# con <- dbConnect(SQLite(), "alldata.db")

l <- as_tibble(paramsMatch(experimentIDs = ExperimentIDs))

# list all files
tbs <- list.files(datadir,'^(S[^_]*)_([^_]*)_?r?([^\\.]*)?.csv', recursive = T) %>%
  tibble(filename = .) %>%
  rowwise() %>%
  mutate(S = str_match_all(basename(filename),'(S[^_]*)_([^_]*)_?r?([^\\.]*)?.csv'),
         Session = S[2],
         UniqueName = S[3],
         Run = S[4],
         S = str_match_all(dirname(filename),'data_exp_([^_]+)_([^_]*)_Session.*'),
         ExperimentIDs = S[2],
         Country = S[3]
         ) %>%
  select(-S)

# is_here <- dbListTables(con)

for (tb in unique(tbs$UniqueName)) {
  cat(tb,sep = '\n')
  ddd <- gimmedata(DataDir = datadir, UniqueName = tb) %>%
    mutate(Country = recode(`Experiment ID`, !!!l$Country)) %>%
    janitor::clean_names(case = "parsed")
  if (tb == 'Metacog') {
    ddd <- ddd %>% select(-randomise_trials) %>%
      rename(randomise_trials = randomise_Trials)
  }
  if (tb == 'SelfPref') {
    ddd <- ddd %>% select(-ANSWER) %>%
      rename(ANSWER = answer)
  }
  # dbWriteTable(con,tb,ddd)
  # next
  for (co in unique(ddd$Country)) {
    dd <- filter(ddd, Country == co)
    for (S in unique(dd$Session)){
      d <- filter(dd, Session == S)
      save(d,file = file.path(outdir,paste0('TSD_',co,'_',S,'_',tb,'.RData')), compress = T)
    } 
  }
}

source('Anonymize.R')

# dbDisconnect(con)

# is_indb <- dbListTables(con)
# is_infile <- list.files(pattern = paste0('^(',paste(unique(tbs$UniqueName),sep = '|'),').RData'))
# for (tb in is_indb) {
#   if (sum(is_infile == tb) > 0) next
#   cat(tb,sep = '\n')
#   d <- dbReadTable(con,tb)
#   save(d,file = paste0(tb,'.RData'))
# }
# dbListTables(con)
# 
# dbDisconnect(con)
# 
# con <- dbConnect(SQLite(), "alldata.db")
# 
# tb <- '1back'
# system.time(
#   d <- dbReadTable(con,tb)
# )
# system.time(
#   d <- load(file = paste0(tb,'.RData'))
# )
# system.time(
# d <- gimmedata(DataDir = datadir, UniqueName = tb) %>%
#   mutate(Country = recode(`Experiment ID`, !!!l$Country)) %>%
#   janitor::clean_names(case = "parsed")
# )
