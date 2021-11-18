source('helpers.R')

# process some countries selectively (set to NULL for all)
Country = NULL #'FR'
ExperimentName = Country
UniqueName = NULL

l <- as_tibble(paramsMatch(experimentIDs = ExperimentIDs, Country = Country))

# list all files
tbs <- list.files(dirData,'^(S[^_]*)_([^_]*)_?r?([^\\.]*)?.csv', recursive = T) %>%
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
  select(-S) %>%
  filter(Country %in% ExperimentName)
if (!is.null(UniqueName)) {
  tbs <- filter(tbs, UniqueName %in% !!UniqueName)
}

# is_here <- dbListTables(con)

for (tb in unique(tbs$UniqueName)) {
  cat('\n')
  cat(tb,sep = '\n')
  ddd <- gimmedata(DataDir = dirData, UniqueName = tb, ExperimentID = paste0(l$ExperimentID,collapse = '|')) %>%
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
  
  # next
  for (co in unique(ddd$Country)) {
    dd <- filter(ddd, Country == co)
    for (S in unique(dd$Session)){
      d <- filter(dd, Session == S) %>%
        mutate(Country_Name = recode(Country,!!!countryMapping)) %>%
        select(Country_Name, Country, Session, Unique_Name, Run, PID, everything())
      save(d,file = file.path(dirBlursday,paste0('TSD_',co,'_',S,'_',tb,'.RData')), compress = T)
    } 
  }
}

# copying to SQLite database

# library(DBI)
# library(RSQLite)
# 
# fs <- list.files(dirBlursday,'TSD_[^_]+_[^_]+_[^\\.]+\\.RData', full.names = T)
# allUniqueNames <- unique(str_match(string = fs, pattern = 'TSD_[^_]+_[^_]+_([^\\.]+)\\.RData')[,2])
# 
# con <- dbConnect(SQLite(), "alldata.db")
# pb <- txtProgressBar(min = 0, max = length(allUniqueNames), initial = 0, char = "=",
#                      width = 40, style = 3)
# i <- 0
# for (tb in allUniqueNames) {
#   # cat(tb, sep = '\n')
#   d <- gimmeRdata(DataDir = dirBlursday, UniqueName = tb, verbose = F)
#   copy_to(dest = con,df = d,name = tb, temporary = F, overwrite = T, indexes = list('Country','Participant_Private_ID','Experiment_ID','PID','Session','Unique_Name','Run','Event_index'))
#   i <- i + 1
#   setTxtProgressBar(pb,i)
# }
# 
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
# d <- gimmedata(DataDir = dirData, UniqueName = tb) %>%
#   mutate(Country = recode(`Experiment ID`, !!!l$Country)) %>%
#   janitor::clean_names(case = "parsed")
# )
