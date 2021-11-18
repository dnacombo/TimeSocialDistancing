source('helpers.R')

alldata <- gimmeRdata(DataDir = dirBlursday, fast = T, progress = T, as.list = T)

d <- list()
for (i in 1:length(alldata)) {
  d[[i]] <- alldata[[i]] %>%
    filter({if ("Trial_Number" %in% names(.)) {Trial_Number == 'BEGIN TASK'} else {Question_Key == 'BEGIN QUESTIONNAIRE'}})
}



(RunCount <- bind_rows(d) %>% 
  group_by(Country, Unique_Name, Session, Run) %>%
  slice(1) %>%
  group_by(Country, Unique_Name, Session) %>%
  summarize(N = n()) %>%
  pivot_wider(id_cols = c(Country, Unique_Name), names_from = Session, values_from = N)
  ) %>%
  googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1q9yjwyMWa65-Fs1A73kY4K57Nf5Gvovp9VZgjz5tEDg', sheet = 'ALL')
  
RunCountSplit <- RunCount %>%
  group_by(Country) %>% 
  group_split()


for (RunCountCountry in RunCountSplit) {
  Country <- unique(RunCountCountry$Country)
  RunCountCountry %>%
    googlesheets4::write_sheet(ss = 'https://docs.google.com/spreadsheets/d/1q9yjwyMWa65-Fs1A73kY4K57Nf5Gvovp9VZgjz5tEDg', sheet = Country)
    
}
