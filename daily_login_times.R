datadir <- '/home/maximilien.chaumon_local/ownCloud/Lab/00-Projects/TimeSocialDistancing/TSDshiny/data'

source('helpers.R')

alldata <- gimmeRdata(DataDir = datadir, fast = T, progress = T, as.list = T)

for (i in 1:length(alldata)) {
  alldata[[i]] <- alldata[[i]] %>%
    filter({if ("Trial_Number" %in% names(.)) {Trial_Number == 'BEGIN TASK'} else {Question_Key == 'BEGIN QUESTIONNAIRE'}})
}

daily_login_times <- bind_rows(alldata) %>%
  unite(BEGIN,Trial_Number,Question_Key, na.rm = T) %>%
  select(PID, Session, Unique_Name, Run, BEGIN, Local_Date) %>%
  mutate(Date = lubridate::date(Local_Date)) %>%
  group_by(PID,Session, Date) %>%
  arrange(Local_Date) %>%
  slice(1)

save(daily_login_times,file = file.path(datadir,'Daily_Login_Times.RData'))
