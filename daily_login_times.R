datadir <- '/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA/RData'

source('helpers.R')

d <- gimmeRdata(DataDir = datadir, fast = T, progress = T)
daily_login_times <- d %>%
  filter(Trial_Number == 'BEGIN TASK' | Question_Key == 'BEGIN QUESTIONNAIRE') %>%
  unite(BEGIN,Trial_Number,Question_Key, na.rm = T) %>%
  select(PID, Session, Unique_Name, Run, BEGIN, Local_Date) %>%
  mutate(Date = lubridate::date(Local_Date)) %>%
  group_by(PID,Session, Date) %>%
  arrange(Local_Date) %>%
  slice(1)

save(daily_login_times,file = file.path(datadir,'Daily_Login_Times.RData'))
