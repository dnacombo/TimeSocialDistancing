# This is the Blursday database paper source code.
# This code was used to prepare the following article:
# XXX Ref. to add XXX
#
# The live application of the server can be accessed at 
# https://dnacombo.shinyapps.io/Blursday/
# This code is publicly available at
# https://github.com/dnacombo/TSDshiny

#     Copyright (C) 2021  Maximilien Chaumon
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

source('helpers.R')

d <- gimmeRdata(DataDir = dirBlursday, fast = T, progress = T, as.list = T)

for (i in 1:length(d)) {
  d[[i]] <- d[[i]] %>%
    filter({if ("Trial_Number" %in% names(.)) {Trial_Number == 'BEGIN TASK'} else {Question_Key == 'BEGIN QUESTIONNAIRE'}})
}

daily_login_times <- bind_rows(d) %>%
  unite(BEGIN,Trial_Number,Question_Key, na.rm = T) %>%
  select(PID, Session, Unique_Name, Run, BEGIN, Local_Date) %>%
  mutate(Date = lubridate::date(Local_Date)) %>%
  group_by(PID,Session, Date) %>%
  arrange(Local_Date) %>%
  slice(1)

save(daily_login_times,file = file.path(dirBlursday,'Daily_Login_Times.RData'))
