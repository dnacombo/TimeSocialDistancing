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
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.source('helpers.R')

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
