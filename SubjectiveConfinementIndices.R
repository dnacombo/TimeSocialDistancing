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

SubjectiveConfinementIndices <-  gimmeRdata(dirBlursday, UniqueName = 'UCLA') %>%
  QTranslate() %>%
  filter(!`Question_Key`%in%c("BEGIN QUESTIONNAIRE","END QUESTIONNAIRE")) %>%
  pivot_wider(id_cols = c(Country, Session, Run, PID),names_from=`Question_Key`,values_from=Response, values_fn = first) %>%
  mutate(Reported_Loneliness = as.numeric(`OSRN-2-quantised`) + as.numeric(`OSRN-4-quantised`),
         Felt_Loneliness = as.numeric(`OSRN-11-quantised`) + as.numeric(`OSRN-14-quantised`) + as.numeric(`OSRN-16-quantised`),
         Subjective_Confinement = Reported_Loneliness + Felt_Loneliness) %>%
  select(Country, Session, PID, Reported_Loneliness, Felt_Loneliness, Subjective_Confinement)

save(SubjectiveConfinementIndices, file=file.path(dirBlursday,"SubjectiveConfinementIndices.RData"))
