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


library(tidyverse)
source('helpers.R')


fs <- list.files(dirData,'data_exp_16144.*\\d.zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,'\\.zip',paste0(' (',n,').zip'))
    if (file.exists(file.path(dirData,nf))) {
      file.remove(file.path(dirData,f))
      file.rename(file.path(dirData,nf),file.path(dirData,f))
    }
  }
}

fs <- list.files(dirData,'.* \\(\\d\\).zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,' \\(\\d\\)\\.zip',paste0('.zip'))
    if (file.exists(file.path(dirData,nf))) {
      print(file.path(dirData,f))
      print(file.path(dirData,nf),file.path(dirData,f))
    }
  }
}


