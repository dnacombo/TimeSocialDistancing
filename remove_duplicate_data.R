
library(tidyverse)

datadir <-'/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'

fs <- list.files(datadir,'data_exp_16144.*\\d.zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,'\\.zip',paste0(' (',n,').zip'))
    if (file.exists(file.path(datadir,nf))) {
      file.remove(file.path(datadir,f))
      file.rename(file.path(datadir,nf),file.path(datadir,f))
    }
  }
}

fs <- list.files(datadir,'.* \\(\\d\\).zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,' \\(\\d\\)\\.zip',paste0('.zip'))
    if (file.exists(file.path(datadir,nf))) {
      print(file.path(datadir,f))
      print(file.path(datadir,nf),file.path(datadir,f))
    }
  }
}


