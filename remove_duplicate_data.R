
library(tidyverse)

rootdir <-'/home/maximilien.chaumon/ownCloud/Lab/00-Projects/TimeSocialDistancing/DATA'

fs <- list.files(rootdir,'data_exp_16144.*\\d.zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,'\\.zip',paste0(' (',n,').zip'))
    if (file.exists(file.path(rootdir,nf))) {
      file.remove(file.path(rootdir,f))
      file.rename(file.path(rootdir,nf),file.path(rootdir,f))
    }
  }
}

fs <- list.files(rootdir,'.* \\(\\d\\).zip')

for (f in fs) {
  for (n in 1:4) {
    nf <- str_replace(f,' \\(\\d\\)\\.zip',paste0('.zip'))
    if (file.exists(file.path(rootdir,nf))) {
      print(file.path(rootdir,f))
      print(file.path(rootdir,nf),file.path(rootdir,f))
    }
  }
}


