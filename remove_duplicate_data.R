
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


