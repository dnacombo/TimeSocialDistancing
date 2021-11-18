source('helpers.R')

alldata <- gimmeRdata(dirBlursday, fast = T, progress = T)


save(alldata,file = file.path(dirData,'AllData.RData'), safe = T)
