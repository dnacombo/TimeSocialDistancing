source('helpers.R')

alldata <- gimmeRdata('TSDshiny/data/', fast = T)

save(alldata,file = 'AllData.RData')
