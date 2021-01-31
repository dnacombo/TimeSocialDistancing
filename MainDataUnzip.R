
source('helpers.R')

################################ FR #######################################
Country = 'FR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)

############################# TR #############################################
Country = 'TR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('DataPIDMatch_', Country[1]),
  params = l,
)


rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)

########################### IN ##################################################
Country = 'IN'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)


############################# GR ################################################
Country = 'GR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)


############################# JP ###############################################
Country = 'JP'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)

############################# CO ###############################################
Country = 'CO'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)

########################### CA-FR ##############################################
Country = 'CA'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)

########################### AR ################################################
Country = 'AR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- params$datadir
  
rmarkdown::render(
  'DataNuUnzip.Rmd',
  params = l,
)
rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('DateLayout_', Country[1]),
  params = l,
)
