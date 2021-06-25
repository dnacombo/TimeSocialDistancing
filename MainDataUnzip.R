
source('helpers.R')

for (Country in c('FR','DE','IT','TR','IN','GR','JP','CO','CA','AR', 'UK')) {
  l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
  l$datadir <- datadir
  # rmarkdown::render(
  #   'DataUnzip.Rmd',
  #   output_file = paste0('Reports/01_DataUnzip_', Country[1]),
  #   params = l,
  # )
  # 
  # rmarkdown::render(
  #   paste0('DataPIDMatch_', Country[1], '.Rmd'),
  #   output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  #   params = l,
  # )
  
  # rmarkdown::render(
  #   paste0('DateLayout.Rmd'),
  #   output_file = paste0('Reports/03_DateLayout_', Country[1]),
  #   params = l,
  # )
  
}

stop('here')

################################ FR #######################################
Country = 'FR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)

# l <- paramsMatch(Country=Country,Session = '5_Control', experimentIDs = ExperimentIDs)
# l$datadir <- datadir
# 
# rmarkdown::render(
#   'DataUnzip.Rmd',
#   params = l,
#   output_file = paste0('Reports/01_DataUnzip_', Country[1], '_ControlSession'),
# )

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

################################ IT #######################################
Country = 'IT'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

############################# TR #############################################
Country = 'TR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir
  
rmarkdown::render(
  'DataUnzip.Rmd',
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)


rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

########################### IN ##################################################
Country = 'IN'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir
  
rmarkdown::render(
  'DataUnzip.Rmd',
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)


############################# GR ################################################
Country = 'GR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir
  
rmarkdown::render(
  'DataUnzip.Rmd',
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)


############################# JP ###############################################
Country = 'JP'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir
  
rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)

rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

############################# CO ###############################################
Country = 'CO'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir
  
rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

########################### CA-FR ##############################################
Country = 'CA'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

########################### DE ##############################################
Country = 'DE'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)

rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

########################### AR ################################################
Country = 'AR'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)
rmarkdown::render(
  paste0('DataPIDMatch_', Country[1], '.Rmd'),
  output_file = paste0('Reports/02_DataPIDMatch_', Country[1]),
  params = l,
)
rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)

########################### UK ################################################
Country = 'UK'

l <- paramsMatch(Country=Country,experimentIDs = ExperimentIDs)
l$datadir <- datadir

rmarkdown::render(
  'DataUnzip.Rmd',
  params = l,
  output_file = paste0('Reports/01_DataUnzip_', Country[1]),
)
rmarkdown::render(
  paste0('DateLayout.Rmd'),
  output_file = paste0('Reports/03_DateLayout_', Country[1]),
  params = l,
)
