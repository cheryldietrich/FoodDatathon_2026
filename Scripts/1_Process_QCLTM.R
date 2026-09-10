library(tidyr)
library(arrow)
library(dplyr)
library(here)

here::i_am("Scripts/1_Process_QCLTM.R")

#QCL data downloaded August 27, 2026 from: https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data_(Normalized).zip

unzip(here('Data', 'raw', 'Production_Crops_Livestock_E_All_Data_(Normalized).zip'), exdir=here('Data', 'raw', 'metadata'))
#move the non-meta data to the raw data folder
file.rename(from = here('Data', 'raw', 'metadata', 'Production_Crops_Livestock_E_All_Data_(Normalized).csv'), to = here('Data', 'raw', 'Production_Crops_Livestock_E_All_Data_(Normalized).csv'))


QCL_df <- read.csv(here('Data', 'raw', 'Production_Crops_Livestock_E_All_Data_(Normalized).csv'))
QCL_pqt <- arrow::write_parquet(QCL_df, sink = here::here('Data', 'raw', "QCL_pqt.parquet"))

#filtering maize, rice, and wheat
QCL_staples <- QCL_pqt |>
   filter(Item.Code %in% c(15, 27, 56)) 

arrow::write_parquet(QCL_staples, sink = here::here('Data', 'processed', 'QCL_staples.parquet'))

#Clearing memory for the QCL
rm(QCL_df)
rm(QCL_pqt)
rm(QCL_staples)

#Trade Matrix data downloaded August 27, 2026 from: https://bulks-faostat.fao.org/production/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip
unzip(here('Data', 'raw', 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'), exdir=here('Data', 'raw', 'metadata'))
#move the non-meta data to the raw data folder
file.rename(from = here('Data', 'raw', 'metadata', 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv'), to = here('Data', 'raw', 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv'))

#bulk_TM <- unzip('./Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip')

TM_df <- read.csv(here::here('Data', 'raw', 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv'))

TM_pqt <- arrow::write_parquet(TM_df, sink=here::here('Data', 'raw', 'TM_pqt.parquet'))

TM_staples <- TM_pqt |>
    filter(Item.Code %in% c(15, 27, 56))


arrow::write_parquet(TM_staples, sink = here::here('Data', 'processed', 'TM_staples.parquet'))

#Clearing memory for the Trade Matrix
rm(TM_df)
rm(TM_pqt)
rm(TM_staples)

#Clearing for the session
gc()

