library(tidyr)
library(arrow)
library(dplyr)
library(here)

here::i_am("Scripts/1_Process_QCLTM.R")

#bulk_QCL <- unzip('./Production_Crops_Livestock_E_All_Data_(Normalized).zip')

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

