library(tidyr)
library(arrow)
library(dplyr)
library(janitor)
library(stringr)
library(here)

here::i_am("Scripts/2_Process_FBS.R")

#unzip from downloaded file
#unzip(here('Data', 'raw', 'FoodBalanceSheets_E_All_Data_(Normalized).zip'), exdir=here('Data', 'raw', 'metadata'))
#unzip(here('Data', 'raw', 'FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip'), exdir=here('Data', 'raw', 'metadata'))

#move the non-meta data to the raw data folder
file.rename(from = here('Data', 'raw', 'metadata', 'FoodBalanceSheets_E_All_Data_(Normalized).csv'), to = here('Data', 'raw', 'FoodBalanceSheets_E_All_Data_(Normalized).csv'))
file.rename(from = here('Data', 'raw', 'metadata', 'FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv'), to = here('Data', 'raw', 'FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv'))

#Delete the .zip file
file.remove(here('Data', 'raw', 'FoodBalanceSheets_E_All_Data_(Normalized).zip'))
file.remove(here('Data', 'raw', 'FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip'))

#make into parquet file
FBS_df <- read.csv(here('Data', 'raw', 'FoodBalanceSheets_E_All_Data_(Normalized).csv'))
FBS_pqt <- arrow::write_parquet(FBS_df, sink= here::here('Data', 'raw', 'FBS_pqt.parquet'))

FBShist_df <- read.csv(here('Data', 'raw', 'FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv'), fileEncoding = "windows-1252")
FBShist_pqt <- arrow::write_parquet(FBShist_df, sink= here::here('Data', 'raw', 'FBShist_pqt.parquet'))

#filter maize, rice, and wheat
#FBS has Commodity Item codes which are kind of different from the trade and production codes
#Current FBS: 2511 = Wheat and products, 2514 = Maize and products, 2807 = Rice and products
FBS_staples <- FBS_df |> 
  filter(Item.Code %in% c(2511, 2514, 2807))

arrow::write_parquet(FBS_staples, sink = here::here('Data', 'processed', 'FBS_staples.parquet'))

#IMPORTANT: FAOSTAT changed rice's FBS item code between the historic and current
#series. Historic FBS uses 2805 ("Rice (Milled Equivalent)") for rice, not 2807
#(which doesn't exist in the historic file at all). Wheat (2511) and maize (2514)
#codes are unchanged. Filtering historic on 2807 (as before) silently drops rice.
FBShist_staples <- FBShist_df |>
  filter(Item.Code %in% c(2511, 2514, 2805))

arrow::write_parquet(FBShist_staples, sink = here::here('Data', 'processed', 'FBShist_staples.parquet'))



#Clearing memory for the Trade Matrix
rm(FBS_df, FBShist_df, FBS_pqt, FBShist_pqt, FBS_staples, FBShist_staples)

gc()
