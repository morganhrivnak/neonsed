#load packages 
library(neonUtilities)
library(raster)
library(tidyverse)

#download data, stack, and read in one step!
p <- loadByProduct(dpID="DP4.00131.001",
                   site=c("CUPE"),
                   startdate="2017-01",
                   enddate="2021-12",
                   package="expanded")

#override no internet for neon load by product 
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

p$asc_fieldDataStation %>% filter(grepl(".SS", physicalSedimentSampleID))
  

names(p)
lambdas = read_csv("https://raw.githubusercontent.com/jswesner/neon_size_spectra-slim/main/data/lambdas.csv")

p$asc_externalLabSummary %>% distinct(analyte) %>% arrange(desc(analyte))

p$asc_fieldDataZone %>% distinct(habitatType) 

p$categoricalCodes_00131 %>% as_tibble() %>% 
  print(n = Inf)

p$geo_pebbleCount %>% as_tibble() %>% 
  filter(siteID == "CUPE") %>% 
  distinct(endDate, pebbleSize, measurementLocation)
