#load packages 
library(neonUtilities)
library(raster)

#download data, stack, and read in one step!
p <- loadByProduct(dpID="DP1.20194.001",
                   site=c("CUPE"),
                   startdate="2017-01",
                   enddate="2021-12",
                   package="expanded")

#i lost my work because it didnt save right :(