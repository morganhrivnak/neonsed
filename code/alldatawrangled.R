#all data wrangled 

library(neonUtilities)
library(raster)
library(tidyverse)
library(sizeSpectra)
library(dplyr)
library(tibble)

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

p.3 <- loadByProduct(dpID="DP4.00131.001",
                     site=c("CUPE", "REDB", "BLUE", "CARI", "LECO", "GUIL", 
                            "BIG", "OKSR", "MART", "HOPB", "ARIK", "BLDE", "MCDI", "MCRA", 
                            "POSE", "SYCA", "WLOU", "PRIN", "TECR", "WALK", "KING", "LEWI"),
                     startdate=NA,
                     enddate=NA,
                     package="expanded")

p.3_wrangled = p.3$geo_pebbleFieldData %>% 
  as_tibble() %>% 
  select(eventID, starts_with("pebbleC")) %>% 
  pivot_longer(cols = -eventID,
               values_to = "percent_lessorequal") %>% 
  mutate(size = parse_number(percent_lessorequal),
         site = str_sub(eventID, 1,4),
         percent = parse_number(name))


p.3_wrangled %>% 
  ggplot(aes(x = percent, y = size, fill = site)) +
  geom_point() +
  geom_boxplot(aes(group = interaction(name, site))) +
  geom_line(aes(group = eventID)) +
  scale_y_log10() +
  facet_wrap(~site) +
  NULL

#df table of min and max of all sites ~13k data points 
x <- p.3$geo_pebbleCount
df <- x %>% 
  select(siteID, pebbleCountNumber, pebbleSize, habitatType) %>% 
  separate(col = pebbleSize, into = c("numbers", "category"), sep = "mm: ") %>% 
  separate(col = numbers, into = c("min", "max"), sep = " - ") %>% 
  mutate(max = case_when(grepl("<", min) ~ min,
                         TRUE ~ max),
         min = case_when(grepl("<", min) ~ NA,
                         TRUE ~ min)) %>% 
  mutate(min = parse_number(min)) %>% 
  mutate(max = parse_number(max)) 

saveRDS(p.3_wrangled, file = "alldata_wrangled.rds")
df <- read_rds("alldata_wrangled.rds")
