#load packages 
library(neonUtilities)
library(raster)
library(tidyverse)
library(vcd)

#download data, stack, and read in one step!
p <- loadByProduct(dpID="DP4.00131.001",
                   site=c("CUPE"),
                   startdate="2017-01",
                   enddate="2021-12",
                   package="expanded")

#override no internet for neon load by product 
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

p$geo_pebbleFieldData
names(p)
lambdas = read_csv("https://raw.githubusercontent.com/jswesner/neon_size_spectra-slim/main/data/lambdas.csv")

p$asc_externalLabSummary %>% distinct(analyte) %>% arrange(desc(analyte))


p$categoricalCodes_00131 %>% as_tibble() %>% 
  print(n = Inf)

p$geo_pebbleCount %>% as_tibble() %>% 
  filter(siteID == "CUPE") %>% 
  distinct(endDate, pebbleSize, measurementLocation)

#take out pebble count information form the entire data product 
x <- p$geo_pebbleCount

#remove all of the unnecessary information and separate min max and words
# tidyverse remove columns
df <- x %>% 
  select(siteID, pebbleCountNumber, pebbleSize, habitatType, endDate) %>% 
  separate(col = pebbleSize, into = c("numbers", "category"), sep = "mm: ") %>% 
  separate(col = numbers, into = c("min", "max"), sep = " - ") %>% 
  mutate(max = case_when(grepl("<", min) ~ min,
                         TRUE ~ max),
         min = case_when(grepl("<", min) ~ NA,
                         TRUE ~ min)) %>% 
  mutate(min = parse_number(min)) %>% 
  mutate(max = parse_number(max)) %>% 
  separate(col = endDate, into = c("year", "month/date"), sep = "-0")

saveRDS(df, file="cupedf") 


df %>% 
  group_by(min, max) %>% 
  tally() %>% 
  arrange(-n) %>% 
  ggplot(aes(x = min, y = n)) +
  geom_point()


p$geo_featureInfo %>% 
  summarise()

PhiPlot <- function(wolman.count, phi.max = 512, phi.min = 1, ...){
 #intervals 
phi.n.max <- log(phi.max, base = 2)
phi.n.min <- log(phi.min, base = 2)
phi.n <- c(phi.n.min:phi.n.max)
phi.size <- 2^ phi.n
}


wolman.size <- select(df["min"]) 
wolman.count.f <- factor(df["pebbleCountNumber"], levels = wolman.size)
wolman.count.t <- table(wolman.count.f)

wolman.pcfiner <- 100*cumsum(wolman.count.t)/sum(wolman.count.t)

plot(wolman.size, wolman.pcfiner, # set up plot
     log='x',
     xaxt='n',
     yaxt = 'n',
     xlim=c(phi.max, phi.min),
     type='n',
     las = 1
)

axis(side = 1, at = phi.size, label = phi.size)
axis(side = 2, at = axTicks(2), label = axTicks(2), las = 1)
title(xlab = 'Grain size (mm)', ylab = '% finer')

abline(h = axTicks(2), lty=2, col='grey')
abline(v = phi.size, lty=2, col='grey')

#top discription
axis(side = 3, at = c(256, 64, 2, 0.0625), label=NA)
mtext(text = 'Boulders', side = 3, at = 300, adj=1)
mtext(text = 'Cobbles', side = 3, at = 128)
mtext(text = 'Gravel', side = 3, at = 8)
mtext(text = 'Sand', side = 3, at = 1.5, adj = 0)

# add data
lines(wolman.size, wolman.pcfiner,
      type = 's')



p.2 <- loadByProduct(dpID="DP4.00131.001",
                   site=c("BIGC"),
                   startdate="2019-01",
                   enddate="2021-12",
                   package="expanded") 

y
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))


p.3 <- loadByProduct(dpID="DP4.00131.001",
                   site=c("CUPE", "REDB", "BLUE", "CARI", "LECO", "GUIL", 
                   "BIG", "OKSR", "MART", "HOPB", "ARIK", "BLDE", "MCDI", "MCRA", 
                   "POSE", "SYCA", "WLOU", "PRIN", "TECR", "WALK", "KING", "LEWI"),
                   startdate=NA,
                   enddate="2021-12",
                   package="expanded")
