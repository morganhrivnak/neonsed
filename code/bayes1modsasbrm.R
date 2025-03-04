#packages
library(tidyverse)
library(tidybayes)
library(rstan)
library(brms)
library(janitor)

sedall <- readRDS("alldata_wrangeld.rds")

#ia4 from biol792 in brms
ia4mod_brm = brm(size ~ percent, data=df)

summary(ia4mod_brm)

#save model after computing
saveRDS(ia4mod_brm, file= "models/ia4mod_brm.rds")

plot_data =plot(conditional_effects(ia4mod_brm), points=T)

