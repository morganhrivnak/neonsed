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

library(tidybayes)
library(rstan)
library(brms)
library(janitor)
theme_set(theme_default())

#standardizing both x and y axis and adding row of year from alldata_wrangled

sedall <- readRDS("alldata_wrangled.rds") %>% 
  mutate(percent_s = scale(percent),
         size_s = size/max(size, na.rm = T),
         year = as.character(str_sub(eventID, 6, 9)))

#view(alldata_wrangled)

#ia4 from biol792 in brms
ia4mod_brm = brm(size ~ percent, data=sedall, 
                 family = Gamma(link = "log"))

summary(ia4mod_brm)

ia4mod_brm$data

#save model after computing
saveRDS(ia4mod_brm, file= "models/ia4mod_brm.rds")

max_size = max(sedall$size, na.rm = T)

posteriors = tidybayes::add_epred_draws(ia4mod_brm) %>% 
  mutate(.epred)

plot_data =plot(conditional_effects(ia4mod_brm), points=T) +
  theme_classic() +
  geom_line(aes(color = site))

plot_data <- plot(conditional_effects(ia4mod_brm), points = TRUE)[[1]] +
  ggplot2::aes(color = site)

#gamma mlm with interaction 
ia3mod_brm_int = brm(size ~ percent + site + site*percent, data=sedall, 
                     family = Gamma(link = "log"))

#updating the model with standardized x and y axis
ia3mod_brm_int1 = update(ia3mod_brm_int, newdata = sedall,
                         formula = size_s ~ percent_s + site + site*percent_s,
                         chains = 4)

#gamma mlm interaction with random effects
ia3mod_brm_int2 = update(ia3mod_brm_int, newdata = sedall,
                         formula = size_s ~ percent_s + site + site*percent_s +
                           (1 + percent_s + site + site*percent_s|year),
                         chains = 4)

allsitestogether <- plot(conditional_effects(ia3mod_brm_int2), points = TRUE)

allsitestogether$`percent_s:site` + theme_classic() + facet_wrap( ~site)

#this one doesn't work because of priors needing to be defined more
ia3mod_brm_int_ra = brm(size_s ~ percent_s + site + site*percent_s +
                          (1 + percent_s + site + site*percent_s|year), 
                        data = sedall,
                        family = Gamma(link = "log"))

#removing the *site in random effects and defining priors 
ia3mod_brm_int_ra2 = brm(size_s ~ percent_s + site + site*percent_s +
                           (1 + percent_s + site|year), 
                         data = sedall,
                         family = Gamma(link = "log")) 
prior = c(prior(normal(0.01, 0.1), class = "Intercept") ,
          #prior(normal(0, 1), class = "b"), #duplicate prior won't run
          prior(exponential(5), class = "sd"))

saveRDS(ia3mod_brm_int_ra2, file= "models/ia4mod_brm_int_ra2.rds")
#works but it still took a long time to work            


#trying to figure out why it wouldn't work :\
fit = update(ia3mod_brm_int_ra2, chains =3, iter= 1000 ,
             newdata = ia3mod_brm_int_ra2$data ,
             formula = (size_s ~ percent_s + site + site*percent_s +
                          (1 + percent_s + site + percent_s|year )))

sedall %>% 
  ggplot(aes(y= size_s, x = percent_s)) +
  geom_point() +
  facet_wrap(~site)

new_data <- expand_grid(
  percent_s = seq(min(sedall$percent_s, na.rm = TRUE), 
                  max(sedall$percent_s, na.rm = TRUE), 
                  length.out = 50),  
  site = unique(sedall$site),  
  year = unique(sedall$year)  
)

#posterior predictions
mod_posteriors <- new_data %>% 
  tidybayes::add_epred_draws(ia4mod_brm_int_ra2)


#Warning message: rows containing NAs were excluded from the model. this is just 

mod_posteriors_summary = mod_posteriors %>% 
  group_by(percent_s, site, year) %>% 
  median_qi(.epred)

mod_posteriors_summary %>% 
  ggplot(aes(x = percent_s, y = .epred, color = year)) +
  geom_line() +
  facet_wrap(~site, scales = "free_y") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = "year"), alpha = 0.2) +
  geom_point(data = sedall, aes(y = size_s)) +
  scale_y_log10()


sedall %>% 
  ggplot(aes(x = size_s)) + 
  geom_density()

#checking posteriors (predictive posteriors)
pp_check(ia4mod_brm_int_ra2, type = "dens_overlay_grouped", group = "site") + scale_x_log10() 

pp_check(ia4mod_brm_int_ra2, type = "boxplot") + scale_y_log10()