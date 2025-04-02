#size of sediment (percent) compare lambdas

library(tidyverse)
library(tidybayes)
library(rstan)
library(brms)
library(janitor)
theme_set(theme_default())

#wrangle quantification of sediment and jeff lambdas together
lambdas = read.csv("lambdas.csv")
sediment_size = readRDS("mod_posteriors_unstd.rds")

#making year a character and not a number
lambdas$year <- as.character(lambdas$year)
sediment_size$year <- as.character(sediment_size$year)

#left joining the two data sets
df = lambdas %>% 
  left_join(sediment_size %>% select(site, year, size), 
            by = c("site_id" = "site", "year" = "year")
            )
 
#save wrangled combined data
saveRDS(df, "wesnerlambdas_sediment_combo_df.rds")
lss <- readRDS("wesnerlambdas_sediment_combo_df.rds")

#selecting only the collumns I need
lss_df <- lss %>% 
  select("year", "site_id", "size", "lambda", "log_om_s", "log_gpp_s", "sample_id")

#save this df because it is the one that will actually be being used
saveRDS(lss_df, "lambda_sediment_df.rds")
#when you would use lss_df <- readRDS("name")

#model relationship between the lambda and sediment size

lss.m1 <- brm(
formula = lambda ~ size, 
data = lss_df
)

#an exclusion of NAs meaning that lambdas that did not have a corresponding 
#sediment size value due to NEON  sampling problems are not included

#plot lss.m1
ggplot(lss_df, aes(x = size, y = lambda)) +
  geom_point(alpha = 0.6) + 
  labs(
    title = "Bayesian Fit: Lambda ~ Size",
    x = "Size",
    y = "Lambda"
  ) + 
  theme_default()

#plot and model doesn't tell you everything about relationship at individual sites

#model looking at lambda and size but adding random intercepts and slopes with site locations

lss.m2 <- brm(
  formula = lambda ~ size + (1 + size | site_id), 
  data = lss_df
)

#posteriors
lss.m2.posts <- lss.m2 %>%
  add_epred_draws(newdata = lss_df, 
                  re_formula = NULL, 
                  allow_new_levels = TRUE, 
                  ndraws = 100)

#plot lss.m2
ggplot(lss_df, aes(x = size, y = lambda)) +
  facet_wrap(~site_id, scales = "fixed") +
  geom_point(data = lss.m2.posts, aes(y = lambda)) +
  labs(
    title = "Bayesian Model: Lambda ~ Size with Varying Slopes and Intercepts",
    x = "Size",
    y = "Lambda") +
  theme_classic()

