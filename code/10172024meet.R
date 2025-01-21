df %>% 
  # filter(siteID == "") %>% 
  group_by(min, max, siteID) %>% 
  tally() %>%
  ungroup %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(x = max, y = percent)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~siteID)


df %>% 
  filter(siteID == "GUIL") %>%
  group_by(min, max, siteID) %>% 
  tally() %>%
  ungroup %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(cumulative = cumsum(percent))
#ONE RESPONSE VARIABLE ONE PREDICTOR