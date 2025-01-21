

p.2_wrangled = p.2$geo_pebbleFieldData %>% 
  as_tibble() %>% 
  select(eventID, starts_with("pebbleC")) %>% 
  pivot_longer(cols = -eventID,
               values_to = "percent_lessorequal") %>% 
  mutate(size = parse_number(percent_lessorequal),
         site = str_sub(eventID, 1,4),
         percent = parse_number(name))


p.2_wrangled %>% 
  ggplot(aes(x = percent, y = size, fill = site)) +
  geom_point() +
  geom_boxplot(aes(group = interaction(name, site))) +
  geom_line(aes(group = eventID)) +
  # scale_y_log10() +
  facet_wrap(~site)
