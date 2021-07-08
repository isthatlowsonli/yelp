explore_bru %>% 
  ungroup() %>% 
  mutate(year = str_extract(date,"\\d{4}")) %>% 
  group_by(business_id, year) %>% 
  summarise(count = n(), real_stars = mean(stars0, na.rm = TRUE), elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>% 
  mutate(elite_density = elite_count / network_size) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(avg_elite_density = mean(elite_density), yearly_review_sum = sum(count)) %>% 
  arrange(desc(year)) %>% 
  ggplot(aes(x = year, y = avg_elite_density)) +
  geom_bar(stat = "identity", width = 0.2) +
  theme_minimal()
