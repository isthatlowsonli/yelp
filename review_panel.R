reviews_year <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = lubridate::date(date), year = lubridate::year(date)) %>%
  group_by(user_id, year) %>%
  summarise(reviews = n()) %>% 
  ungroup()

lag_mean_elite_density <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = lubridate::date(date), year = lubridate::year(date)) %>%
  group_by(business_id, year) %>%
  mutate(elite_count = sum(effective_elite),
         network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  select(business_id, year, elite_density, user_id) %>%
  ungroup() %>%
  filter(if_all(.col = everything(), .fns = ~ !is.na(.))) %>%
  group_by(user_id, year) %>%
  summarise(mean_elite_density = mean(elite_density, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(user_id) %>%
  mutate(lag_mean_elite_density = lag(mean_elite_density, order_by = year)) %>%
  arrange(user_id, year) %>%
  filter(if_all(.col = everything(), .fns = ~ !is.na(.))) %>% 
  ungroup()

reviews_year_panel_ok <-
  reviews_year %>%
  left_join(lag_mean_elite_density, by = c("user_id", "year")) %>%
  filter(if_all(.col = everything(), .fns = ~ !is.na(.))) 

plm_review_year_pl <-
  reviews_year_panel_ok %>%
  plm(formula = reviews ~ lag_mean_elite_density, data = ., model = "pooling", index = c("user_id", "year"))

plm_review_year_fe <-
  reviews_year_panel_ok %>%
  plm(formula = reviews ~ lag_mean_elite_density, data = ., model = "within", index = c("user_id", "year"))

plm_review_year_tw <-
  reviews_year_panel_ok %>%
  plm(formula = reviews ~ lag_mean_elite_density, data = ., effect = "twoways", model = "within", index = c("user_id", "year"))

plm_review_year_rd <-
  reviews_year_panel_ok %>%
  plm(formula = reviews ~ lag_mean_elite_density, data = ., model = "random", index = c("user_id", "year"))



rob_se <- list(sqrt(diag(vcovHC(plm_review_year_pl))),
               sqrt(diag(vcovSCC(plm_review_year_fe))),
               sqrt(diag(vcovSCC(plm_review_year_tw))))


stargazer::stargazer(
  plm_review_year_pl,
  plm_review_year_fe,
  plm_review_year_tw,
  se = rob_se,
  type = "latex",
  title = "Previous Mean Elite Density of Reviewed Restaurant vs. Current Review Count, Unbalanced Panel",
  style = "aer",
  align = TRUE,
  column.labels = c("Pooling OLS", "Individual FE", "Individual-Year FE")
)



pFtest(plm_review_year_fe,plm_review_year_pl) # fixed effect is a better choice 
plm::phtest(plm_review_year_fe,plm_review_year_rd) # fixed effect is a better choice
pFtest(plm_review_year_tw,plm_review_year_fe) # time fixed effect is better 
pcdtest(plm_review_year_tw,test = "lm") # have cross-sectional dependence for large N small T
pcdtest(plm_review_year_tw,test = "cd") # have cross-sectional dependence for large N small T
pbgtest(plm_review_year_tw) # serial correlation in idiosyncratic errors
