# percentage change
by_review_pc <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = lubridate::date(date), year = year(date)) %>%
  arrange(business_id, year) %>%
  group_by(business_id, year) %>%
  summarise(reviews = n_distinct(review_id)) %>%
  ungroup() %>%
  group_by(business_id) %>%
  mutate(lag_reviews = lag(reviews, order_by = year)) %>%
  select(business_id, year, reviews, lag_reviews) %>%
  filter(!is.na(lag_reviews)) %>%
  mutate(review_pc = (reviews - lag_reviews) / lag_reviews)

by_ed <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = lubridate::date(date), year = year(date)) %>%
  group_by(business_id, year) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  ungroup() %>%
  distinct(business_id, year, .keep_all = TRUE) %>%
  select(business_id, year, elite_density, price_level, noise_level, parking) %>%
  arrange(business_id, year)

review_pc_ok <-
  by_review_pc %>%
  inner_join(by_ed, by = c("business_id", "year")) %>%
  ungroup() %>%
  arrange(business_id, year) %>%
  group_by(business_id) %>%
  mutate(lag_elite_density = lag(elite_density, order_by = year)) %>%
  filter(!is.na(lag_elite_density)) %>% 
  mutate(price_level = as.character(price_level))

glimpse(review_pc_ok)

lm_review_pc <-
  review_pc_ok %>%
  lm(review_pc ~ lag_elite_density + price_level + noise_level + parking, data = .)

# plm_review_pc <-
#   review_pc_ok %>%
#   plm(review_pc ~ lag_elite_density + price_level + noise_level + parking,
#     data = ., index = c("business_id", "year"),
#     model = "within"
#   )


lm_review_pc %>% 
  stargazer::stargazer(
    type = "text",
    title = "Previous Elite density vs. the current review count (percentage change)",
    style = "aer",
    align = TRUE
  )




# plm

plm_review_pl <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
  data = review_pc_ok,
  index = c("business_id", "year"),
  model = "pooling"
)


plm_review_fe <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
  data = review_pc_ok,
  index = c("business_id", "year"),
  model = "within"
)

plm_review_tw <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
  data = review_pc_ok,
  index = c("business_id", "year"),
  model = "within",
  effect = "twoways"
)

plm_review_rd <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
  data = review_pc_ok,
  index = c("business_id", "year"),
  model = "random"
)

plm_review_t <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
  data = review_pc_ok,
  index = c("business_id", "year"),
  model = "within",
  effect = "time"
)

# plm_review_fd <- plm(reviews ~ lag_elite_density + price_level + noise_level + parking,
#   data = review_pc_ok,
#   index = c("business_id", "year"),
#   model = "fd"
# )

rob_se <- list(sqrt(diag(vcovHC(plm_review_pl))),
               sqrt(diag(vcovHC(plm_review_fd))),
               sqrt(diag(vcovSCC(plm_review_fe))),
               sqrt(diag(vcovSCC(plm_review_tw))))


stargazer::stargazer(
  plm_review_pl,
  plm_review_fd,
  plm_review_fe,
  plm_review_tw,
  se = rob_se,
  type = "latex",
  title = "Previous Elite density vs. the current review count ",
  style = "aer",
  align = TRUE,
  column.labels = c("Pooling OLS","First Difference","Restaurant FE","Restaurant-Year FE")
)



pFtest(plm_review_fe,plm_review_pl) # fixed effect is a better choice 
plm::phtest(plm_review_fe,plm_review_rd) # fixed effect is a better choice
pFtest(plm_review_tw,plm_review_fe) # time fixed effect is better 
pcdtest(plm_review_tw,test = "lm") # have cross-sectional dependence for large N small T
pcdtest(plm_review_tw,test = "cd") # have cross-sectional dependence for large N small T
pbgtest(plm_review_tw) # serial correlation in idiosyncratic errors

summary(plm_review_tw)
