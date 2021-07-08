# previous elite count vs. current reviews
one_year_review <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 365) %>%
  group_by(business_id) %>%
  mutate(reviews = n_distinct(review_id)) %>%
  distinct(business_id, .keep_all = TRUE) %>%
  select(business_id, reviews)

two_year_review <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 730) %>%
  group_by(business_id) %>%
  mutate(reviews = n_distinct(review_id)) %>%
  distinct(business_id, .keep_all = TRUE) %>%
  select(business_id, reviews)

three_year_review <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 1095) %>%
  group_by(business_id) %>%
  mutate(reviews = n_distinct(review_id)) %>%
  distinct(business_id, .keep_all = TRUE) %>%
  select(business_id, reviews)

four_year_review <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 1460) %>%
  group_by(business_id) %>%
  mutate(reviews = n_distinct(review_id)) %>%
  distinct(business_id, .keep_all = TRUE) %>%
  select(business_id, reviews)

# prepare regression
# baseline: one year vs one year, linear model
lm_one_vs_one_review_ec <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 730) %>%
  filter(as.Date("2018-12-31") - date >= 365) %>%
  group_by(business_id) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  ungroup() %>%
  distinct(business_id, .keep_all = TRUE) %>%
  inner_join(one_year_review, by = "business_id") %>%
  mutate(price_level = as.character(price_level)) %>%
  lm(formula = reviews ~ elite_count  + network_size + price_level + noise_level + parking)

lm_two_vs_two_review_ec <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 1460) %>%
  filter(as.Date("2018-12-31") - date >= 730) %>%
  group_by(business_id) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  ungroup() %>%
  distinct(business_id, .keep_all = TRUE) %>%
  inner_join(two_year_review, by = "business_id") %>%
  mutate(price_level = as.character(price_level)) %>%
  lm(formula = reviews ~ elite_count + price_level + noise_level + parking)


lm_three_vs_three_review_ec <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 2190) %>%
  filter(as.Date("2018-12-31") - date >= 1095) %>%
  group_by(business_id) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count /network_size ) %>%
  ungroup() %>%
  distinct(business_id, .keep_all = TRUE) %>%
  inner_join(three_year_review, by = "business_id") %>%
  mutate(price_level = as.character(price_level)) %>%
  lm(formula = reviews ~ elite_count + price_level + noise_level + parking)


lm_four_vs_four_review_ec <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 2920) %>%
  filter(as.Date("2018-12-31") - date >= 1460) %>%
  group_by(business_id) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  ungroup() %>%
  distinct(business_id, .keep_all = TRUE) %>%
  inner_join(four_year_review, by = "business_id") %>%
  mutate(price_level = as.character(price_level)) %>%
  lm(formula = reviews ~ elite_count  + price_level + noise_level + parking)

stargazer::stargazer(
  lm_one_vs_one_review_ec,
  lm_two_vs_two_review_ec,
  lm_three_vs_three_review_ec,
  lm_four_vs_four_review_ec,
  type = "text",
  title = "Previous Elite count vs. the current review count",
  style = "aer",
  align = TRUE,
  column.labels = c("1v1", "2v2", "3v3", "4v4")
)

car::vif(lm_one_vs_one_review_ec)

# semi-paremertics estimation: double machine learning
# Partially linear IV regression model (PLIV)
install.packages("DoubleML")
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)

test_data <-
  explore_bru %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  filter(as.Date("2018-12-31") - date < 730) %>%
  filter(as.Date("2018-12-31") - date >= 365) %>%
  group_by(business_id) %>%
  mutate(elite_count = sum(effective_elite), network_size = n_distinct(user_id)) %>%
  mutate(elite_density = elite_count / network_size) %>%
  ungroup() %>%
  distinct(business_id, .keep_all = TRUE) %>%
  inner_join(one_year_review, by = "business_id") %>%
  mutate(price_level = as.character(price_level))

test_data <-
  test_data %>%
  filter(across(.cols = c(price_level, noise_level, parking), ~ !is.na(.x)))

learner <- lrn("regr.ranger", num.trees = 100, mtry = 3, min.node.size = 2, max.depth = 5)
ml_g <- learner$clone()
ml_m <- learner$clone()
ml_r <- learner$clone()
set.seed(2222)
data <- make_pliv_CHS2015(alpha = 0.5, n_obs = 500, dim_x = 20, dim_z = 1, return_type = "data.table")
obj_dml_data <- double_ml_data_from_data_frame(
  df = test_data,
  x_cols = c("price_level", "noise_level", "parking"),
  y_col = "reviews",
  d_cols = "elite_density",
  z_cols = "stars0"
)

dml_pliv_obj <- DoubleMLPLIV$new(obj_dml_data, ml_g, ml_m, ml_r)
dml_pliv_obj$fit()
print(dml_pliv_obj)


# review count ~ elite count 


