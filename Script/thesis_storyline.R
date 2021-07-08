# probability of algorithm recommended review from elites
# that larger than one from normal users

explore_bru %>%
  mutate(effective_elite = as.character(effective_elite)) %>%
  group_by(business_id, effective_elite) %>%
  summarise(review_count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = effective_elite, values_from = review_count, values_fill = 0) %>%
  select(business_id, normal_user = `0`, elite_user = `1`) %>%
  mutate(elite_comment_more = if_else(elite_user > normal_user, 1, 0)) %>%
  summarise(prob_elite_comment_more = mean(elite_comment_more))


# prob_elite_comment_more
# <dbl>
#   1                 0.00793

# review count
explore_bru %>%
  group_by(business_id) %>%
  summarise(log_review_count = log(n())) %>%
  ungroup() %>%
  skim(log_review_count)

# mean: 3.96, sd: 1.46

# elite density
explore_bru %>%
  group_by(business_id) %>%
  summarise(total_reviews = n(), elite_reviews = sum(effective_elite)) %>%
  ungroup() %>%
  mutate(log_elite_comment_ratio = log((elite_reviews + 1) / total_reviews)) %>%
  skim(log_elite_comment_ratio)

# mean: -1.84, sd: 0.581

# prepare data
early_stage <-
  explore_bru %>%
  group_by(business_id) %>%
  summarise(log_review_count = log(n())) %>%
  ungroup() %>%
  mutate(early_stage = if_else(log_review_count < (3.96 - 1.46), 1, 0))

elite_densed <- 
  explore_bru %>%
  group_by(business_id) %>%
  summarise(total_reviews = n(), elite_reviews = sum(effective_elite)) %>%
  ungroup() %>%
  mutate(log_elite_comment_ratio = log((elite_reviews + 1) / total_reviews)) %>%
  mutate(elite_densed = if_else(log_elite_comment_ratio > (-1.84+0.581),1,0)) %>% 
  select(business_id, elite_densed)

early_stage %>% 
  left_join(elite_densed, by = "business_id") %>% 
  group_by(early_stage,elite_densed) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prob = count/sum(count))

# see if these 2 variable are associate 
# since the values are nominal, Cramer's V is suggested 

test <-
  early_stage %>%
  left_join(elite_densed, by = "business_id")

chisq.test(test$early_stage,test$elite_densed)

rcompanion::cramerV(test$early_stage,test$elite_densed, ci = TRUE,  verbose = TRUE)

u_nf_ef_full %>% colnames()

# IV1: ownAvgRatings ~ ownIdentity + avgFriendIdentiy | ownIdentity + avgFriendOfFriend
library(readr)
iv_ok <- read_csv("iv_ok.csv")
iv_ok %>% glimpse()
iv_ok <-
  iv_ok %>%
  select(avg_own_rating = Average_stars, everything())
iv_ok %>% skimr::skim()
library(ivreg)
iv1 <- ivreg(avg_own_rating ~ avg_friend_identity + own_identity | own_identity + avgFansOfFriends, data = iv_ok)
library(sandwich)
summary(iv1, vcov = sandwich, diagnostics = TRUE)

stargazer(iv1,type = "latex",style = "aer", align = TRUE)

# IV2: ownReviewCounts ~ ownIdentity + avgFriendIdentiy | ownIdentity + avgFriendOfFriend
library(readr)
iv2_ok <- read_csv("iv2_ok.csv")
iv2_ok %>% glimpse()
iv2_ok <-
  iv2_ok %>%
  select(own_review_count = review_count, everything()) %>% 
  mutate(log_own_review_count = log(own_review_count))
iv2_ok %>% skimr::skim()
library(ivreg)
iv2 <- ivreg(own_review_count ~ avg_friend_identity + own_identity | own_identity + avgFansOfFriends, data = iv2_ok)
library(sandwich)
summary(iv2, vcov = sandwich, diagnostics = TRUE)

iv2_log <- ivreg(log_own_review_count ~ avg_friend_identity + own_identity | own_identity + avgFansOfFriends, data = iv2_ok)
library(sandwich)
summary(iv2_log, vcov = sandwich, diagnostics = TRUE)

user_yelping_years <-
  bru_no_review_text %>%
  select(user_id, yelping_years) %>%
  distinct()

iv2_2_ok <-
  iv2_ok %>%
  left_join(user_yelping_years, by = "user_id") %>%
  mutate(avg_review_count = own_review_count / yelping_years)

iv2_2 <- ivreg(avg_review_count ~ avg_friend_identity + own_identity | own_identity + avgFansOfFriends, data = iv2_2_ok)
library(sandwich)
summary(iv2_2, vcov = sandwich, diagnostics = TRUE)
  
