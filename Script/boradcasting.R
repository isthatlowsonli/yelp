# match k reviews per reviewers by 
# recency,  user voting, and other review quality factors
# for each reviewer-restaurant pair

# in bru, reorder the review by recency, review compliment (e.g., useful)
# group by bru 

bru_no_review_text %>% colnames()
explore_bru %>% colnames()

explore_bru %>%
  select(
    user_id,
    stars0,
    effective_elite,
    date,
    business_id,
    stars,
    price_level,
    noise_level,
    parking, review_id, useful
  ) %>% 
  mutate(date = as.Date(date)) %>%
  group_by(business_id) %>%
  arrange(business_id, desc(date)) %>% head(100) %>% view()


# count distinct business_id 
explore_bru %>% 
  distinct(business_id) %>% 
  nrow()
# total 2649 distinct business_id 

# count distinct reviews_id 
explore_bru %>% 
  distinct(review_id) %>% 
  nrow()
# total 364196 distinct review_id 

# Sample 100 restaurants and the corresponding reviews
set.seed(20210315)
sample_res <-
  explore_bru %>%
  distinct(business_id) %>%
  sample_n(100) %>% 
  inner_join(explore_bru, by = "business_id") %>% 
  select(business_id, user_id, review_id, date) %>% 
  mutate(date = as.Date(date))

# Check the sample results
sample_res %>% 
  nrow()

sample_res %>% 
  head(100) %>% view()

# Implement paralled computation for further JOIN
# https://multidplyr.tidyverse.org/articles/multidplyr.html
# install.packages("multidplyr")
library(multidplyr)
parallel::detectCores()
cluster <- new_cluster(32)
cluster_library(cluster, "tidyverse")

sample_res1 <-
  sample_res %>%
  group_by(business_id) %>%
  partition(cluster)

sample_res_joined <-
  sample_res1 %>%
  left_join(sample_res1, by = "business_id")

sample_res_joined %>% 
  collect() %>% 
  colnames()
# [1] "business_id" "user_id.x"   "review_id.x"
# [4] "date.x"      "user_id.y"   "review_id.y"
# [7] "date.y"  

sample_res_joined %>% 
  nrow()

# Filter the unqualified review-review pair
sample_res_joined_filtered <-
  sample_res_joined %>%
  filter(date.x > date.y)

# Check the filtered results
sample_res_joined_filtered %>% 
  nrow()
# [1] 2824103


# Select K reviews per user-restaurant pair
# K = 10
# Method 1: order the review by recency, user voting and text length
# create recency first, recency = date.x-date.y 
sample_res_joined_filtered %>% 
  head(10) %>% 
  mutate(recency = (date.x - date.y) %>% as.numeric()) %>% view()

sample_res_joined_filtered <-
  sample_res_joined_filtered %>% 
  mutate(recency = (date.x - date.y) %>% as.numeric())

sample_res_joined_filtered_full <-
  sample_res_joined_filtered %>%
  collect() %>%
  ungroup() %>%
  left_join(explore_bru, by = c("review_id.y" = "review_id"))

sample_res_joined_filtered_full %>% 
  glimpse()

# export the file for Dremio to calculate the word count
rio::export(sample_res_joined_filtered_full,"./Data/sample_res_joined_filtered_full.csv")

# Filter data in Dremio 
# SELECT *
#   FROM   (SELECT *,
#           Count(*)
#           OVER (
#             PARTITION BY "business_id.x", "review_id.x"
#             ORDER BY "recency", "useful" DESC, "text_length" DESC ) AS slice_id
#           FROM   sample_res_joined_filtered_full_from_r)
# WHERE  slice_id <= 10 
library(readr)
resample_100_to_r <- read_csv("Data/resample_100_to_r.csv")

resample_100_to_r %>% glimpse()

resample_100_others <-
  resample_100_to_r %>%
  group_by(review_id.x) %>%
  summarise(others_avg_identity = mean(effective_elite), others_avg_rating = mean(stars0))

# use filtering join
resample_100_final <-
  explore_bru %>%
  semi_join(resample_100_others, by = c("review_id" = "review_id.x")) %>% 
  left_join(resample_100_others, by = c("review_id" = "review_id.x")) %>% 
  arrange(business_id, review_id)

resample_100_final <- 
  resample_100_final %>% 
  mutate(price_level = as.character(price_level))

broadcasting1 <- lm(stars ~ effective_elite + others_avg_identity + others_avg_rating + price_level + noise_level + parking, data = resample_100_final)
library(stargazer)
stargazer(broadcasting1,type = "text",style = "aer", align = TRUE)

# consider pure broadcasting: user without friend
# extract users without friends in Neo4j

# MATCH (u:User)-[:WROTE]->(:Review)-[:REVIEWS]->(b:Business)-[:IN_CATEGORY]->(c:Category)
# WHERE b.is_open = 1
# AND b.state = 'AZ'
# AND b.city = 'Phoenix'
# AND c.id CONTAINS 'Restaurant'
# WITH DISTINCT u
# MATCH (u)-[f:FRIEND]-(u2)
# WHERE u2.name is null
# WITH u.id AS user_id, count(f) AS f_count
# WHERE f_count = 1
# RETURN distinct user_id

library(readr)
u_nf <- read_csv("~/neo4j-community-4.0.0/import/u_nf.csv")

resample_100_final_loner <-
  resample_100_final %>%
  semi_join(u_nf, by = c("user_id" = "user_id")) %>%
  # nrow() notice that 7,016 out of 12,909 reviews are selected. (54% selected)
  arrange(business_id, review_id)

broadcasting2 <- lm(stars ~ effective_elite + others_avg_identity + others_avg_rating + price_level + noise_level + parking, data = resample_100_final_loner)
library(stargazer)
stargazer(broadcasting1,broadcasting2,type = "latex",style = "aer", align = TRUE)




# Assume Yelp sort the reviews by weighting the random select results (skip)

