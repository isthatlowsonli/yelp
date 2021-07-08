# PX, AZ restaurants reviewers' network
# Including friends of friends, lagest component

library(readr)
comm_lv_full <- read_csv("20eec074-622f-0053-e7cf-4e013db1a600.csv", 
                                                  col_types = cols(comm_lv_1 = col_character(), 
                                                                   comm_lv_2 = col_character(), comm_lv_3 = col_character(), 
                                                                   elite = col_character(), neo_id = col_character()
                                                                   #yelping_since = col_datetime(format = "%Y-%m-%d %H:%M:%S")
                                                                   ))

comm_lv_full %>% skimr::skim()

comm_lv_full %>% 
  # sample_n(100) %>% 
  mutate(has_elite = if_else(is.na(elite),0,1)) %>% 
  group_by(has_elite) %>% 
  summarise(average_stars_given = mean(average_stars))
  

comm_lv_full %>%
  # mutate(has_elite = if_else(is.na(elite), 0, 1)) %>%
  mutate(days_diff = difftime(time1 = "2019-12-31", time2 = date(yelping_since), units = "days") %>% as.numeric()) %>%
  mutate(yelping_years = round(days_diff/365,3)) %>%
  select(-days_diff) %>% rio::export("comm_lv_full_mutated.csv")
  # mutate(day_diff = difftime(time1 = today(tzone = "UTC"), time2 = date(yelping_since), units = "days") %>% as.integer()) %>% 
  lm(formula = average_stars ~ has_elite + day_diff, data = .) %>%
  stargazer::stargazer(type = "text")


  library(readr)
  comm_lv_label_mo_full <- read_csv("comm_lv_label_mo_full.csv", 
                                    col_types = cols(comm_label = col_character(), 
                                                     comm_lv_1 = col_character(), comm_lv_2 = col_character(), 
                                                     comm_lv_3 = col_character(), comm_mo = col_character(), 
                                                     elite = col_character(), neo_id = col_character(), 
                                                     yelping_since = col_character()))
  
  
  comm_lv_label_mo_full %>%
    # mutate(has_elite = if_else(is.na(elite), 0, 1)) %>%
    mutate(days_diff = difftime(time1 = "2019-12-31", time2 = date(yelping_since), units = "days") %>% as.numeric()) %>%
    mutate(yelping_years = round(days_diff/365,3)) %>% select(yelping_years) %>% head(10) %>% view()
    select(-days_diff) %>% rio::export("comm_lv_label_mo_full_mutated.csv")
  
  View(comm_lv_label_mo_full)
  
  
  
  comm_lv_label_mo_full %>% 
    group_by(comm_lv_1) %>% 
    summarise(member_count = n()) %>% 
    ungroup() %>% 
    filter(member_count>50,member_count<2000) %>% 
    arrange(desc(member_count))
  
  
  
  comm_lv %>%
    igraph::membership() %>%
    tibble::enframe() %>%
    select(noe_id = name, comm_lv_r = value) %>%
    rio::export("comm_lv_r.csv")

