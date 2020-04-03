# import relationship ----
library(readr)
friendship <- read_csv("~/neo4j-community-3.5.15/import/friendship.csv")


# count all path
friendship %>%
  nrow()
# [1] 18619580

# count distinct start node
friendship %>%
  distinct(from) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1          82810

# count distinct end node
friendship %>%
  distinct(to) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1         358697

# count distinct path
friendship %>%
  distinct(from, to) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1        2352204

# extract distinct path
friendship_distinct <-
  friendship %>%
  distinct(from, to)

# check if distinct path is directed
friendship_distinct %>% head(100)

friendship_distinct %>% 
  filter(from == 203675 & to ==258040) %>% view() 

friendship_distinct %>% 
  filter(from == 258040 & to ==203675) %>% view() 

# the path is undirected 

# if we try to load the second version of the export, we can confirm the result ----
library(readr)
friendship2 <- read_csv("~/neo4j-community-3.5.15/import/friendship2.csv")

# count all path
friendship2 %>%
  nrow()
# [1] 2352204

# count distinct start node
friendship2 %>%
  distinct(from) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1          82810

# count distinct end node
friendship2 %>%
  distinct(to) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1         358697

# count distinct path
friendship2 %>%
  distinct(from, to) %>%
  summarise(distinct_count = n())

# # A tibble: 1 x 1
# distinct_count
# <int>
#   1        2352204

# notice that the original file is already distinct in path

# import nodes ---- 
library(readr)
node <- read_csv("~/neo4j-community-3.5.15/import/node.csv")

# check for distinct node 
node %>% 
  distinct(neo_id) %>% 
  nrow()

# [1] 436905

# test igraph object
graph_object <- igraph::graph_from_data_frame(
  d = friendship2, 
  directed = FALSE, 
  vertices = node
)



