library(readr)
px_res <- read_csv("~/yelp_project/Data/21bfdebe-f1b8-7957-1e7b-b0be23ab6600.csv")
library(dplyr)
px_res <- px_res %>%
  arrange(desc(stars))

library(ggmap)
register_google(key = "AIzaSyAA2s7wJxFu1ehkqTRXxEsObQ76ynMoApg")

p <- ggmap(get_googlemap("Phoenix, Arizona",
  maptype = "terrain",
  color = "color"
))

library(ggplot2)
p +
  geom_point(data = px_res, aes(x = longitude, y = latitude, size = review_count, color = stars), shape = 20, stroke = FALSE) +
  scale_size_continuous(name = "Review", range = c(1, 10)) +
  scale_color_viridis_c(
    # option = "magma",
    name = "Stars", alpha = 0.5
  ) +
  theme_void()
