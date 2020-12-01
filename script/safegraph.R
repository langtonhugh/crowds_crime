library(tidycensus)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(purrr)

# safe_df <- read_csv("data/neighborhood_patterns_june_2020.gz")

# cali_df <- safe_df %>% 
#   filter(region == "CA")

# write_csv(x = cali_df, path = "data/cali_nhood_safegraph.csv")

# safegraph nhood data subset for california.
cali_df <- read_csv("data/cali_nhood_safegraph.csv")

# grab the variables that are most interesting.
# cali_sub_df <- cali_df %>% 
#   select(area,
#          raw_stop_counts, raw_device_counts,
#          stops_by_day, stops_by_each_hour, 
#          popularity_by_hour_monday,
#          popularity_by_hour_tuesday,
#          popularity_by_hour_wednesday,
#          popularity_by_hour_thursday,
#          popularity_by_hour_friday,
#          popularity_by_hour_saturday,
#          popularity_by_hour_sunday)

# write_csv(x = cali_sub_df, path = "data/cali_subset_nhood_safegraph.csv")

# separate(col = popularity_by_hour_monday, sep = ",", into = paste0("t", 1:24)) 

# cali_sub_df <- cali_sub_df %>% 
#   mutate(t1 = gsub(x = cali_sub_df$t1, pattern = "\\[", replacement = ""),
#          t24 = gsub(x = cali_sub_df$t24, pattern = "\\]", replacement = ""))
# 
# # transform and aggregate for test.
# cali_sub_long_df <- cali_sub_df %>% 
#   pivot_longer(cols = -area, names_to = "time", values_to = "ppl") %>% 
#   mutate(ppl = as.numeric(ppl)) %>% 
#   group_by(area) %>% 
#   summarise(total_ppl = sum(ppl))
# 
# # read in census block shapefile
# cali_sf <- st_read("data/tl_2016_06_bg.shp")
# 
# # la census blocks
# la_df <- read_csv("data/Monthly_Dockless_Vehicles_Trip_Start_By_Census_Block_Group.csv")
# la_census_vec <- la_df$geography_key
# 
# # subset
# la_sf <- cali_sf %>% 
#   filter(GEOID %in% la_census_vec) %>% 
#   rename(area = GEOID)
# 
# # plot
# # ggplot(data = la_sf) +
# #   geom_sf()
# 
# # join
# la_safe_sf <- left_join(la_sf, cali_sub_long_df)
# 
# # plot
# ggplot(data = la_safe_sf) +
#   geom_histogram(mapping = aes(x = total_ppl))
# 
# # map
# ggplot(data = la_safe_sf) +
#   geom_sf(mapping = aes(fill = total_ppl), colour = "transparent")
# 
# # remove outliers (e.g. airport)
# la_safe_sub_sf <- la_safe_sf %>%
#   filter(total_ppl < 3000)
# 
# ggplot(data = la_safe_sub_sf) +
#   geom_histogram(mapping = aes(x = total_ppl), bins = 60)
# 
# # map
# ggplot(data = la_safe_sub_sf) +
#   geom_sf(mapping = aes(fill = total_ppl), colour = "transparent")
# 
# # airport case study
# airport_sf <- la_safe_sf %>% 
#   arrange(desc(total_ppl)) %>% 
#   slice(2) # trial and error matched to map shape... soz. plot(st_geometry(airport_sf)) 
# 
# airport_code <- airport_sf$area
# 
# # keep hourly counts
# la_long_df <- cali_sub_df %>% 
#   filter(area %in% la_census_vec) 
# 
# # plot airport
# airport_plot_df <- la_long_df %>% 
#   filter(area == airport_code) %>% 
#   pivot_longer(cols = -area) 
# 
# airport_plot_df %>% 
#   mutate(name = gsub(x = airport_plot_df$name, pattern = "t", replacement = ""),
#          name = as.numeric(name)) %>% 
# ggplot() +
#   geom_line(mapping = aes(x = name, y = value, group = 1))
# 
# 
# # join
# la_safe_daily_sf <- left_join(la_sf, la_long_df)
# 
# # animate
