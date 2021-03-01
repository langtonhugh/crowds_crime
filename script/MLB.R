library(tmaptools)
library(maps)
library(osmdata)
library(cowplot)
library(crimedata)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)
library(purrr)

# Major League Baseball team data obtained from https://www.retrosheet.org/gamelogs/index.html.
gl_2018_df <- read_csv("data/GL2018.txt", col_names = F)

# Details on variables obtained from https://www.retrosheet.org/gamelogs/glfields.txt.
gl_2018_df <- gl_2018_df %>% 
  rename(date = X1,             # yyyymmdd
         away_team = X4,        # away team
         home_team = X7,        # home team
         day_night = X13,       # day or night team
         completed = X14,       # details if game completed at later date
         pitch     = X17,       # pitch ID  
         attendance = X18,      # attendance
         game_time = X19) %>%   # game time in minutes
  select(date, away_team, home_team, day_night, completed, pitch, attendance, game_time) 

# Check missings.
lapply(gl_2018_df, function(x) sum(is.na(x)))

# Only issue is with 'complete' with 2 missings, so let's drop them, because these games might
#have been moved to another date.
gl_2018_df <- gl_2018_df %>% 
  filter(is.na(completed))

# Retrieve pitch ID names from retrosheet website. This is also saved locally as parkcode.txt.
pitch_df <- read_csv("https://www.retrosheet.org/parkcode.txt")

# Select cols needed.
pitch_sub_df <- pitch_df %>% 
  select(PARKID, NAME, CITY, STATE) %>% 
  rename(pitch = PARKID)

# Left join with game-level data so we know the stadium names and city.
gl_2018_pitch_df <- left_join(gl_2018_df, pitch_sub_df)

# Get crime data.
crimes_sf <- get_crime_data(
  years = 2018, 
  type = "core",
  output = "sf"
) 

# Check punctuation of retrosheet city names.
unique(gl_2018_pitch_df$CITY)

# Clean MLB city names, format date column, and filter out cities not contained in crime data.
gl_2018_pitch_df <- gl_2018_pitch_df %>% 
  mutate(city_clean = gsub('[[:punct:] ]+',' ', CITY),
         game_date_ymd = ymd(date)) %>% 
  filter(city_clean %in% crimes_sf$city_name)

# Clean crimes dates, retain only cities/dates for which there is a game, change city name to character.
crimes_sub_sf <- crimes_sf %>%
  mutate(date_ymd = date(date_single)) %>% 
  filter(date_ymd %in% gl_2018_pitch_df$game_date_ymd,
         city_name %in% gl_2018_pitch_df$city_clean) %>% 
  mutate(city_name = as.character(city_name)) 

# Check cities matching.
unique(sort(gl_2018_pitch_df$city_clean))
unique(sort(crimes_sub_sf$city_name))

# To retrieve in stadium coordinates using a Google Map query. First we clean two of the names to make
# them match with Google Map search.  AT&T is now called Oracle Park: https://en.wikipedia.org/wiki/Oracle_Park.
gl_2018_pitch_df <- gl_2018_pitch_df %>% 
  mutate(NAME = if_else(NAME ==  "Guaranteed Rate Field;U.S. Cellular Field",
                             "Guaranteed Rate Field",
                             NAME),
         NAME = if_else(NAME == "AT&T Park",
                        "Oracle Park",
                        NAME))

# Create vector of unique stadium names.
stadiums_vec <- unique(gl_2018_pitch_df$NAME)

# Create empty list for geocoding results.
stads_list <- list()

# For each stadium name, retrieve the xy coorindates from Google Maps.
for (i in stadiums_vec) {
  geocode_result <-  geocode_OSM(i)
  stads_list[[i]] <- geocode_result$coords
}

# Bind results together.
stads_df <- bind_rows(stads_list, .id = "stadium_name")

# Save in case change in Google Map query function.
# write_csv(x = stads_df, path = "data/stadium_coords.csv")

# Convert to sf object.
stads_sf <- st_as_sf(stads_df, coords = c(x = "x", y = "y"), crs = 4326)

# USA map for check.
usa_sf <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
usa_sf <- st_transform(usa_sf, 4326)

# Plot.
# ggplot() +
#   geom_sf(data = usa_sf) +
#   geom_sf(data = stads_sf)

# Transform stadiums, crimes and USA to a projected CRS (https://epsg.io/2163).
# Note: what is the impact of choosing a different projection?
stads_sf      <- st_transform(stads_sf, 2163)

# for mac, or any older gdal/proj version you might need this (see github issue: https://github.com/r-spatial/sf/issues/1419):
# st_crs(crimes_sub_sf) = 4326 # and ignore the subsequent warning
# st_crs(crimes_sub_sf$geometry) <- 4326

crimes_sub_sf <- st_transform(crimes_sub_sf, 2163)

usa_sf <- st_transform(usa_sf, 2163)

# Visual inspection.
# ggplot() +
#   geom_sf(data = usa_sf) +
#   geom_sf(data = stads_sf)

# Create 1-mile buffers around stadiums.
stads_buffers_sf <- st_buffer(stads_sf, dist = 1609)

# Save for inspection in QGIS.
# st_write(obj = stads_sf        , dsn = "data/stats_coords.shp")
# st_write(obj = crimes_sub_sf   , dsn = "data/crimes_sub.shp")
# st_write(obj = stads_buffers_sf, dsn = "data/stads_buffers.shp")

# Visual inspection of first observation.
# ggplot() +
#   geom_sf(data = slice(stads_sf, 1), col = "red") +
#   geom_sf(data = slice(stads_buffers_sf, 1), fill = "transparent")

# Intersection between buffers and crimes.
stads_crimes_buffers_sf <- st_intersection(stads_buffers_sf, crimes_sub_sf)

# Aggregate by stadium and by day.
stads_crimes_df <- stads_crimes_buffers_sf %>% 
  filter(offense_group %in% c("assault offenses", "larceny/theft offenses")) %>% # filter assault and larceny/theft.
  as_tibble() %>% 
  group_by(stadium_name, date_ymd) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(stadium_name, date_ymd, fill = list(crime_count = 0))

# Check frequencies across all days.
ggplot(data = stads_crimes_df) +
  geom_histogram(mapping = aes(crime_count), bins = 30) 

# Rename variables for join.
stads_crimes_df <- stads_crimes_df %>% 
  rename(NAME = stadium_name,
         game_date_ymd = date_ymd)

# Join back attendance data.
gl_stads_crimes_df <- left_join(gl_2018_pitch_df, stads_crimes_df)

# Check variables.
min(gl_stads_crimes_df$crime_count) # some zeros
min(gl_stads_crimes_df$attendance)  # some zeros

ggplot(data = gl_stads_crimes_df) +
  geom_histogram(mapping = aes(x = attendance), bins = 30)

ggplot(data = gl_stads_crimes_df) +
  geom_histogram(mapping = aes(x = crime_count), bins = 30)

p_att <- ggplot(data = gl_stads_crimes_df) +
  geom_histogram(mapping = aes(x = attendance), bins = 30) +
  facet_wrap(~NAME, scales = "free")

ggsave(plot = p_att, filename = "visuals/att_facet.png", height = 10, width = 25, unit = "cm")

# Only keep games with more than 1 crime, more than 1 attendence, and calculate a rate.
gl_stads_sub_crimes_df <- gl_stads_crimes_df %>%
  filter(attendance > 1 & crime_count > 1) %>% 
  mutate(crime_rate = 10000*(crime_count/attendance)) 

# Scientific notation off.
options(scipen=99999)

# Visualize relationship across all teams.
p_counts <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_count), size = 0.5) +
  theme_bw() +
  theme(legend.position = "none")

p_rates <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_rate), size = 0.5) +
  theme_bw() +
  theme(legend.position = "none")

main_plot <- plot_grid(p_counts, p_rates, nrow = 1)

# Save.
ggsave(plot = main_plot, filename = "visuals/mbl.png", height = 10, width = 25, unit = "cm")

# Visualize relationship by team.
pt_counts <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_count, colour = NAME), size = 0.7) +
  facet_wrap(~NAME, scales = "free") +
  labs(y = "Crime count") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6))  

pt_rates <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_rate, colour = NAME), size = 0.7) +
  facet_wrap(~NAME, scales = "free") +
  labs(y = "Crime rate per 10,000 attendees") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6)) 

# Plot.
facet_plot <- plot_grid(pt_counts, pt_rates, nrow = 1)

title_plot <- ggdraw() +
  draw_label("Relationship between crime and attendance at Major League Baseball games during 2018.", size = 10, hjust = 0.5)


full_plot <- plot_grid(title_plot, facet_plot, nrow = 2, rel_heights = c(0.1,1))

# Save.
ggsave(plot = full_plot, filename = "visuals/mbl_facet.png", height = 12, width = 30, unit = "cm")

# Checking building footprint data from OSM.

# Define bounding boxes around stadium locations.
stad_bb_list <- list()

for (i in sort(stads_buffers_sf$stadium_name)) {

bb_sf <- stads_buffers_sf %>%
  filter(stadium_name == i) %>% 
  st_bbox() %>% 
  st_as_sfc(crs = 2163) %>% 
  st_transform(crs = 4326) %>% 
  st_bbox()

stad_bb_list[[i]] <- bb_sf

}

# Extract bb numbers into lists.
bb_fun <- function(x) {
  c(x[[1]],x[[2]],x[[3]],x[[4]])
}
  
bb_extracted_list <- lapply(stad_bb_list, bb_fun)  

# Loop OSM query for each bounding box.
osm_fun <- function(x){
  opq(bbox = x) %>%
    add_osm_feature(key = 'building') %>% 
    osmdata_sf()
}

osm_result_list <- lapply(bb_extracted_list, osm_fun)

# Extract polygons from results and project.
osm_result_poly_list <- lapply(osm_result_list, function(x) x$osm_polygons %>% st_transform(crs = 2163))

# Split buffer df into list.
stads_buffers_list <- group_split(stads_buffers_sf, stadium_name)

# Name elements according to other list (alphabetical).
names(stads_buffers_list) <- names(osm_result_poly_list)

# Example plot. 
plot(st_geometry(osm_result_poly_list[[10]]))
plot(st_geometry(stads_buffers_list[[10]]), add = T, border = "red")

# Check validity. This may change over time due to people's edits.
validity_check <- lapply(osm_result_poly_list, st_is_valid)

# Tabulate. For me, there is just at the Yankee Stadium II - the tenth element in the list.
lapply(validity_check, table)

# Resolve the invalid geometry.
osm_result_poly_list[[10]] <- st_make_valid(osm_result_poly_list[[10]])

# Check it is resolved. Yes.
table(st_is_valid(osm_result_poly_list[[10]]))

# We still get some invalid warnings with the intersections below, so we try a buffer hack.
# https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec
osm_result_poly_list <- lapply(osm_result_poly_list, function(x){st_buffer(x = x, dist = 0)})

# Run intersection on each element pairs of these lists.
osm_result_poly_clipped_list <- map2(stads_buffers_list, osm_result_poly_list, st_intersection)

# Calculate areal building footprint for each stadium buffer.
footprint_list <- lapply(osm_result_poly_clipped_list, function(x) sum(st_area(x)))

# Get footprint measurements into df.
footprint_df <- footprint_list %>% 
  bind_rows() %>% 
  mutate(id = 1) %>% 
  pivot_longer(cols = -id, names_to = "NAME", values_to = "area") %>% 
  select(-id) %>% 
  rename(build_footprint = area) 

# Join info back with the game data.
gl_stads_sub_crimes_df <- left_join(gl_stads_sub_crimes_df, footprint_df)

# Calculate total area of buffer (same for each stadium so I just use first element in list).
buffer_area <- st_area(stads_buffers_list[[1]])

# Calculate attendance density. 
gl_stads_sub_crimes_df <- gl_stads_sub_crimes_df %>% 
  mutate(area_total = buffer_area,
         free_footprint = area_total-build_footprint,
         attend_density = attendance/free_footprint,
         attend_density_n = as.numeric(attend_density))

# Relationship between crowd density and crime across all stadiums.
p_agg_density <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_jitter(mapping = aes(x = attend_density_n, y = crime_count, colour = NAME)) + # changed to jitter because looked not so nice as the other one due to fewer crimes... 
  labs(x = "Crowd density", y = "Crime count", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7))
        
        
        
ggsave(plot = p_agg_density, filename = "visuals/density_total.png", height = 14, width = 14, unit = "cm")

# Visualize for counts and rates by stadium.
p_c_density <- ggplot(data = gl_stads_sub_crimes_df) +
  theme_bw() +
  geom_point(mapping = aes(x = attend_density_n, y = crime_count, colour = NAME), size = 0.7) +
  facet_wrap(~ NAME, scale = "free") +
  labs(x = NULL, y = "Crime count") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6))

p_cr_density <- ggplot(data = gl_stads_sub_crimes_df) +
  theme_bw() +
  geom_point(mapping = aes(x = attend_density_n, y = crime_rate, colour = NAME), size = 0.7) +
  facet_wrap(~ NAME, scale = "free") +
  labs(x = "Crowd density", y = "Crime rate per 10,000 attendees") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6)) 

# Plot nrow = 2
density_facet_plot <- plot_grid(p_c_density, p_cr_density, nrow = 2)

# Save basic plot.
ggsave(plot = density_facet_plot, filename = "visuals/density_facet.png", height = 22, width = 16, unit = "cm")

# Plot nrow = 1
density_facet_plot <- plot_grid(p_c_density, p_cr_density, nrow = 1)

# Add title.
title_plot <- ggdraw() +
  draw_label("Relationship between crime and crowd density at Major League Baseball games during 2018.", size = 10, hjust = 0.5)

density_full_plot <- plot_grid(title_plot, density_facet_plot, nrow = 2, rel_heights = c(0.1,1))

# Save with title.
ggsave(plot = density_full_plot, filename = "visuals/density_facet.png", height = 12, width = 30, unit = "cm")

# Check distribution of attendance and attendance density.
# p1 <- ggplot(data = gl_stads_sub_crimes_df) +
#   geom_density(mapping = aes(attendance)) +
#   facet_wrap(~ NAME, scale = "free")
# 
# p2 <- ggplot(data = gl_stads_sub_crimes_df) +
#   geom_density(mapping = aes(attend_density_n)) +
#   facet_wrap(~ NAME, scale = "free")
# 
# plot_grid(p1, p2) 

# Make example plot of stadium footprints. We use Wrigley Field and Dogers Stadium
# as they are quite different in terms of how built-up they are.

# Pull crimes for each.
dodge_crimes_sf <- stads_crimes_buffers_sf %>% 
  filter(stadium_name == "Dodger Stadium")

wrigley_crimes_sf <- stads_crimes_buffers_sf %>% 
  filter(stadium_name == "Wrigley Field")

# Subset all buildings for each.
dodge_build_sf <- osm_result_poly_list[["Dodger Stadium"]]
wrigley_build_sf <- osm_result_poly_list[["Wrigley Field"]]

# Subset all buildings clipped to the buffer for each.
dodge_build_buf_sf <- osm_result_poly_clipped_list[["Dodger Stadium"]]
wrigley_build_buf_sf <- osm_result_poly_clipped_list[["Wrigley Field"]]

# Subset actual buffer for each.
dodge_buf_sf <- stads_buffers_list[["Dodger Stadium"]]
wrigley_buf_sf <- stads_buffers_list[["Wrigley Field"]]

# Plot Guaranteed Rate Field.
dodge_map_gg <- ggplot() +
  geom_sf(data = dodge_buf_sf, col = "black") +
  geom_sf(data = dodge_build_sf, size = 0.1, col = "darkgrey", fill = "darkgrey", alpha = 0.8) +
  geom_sf(data = dodge_build_buf_sf, size = 0.1, fill = "black", colour = "black", alpha = 1) +
  geom_sf(data = dodge_crimes_sf, col = "red", alpha = 0.2) +
  scale_fill_viridis_c() +
  labs(fill = NULL, title = "Dodgers Stadium, Los Angeles") +
  theme_bw() +
  theme(plot.title = element_text(size = 9),
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6)) 

# Plot wrigleyfman Stadium.
wrigley_map_gg <- ggplot() +
  geom_sf(data = wrigley_buf_sf, col = "black") +
  geom_sf(data = wrigley_build_sf, size = 0.1, col = "darkgrey", fill = "darkgrey", alpha = 0.8) +
  geom_sf(data = wrigley_build_buf_sf, size = 0.1, fill = "black", colour = "black", alpha = 1) +
  geom_sf(data = wrigley_crimes_sf, col = "red", alpha = 0.2) +
  scale_fill_viridis_c() +
  labs(fill = NULL, title = "Wrigley Field, Chicago") +
  theme_bw() +
  theme(plot.title = element_text(size = 9),
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 7),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6)) 

# Arrange.
maps_plot <- plot_grid(dodge_map_gg, wrigley_map_gg, nrow = 2)

# Save.
ggsave(plot = maps_plot, filename = "visuals/dodge_wrigley_maps.png", height = 26, width = 16, unit = "cm")

# Correlation test. Note that there are some ties.
count_test <- cor.test(gl_stads_sub_crimes_df$attend_density_n, gl_stads_sub_crimes_df$crime_count, method = "spearman")
plot(gl_stads_sub_crimes_df$attend_density_n, gl_stads_sub_crimes_df$crime_count)
count_test

# ================================================ #
# Save workspace so we have this.                  #
# save.image(file = "mlb_density_workspace.RData") #
load(file = "mlb_density_workspace.RData") 
# ================================================ #

# Distribution of dependent variable.
ggplot(data = gl_stads_sub_crimes_df) +
  geom_density(mapping = aes(x = crime_count))

# Check log.
ggplot(data = gl_stads_sub_crimes_df) +
  geom_density(mapping = aes(x = log(crime_count)))

# Create dummies for control (not required - used for checks).
gl_stads_sub_crimes_df <- gl_stads_sub_crimes_df %>% 
  fastDummies::dummy_columns(select_columns = "NAME")

# Is density better than population count to predict crime?

# Scientific notation off to see full results.
options(scipen=99999

# Models 1: attendance and attendance density as IV.
m1a <- glm(crime_count ~ attendance, data = gl_stads_sub_crimes_df, family = "poisson")
summary(m1a)

m1b <- glm(crime_count ~ attend_density_n, data = gl_stads_sub_crimes_df, family = "poisson")
summary(m1b)

m1c <- lm(log(crime_count) ~ attendance, data = gl_stads_sub_crimes_df)
summary(m1c)

m1d <- lm(log(crime_count) ~ attend_density_n, data = gl_stads_sub_crimes_df)
summary(m1d)

# Models 2: controlling for stadium and interaction.
m2a <- glm(crime_count ~ attendance + attendance*NAME, data = gl_stads_sub_crimes_df, family = "poisson")
summary(m2a)

m2b <- glm(crime_count ~ attend_density_n + attend_density_n*NAME, data = gl_stads_sub_crimes_df, family = "poisson")
summary(m2b)

m2c <- lm(log(crime_count) ~ attendance + attendance*NAME, data = gl_stads_sub_crimes_df)
summary(m2c)

m2d <- lm(log(crime_count) ~ attend_density_n + attend_density_n*NAME, data = gl_stads_sub_crimes_df)
summary(m2d)
