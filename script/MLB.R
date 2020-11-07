library(tmaptools)
library(maps)
library(cowplot)
library(crimedata)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(sf)

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

# Save workspace so we have this.
# save.image(file = "mlb_workspace.RData")

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
usa_sf <- st_as_sf(map("state", fill=TRUE, plot =FALSE))
usa_sf <- st_transform(usa_sf, 4326)

# Plot.
# ggplot() +
#   geom_sf(data = usa_sf) +
#   geom_sf(data = stads_sf)

# Transform stadiums, crimes and USA to a projected CRS (https://epsg.io/2163).
# Note: what is the impact of choosing a different projection?
stads_sf      <- st_transform(stads_sf, 2163)
crimes_sub_sf <- st_transform(crimes_sub_sf, 2163)
usa_sf        <- st_transform(usa_sf, 2163)

# Visual inspection.
# ggplot() +
#   geom_sf(data = usa_sf) +
#   geom_sf(data = stads_sf)

# Create buffers around stadiums.
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
  as_tibble() %>% 
  group_by(stadium_name, date_ymd) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(stadium_name, date_ymd, fill = list(crime_count = 0))
  
# Check frequencies across all days.
# ggplot(data = stads_crimes_df) +
#   geom_histogram(mapping = aes(crime_count), bins = 30)

# Rename variables for join.
stads_crimes_df <- stads_crimes_df %>% 
  rename(NAME = stadium_name,
         game_date_ymd = date_ymd)

# Join back attendance data.
gl_stads_crimes_df <- left_join(gl_2018_pitch_df, stads_crimes_df)

# Only keep games with more than 1 crime, and calculate a rate.
gl_stads_sub_crimes_df <- gl_stads_crimes_df %>% 
  filter(crime_count > 1) %>% 
  mutate(crime_rate = 10000*(crime_count/attendance))

# Scientific notation off.
options(scipen=99999)

# Visualize attendance. If not much variation, can we really adjust by this?
# ggplot(data = gl_stads_sub_crimes_df) +
#   geom_histogram(mapping = aes(x = attendance), bins = 60) +
#   facet_wrap(~NAME, scales = "free_y")

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

# Correlation.
count_test <- cor.test(gl_stads_sub_crimes_df$attendance, gl_stads_sub_crimes_df$crime_count, method = "spearman")
rate_test  <- cor.test(gl_stads_sub_crimes_df$attendance, gl_stads_sub_crimes_df$crime_rate , method = "spearman")

# Visualize relationship by team.
pt_counts <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_count, colour = NAME), size = 0.5) +
  facet_wrap(~NAME, scales = "free") +
  labs(y = "Crime count") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 4.5),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6))  

pt_rates <- ggplot(data = gl_stads_sub_crimes_df) +
  geom_point(mapping = aes(x = attendance, y = crime_rate, colour = NAME), size = 0.5) +
  facet_wrap(~NAME, scales = "free") +
  labs(y = "Crime rate per 10,000 attendees") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 4.5),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 6)) 

# Plot.
full_plot <- plot_grid(pt_counts, pt_rates, nrow = 1)

# Save.
ggsave(plot = full_plot, filename = "visuals/mbl_facet.png", height = 12, width = 30, unit = "cm")





