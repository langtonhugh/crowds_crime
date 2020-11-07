library(crimedata)     # crime data
library(nflscrapR)     # NFL data
library(janitor)       # cleaning
library(dplyr)         # wrangling
library(stringr)       # wrangling
library(tidyr)         # wrangling
library(sf)            # maps
library(lubridate)     # dates
library(maps)          # boundaries
library(ggplot2)       # visual
library(pdftools)      # get pdf data


# NFL attendance data.
nfl_attend <- read.csv("data/nfl_2018.csv")

# NFL location data.
nfl_coords <- read.csv("https://raw.githubusercontent.com/Sinbad311/CloudProject/master/NFL%20Stadium%20Latitude%20and%20Longtitude.csv")

# Manual check suggests that the coordinates for LA teams might not be correct for 2018 (the crime
# data year) because both teams rellocated in the last decade or so.

# Clean coords.
nfl_coords <- nfl_coords %>% 
  clean_names() %>% 
  mutate(team = if_else(team == "Forty-Niners", "49ers", as.character(team)))

# Clean attendance.
nfl_attend <- nfl_attend %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(team = last(str_split(tm, " ")[[1]])) %>% 
  left_join(., nfl_coords, by = c('team' = 'team')) %>% 
  filter(!is.na(conference))

# Cities in the open crime data.
crimedata_cities <- data.frame(
  city = c("Chicago", "Detroit", "Fort Worth", "Kansas City", "Los Angeles", "Louisville", "New York", "San Francisco", "Tucson", "Virginia	Beach")
)

# Further clean and join.
crim_nfl <- nfl_attend %>% 
  mutate(city = str_replace(tm, team, ""), 
         city = trimws(city)) %>% 
  left_join(crimedata_cities) %>% 
  select(-home, -away, -total, -x, -pic) %>% 
  pivot_longer(cols = starts_with("week"), names_to = "week", values_to = "attend") %>% 
  mutate(attend_clean = str_replace(attend, "\\\\", ""),
         attend_clean = str_replace(attend_clean, "\\*", ""),
         attend_clean = trimws(attend_clean),
         attend_clean = ifelse(attend_clean == "Bye", NA, attend_clean)) %>% 
  drop_na(attend_clean)

# Read in pdf.
thing <- pdf_text("https://nflcommunications.com/Documents/2018%20Offseason/04%2019%2018%20-%202018%20Schedule%20Release.pdf") %>% strsplit(split = "\n")

# Subset pages 3 to 9 for the relevant info.
thing <- unlist(thing[3:9]) 

# Create df to grab week titles.
weeknums <- data.frame(
  rownums = which(grepl("WEEK", thing)), 
  weeks = thing[which(grepl("WEEK", thing))]
)

# Create df for dates of games.
datenums <- data.frame(
  rownums = which(grepl(", 2018", thing)), 
  dates = thing[which(grepl(", 2018", thing))]
)

# Create data frame for games e.g. team1 at team2.
gamenums <- data.frame(
  rownums = which(grepl(" at ", thing)), 
  games = thing[which(grepl(" at ", thing))]
)

# IDs.
link_df <- data.frame(
  linkthing = c(1:329)
)

# Loop through weeks.
datalist <- list()
for (i in 1:nrow(weeknums)) {
  if (i < nrow(weeknums)) {
    datalist[[i]] = data.frame(
      linkthing = c((weeknums$rownums[i]-weeknums$rownums[1]+1):(weeknums$rownums[i + 1] - weeknums$rownums[1])),
      weeknum = weeknums$weeks[i])
  } else {
    datalist[[i]] = data.frame(linkthing = c(weeknums$rownums[i]:331),
                               weeknum = weeknums$weeks[i])
  }
}

results <- do.call(rbind, datalist)

link_df <- left_join(link_df, results)

# Loop through dates.
datalist <- list()
for (i in 1:nrow(datenums)) {
  if (i < nrow(datenums)) {
    datalist[[i]] = data.frame(
      linkthing = c((datenums$rownums[i]-datenums$rownums[1]+1):(datenums$rownums[i + 1] - datenums$rownums[1])),
      datenum = datenums$dates[i])
  } else {
    datalist[[i]] = data.frame(linkthing = c(datenums$rownums[i]:331),
                               datenum = datenums$dates[i])
  }
}
results <- do.call(rbind, datalist)

link_df <- left_join(link_df, results)

# Look through versus teams.
datalist <- list()
for (i in 1:nrow(gamenums)) {
  if (i < nrow(gamenums)) {
    datalist[[i]] = data.frame(
      linkthing = c((gamenums$rownums[i]-gamenums$rownums[1]+1):(gamenums$rownums[i + 1] - gamenums$rownums[1])),
      game = gamenums$games[i])
  } else {
    datalist[[i]] = data.frame(linkthing = c(gamenums$rownums[i]:331),
                               game = gamenums$games[i])
  }
}

results <- do.call(rbind, datalist)

# Clean and tidy up.
link_cleaned_df <- link_df %>% 
  left_join(results) %>% 
  select(-linkthing) %>% 
  unique() %>% 
  rowwise() %>%
  mutate(datenum = trimws(datenum)) %>% 
  separate(col = datenum, sep = " ", into = c("dayweek", "month", "daynum", "year"), remove = F) %>% 
  unite(col = "full_date", month, daynum, year, sep = " ") %>%
  filter(full_date != "NA NA NA") %>% 
  mutate(full_date_ymd = mdy(full_date),
         week          = trimws(tolower(gsub(" ", "_", trimws(weeknum))))) %>%
  separate(game, into = c("game_cleaned", "time1", "time2", "network"), sep = "  +", remove = F) %>% 
  separate(game_cleaned, into = c("away","home"), sep = " at ", remove = F) %>% # warning is fine - just intro NA.
  mutate(home  = str_replace(home, " \\(.*\\)", ""),       
         home  = str_replace_all(home, "[[:punct:]]", ""),
         away  = str_replace_all(away, "[[:punct:]]", ""),
         time1 = str_replace_all(time1, pattern = "FOX/NFLN\r|NBC\r|ESPN\r|FOX\r|FOX/NFLN" , replacement = ""),
         time2 = str_replace_all(time2, pattern = "FOX/NFLN\r|NBC\r|ESPN\r|FOX\r|CBS| \r|NFLN\r|TBD|\r" , replacement = ""))

# Leaving the time cleaning for now because I think crime on the whole day is more reasonable
# than a specific time frame due to pre and after-events.

# Join.
all_nfl <- crim_nfl %>% 
  rename(home = tm) %>% 
  left_join(link_cleaned_df) %>% 
  drop_na(full_date_ymd) %>% 
  filter(city %in% crimedata_cities$city)

# Download open crime data for 2018 and filter for the cities with NFL games.
crimes_sf <- get_crime_data(
  years = 2018, 
  cities = unique(all_nfl$city), # comment out for crimes_all_sf
  type = "core",
  output = "sf"
) 

# crimes_all_sf <- crimes_sf
# st_write(obj = crimes_all_sf, dsn = "data/crimes_all.shp")


# Format dates and subset crimes for the dates on which there were NFL games.
crimes_filter_sf <- crimes_sf %>% 
  mutate(date_single = str_extract(date_single, "^.{10}"),
         date_single_ymd = ymd(date_single)) %>% 
  filter(date_single_ymd %in% all_nfl$full_date_ymd) %>% 
  st_transform(2163)

# USA map for check
usa_sf <- st_as_sf(map("state", fill=TRUE, plot =FALSE))
usa_sf <- st_transform(usa_sf, st_crs(crimes_filter_sf))
plot(st_geometry(usa_sf))
plot(st_geometry(crimes_filter_sf), add = T, col = "red")

# Stadium location checks
stads_df <- all_nfl %>%
  distinct(home, .keep_all = T) %>% 
  select(home, latitude, longitude)

# Create buffer around stadium locations.
nfl_buffers_sf <- all_nfl %>%
  distinct(home, .keep_all = T) %>% 
  select(home, latitude, longitude) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326) %>% 
  st_transform(2163) %>% 
  st_buffer(dist = 1609)

# usa_sf <- st_as_sf(map("state", fill=TRUE, plot =FALSE))
# usa_sf <- st_transform(usa_sf, st_crs(nfl_buffers_sf))
ggplot() +
  geom_sf(data = usa_sf) +
  geom_sf(data = crimes_filter_sf, col = "pink", alpha = 0.1) +
  geom_sf(data = nfl_buffers_sf, col = "red", size = 1) 


# save for QGIS
st_write(obj = nfl_buffers_sf  , dsn = "data/nfl_buffers.shp")
# st_write(obj = crimes_filter_sf, dsn = "data/crimes_filter.shp")
st_write(obj = crimes_sf, dsn = "data/crimes.shp")


# plot check
# plot(st_geometry(filter(nfl_buffers_sf, home == "San Francisco 49ers")))
# plot(st_geometry(filter(crimes_filter_sf)), add = T)
# 
# plot(st_geometry(filter(nfl_buffers_sf, home == "Los Angeles Chargers")))
# plot(st_geometry(filter(crimes_sf)), add = T)

# test
# crimes_sf <- st_transform(crimes_sf, 2163)

# Clip crimes by these buffers.
nfl_crimes_sf <- st_intersection(nfl_buffers_sf, crimes_filter_sf)

# Note we have lost some - I think this is because (1) crime data does not capture New Jersey where the
# New York teams' stadiums are, and (2) the LA teams stadium location is San Diego.
unique(nfl_crimes_sf$home)

# Join nfl_crimes_sf with all_nfl

# city_name         city
# home              home
# date_single_ymd   full_date_ymd

# Aggregate by team
nfl_crimes_df <- nfl_crimes_sf %>% 
  st_set_geometry(NULL) %>% 
  group_by(home, date_single_ymd) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  rename(full_date_ymd = date_single_ymd) 

# Join attendence data.
nfl_crimes_att_df <- nfl_crimes_df %>% 
  left_join(all_nfl) %>% 
  drop_na(team) %>% 
  mutate(attend = as.numeric(attend),
         crime_rate = (crime_count/attend)*1000)
  

# Visual
ggplot(data = nfl_crimes_att_df, mapping = aes(x = attend, y = crime_count, colour = home)) +
  geom_point() 

ggplot(data = nfl_crimes_att_df, mapping = aes(x = attend, y = crime_rate, colour = home)) +
  geom_point() 

# Test
cor.test(nfl_crimes_att_df$crime_count, nfl_crimes_att_df$attend, method = "pearson") # nowt

#============================================================

# ggplot()+ 
#   geom_sf(data = st_geometry(usa_poly %>% filter(name != "Hawaii", name != "Alaska"))) + 
#   geom_sf(data = nfl_crimes_sf, col = "red") + 
#   theme_void() + 
#   theme(panel.grid.major = element_line(colour = "white"))
# 
# crimes_against_persons <- nfl_crimes_sf %>% 
#   st_set_geometry(NULL) %>% 
#   filter(offense_against == "persons") %>% 
#   group_by(tm, week, attendance) %>% 
#   count()
# 
# 
# 
# crimes_against_property <- nfl_crimes_sf %>% 
#   st_set_geometry(NULL) %>% 
#   filter(offense_against == "property") %>% 
#   group_by(tm, week, attendance) %>% 
#   count() %>% 
#   mutate(crime_per_1000_ppl = n/attendance*1000)
# 
# ggplot(data = crimes_against_property, aes(x = attendance, y = crime_per_1000_ppl)) + 
#   geom_point() +
#   geom_line() + 
#   facet_wrap(~tm, scales = "free")



