library(crimedata)     # crime data
library(nflscrapR)     # NFL data
library(janitor)       # cleaning
library(dplyr)         # wrangling
library(stringr)       # wrangling
library(tidyr)         # wrangling
library(sf)            # maps
library(lubridate)     # dates
library(rnaturalearth) # boundaries
library(ggplot2)       # visual
library(pdftools)      # get pdf data


# NFL attendance data.
nfl_attend <- read.csv("data/nfl_2018.csv")

# NFL location data.
nfl_coords <- read.csv("https://raw.githubusercontent.com/Sinbad311/CloudProject/master/NFL%20Stadium%20Latitude%20and%20Longtitude.csv")

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

weeknums <- data.frame(
  rownums = which(grepl("WEEK", thing)), 
  weeks = thing[which(grepl("WEEK", thing))]
)

datenums <- data.frame(
  rownums = which(grepl(", 2018", thing)), 
  dates = thing[which(grepl(", 2018", thing))]
)

gamenums <- data.frame(
  rownums = which(grepl(" at ", thing)), 
  games = thing[which(grepl(" at ", thing))]
)

linkdf <- data.frame(
  linkthing = c(1:329)
)


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

linkdf <- left_join(linkdf, results)


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

linkdf <- left_join(linkdf, results)


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

link_cleaned_df <- linkdf %>% 
  left_join(results) %>% 
  select(-linkthing) %>% 
  unique() %>% 
  rowwise() %>%
  mutate(datenum = trimws(datenum)) %>% 
  separate(col = datenum, sep = " ", into = c("dayweek", "month", "daynum", "year"), remove = F) %>% 
  unite(col = "full_date", month, daynum, year, sep = " ") %>%
  filter(full_date != "NA NA NA") %>% 
  mutate(full_date_mdy = mdy(full_date))


#=================================================================================================

  mutate(game_date     = trimws(paste(str_split(datenum, ",")[[1]][2:3], collapse = " ")))
  
  mutate(game_date = trimws(paste(str_split(datenum, ",")[[1]][2:3], collapse = " ")), 
         game_time = gsub(str_split(trimws(game), "   ")[[1]][1], "", trimws(game)), 
         home_team =  trimws(str_split(str_split(trimws(game), "   ")[[1]][1], " at ")[[1]][2]), 
         home_team = gsub("#", "", home_team),
         home_team = gsub("\\*", "", home_team),
         home_team = trimws(str_split(home_team, "\\(")[[1]][1]),
         game_datetime = mdy(game_date), 
         game_week = trimws(tolower(gsub(" ", "_", trimws(weeknum)))))



all_nfl <- left_join(crim_nfl, linkdf, by = c("tm" = "home_team", "week" = "game_week"))

crimes <- get_crime_data(
  years = 2018, 
  cities = unique(crim_nfl$city), 
  type = "core"
) 

crimes <- crimes %>% 
  mutate(date_single_2 = ymd_hms(date_single))

crimes$date_single_2 <- as.Date(crimes$date_single_2)

nfl_crimes <- left_join(all_nfl, crimes, by = c("game_datetime" = "date_single_2"))

# Using Albers projection (https://en.wikipedia.org/wiki/Albers_projection) for which crs is 9822
nfl_crimes_sf <- st_as_sf(nfl_crimes, coords = c("longitude.y", "latitude.y"), crs = 4326)
nfl_crimes_sf <- st_transform(nfl_crimes_sf, crs = 102008)

nfl_coords_sf <- st_as_sf(nfl_coords, coords = c("longitude", "latitude"), crs = 4326)
nfl_stadiums_sf <- st_as_sf(nfl_coords_sf, coords = c("longitude", "latitude"), crs = 4326)
nfl_stadiums_sf <- st_transform(nfl_coords_sf, crs = 102008)


usa_poly <- ne_states(geounit = "United States of America", returnclass = 'sf')
usa_poly <- st_transform(usa_poly, crs = 102008)


ggplot() +
  geom_sf(data = st_geometry(usa_poly %>% filter(name != "Hawaii", name != "Alaska"))) + 
  geom_sf(data = st_geometry(nfl_stadiums_sf), col = "blue") + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = "white"))

staduim_buff <- st_buffer(nfl_stadiums_sf, 1609) #make 1 mile buffer
nfl_crimes_sf <- st_intersection(staduim_buff, nfl_crimes_sf) #select only crimes within these buffers

count(nfl_crimes_sf, tm)


ggplot()+ 
  geom_sf(data = st_geometry(usa_poly %>% filter(name != "Hawaii", name != "Alaska"))) + 
  geom_sf(data = nfl_crimes_sf, col = "red") + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = "white"))

crimes_against_persons <- nfl_crimes_sf %>% 
  st_set_geometry(NULL) %>% 
  filter(offense_against == "persons") %>% 
  group_by(tm, week, attendance) %>% 
  count()



crimes_against_property <- nfl_crimes_sf %>% 
  st_set_geometry(NULL) %>% 
  filter(offense_against == "property") %>% 
  group_by(tm, week, attendance) %>% 
  count() %>% 
  mutate(crime_per_1000_ppl = n/attendance*1000)

ggplot(data = crimes_against_property, aes(x = attendance, y = crime_per_1000_ppl)) + 
  geom_point() +
  geom_line() + 
  facet_wrap(~tm, scales = "free")



