library(crimedata) # crime data
library(nflscrapR) # NFL data
library(janitor) # for cleaning
library(dplyr) # for wrangling
library(stringr) # for wrangling
library(tidyr) # for wrangling
library(sf)
library(lubridate)


nfl_attend <- read.csv("stadium_attendance_data/nfl.csv")
nfl_coords <- read.csv("https://raw.githubusercontent.com/Sinbad311/CloudProject/master/NFL%20Stadium%20Latitude%20and%20Longtitude.csv")

nfl_coords <- nfl_coords %>% 
  clean_names() %>% 
  mutate(team = if_else(team == "Forty-Niners", "49ers", as.character(team)))

nfl_attend <- nfl_attend %>% 
  clean_names() %>% 
  rowwise() %>% 
  mutate(team = last(str_split(tm, " ")[[1]])) %>% 
  left_join(., nfl_coords, by = c('team' = 'team')) %>% 
  filter(!is.na(conference))


crimedata_cities <- data.frame(
  city = c("Chicago", "Detroit", "Fort Worth", "Kansas City", "Los Angeles", "Louisville", "New York", "San Francisco", "Tucson", "Virginia	Beach")
)


crim_nfl <- nfl_attend %>% 
  mutate(city = gsub(team, "", tm), 
         city = trimws(city)) %>% 
 left_join(crimedata_cities, ., by = c('city' = 'city')) %>% 
  select(-home, -away, -total, -x, -pic) %>% 
  pivot_longer(cols = starts_with("week"), names_to = "week") %>% 
  mutate(attendance = as.numeric(ifelse(grepl("\\*", value), NA, as.character(value))), 
         tm = trimws(tm), 
         week = trimws(week)) %>% 
  filter(!is.na(attendance))


#need to get dates from stupid pdf 
library(pdftools)


thing <- pdf_text("04-17-19-schedule.pdf") %>% strsplit(split = "\n")
thing <- unlist(thing[4:11])

weeknums <- data.frame(
  rownums = which(grepl("WEEK", thing)), 
  weeks = thing[which(grepl("WEEK", thing))]
)
datenums <- data.frame(
  rownums = which(grepl(", 2019", thing)), 
  dates = thing[which(grepl(", 2019", thing))]
)

gamenums <- data.frame(
  rownums = which(grepl(" at ", thing)), 
  games = thing[which(grepl(" at ", thing))]
)

linkdf <- data.frame(
  linkthing = c(1:331)
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

linkdf <- left_join(linkdf, results, by = c("linkthing", "linkthing"))


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

linkdf <- left_join(linkdf, results, by = c("linkthing", "linkthing"))


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


linkdf <- left_join(linkdf, results, by = c("linkthing", "linkthing")) %>% 
  select(-linkthing) %>% 
  unique() %>% 
  rowwise() %>% 
  mutate(game_date = trimws(paste(str_split(datenum, ",")[[1]][2:3], collapse = " ")), 
         game_time = gsub(str_split(trimws(game), "   ")[[1]][1], "", trimws(game)), 
         home_team =  trimws(str_split(str_split(trimws(game), "   ")[[1]][1], " at ")[[1]][2]), 
         home_team = gsub("#", "", home_team), 
         home_team = trimws(str_split(home_team, "\\(")[[1]][1]),
         game_datetime = mdy(game_date), 
         game_week = trimws(tolower(gsub(" ", "_", trimws(weeknum)))))



all_nfl <- left_join(crim_nfl, linkdf, by = c("tm" = "home_team", "week" = "game_week"))



crimes <- get_crime_data(
  years = 2019, 
  cities = unique(crim_nfl$city), 
  type = "core"
) 


crime_data <- get_crime_data()


