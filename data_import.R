library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(plotly)

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("data/ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

ext_tracks_clean <- ext_tracks %>%
    # keep only the relevant columns
    select(storm_name, month, day, hour, year, latitude, 
           longitude, starts_with("radius_")) %>% 
    # create a new storm_id based on name and year
    mutate(storm_id = str_c(str_to_title(storm_name), year, sep = "-"),
           date = str_c(year, month, day, sep = "-"),
           time = str_c(hour, ":00:00"),
           date = str_c(date, time, sep = " "),
           date = ymd_hms(date),
           longitude = -longitude) %>%
    # drop the columns we no longer need
    select(-day, -month, -year, -hour, -time) %>% 
    gather(-storm_id, -storm_name, -latitude, -longitude, -date,
           key = key,
           value = value) %>%
    mutate(key = str_remove(key, "radius_")) %>% 
    separate(key, into = c("wind_speed", "key2")) %>% 
    spread(key2, value)

ggplot(data = ext_tracks_clean, aes(x = longitude, y = latitude)) +
    geom_point()

data_ike <- ext_tracks_clean %>% 
    filter(storm_name == "IKE")

ggplot(data = data_ike, aes(x = longitude, y = latitude)) +
    geom_point() +
    coord_map()

usa <- map_data("usa")
states <- map_data("state")
texas <- states %>% filter(region == "texas")
ike_plot <- ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", 
                 col = "black", alpha = 0.5) + 
    coord_fixed(1.3) +
    geom_point(data = data_ike, aes(x = longitude, y = latitude, label = date), 
               col = "red")

ggplotly(ike_plot, tooltip = c("label"))

texas_ike <- ggplot(data = texas) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", 
                 col = "black", alpha = 0.5) + 
    coord_fixed(1.3) +
    geom_point(data = data_ike, aes(x = longitude, y = latitude, label = date))

ggplotly(texas_ike, tooltip = c("label"))

# based on the above visualisations use 2008-09-13 06:00:00
data_ike_final <- data_ike %>%
    filter(date == ymd_hms("2008-09-13 06:00:00"))
