#' Read hurricane data
#' 
#' @importFrom readr read.fwf fwf_widths
#'
#' @param filename path to hurricane data to be read
#' 
#' @example 
#' \dontrun{
#'   read_hur_data("data/ebtrk_atlc_1988_2015.txt")
#' }
#' 
#' @export
read_hur_data <- function(filename) {
    
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 
                           3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                             "hour", "year", "latitude", "longitude",
                             "max_wind", "min_pressure", "rad_max_wind",
                             "eye_diameter", "pressure_1", "pressure_2",
                             paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final")
    
    ext_tracks <- readr::read_fwf(filename, 
                                  fwf_widths(ext_tracks_widths, 
                                             ext_tracks_colnames),
                                  na = "-99")
    
    return(ext_tracks)
    
}


#' Tidy hurricane data that conforms to assignment criteria
#'
#' takes the raw hurricane data and cleans it according to assignment
#' instructions
#'
#' @importFrom stringr str_c str_to_title
#' @importFrom dplyr mutate select
#' @importFrom tidyr gather spread
#' @importFrom magritrr %>%
#' @importFrom tidyselect starts_with
#'
#' @param data data to tidy
#'
#' @export
tidy_hur_data <- function(data) {
    data %>% 
        # Configure the storm_id and date 
        mutate(
            storm_id = str_c(str_to_title(storm_name), year, sep = '-'),
            date = str_c(year, '-', month, '-', day, ' ', hour, ':', 
                         '00', ':', '00'),
            longitude = -longitude
        ) %>% 
        # Select only the relevant columns
        select(storm_id, date, longitude, latitude, 
                      starts_with("radius_")) %>%
        gather(-storm_id, -date,-latitude, -longitude,
                      key = key,
                      value = value) %>% 
        mutate(key = str_remove(key, "radius_")) %>% 
        separate(key, into = c("wind_speed", "key2")) %>% 
        spread(key2, value) %>% 
        select(storm_id, date, latitude, longitude, wind_speed,
               ne, nw, se, sw)
}


#' Function to build the hurricane geom
#'
#' @importFrom ggplot2 layer
#'
#' @param mapping Set of aesthetic mappings
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a
#'   warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'   the default, includes if any aesthetics are mapped. FALSE never includes,
#'   and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define
#'   both data and aesthetics and shouldn't inherit behaviour from the default
#'   plot specification, e.g. borders.
#'
#' @export
geom_hurricane <- function(mapping = NULL, 
                           data = NULL, 
                           stat = "identity",
                           position = "identity", 
                           na.rm = FALSE,
                           show.legend = NA, 
                           inherit.aes = TRUE, ...){
    layer(data = data, mapping = mapping, stat = stat,
        geom = GeomHurricane,
        position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


#' Function to construct a new class for the geom_hurricane
#'
#' this function will create a wind radii for a given storm obseravtion. It is
#' to be used with maps construct with get_map
#'
#' @importFrom ggplot2 ggproto
#' @importFrom base data.frame as.character
#' @importFrom dplyr bind_rows rename mutate
#' @importFrom grid polygonGrob gpar
#' @importFrom geosphere destPoint
#'
#' @param required_aes required aesthetic arguments for the geom_hurricane
#'   supplied in character vector
#' @param default_aes default values for aesthetic arguments
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group where the bulk of this geom is constructed
#'
#' @examples
#' \dontrun{
#'   geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, color = wind_speed)
#' }
#'
#' @export
GeomHurricane <- ggplot2::ggproto(
    "GeomHurricane", 
    Geom,
    required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
    default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
    draw_key = draw_key_polygon,
    draw_group = function(data, panel_scales, coord) {
        ## Transform the data first
        coords <- coord$transform(data, panel_scales)
                                             
        # Convert nautical miles to meters and multiply by scale factor
        data <- data %>% 
            mutate(r_ne = r_ne * 1852 * scale_radii,
                   r_se = r_se * 1852 * scale_radii,
                   r_sw = r_sw * 1852 * scale_radii,
                   r_nw = r_nw * 1852 * scale_radii)
        
        # Loop over the data and create the points for each quandrant
        for (i in 1:nrow(data)) {
            # Create the Northwest Quandrant
            df_nw <- data.frame(
                colour = data[i,]$colour,
                fill = data[i,]$fill,
                geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                     b = 270:360,
                                     d = data[i,]$r_nw),
                group = data[i,]$group,
                PANEL = data[i,]$PANEL,
                alpha = data[i,]$alpha)
            
            # Create the Northeast Quandrant
            df_ne <- data.frame(
                colour = data[i,]$colour,
                fill = data[i,]$fill,
                geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                     b = 1:90,
                                     d = data[i,]$r_ne),
                group = data[i,]$group,
                PANEL = data[i,]$PANEL,
                alpha = data[i,]$alpha)
            
            # Create the Southeast Quandrant
            df_se <- data.frame(
                colour = data[i,]$colour,
                fill = data[i,]$fill,
                geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                     b = 90:180,
                                     d = data[i,]$r_se),
                group = data[i,]$group,
                PANEL = data[i,]$PANEL,
                alpha = data[i,]$alpha)
            
            # Create the Southwest Quandrant
            df_sw <- data.frame(
                colour = data[i,]$colour,
                fill = data[i,]$fill,
                geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                     b = 180:270,
                                     d = data[i,]$r_sw),
                group = data[i,]$group,
                PANEL = data[i,]$PANEL,
                alpha = data[i,]$alpha)
            
        # bind all the rows into a dataframe
        df_points <- bind_rows(list(df_nw, df_ne, df_se, df_sw))}
        
        # Rename columns x and y from lon and lat repectively
        df_points <- df_points %>% rename(
            x = lon,
            y = lat)
        
        # Convert to character
        df_points$colour <- as.character(df_points$colour)
        df_points$fill <- as.character(df_points$fill)
        
        ## transform data points
        coords_df <- coord$transform(df_points, panel_scales)
                                             
        ## Construct grid polygon
        grid::polygonGrob(x = coords_df$x,
                          y = coords_df$y,
                          gp = grid::gpar(col = coords_df$colour, 
                                          fill = coords_df$fill, 
                                          alpha = coords_df$alpha))
                                         }
                                         
)

# Load relevant packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(geosphere)
library(ggmap)

# Read data, tidy it and filter it for Ike-2008 & September 13 @ 6am
ike_data <- read_hur_data("data/ebtrk_atlc_1988_2015.txt") %>% 
    tidy_hur_data() %>% 
    filter(storm_id == "Ike-2008", date == "2008-09-13 06:00:00")

# Create map and add hurricane Ike storm observation

# create the Louisiana and Texas map (via a bounding box)
tl_bbox <- c(-107, 24, -86, 37)
map_tl <- get_stamenmap(tl_bbox, zoom = 6, maptype = "toner-background")

# test the map without the hurricane geom
ggmap(map_tl) 

# add the hurricane geom to the map
ggmap(map_tl) +
    geom_hurricane(data = ike_data,
                   aes(x = longitude, y = latitude, 
                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                       fill = wind_speed, color = wind_speed), alpha = 0.6) + 
    scale_color_manual(name = "Wind speed (kts)", 
                       values = c("red", "orange", "yellow")) + 
    scale_fill_manual(name = "Wind speed (kts)", 
                      values = c("red", "orange", "yellow")) +
    ggtitle("Wind speeds for hurricane Ike on September 13, 2008 at 06:00")

