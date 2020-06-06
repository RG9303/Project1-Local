library(ggmap)
library(readr)
library(geosphere)
library(roxygen2)

#' @title GeomMap.R
#' @description This code include some function using the hurricane data for all locations in a map. First, we read the database
#' with format txt and transform a new database described by the wind speed principally.

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

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

#' @details The function \code{CapStr} converts a string with upper and lower letters in a specific position. We use this function when we
#' made the new data frame.

CapStr <- function(y){
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), tolower(substring(c, 2)),
        sep="", collapse=" ")
}

storm_observation <- data.frame(matrix(ncol = 9, nrow = nrow(ext_tracks)*3))
colnames(storm_observation) <- c("storm_id", "date", "latitude", "longitude", "wind_speed",
                                 "ne", "nw", "se", "sw")
storm_observation$wind_speed <- as.factor(c(rep(34, nrow(ext_tracks)), rep(50, nrow(ext_tracks)), rep(64, nrow(ext_tracks))))
storm_observation$ne <- c(ext_tracks$radius_34_ne, ext_tracks$radius_50_ne, ext_tracks$radius_64_ne)
storm_observation$nw <- c(ext_tracks$radius_34_nw, ext_tracks$radius_50_nw, ext_tracks$radius_64_nw)
storm_observation$se <- c(ext_tracks$radius_34_se, ext_tracks$radius_50_se, ext_tracks$radius_64_se)
storm_observation$sw <- c(ext_tracks$radius_34_sw, ext_tracks$radius_50_sw, ext_tracks$radius_64_sw)
storm_observation$date <- rep(strptime(paste0(ext_tracks$year, "-", ext_tracks$month, "-", ext_tracks$day, " ", ext_tracks$hour, ":00:00"), format="%Y-%m-%d %H:%M:%S"), 3)
storm_observation$storm_id <- rep(paste0(as.character(sapply(ext_tracks$storm_name, CapStr)), "-", ext_tracks$year), 3)
storm_observation$latitude <- rep(ext_tracks$latitude, 3)
storm_observation$longitude <- rep(-1*ext_tracks$longitude, 3)
storm_observation <- storm_observation[!is.na(storm_observation$ne),]
storm_observation <- storm_observation[!is.na(storm_observation$nw),]
storm_observation <- storm_observation[!is.na(storm_observation$se),]
storm_observation <- storm_observation[!is.na(storm_observation$sw),]

#' @details This is an example about a particular observation with the katrina hurricane in a specific time.

head(storm_observation[which(storm_observation$storm_id=="Katrina-2005" & storm_observation$date=="2005-08-29 12:00:00"),])


#' @title geom_hurricane
#' @description This function make a map across ggplot2 using a particular data frame with the parameter specified in another function.
#' @param ... We defined some parameters by default associated to this ggplot.
#' @return if the function GeomHurricane exists, this function will create an object that depends a coordenates and a scale radii.
#' @details you need install packages like ggplot2 and geomsphere before this or it may result in an error
#' @importFrom ggplot2 ggproto draw_key_polygon
#' @example geom_hurricane(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
#' @export
#' 
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}

#' @title draw_group
#' @description This function make an object with specific characteristics with differents radii.
#' @param data is a dataframe with a particular structure
#' @param panel_scales combine the parameters with the database
#' @param coord specify some radii for the object
#' @return an object geom that it will be used in the map
#' @details you need install packages like ggplot2 and geomsphere before this or it may result in an error
#' @importFrom geoshpere destPoint 
#' @importFrom grid polygonGrob
#' @example draw_group(data, panel_scales, coord)
#' @export
#' 

GeomHurricane <- ggplot2::ggproto(
  "GeomHurricane", 
  ggplot2::Geom,
  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
  default_aes = ggplot2::aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1, alpha = 0.8, scale_radii = 1),
  draw_key = ggplot2::draw_key_polygon,
  draw_group = function(data, panel_scales, coord){
    
    point_obs = c(data[1,]$x, data[1,]$y)
    color <- data[1,]$colour
    fill <- data[1,]$fill
    alpha <- data[1,]$alpha
    scale_radii = data[1,]$scale_radii
    
    points_polygon = geosphere::destPoint(p = point_obs, b=1:90, d = data[1,]$r_ne * 1852 * scale_radii)
    data_ne <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    points_polygon = geosphere::destPoint(p = point_obs, b=90:180, d = data[1,]$r_se * 1852 * scale_radii)
    data_se <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    
    points_polygon = geosphere::destPoint(p = point_obs, b=180:270, d = data[1,]$r_sw * 1852 * scale_radii)
    data_sw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    
    points_polygon = geosphere::destPoint(p = point_obs, b=270:360, d = data[1,]$r_nw * 1852 * scale_radii)
    data_nw <- data.frame(x = c(points_polygon[,"lon"], point_obs[1]),
                          y = c(points_polygon[,"lat"], point_obs[2])
    )
    
    data_all <- rbind(data_ne, data_se, data_nw, data_sw)
    coords <- coord$transform(data_all, panel_scales)
    
    grid::polygonGrob(x = coords$x,
                      y = coords$y,
                      gp = grid::gpar(col = color, fill = fill, alpha = alpha))
  }
)


#' @details this is an example with katrina data in a particular date.
#' 
katrina <- storm_observation[which(storm_observation$storm_id=="Katrina-2005" & storm_observation$date=="2005-08-29 12:00:00"),]


#' @details This is the example using the functions in the ggplot with the map. We use a specific zoom and we need an API of Google
#' if you run the code.
ggplot(data = katrina) +
  geom_hurricane(aes(x = longitude, y = latitude,
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) + 
  xlim(-93,-86) + ylim(26,33)


map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

base_map +
  geom_hurricane(data = katrina, aes(x = longitude, y = latitude,
                                     r_ne = ne, r_se = se,
                                     r_nw = nw, r_sw = sw,
                                     fill = wind_speed,
                                     color = wind_speed)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow"))
