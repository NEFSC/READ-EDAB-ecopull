#' Get long term seasonal mean
#'
#' This function gets the long term seasonal mean
#' @param ltm The long term temperature data (raster)
#' @param shapefiles Shape files for the regions to mask to
#' @param region_name The names of the regions to calculate
#' @param anom Whether to calculate seasonal anomaly
#' @return A raster (cropped to Northeast US)
#' @importFrom magrittr %>%
#' @export

seasonal_epu_ltm <- function(ltm,
                             shapefiles = ecodata::epu_sf %>%
                               filter(EPU != "SS"),
                             region_name){
  ltm <- mask(ltm, shapefiles[shapefiles[[1]] == region_name,])
  ltm_out <- mean(ltm@data@values, na.rm = T)
  return(ltm_out)
}

#' Get group mean
#'
#' This function gets the group mean by season
#' @param ltm_path The file path to the long term mean data
#' @param fname The file path to the raster file to use as starting input
#' @param shapefiles Shape files for the regions to mask to. Defaults to EPUs.
#' @param region_name The names of the regions to calculate
#' @param anom Whether to calculate seasonal anomaly
#' @return A data frame
#' @importFrom magrittr %>%
#' @export

get_group_mean <- function(ltm_path,
                           fname,
                           shapefiles = ecodata::epu_sf %>%
                             filter(EPU != "SS"),
                           region_name,
                           anom = TRUE){

  # get seasonal mean data
  ltm <- raster::stack(ltm_path)

  # already rotated and cropped
  ltm <- raster::crop(ltm, extent(280,300,30,50))
  ltm <- raster::rotate(ltm)

  winter.ltm <- ltm[[1:90]]
  winter.ltm <- raster::stackApply(winter.ltm, indices = rep(1,nlayers(winter.ltm)),mean)

  spring.ltm <- ltm[[91:181]]
  spring.ltm <- raster::stackApply(spring.ltm, indices = rep(1,nlayers(spring.ltm)),mean)

  summer.ltm <- ltm[[182:273]]
  summer.ltm <- raster::stackApply(summer.ltm, indices = rep(1,nlayers(summer.ltm)),mean)

  fall.ltm <- ltm[[274:365]]
  fall.ltm <- raster::stackApply(fall.ltm, indices = rep(1,nlayers(fall.ltm)),mean)

  #Import raster data ----
  raw <- raster::stack(fname)

  raster::crs(raw) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #Get layer index and map to year ----
  message('Getting index')
  year <- NULL

  # Get Year column from filename instead of embeded in data
  year <- rbind(year, data.frame(Time = rep(c(str_extract(fname,"\\d{4}")), nlayers(raw))))

  year_split <- year %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(day = 1:n()) %>%
    dplyr::mutate(leap = ifelse(n() == 365,"common","leap")) %>%
    dplyr::group_by(leap) %>%
    dplyr::mutate(season = ifelse(day <= 90 & leap == "common", "winter",
                                  ifelse(day > 90 & day <= 181 & leap == "common", "spring",
                                         ifelse(day > 181 & day <= 273 & leap == "common", "summer",
                                                ifelse(day > 273 & leap == "common", "fall",

                                                       ifelse(day <= 91 & leap == "leap", "winter",
                                                              ifelse(day > 91 & day <= 181 & leap == "leap", "spring",
                                                                     ifelse(day > 181 & day <= 273 & leap == "leap", "summer",
                                                                            ifelse(day > 273 & leap == "leap", "fall",NA)
                                                                            )))))))) %>%
    dplyr::group_by(Time, leap, season) %>%
    dplyr::mutate(index = paste(Time, season))

  if (any(is.na(year_split))){
    message("NA in year")
  }

  #Rotate from 0-360 to -180-180 ----
  message(paste('Rotating',fname))
  raw1 <- raster::rotate(raw)

  #Split data on layer index - stackApply will break if there are too many layers ----
  g1 <- year_split %>%
    dplyr::filter(index %in% unique(.$index)[1:10]) %>%
    dplyr::pull(index)

  g2 <- year_split %>%
    dplyr::filter(!index %in% unique(.$index)[1:10]) %>%
    dplyr::pull(index)

  #Apply and combine ----
  message(paste('Finding mean'))
  n <- raster::nlayers(raw1)
  rawMean1 <- raster::stackApply(raw1[[1:length(g1)]], indices = g1, mean)
  if(length(g2) > 0){
    rawMean2 <- raster::stackApply(raw1[[(length(g1) + 1):n]], indices = g2, mean)
    rawMean <- raster::stack(rawMean1,rawMean2)
  } else {
    rawMean <- rawMean1
  }

  #Mask output to EPU ----
  message(paste('Masking to',region_name))
  out <- raster::mask(rawMean, shapefiles[shapefiles[[1]] == region_name,])

  #Find seasonal anomaly ----
  mean_sst <- NULL
  for (i in 1:nlayers(out)){

    if (anom){
      season <- str_extract(names(out[[i]]),"winter|spring|summer|fall")
      message(paste('Finding',season,'SST anomaly for',region_name))
      sst <- mean(out[[i]]@data@values, na.rm = T) - seasonal_epu_ltm(ltm = get(paste0(season,".ltm")),
                                                                      shapefiles = shapefiles,
                                                                      region_name = region_name)
      var <- "OISST anomaly"
    } else {
      sst <- mean(out[[i]]@data@values, na.rm = T)
      var <- "absolute"
    }

    year =  str_extract(out@data@names[i],"\\d{4}")
    df <- data.frame(Value = sst,
                     year = year,
                     EPU = region_name,
                     Var = paste(season, var))

    assign('mean_sst',rbind(mean_sst, df))
  }
  return(mean_sst)
}

# get_group_mean(ltm_path = here::here("data-raw","gridded","ltm", "internet_ltm.grd"),
#                fname = here::here("data-raw","gridded","sst_data", "test_2017.grd"),
#                region_name = "MAB")
#
# get_group_mean(ltm_path = here::here("data-raw","gridded","ltm", "internet_ltm.grd"),
#                fname = here::here("data-raw","gridded","sst_data", "test_2017.grd"),
#                shapefiles = ecodata::ESP_sf,
#                region_name = "atlantic_menhaden_south_spring")
