
make_seasonal_oisst <- function(rasterfile,
                                type) {
  dat <- raster::stack(rasterfile)
  dat <- raster::rotate(dat)
  out <- tibble::tibble()

  for(i in c("GOM", "GB", "MAB")) {
    for (j in c("winter", "spring", "summer", "fall")) {

### not different!

      day_first <- dplyr::case_when(j == "winter" ~ 1,
                                    j == "spring" ~ 91,
                                    j == "summer" ~ 182,
                                    j == "fall" ~ 274)
      day_last <- dplyr::case_when(j == "winter" ~ 90,
                                   j == "spring" ~ 181,
                                   j == "summer" ~ 273,
                                   j == "fall" ~ 365)

      seasonal_dat <- try(dat[[day_first:day_last]])
      if(class(seasonal_dat) != "try-error"){
        this_dat <- raster::mask(dat[[day_first:day_last]],
                                 sf::as_Spatial(ecodata::epu_sf[ecodata::epu_sf$EPU == i,]))

        # get seasonal mean for each raster cell
        results <- raster::stackApply(this_dat,
                                      indices = rep(1, raster::nlayers(this_dat)),
                                      mean)

        # mean all the raster cells to 1 value
        if(type == "ltm") {
          results <- tibble::tibble(EPU = i,
                                    Season = j,
                                    LTM = mean(results@data@values, na.rm = TRUE))
        } else if(type == "annual") {
          results <- tibble::tibble(EPU = i,
                                    Season = j,
                                    Mean_temp = mean(results@data@values, na.rm = TRUE),
                                    Year = as.numeric(stringr::str_extract(rasterfile, "[:digit:]{4}")))
        }
        out <- rbind(out, results)
      }
    }
  }
  return(out)
}
# make_annual_means("C:/Users/abigail.tyrell/Documents/ecopull/data-raw/gridded/sst_data/test_1982.grd")
# test <- lapply(c("C:/Users/abigail.tyrell/Documents/ecopull/data-raw/gridded/sst_data/test_1982.grd",
#          "C:/Users/abigail.tyrell/Documents/ecopull/data-raw/gridded/sst_data/test_1983.grd"),
#        FUN = make_annual_means)
# data.table::rbindlist(test) %>%
#   tibble::as_tibble()


spatial_analysis <- function(raster, # spatial data, one year of data with layers for each day
                             shape, # shapefile to mask to
                             timeframe # days of year (vector)
                             ) {
  dat <- raster::stack(rasterfile)
  dat <- raster::rotate(dat)

  seasonal_dat <- try(dat[[timeframe]])
  if(class(seasonal_dat) != "try-error"){
    this_dat <- raster::mask(dat[[timeframe]],
                             sf::as_Spatial(shape))

    # get seasonal mean for each raster cell
    results <- raster::stackApply(this_dat,
                                  indices = rep(1, raster::nlayers(this_dat)),
                                  mean)

    # mean all the raster cells to 1 value
      results <- tibble::tibble(Mean_temp = mean(results@data@values, na.rm = TRUE),
                                Year = as.numeric(stringr::str_extract(rasterfile, "[:digit:]{4}")))

  }
}
