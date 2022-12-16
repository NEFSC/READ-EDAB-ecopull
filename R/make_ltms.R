
make_ltms <- function(rasterfile) {
  ltm <- raster::stack(rasterfile)
  ltm <- raster::rotate(ltm)
  out <- tibble::tibble()

  # message("starting loop")
  for(i in c("GOM", "GB", "MAB")) {
    for (j in c("winter", "spring", "summer", "fall")) {

      day_first <- dplyr::case_when(j == "winter" ~ 1,
                                    j == "spring" ~ 91,
                                    j == "summer" ~ 182,
                                    j == "fall" ~ 274)
      day_last <- dplyr::case_when(j == "winter" ~ 90,
                                   j == "spring" ~ 181,
                                   j == "summer" ~ 273,
                                   j == "fall" ~ 365)

      this_dat <- raster::mask(ltm[[day_first:day_last]],
                       sf::as_Spatial(ecodata::epu_sf[ecodata::epu_sf$EPU == i,]))
      # message("masked")
      # get seasonal mean for each raster cell
      results <- raster::stackApply(this_dat,
                                    indices = rep(1, raster::nlayers(this_dat)),
                                    mean)
      results <- tibble::tibble(EPU = i,
                                Season = j,
                                # mean all the raster cells to 1 value
                                LTM = mean(results@data@values, na.rm = TRUE))
      # print(results)
      out <- rbind(out, results)
    }
  }
  return(out)
}
