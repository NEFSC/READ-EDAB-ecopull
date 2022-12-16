
`%>%` <- magrittr::`%>%`

ltm1 <- mask(ltm[[180]], epu[epu$EPU == "MAB",])

weighted_temp <- sum((ltm1@data@values *
                        raster::area(ltm1, na.rm = TRUE)@data@values),
                     na.rm = TRUE) /
  sum(raster::area(ltm1, na.rm = TRUE)@data@values, na.rm = TRUE)
weighted_temp

mean(ltm1@data@values, na.rm = TRUE)

## create seasonal EPU long term means
ltm <- raster::stack(file.path(ltm.dir,seasonal_oisst_anom_nc))
ltm <- raster::rotate(ltm)
out <- tibble::tibble()
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

    this_dat <- mask(ltm[[day_first:day_last]],
                     ecodata::epu_sf[ecodata::epu_sf$EPU == i,])
    # get seasonal mean for each raster cell
    results <- raster::stackApply(this_dat, indices = rep(1,nlayers(this_dat)),
                                  mean)
    results <- tibble::tibble(EPU = i,
                              Season = j,
                              # mean all the raster cells to 1 value
                              LTM = mean(results@data@values, na.rm = TRUE))
    print(results)
    out <- rbind(out, results)
  }
}
out
