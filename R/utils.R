#' Convert netcdf to raster
#'
#' This function converts a netcdf object to a raster object
#' @param nc The nc object
#' @param varnum The index of the variable to convert to a raster
#' @return A raster (cropped to Northeast US)
#' @importFrom magrittr %>%
#' @export

nc_to_raster <- function(nc, varnum) {
  message("Your var is: ", print(nc$var[[varnum]]$name))

  # size and dimension
  v <- nc$var[[varnum]]
  size <- v$varsize
  dims <- v$ndims
  nt <- size[dims] # length of time dimension
  lat <- nc$dim$lat$vals # latitude position
  lon <- nc$dim$lon$vals # longitude position

  # read sst variable
  message("Converting layers to raster...")
  r <- list()
  for (j in 1:nt) {
    start <- rep(1, dims) # begin with start=(1,1,...,1)
    start[dims] <- j # change to start=(1,1,...,i) to read    timestep i
    count <- size # begin with count=(nx,ny,...,nt), reads entire var
    count[dims] <- 1 # change to count=(nx,ny,...,1) to read 1 tstep

    dt <- ncdf4::ncvar_get(nc,
      varid = "sst",
      start = start,
      count = count
    )

    # convert to raster
    r[j] <- raster::raster(dt,
      crs = "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
    )
  }

  # create layer stack with time dimension
  r <- raster::stack(r)

  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(c(range(lat), range(lon)))

  r <- r %>%
    raster::t() %>%
    raster::flip(direction = "y")

  # ## get seasonal gridded means
  # message("Getting seasonal gridded means...")
  # message("Getting winter gridded mean...")
  # winter <- try(r[[1:90]], silent = TRUE)
  # if (class(winter) != "try-error") {
  #   winter <- raster::stackApply(winter, indices = rep(1, raster::nlayers(winter)), mean)
  # } else {
  #   winter <- NULL
  #   message("Winter data is missing or incomplete. Not creating winter layer.")
  # }
  #
  # message("Getting spring gridded mean...")
  # spring <- try(r[[91:181]], silent = TRUE)
  # if (class(spring) != "try-error") {
  #   spring <- raster::stackApply(spring, indices = rep(1, raster::nlayers(spring)), mean)
  # } else {
  #   spring <- NULL
  #   message("Spring data is missing or incomplete. Not creating spring layer.")
  # }
  #
  # message("Getting summer gridded mean...")
  # summer <- try(r[[182:273]], silent = TRUE) # %>% suppressMessages() %>% suppressWarnings()
  # if (class(summer) != "try-error") {
  #   summer <- raster::stackApply(summer, indices = rep(1, raster::nlayers(summer)), mean)
  # } else {
  #   summer <- NULL
  #   message("Summer data is missing or incomplete. Not creating summer layer.")
  # }
  #
  # message("Getting fall gridded mean...")
  # fall <- try(r[[274:365]], silent = TRUE)
  # if (class(fall) != "try-error") {
  #   fall <- raster::stackApply(fall, indices = rep(1, raster::nlayers(fall)), mean)
  # } else {
  #   fall <- NULL
  #   message("Fall data is missing or incomplete. Not creating fall layer.")
  # }
  #
  # ## Join seasonal gridded mean raster layers in ltm stack
  # message("Creating seasonal means stack...")
  # r <- raster::stack(winter, spring, summer, fall)
  # raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # message("Cropping to Northeast US...")
  ne_data <- raster::crop(r, raster::extent(280, 300, 30, 50))
  message("Done!")

  return(ne_data)
}


# `%>%` <- magrittr::`%>%`
# nc <- ncdf4::nc_open("../../Downloads/sst.day.mean.2021.v2.nc")

# nc_to_raster(nc = nc, varnum = 1)
