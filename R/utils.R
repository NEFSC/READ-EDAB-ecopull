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

  message("Cropping to Northeast US...")
  ne_data <- raster::crop(r, raster::extent(280, 300, 30, 50))
  message("Done!")

  return(ne_data)
}


# `%>%` <- magrittr::`%>%`
# nc <- ncdf4::nc_open("../../Downloads/sst.day.mean.2021.v2.nc")

# nc_to_raster(nc = nc, varnum = 1)
