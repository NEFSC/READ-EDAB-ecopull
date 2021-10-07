#' Create null objects
#'
#' This function creates null objects from a vector of names
#' @param names Vector of names to be turned into null objects
#' @return Null objects saved into the working environment
#' @export

create_null <- function(names){
  for(i in names){
    text <- knitr::knit_expand(text = "{{name}} <<- NULL",
                               name = i)
    eval(parse(text = text))
  }
}

#' Convert netcdf to raster
#'
#' This function converts a netcdf object to a raster object
#' @param nc The nc file path
#' @param varname The name of the variable to convert to a raster
#' @param extent The latitude and longitude range the data should have, of the form c(xmin, xmax, ymin, ymax). Defaults to `c(0, 360, -90, 90)`
#' @param crop An extent object to use to crop data. Defaults to `raster::extent(280, 300, 30, 50)` (Northeast US)
#' @param show_images Boolean. Whether to display images of the data. Useful to check if cropping is occurring correctly.
#' @return A raster brick
#' @importFrom magrittr %>%
#' @export

nc_to_raster <- function(nc,
                         varname,
                         extent = c(0, 360, -90, 90),
                         crop = raster::extent(280, 300, 30, 50),
                         show_images = FALSE) {

  message("Reading .nc as brick...")

  r <- raster::brick(nc, varname = varname)

  message("Setting CRS...")
  raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)

  if(show_images){
    par(mfrow = c(1,2))
    raster::plot(r, 1, sub = "Full dataset")
  }

  message("Cropping data...")
  ne_data <- raster::crop(r, crop)

  if(show_images){
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1,1))
  }

  message("Done!")

  return(ne_data)
}


# `%>%` <- magrittr::`%>%`
# nc <- ncdf4::nc_open("../../Downloads/sst.day.mean.2021.v2.nc")

# nc_to_raster(nc = nc, varnum = 1)
