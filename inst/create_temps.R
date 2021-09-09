`%>%` <- magrittr::`%>%`

# open file
nc <- ncdf4::nc_open(name)

# print(nc)
# print(nc$var[[1]]$name)

# size and dimension
v <- nc$var[[1]]
size <- v$varsize
dims <- v$ndims
nt <- size[dims] # length of time dimension
lat <- nc$dim$lat$vals # latitude position
lon <- nc$dim$lon$vals # longitude position

# read sst variable
r <- list()
for (i in 1:nt) {
  start <- rep(1, dims) # begin with start=(1,1,...,1)
  start[dims] <- i # change to start=(1,1,...,i) to read    timestep i
  count <- size # begin with count=(nx,ny,...,nt), reads entire var
  count[dims] <- 1 # change to count=(nx,ny,...,1) to read 1 tstep

  dt <- ncdf4::ncvar_get(nc,
    varid = "sst", start = start,
    count = count
  )

  # convert to raster
  r[i] <- raster::raster(dt,
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

## get seasonal gridded means
winter <- r[[1:90]]
winter <- raster::stackApply(winter, indices = rep(1, raster::nlayers(winter)), mean)

spring <- r[[91:181]]
spring <- raster::stackApply(spring, indices = rep(1, raster::nlayers(spring)), mean)

summer <- r[[182:273]]
summer <- raster::stackApply(summer, indices = rep(1, raster::nlayers(summer)), mean)

fall <- r[[274:365]]
fall <- raster::stackApply(fall, indices = rep(1, raster::nlayers(fall)), mean)

## Join seasonal gridded mean raster layers in ltm stack
r <- raster::stack(winter, spring, summer, fall)
raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ne_data <- raster::crop(r, raster::extent(280, 300, 30, 50))

# print(ne_data)

# raster::plot(ne_data)

# save to package
test_{{year}} <- ne_data
usethis::use_data(test_{{year}}, overwrite = TRUE)

# close nc file
ncdf4::nc_close(nc)
