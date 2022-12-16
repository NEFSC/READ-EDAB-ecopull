`%>%` <- magrittr::`%>%`

### https://www.py4u.net/discuss/881476

# read ncdf file
#nc <- ncdf4::nc_open("../../Downloads/sst.day.mean.ltm.1982-2010.nc")
nc <- ncdf4::nc_open(file.path(here::here("data-raw/sst.day.mean.ltm.1991-2020.nc")))
# check which variable to use
nc$var[[2]]$name

# extract variable name, size and dimension
v <- nc$var[[2]]
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

ecodata_ltm <- r %>%
  raster::t() %>%
  raster::flip(direction = "y")

## get seasonal gridded means
winter.ltm <- ecodata_ltm[[1:90]]
winter.ltm <- raster::stackApply(winter.ltm, indices = rep(1,raster::nlayers(winter.ltm)),mean)

spring.ltm <- ecodata_ltm[[91:181]]
spring.ltm <- raster::stackApply(spring.ltm, indices = rep(1,raster::nlayers(spring.ltm)),mean)

summer.ltm <- ecodata_ltm[[182:273]]
summer.ltm <- raster::stackApply(summer.ltm, indices = rep(1,raster::nlayers(summer.ltm)),mean)

fall.ltm <- ecodata_ltm[[274:365]]
fall.ltm <- raster::stackApply(fall.ltm, indices = rep(1,raster::nlayers(fall.ltm)),mean)

## Join seasonal gridded mean raster layers in ltm stack
ecodata_ltm <- raster::stack(winter.ltm, spring.ltm, summer.ltm, fall.ltm)
raster::crs(ecodata_ltm) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ltm <- raster::crop(ecodata_ltm, raster::extent(280,300,30,50))

print(ecodata_ltm)

raster::plot(ltm)

usethis::use_data(ltm, overwrite = TRUE)
