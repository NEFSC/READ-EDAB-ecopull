`%>%` <- magrittr::`%>%`

ecodata_ltm <- ncdf4::nc_open("../../Downloads/sst.day.mean.ltm.1982-2010.nc")

# save as rds
#saveRDS(ecodata_ltm, file = here::here("data-raw", "ecodata_ltm.RDS"))

# read in again
#ecodata_ltm <- readRDS(here::here("data-raw", "ecodata_ltm.RDS"))


### https://www.py4u.net/discuss/881476

# load package
library(sp)
library(raster)
library(ncdf4)

# read ncdf file
nc<-ncdf4::nc_open("../../Downloads/sst.day.mean.ltm.1982-2010.nc")

nc$var[[2]]$name


# extract variable name, size and dimension
v <- nc$var[[2]]
size <- v$varsize
dims <- v$ndims
nt <- size[dims]              # length of time dimension
lat <- nc$dim$latitude$vals   # latitude position
lon <- nc$dim$longitude$vals  # longitude position

# read sst variable
r<-list()
for (i in 1:nt) {
  start <- rep(1,dims)     # begin with start=(1,1,...,1)
  start[dims] <- i             # change to start=(1,1,...,i) to read    timestep i
  count <- size                # begin with count=(nx,ny,...,nt), reads entire var
  count[dims] <- 1             # change to count=(nx,ny,...,1) to read 1 tstep
#  print(count)

  dt<-ncdf4::ncvar_get(nc, varid = 'sst', start = start,
                       count = count
                       )

  # convert to raster
  r[i]<-raster::raster(dt)
}

# create layer stack with time dimension
r<-raster::stack(r)

# transpose the raster to have correct orientation
rt<-t(r)
raster::extent(rt)<-raster::extent(c(range(lon), range(lat)))
# didn't work

# plot the result
sp::spplot(rt)
# kind of a mess

ecodata_ltm <- rt

print(ecodata_ltm)

usethis::use_data(ecodata_ltm, overwrite = TRUE)
