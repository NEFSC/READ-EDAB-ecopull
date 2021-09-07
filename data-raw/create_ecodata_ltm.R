ecodata_ltm <- ncdf4::nc_open("../../Downloads/sst.day.mean.ltm.1982-2010.nc")

print(ecodata_ltm)

usethis::use_data(ecodata_ltm)
