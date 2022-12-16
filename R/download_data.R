

# unfortunately, the .nc is corrupted when downloaded like this

download_data <- function(years) {
  for(i in years) {
    name <- paste0(i, ".nc")
    filename <- here::here("data-raw","gridded", "sst_data", paste0("test_", i, ".grd"))

    url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", i, ".v2.nc")
    download.file(url, destfile = name)

    text <- knitr::knit_expand(text = "test_{{year}} <- ecopull::nc_to_raster(nc = name, varname = 'sst')
                                              raster::writeRaster(test_{{year}}, filename = filename, overwrite = TRUE)",
                               year = i)
    # print(text)
    try(eval(parse(text = text)))
    unlink(name) # remove nc file to save space
    message(paste("downloaded", i))
  }
}

# download_data(2021)
