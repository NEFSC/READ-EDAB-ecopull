

# Load packages required to define the pipeline:
library(targets)
`%>%` <- magrittr::`%>%`
# library(tarchetypes) # Load other packages as needed. # nolint

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

list(

  # download new year's sst data and save in folder
  # download from psl, for example:
  # https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.2021.v2.nc
  # test_2021 <- ecopull::nc_to_raster(nc = "2021.nc", varname = "sst")
  # raster::writeRaster(test_2021, filename = "data-raw/gridded/sst_data/test_2021.grd", overwrite = TRUE)

  # read in long term data
  tar_target(ltm_file,
             here::here("data-raw","gridded","ltm","internet_ltm.grd"),
             format = "file"),
  # calculate ltm for each region x season
  tar_target(ltms,
             make_seasonal_oisst(ltm_file,
                                 type = "ltm")),

  # read in yearly data
  tar_target(sst_files,
             list.files(here::here("data-raw","gridded","sst_data"),
                        full.names = TRUE,
                        pattern = ".grd")[2:41], # 1981 isn't a full year
             format = "file"),
  # calculate mean temp for each region x season x year
  tar_target(annual_ssts,
             {
               lapply(sst_files,
                      FUN = make_seasonal_oisst,
                      type = "annual") %>%
               data.table::rbindlist() %>%
                 tibble::as_tibble()
             }),

  # subtract ltm to get anomaly
  tar_target(sst_anomaly,
             {
               dplyr::full_join(annual_ssts,
                                ltms) %>%
                 dplyr::filter(Year > 1981) %>%
                 dplyr::mutate(Value = Mean_temp - LTM,
                               Var = paste(EPU, Season, "sst_anomaly", sep = "_")) %>%
                 dplyr::select(Year, EPU, Var, Value)
             })
)
