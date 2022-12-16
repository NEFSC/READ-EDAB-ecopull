

# Load packages required to define the pipeline:
library(targets)
`%>%` <- magrittr::`%>%`
# library(tarchetypes) # Load other packages as needed. # nolint

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# download new year's sst data and save in folder
# download from psl, for example:
# https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.2022.v2.nc
# test_2022 <- ecopull::nc_to_raster(nc = "data-raw/sst.day.mean.2022.v2.nc", varname = "sst")
# raster::writeRaster(test_2022, filename = "data-raw/gridded/sst_data/test_2022.grd", overwrite = TRUE)
# downloading the .nc from url in the script breaks the file

# # download new ltm
# test <- ecopull::nc_to_raster(nc = here::here("data-raw/gridded/ltm/sst.day.mean.ltm.1991-2020.nc"),
#                               varname = "sst")
# raster::writeRaster(test, filename = "data-raw/gridded/ltm/new_internet_ltm.grd",
#                     overwrite = TRUE)

list(
  # read in long term data
  tar_target(ltm_file,
             here::here("data-raw","gridded","ltm","internet_ltm.grd"),
             format = "file"),
  tar_target(new_ltm_file,
             here::here("data-raw","gridded","ltm","new_internet_ltm.grd"),
             format = "file"),
  # calculate ltm for each region x season
  tar_target(ltms,
             make_seasonal_oisst(ltm_file,
                                 type = "ltm")),
  tar_target(new_ltms,
             make_seasonal_oisst(new_ltm_file,
                                 type = "ltm")),

  # read in yearly data
  tar_target(sst_files,
             list.files(here::here("data-raw","gridded","sst_data"),
                        full.names = TRUE,
                        pattern = ".grd")[2:42], # 1981 isn't a full year
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
             }),
  tar_target(new_sst_anomaly,
             {
               dplyr::full_join(annual_ssts,
                                new_ltms) %>%
                 dplyr::filter(Year > 1981) %>%
                 dplyr::mutate(Value = Mean_temp - LTM,
                               Var = paste(EPU, Season, "sst_anomaly", sep = "_")) %>%
                 dplyr::select(Year, EPU, Var, Value)
             }),

  # save data to package
  tar_target(save_data,
             usethis::use_data(sst_anomaly, overwrite = TRUE)),
  tar_target(save_new_data,
             usethis::use_data(new_sst_anomaly, overwrite = TRUE)

  )
)
