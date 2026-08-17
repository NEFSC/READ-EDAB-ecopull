#Command Line Local
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST" "//nefscdata/EDAB_Dev/atyrell" "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

# Command Line Cloud
#Rscript ~/EDAB_Workflows/GLORYS_automation_test.R "~/EDAB_Datasets/OISST/V2/SOURCE/SST" "~/EDAB_Dev/atyrell" "~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  ltm_file = args[3]
  print('Using command line arguments')
} else {
  # input_folder = '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST'
  # output_folder = '//nefscdata/EDAB_Dev/atyrell'
  # ltm_file = '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc'

  input_folder = '~/EDAB_Datasets/OISST/V2/SOURCE/SST'
  output_folder = '~/EDAB_Dev/atyrell'
  ltm_file = '~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc'

  message('Using default arguments')
}

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir = function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

check.dir(output_folder)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}
# if (!dir.exists(supp.dir)) {
#   stop(paste0('Supplemental directory does not exist: ', supp.dir))
# }

# (1) calculate annual mean by EPU

message("Calculating annual means...")
annual_mean <- EDABUtilities::make_2d_summary_ts(
  agg.time = "days",
  data.in = EDABUtilities::convert_2d_longitude_gridded(list.files(
    input_folder,
    full.names = TRUE
  )[1:3]),
  file.time = 'annual',
  output.files = NULL,
  shp.file = system.file(
    'data',
    'EPU_NOESTUARIES.shp',
    package = 'EDABUtilities'
  ),
  var.name = "sst",
  area.names = c("MAB", "GB", "GOM", "SS"),
  statistic = 'mean',
  tz = NA,
  touches = TRUE,
  write.out = F
)

annual_mean_output <- purrr::reduce(annual_mean,
                                    ~ {terra::as.data.frame(.x, na.rm = FALSE)
                                      dplyr::bind_rows(.x, .y)}
                                    )


# (2) calculate climatology
message("Finished calculating annual means. Calculating climatology...")
climatology <- EDABUtilities::make_2d_summary_ts(
  agg.time = "days",
  data.in = EDABUtilities::convert_2d_longitude_gridded(ltm_file),
  file.time = 'annual',
  output.files = NULL,
  shp.file = system.file(
    'data',
    'EPU_NOESTUARIES.shp',
    package = 'EDABUtilities'
  ),
  var.name = "sst",
  area.names = c("MAB", "GB", "GOM", "SS"),
  statistic = 'mean',
  tz = NA,
  touches = TRUE,
  write.out = F
) |>
  terra::as.data.frame(na.rm = FALSE)

# (3) calculate anomaly by subtracting climatology
message("Finished calculating climatology. Calculating anomalies...")
anomaly <- dplyr::full_join(
  climatology |>
    dplyr::select(time, area, value) |>
    dplyr::mutate(month = lubridate::month(time), day = lubridate::day(time)) |>
    dplyr::rename(climatology = value) |>
    dplyr::select(-time),
  annual_mean_output |>
    dplyr::select(time, area, value) |>
    dplyr::mutate(
      month = lubridate::month(time),
      day = lubridate::day(time),
      year = lubridate::year(time)
    )
) |>
  dplyr::mutate(anom_value = value - climatology,
                season = dplyr::case_when(month %in% 1:3 ~ "Winter",
                                          month %in% 4:6 ~ "Spring",
                                          month %in% 7:9 ~ "Summer",
                                          month %in% 10:12 ~ "Fall")) |>
  dplyr::group_by(year, season, area) |>
  dplyr::summarise(data_value = mean(value, na.rm = TRUE))

# (4) save to network drive
message("Finished calculating anomalies. Saving to: ", output_folder)
write.csv(
  anomaly,
  file = file.path(output_folder, paste0("oisst_anomaly_", Sys.Date(), ".csv"))
)

message('Done: OISST Anomaly')
