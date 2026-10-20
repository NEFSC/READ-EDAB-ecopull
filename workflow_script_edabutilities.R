#Command Line Local
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST" "//nefscdata/EDAB_Dev/atyrell" "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

# Command Line Cloud
#Rscript ~/EDAB_Workflows/GLORYS_automation_test.R "~/EDAB_Datasets/OISST/V2/SOURCE/SST" "~/EDAB_Dev/atyrell" "~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

#Gets arguments from command line
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder <- args[1]
  output_folder <- args[2]
  ltm_file <- args[3]
  print('Using command line arguments')
} else {
  input_folder <- '//nefscdata/EDAB_Datasets/'
  output_folder <- '//nefscdata/EDAB_Dev/jcaracappa/'
  ltm_file <- '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/GLOBAL_25KM_DAILY_RAN/SST_LTM/sst.day.mean.ltm.1991-2020.nc'

  # input_folder <- '~/EDAB_Datasets/OISST/V2/SOURCE/SST'
  # output_folder <- '~/EDAB_Dev/atyrell'
  # ltm_file <- '~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc'
  # ltm_file = '~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/oisst_clim_test.nc'

  message('Using default arguments')
}

#Setup Directories

# pak::pak("NEFSC/READ_EDAB_Utilities@#36/convert_longitude")
input_dir_oisst <- paste0(
  input_folder,
  'OISST/V2/SOURCE/GLOBAL_25KM_DAILY_RAN/SST'
)
output_dir_oisst <- paste0(output_folder, 'OISST_SOE/')

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir <- function(file) {
  if (!dir.exists(file)) {
    dir.create(file, recursive = T)
  }
}

check.dir(output_dir_oisst)
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}

if (!file.exists(ltm_file)) {
  stop(paste0('Long-term mean file does not exist: ', ltm_file))
}

# Input Definitions

shp.file <- system.file(
  'data',
  'EPU_NOESTUARIES.shp',
  package = 'EDABUtilities'
)

input.files <- list.files(
  input_dir_oisst,
  full.names = TRUE
)
input.file.years <- stringr::str_extract(basename(input.files), "\\d{4}") |>
  as.numeric()

start.year <- min(input.file.years)
stop.year <- max(input.file.years)

#If TRUE reruns old files
redo <- F

#Setup Output Directories

##NEUS OISST Crop
oisst.crop.dir <- paste0(input_folder, 'OISST/V2/PRODUCTS/OISST_NEUS_crop/')
check.dir(oisst.crop.dir)
oisst.crop.prefix <- 'OISST_NEUS_crop_'


##Annual mean for NEUS
annual.mean.dir <- file.path(output_dir_oisst, 'OISST_annual_mean/')
check.dir(annual.mean.dir)
annual.mean.prefix <- 'OISST_annual_mean_'


## Daily EPU for heatwave
daily.epu.dir <- file.path(output_dir_oisst, 'OISST_daily_epu/')
check.dir(daily.epu.dir)
daily.epu.prefix <- 'OISST_daily_epu_'

#Make LTM Climatology Outside loop

ltm.crop.file <- paste0(dirname(ltm_file), '/NEUS_crop_', basename(ltm_file))

if (!file.exists(ltm.crop.file) | redo == T) {
  EDABUtilities::crop_nc_2d(
    ltm_file,
    shp.file = shp.file,
    write.out = T,
    output.file = ltm.crop.file
  )
} else {
  message("LTM regional crop already exists. Skipping...")
}

ltm.clim.file <- paste0(
  output_dir_oisst,
  'OISST_SOE_climatology_',
  basename(ltm_file)
)
ltm.clim.file <- gsub('.nc', '.rds', ltm.clim.file)

#make climatology
if (!file.exists(ltm.crop.file) | redo == T) {
  EDABUtilities::make_2d_summary_ts(
    agg.time = "days",
    data.in = ltm.crop.file,
    file.time = 'annual',
    output.files = ltm.clim.file,
    shp.file = shp.file,
    var.name = "sst",
    area.names = c("MAB", "GB", "GOM", "SS"),
    statistic = 'mean',
    tz = NA,
    touches = TRUE,
    write.out = T
  )
} else {
  message("LTM climatology already exists. Skipping...")
}

#Run through annual files

for (y in 1:length(input.file.years)) {
  message("Calculating OISST NEUS crop...")
  oisst.crop.file <- file.path(
    oisst.crop.dir,
    paste0(oisst.crop.prefix, input.file.years[y], '.nc')
  )

  if (!file.exists(oisst.crop.file) | redo == T) {
    message("Calculating annual means...")
    EDABUtilities::crop_nc_2d(
      data.in = input.files[y],
      shp.file = shp.file,
      write.out = T,
      output.files = oisst.crop.file
    )
  } else {
    message(
      "OISST regional crop already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )
  }

  # calculate annual mean by EPU ----

  message("Calculating annual means...")
  annual.mean.file <- file.path(
    annual.mean.dir,
    paste0(annual.mean.prefix, input.file.years[y], '.rds')
  )

  if (!file.exists(annual.mean.file) | redo == T) {
    EDABUtilities::make_2d_summary_ts(
      agg.time = "days",
      data.in = oisst.crop.file,
      file.time = 'annual',
      output.files = annual.mean.file,
      shp.file = shp.file,
      var.name = "sst",
      area.names = c("MAB", "GB", "GOM", "SS"),
      statistics = 'mean',
      tz = NA,
      touches = TRUE,
      write.out = T
    )
  } else {
    message(
      "OISST Annual mean file already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )
  }

  #Calculate Daily mean by EPU

  epu.names <- c('MAB', 'GB', 'GOM', 'SS')

  daily.epu.file <- file.path(
    daily.epu.dir,
    paste0(daily.epu.prefix, input.file.years[y], '.csv')
  )

  if (!file.exists(daily.epu.file) | redo == T) {
    out.ls <- list()
    for (i in 1:length(epu.names)) {
      this.data.rast <- EDABUtilities::make_2d_summary_gridded(
        data.in = oisst.crop.file,
        write.out = F,
        shp.file = shp.file,
        var.name = 'sst',
        agg.time = 'days',
        statistic = 'mean',
        touches = F,
        file.time = 'daily',
        area.names = epu.names[i]
      )
      out.epu.ls <- list()
      for (j in 1:length(this.data.rast)) {
        out.epu.ls[[j]] <- terra::as.data.frame(
          terra::rast(this.data.rast[j][[1]]),
          xy = T,
          time = T,
          wide = F
        ) |>
          dplyr::select(-layer) |>
          dplyr::group_by(time) |>
          dplyr::summarise(
            sst.mean = mean(values, na.rm = T),
            sst.sd = sd(values, na.rm = T)
          ) |>
          dplyr::rename(date = 'time') |>
          dplyr::mutate(EPU = epu.names[i], source = 'GLORYS')
      }
      out.ls[[i]] <- dplyr::bind_rows(out.epu.ls)
    }

    daily.epu.heatwave <- dplyr::bind_rows(out.ls)

    write.csv(
      daily.epu.heatwave,
      file = daily.epu.file,
      row.names = F
    )
  } else {
    message(
      "OISST Daily EPU file already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )
  }

  # annual_mean_output <- purrr::reduce(
  #   annual_mean,
  #   ~ {
  #     terra::as.data.frame(.x, na.rm = FALSE)
  #     dplyr::bind_rows(.x, .y)
  #   }
  # )

  # saveRDS(annual_mean_output, here::here("data-raw/annual_mean_output.rds"))
  # annual_mean_output <- readRDS( here::here("data-raw/annual_mean_output.rds"))
}

# calculate anomaly by subtracting climatology ----

oisst.anomaly.file <- paste0(
  output_dir_oisst,
  paste0("OISST_anomaly/oisst_anomaly_", Sys.Date(), ".csv")
)
check.dir(dirname(oisst.anomaly.file))

if (!file.exists(oisst.anomaly.file) | redo == T) {
  annual.mean.files <- list.files(
    pattern = annual.mean.prefix,
    path = annual.mean.dir,
    full.names = T
  )
  annual.mean.all <- lapply(annual.mean.files, readRDS) |> dplyr::bind_rows()

  anomaly <- dplyr::full_join(
    readRDS(ltm.clim.file) |>
      dplyr::select(time, area, value) |>
      dplyr::mutate(
        month = lubridate::month(time),
        day = lubridate::day(time)
      ) |>
      dplyr::rename(climatology = value) |>
      dplyr::select(-time),
    annual.mean.all |>
      dplyr::select(time, area, value) |>
      dplyr::mutate(
        month = lubridate::month(time),
        day = lubridate::day(time),
        year = lubridate::year(time)
      )
  ) |>
    dplyr::mutate(
      anom_value = value - climatology,
      season = dplyr::case_when(
        month %in% 1:3 ~ "Winter",
        month %in% 4:6 ~ "Spring",
        month %in% 7:9 ~ "Summer",
        month %in% 10:12 ~ "Fall"
      )
    ) |>
    dplyr::group_by(year, season, area) |>
    dplyr::summarise(data_value = mean(anom_value, na.rm = TRUE))

  # (4) save to network drive ----
  message("Finished calculating anomalies. Saving to: ", output_dir_oisst)
  write.csv(
    anomaly,
    file = oisst.anomaly.file,
    row.names = F
  )

  message('Done: OISST Anomaly')
} else {
  message('OISST Anomaly already exists. Skipping...')
}

#Do Surface Heatwave data from daily EPU mean
daily.epu.heatwave.file <- file.path(
  output_dir_oisst,
  paste0(
    'OISST_daily_epu_heatwave/OISST_daily_epu_heatwave',
    start.year,
    '_',
    stop.year,
    '_',
    Sys.Date(),
    '.csv'
  )
)
check.dir(dirname(daily.epu.heatwave.file))


if (!file.exists(daily.epu.heatwave.file) | redo == T) {
  daily.epu.files <- list.files(
    pattern = daily.epu.prefix,
    path = daily.epu.dir,
    full.names = T
  )
  daily.epu.out <- lapply(daily.epu.files, read.csv) |>
    dplyr::bind_rows()

  write.csv(daily.epu.out, daily.epu.heatwave.file, row.names = F)
} else {
  message('OISST Daily EPU Heatwave already exists. Skipping...')
}
