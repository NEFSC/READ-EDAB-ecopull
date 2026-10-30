# Command Line Local
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST" "//nefscdata/EDAB_Dev/atyrell" "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

# Command Line Cloud
#Rscript ~/EDAB_Workflows/GLORYS_automation_test.R "~/EDAB_Datasets/OISST/V2/SOURCE/SST" "~/EDAB_Dev/atyrell" "~/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc"

#Gets arguments from command line[cite: 2]
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
  message('Using default arguments')
}

# Helper functions for registry checking and appending[cite: 1]
check.dir <- function(file) {
  if (!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = T)
  }
}

get_recent_file <- function(registry.file, dataset.name) {
  registry <- read.csv(registry.file)
  dataset.files <- registry[registry$dataset.name == dataset.name, ]
  recent.entry <- dataset.files[which.max(as.Date(dataset.files$run.time)), ]
  recent.file <- paste0(
    recent.entry$mount.dir,
    recent.entry$sub.dir,
    recent.entry$file.name
  )
  return(recent.file)
}

check.date.file <- function(
  registry.file,
  dataset.name,
  input.dates,
  mount.dir = ""
) {
  registry <- read.csv(registry.file)
  dataset.files <- registry[registry$dataset.name == dataset.name, ]

  if (nrow(dataset.files) == 0) {
    out.ls <- list(
      'same.run.date' = F,
      'same.input.date' = F,
      'file.exists' = F
    )
  } else {
    which.recent <- which.max(as.POSIXct(dataset.files$run.time))
    recent.entry <- dataset.files[which.recent, ]
    recent.file <- paste0(
      mount.dir,
      recent.entry$sub.dir,
      recent.entry$file.name
    )
    current.date <- Sys.Date()
    same.run.date <- ifelse(current.date == recent.entry$run.date, T, F)
    new.start.date <- min(input.dates, na.rm = T)
    new.end.date <- max(input.dates, na.rm = T)
    same.input.date <- ifelse(
      new.start.date >= recent.entry$start.date |
        new.end.date <= recent.entry$end.date,
      T,
      F
    )

    out.ls <- list(
      'same.run.date' = same.run.date,
      'same.input.date' = same.input.date,
      'file.exists' = file.exists(recent.file)
    )
  }
  return(out.ls)
}

append.registry <- function(
  dataset.name,
  file.name,
  registry.file,
  input.dates,
  mount.dir,
  sub.dir
) {
  reg.entry <- data.frame(
    dataset.name = dataset.name,
    mount.dir = mount.dir,
    sub.dir = sub.dir,
    file.name = basename(file.name),
    run.date = Sys.Date(),
    run.time = Sys.time(),
    file.checksum = tools::md5sum(file.name),
    start.date = min(input.dates, na.rm = T),
    end.date = max(input.dates, na.rm = T)
  )
  write.table(
    reg.entry,
    file = registry.file,
    append = T,
    sep = ',',
    col.names = F,
    row.names = F
  )
}

# Setup Directories[cite: 2]

mount.dir <- ""

input_dir_oisst <- paste0(
  input_folder,
  'OISST/V2/SOURCE/GLOBAL_25KM_DAILY_RAN/SST'
)
output_dir_oisst <- paste0(output_folder, 'OISST_SOE/')

message(paste0('input_folder: ', input_folder))
message(paste0('output_folder: ', output_folder))

check.dir(paste0(output_dir_oisst, "dummy.txt"))
if (!dir.exists(input_folder)) {
  stop(paste0('Input directory does not exist: ', input_folder))
}
if (!file.exists(ltm_file)) {
  stop(paste0('Long-term mean file does not exist: ', ltm_file))
}

# Registry Initialization[cite: 1]
registry.file <- file.path(output_dir_oisst, 'oisst_soe_registry.csv')
if (!file.exists(registry.file)) {
  reg.cols <- c(
    'dataset.name',
    'mount.dir',
    'sub.dir',
    'file.name',
    'run.date',
    'run.time',
    'file.checksum',
    'start.date',
    'end.date'
  )
  df.start <- data.frame(matrix(ncol = length(reg.cols), nrow = 0))
  colnames(df.start) <- reg.cols
  write.csv(df.start, registry.file, row.names = F)

  message('Registry File Written to: ', registry.file)
}


# Input Definitions[cite: 2]
shp.file <- system.file(
  'data',
  'EPU_NOESTUARIES.shp',
  package = 'EDABUtilities'
)
input.files <- list.files(input_dir_oisst, full.names = TRUE)
input.file.years <- stringr::str_extract(basename(input.files), "\\d{4}") |>
  as.numeric()

start.year <- min(input.file.years)
stop.year <- max(input.file.years)
all.input.dates <- as.Date(paste0(input.file.years, "-01-01"))

# If TRUE reruns old files[cite: 2]
redo <- F

# Setup Output Directories[cite: 2]
oisst.crop.dir <- paste0(input_folder, 'OISST/V2/PRODUCTS/OISST_NEUS_crop/')
check.dir(paste0(oisst.crop.dir, "dummy.txt"))
oisst.crop.prefix <- 'OISST_NEUS_crop_'

annual.mean.dir <- paste0(output_dir_oisst, 'OISST_annual_mean/')
check.dir(paste0(annual.mean.dir, "dummy.txt"))
annual.mean.prefix <- 'OISST_annual_mean_'

daily.epu.dir <- paste0(output_dir_oisst, 'OISST_daily_epu/')
check.dir(paste0(daily.epu.dir, "dummy.txt"))
daily.epu.prefix <- 'OISST_daily_epu_'

# Make LTM Climatology Outside loop
ltm.crop.file <- paste0(dirname(ltm_file), '/NEUS_crop_', basename(ltm_file))
ltm.crop.check <- check.date.file(
  registry.file,
  dataset.name = 'ltm_crop',
  input.dates = all.input.dates,
  mount.dir = ""
)

if (ltm.crop.check$same.input.date & ltm.crop.check$file.exists & !redo) {
  message("LTM regional crop already exists and is up to date. Skipping...")
  ltm.crop.file <- get_recent_file(registry.file, dataset.name = 'ltm_crop')
} else {
  EDABUtilities::crop_nc_2d(
    data.in = ltm_file,
    var.name = 'sst',
    shp.file = shp.file,
    write.out = T,
    output.file = ltm.crop.file
  )

  message('LTM Regional Crop written to: ', ltm.crop.file)

  if (!ltm.crop.check$same.run.date) {
    append.registry(
      dataset.name = 'ltm_crop',
      file.name = ltm.crop.file,
      registry.file = registry.file,
      input.dates = all.input.dates,
      mount.dir = mount.dir,
      sub.dir = paste0(dirname(ltm_file), '/')
    )
  }
}

ltm.clim.file <- paste0(
  output_dir_oisst,
  'OISST_SOE_climatology_',
  basename(ltm_file)
)
ltm.clim.file <- gsub('.nc', '.rds', ltm.clim.file)
ltm.clim.check <- check.date.file(
  registry.file,
  dataset.name = 'ltm_climatology',
  input.dates = all.input.dates,
  mount.dir = ""
)

if (ltm.clim.check$same.input.date & ltm.clim.check$file.exists & !redo) {
  message("LTM climatology already exists and is up to date. Skipping...")
  ltm.clim.file <- get_recent_file(
    registry.file,
    dataset.name = 'ltm_climatology'
  )
} else {
  EDABUtilities::make_2d_summary_ts(
    agg.time = "days",
    data.in = ltm.crop.file,
    file.time = 'annual',
    output.files = ltm.clim.file,
    shp.file = shp.file,
    var.name = "sst",
    area.names = c("MAB", "GB", "GOM", "SS"),
    statistics.spatial = 'mean',
    statistic.temporal = 'mean',
    tz = NA,
    touches = TRUE,
    write.out = T
  )

  message('LTM Climatology written to: ', ltm.clim.file)

  if (!ltm.clim.check$same.run.date) {
    append.registry(
      dataset.name = 'ltm_climatology',
      file.name = ltm.clim.file,
      registry.file = registry.file,
      input.dates = all.input.dates,
      mount.dir = mount.dir,
      sub.dir = output_dir_oisst
    )
  }
}

# Run through annual files
for (y in 1:length(input.file.years)) {
  year.dates <- as.Date(paste0(input.file.years[y], "-01-01"))

  message("Calculating OISST NEUS crop...")
  oisst.crop.file <- paste0(
    oisst.crop.dir,
    oisst.crop.prefix,
    input.file.years[y],
    '.nc'
  )
  oisst.crop.check <- check.date.file(
    registry.file,
    dataset.name = paste0('oisst_crop_', input.file.years[y]),
    input.dates = year.dates,
    mount.dir = ""
  )

  if (oisst.crop.check$same.input.date & oisst.crop.check$file.exists & !redo) {
    message(
      "OISST regional crop already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )
    oisst.crop.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('oisst_crop_', input.file.years[y])
    )
  } else {
    EDABUtilities::crop_nc_2d(
      data.in = input.files[y],
      shp.file = shp.file,
      write.out = T,
      output.files = oisst.crop.file
    )
    if (!oisst.crop.check$same.run.date) {
      append.registry(
        dataset.name = paste0('oisst_crop_', input.file.years[y]),
        file.name = oisst.crop.file,
        registry.file = registry.file,
        input.dates = year.dates,
        mount.dir = mount.dir,
        sub.dir = oisst.crop.dir
      )
    }
  }

  # Calculate annual mean by EPU ----
  message("Calculating daily mean by EPU...")
  annual.mean.file <- paste0(
    annual.mean.dir,
    annual.mean.prefix,
    input.file.years[y],
    '.rds'
  )

  annual.mean.check <- check.date.file(
    registry.file,
    dataset.name = paste0('annual_mean_', input.file.years[y]),
    input.dates = year.dates,
    mount.dir = ""
  )

  if (
    annual.mean.check$same.input.date & annual.mean.check$file.exists & !redo
  ) {
    message(
      "OISST Annual mean file already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )

    oisst.crop.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('oisst_crop_', input.file.years[y])
    )
  } else {
    EDABUtilities::crop_nc_2d(
      data.in = input.files[y],
      shp.file = shp.file,
      var.name = 'sst',
      write.out = T,
      output.files = oisst.crop.file
    )
    message('OISST NES crop written to: ', oisst.crop.file)
    if (!oisst.crop.check$same.run.date) {
      append.registry(
        dataset.name = paste0('oisst_crop_', input.file.years[y]),
        file.name = oisst.crop.file,
        registry.file = registry.file,
        input.dates = year.dates,
        mount.dir = mount.dir,
        sub.dir = oisst.crop.dir
      )
    }
  }

  # Calculate annual mean by EPU ----
  message("Calculating daily mean by EPU...")
  annual.mean.file <- paste0(
    annual.mean.dir,
    annual.mean.prefix,
    input.file.years[y],
    '.rds'
  )

  annual.mean.check <- check.date.file(
    registry.file,
    dataset.name = paste0('annual_mean_', input.file.years[y]),
    input.dates = year.dates,
    mount.dir = ""
  )

  if (
    annual.mean.check$same.input.date & annual.mean.check$file.exists & !redo
  ) {
    message(
      "OISST Annual mean file already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )

    annual.mean.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('annual_mean_', input.file.years[y])
    )
  } else {
    EDABUtilities::make_2d_summary_ts(
      agg.time = "days",
      data.in = oisst.crop.file,
      file.time = 'annual',
      output.files = annual.mean.file,
      shp.file = shp.file,
      var.name = "sst",
      area.names = c("MAB", "GB", "GOM", "SS"),
      statistics.spatial = 'mean',
      statistic.temporal = 'mean',
      tz = NA,
      touches = TRUE,
      write.out = T
    )

    message('Annual mean for ', input.file.years[y], ' in: ', annual.mean.file)

    if (!annual.mean.check$same.run.date) {
      append.registry(
        dataset.name = paste0('annual_mean_', input.file.years[y]),
        file.name = annual.mean.file,
        registry.file = registry.file,
        input.dates = year.dates,
        mount.dir = mount.dir,
        sub.dir = annual.mean.dir
      )
    }
  }

  # Calculate Daily mean by EPU ----
  epu.names <- c('MAB', 'GB', 'GOM', 'SS')
  daily.epu.file <- file.path(
    daily.epu.dir,
    paste0(daily.epu.prefix, input.file.years[y], '.csv')
  )
  daily.epu.check <- check.date.file(
    registry.file,
    dataset.name = paste0('daily_epu_', input.file.years[y]),
    input.dates = year.dates,
    mount.dir = ""
  )

  if (daily.epu.check$same.input.date & daily.epu.check$file.exists & !redo) {
    message(
      "OISST Daily EPU file already exists for year ",
      input.file.years[y],
      ". Skipping..."
    )
    daily.epu.file <- get_recent_file(
      registry.file,
      dataset.name = paste0('daily_epu_', input.file.years[y])
    )
  } else {
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
    write.csv(daily.epu.heatwave, file = daily.epu.file, row.names = F)

    message('Daily mean for ', input.file.years[y], ' in: ', daily.epu.file)

    if (!daily.epu.check$same.run.date) {
      append.registry(
        dataset.name = paste0('daily_epu_', input.file.years[y]),
        file.name = daily.epu.file,
        registry.file = registry.file,
        input.dates = year.dates,
        mount.dir = mount.dir,
        sub.dir = daily.epu.dir
      )
    }
  }
}

# Calculate anomaly by subtracting climatology ----
oisst.anomaly.file <- paste0(
  output_dir_oisst,
  paste0("OISST_anomaly/oisst_anomaly_", Sys.Date(), ".csv")
)
check.dir(oisst.anomaly.file)
oisst.anomaly.check <- check.date.file(
  registry.file,
  dataset.name = 'oisst_anomaly',
  input.dates = all.input.dates,
  mount.dir = ""
)

if (
  oisst.anomaly.check$same.input.date & oisst.anomaly.check$file.exists & !redo
) {
  message('OISST Anomaly already exists and is up to date. Skipping...')
  oisst.anomaly.file <- get_recent_file(
    registry.file,
    dataset.name = 'oisst_anomaly'
  )
} else {
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

  write.csv(anomaly, file = oisst.anomaly.file, row.names = F)

  message("OISST Anomaly Indicator written to: ", oisst.anomaly.file)

  if (!oisst.anomaly.check$same.run.date) {
    append.registry(
      dataset.name = 'oisst_anomaly',
      file.name = oisst.anomaly.file,
      registry.file = registry.file,
      input.dates = all.input.dates,
      mount.dir = mount.dir,
      sub.dir = paste0(output_dir_oisst, "OISST_anomaly/")
    )
  }
  message('Done: OISST Anomaly')
}

# Do Surface Heatwave data from daily EPU mean
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
check.dir(daily.epu.heatwave.file)
heatwave.check <- check.date.file(
  registry.file,
  dataset.name = 'daily_epu_heatwave',
  input.dates = all.input.dates,
  mount.dir = ""
)

if (heatwave.check$same.input.date & heatwave.check$file.exists & !redo) {
  message(
    'OISST Daily EPU Heatwave already exists and is up to date. Skipping...'
  )
} else {
  daily.epu.files <- list.files(
    pattern = daily.epu.prefix,
    path = daily.epu.dir,
    full.names = T
  )
  daily.epu.out <- lapply(daily.epu.files, read.csv) |> dplyr::bind_rows()

  write.csv(daily.epu.out, daily.epu.heatwave.file, row.names = F)

  message("OISST Daily EPU Indicator written to: ", daily.epu.heatwave.file)

  if (!heatwave.check$same.run.date) {
    append.registry(
      dataset.name = 'daily_epu_heatwave',
      file.name = daily.epu.heatwave.file,
      registry.file = registry.file,
      input.dates = all.input.dates,
      mount.dir = mount.dir,
      sub.dir = paste0(output_dir_oisst, 'OISST_daily_epu_heatwave/')
    )
  }

  message('OISST Daily EPU Heatwave Done...')
}
