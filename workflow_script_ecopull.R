#Command Line Cloud
#Rscript //nefscdata/EDAB_Workflows/GLORYS_automation_test.R "//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST" "//nefscdata/EDAB_Dev/atyrell"

#Gets arguments from command line
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  print(args)
  input_folder = args[1]
  output_folder = args[2]
  print('Using command line arguments')
} else {
  input_folder = '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST'
  output_folder = '//nefscdata/EDAB_Dev/atyrell'
  print('Using default arguments')
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

# (0) load functions

devtools::load_all()

# (1) calculate annual mean by EPU

annual_mean <- lapply(
  list.files(input_folder, full.names = TRUE),
  FUN = make_seasonal_oisst,
  type = "annual"
) |>
  data.table::rbindlist() |>
  tibble::as_tibble()

# (2) calculate climatology

climatology <- make_seasonal_oisst(
  '//nefscdata/EDAB_Datasets/OISST/V2/SOURCE/SST_LTM/sst.day.mean.ltm.1991-2020.nc',
  type = "ltm"
)

# (3) calculate anomaly by subtracting climatology

# (4) save to network drive

message('Done: OISST Anomaly')
