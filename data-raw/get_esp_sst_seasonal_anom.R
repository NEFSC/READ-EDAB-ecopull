#Analysis of OISST V2 data to extract seasonal SST time series
## Data gathered here
# "https://www.esrl.noaa.gov/psd/cgi-bin/DataAccess.pl?DB_dataset=
# NOAA+High-resolution+Blended+Analysis&DB_variable=Sea+Surface+
# Temperature&DB_statistic=Mean&DB_tid=81047&DB_did=132&DB_vid=2423"
# Spatial paraters
#lat:Begin (36N), End: (46N)
#lon:Begin (-77W), End: (-65W)

library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)

devtools::load_all()

### Changes start here
stock_list2 <- sort(unique(ecodata::ESP_sf$ID))
ecopull::create_null(stock_list2)

fname <- list.files(here::here("data-raw","gridded","sst_data"),
                    pattern = ".grd",
                    full.names = TRUE)

ESP_seasonal_oisst_anom <- NULL
for (e in stock_list2){
  message(e)
  for (i in 1:length(fname)
       ){
    message(fname[i])
    assign(e, rbind(get(e),
                   ecopull::get_group_mean(ltm_path = here::here("data-raw","gridded","ltm", "internet_ltm.grd"),
                                  fname = fname[i],
                                  shapefiles = ecodata::ESP_sf,
                                  region_name = e)))
    ESP_seasonal_oisst_anom <- rbind(ESP_seasonal_oisst_anom, get(e))
  }
}
#ESP_seasonal_oisst_anom

#process output
 ESP_seasonal_oisst_anom <- ESP_seasonal_oisst_anom %>%
   dplyr::mutate(Time = as.numeric(stringr::str_extract(year,"\\d{4}"))) %>%
#                 Var = paste(stringr::str_extract(year, "winter|spring|summer|fall"),"OI SST Anomaly")) %>%
   dplyr::select(-year) %>%
   dplyr::mutate(Units = "degreesC")


# metadata ----
attr(ESP_seasonal_oisst_anom, "tech-doc_url") <- "https://noaa-edab.github.io/tech-doc/seasonal-sst-anomalies.html"
attr(ESP_seasonal_oisst_anom, "data_files")   <- list(
  seasonal_oisst_anom_nc = seasonal_oisst_anom_nc)
attr(ESP_seasonal_oisst_anom, "data_steward") <- c(
  "Kimberly Bastille <kimberly.bastille@noaa.gov>")

usethis::use_data(ESP_seasonal_oisst_anom, overwrite = TRUE)

