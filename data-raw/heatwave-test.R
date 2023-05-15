library(heatwaveR)
library(readr)
`%>%` <- magrittr::`%>%`

raw.dir <- "data-raw"
gom<-read.csv(file.path(raw.dir,"GOM_SST_1982_to_2022_detrended.csv"), header = FALSE) %>%
  dplyr::rename(V1, "t" ="V1",
                V2, "temp" ="V2") %>%
  dplyr::filter(!temp == "temp_detrended") %>%
  dplyr::mutate(temp = as.numeric(temp),
                t = as.Date(t, "%m/%d/%y")) %>%
  tidyr::drop_na()

ts <- heatwaveR::ts2clm(gom, climatologyPeriod = c("1982-01-01", "2022-12-05"))
gom.mhw <- heatwaveR::detect_event(ts)
gom.hw<- gom.mhw$event %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative)%>%
  dplyr::mutate(EPU = "GOM")

write.csv(gom.hw, "data-raw/heatwave_output.csv")
