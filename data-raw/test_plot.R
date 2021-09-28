## Plotting test
#Plotting and data libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ecodata)
library(here)
library(kableExtra)
library(ggrepel)
library(stringr)
library(patchwork)
library(grid)
library(heatwaveR)

#General inline text input for report

#Council
council <- "New England Fishery Management Council"
council_abbr <- "NEFMC"

#Region identifiers
epu <- "Geroges Bank"
epu_abbr <- "GB"
region <- "New England"
region_abbr <- "NE" #Some commercial data organized by "MA" or "NE" regions, not by EPU

############################# GIS SETUP ######################################################

#GIS libraries
library(sf)
library(rgdal)
library(raster)
#CRS
crs <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

#EPU shapefile
epu_sf <- ecodata::epu_sf %>%
  filter(EPU %in% c("MAB","GB","GOM"))

#Map line parameters
map.lwd <- 0.4

# Set lat/lon window for maps
xmin = -77
xmax = -65
ymin = 36
ymax = 45
xlims <- c(xmin, xmax)
ylims <- c(ymin, ymax)

#Time series constants
shade.alpha <- 0.3
shade.fill <- "lightgrey"
lwd <- 1
pcex <- 2
trend.alpha <- 0.5
trend.size <- 2
hline.size <- 1
hline.alpha <- 0.35
hline.lty <- "dashed"
label.size <- 5
hjust.label <- 1.5
letter_size <- 4
feeding.guilds <- c("Apex Predator","Piscivore","Planktivore","Benthivore","Benthos")
x.shade.min <- 2010
x.shade.max <- 2020
#Function for custom ggplot facet labels
label <- function(variable,value){
  return(facet_names[value])
}



old<- ecodata::seasonal_oisst_anom %>%
  dplyr::filter(EPU %in% c("GB","GOM")) %>%
  dplyr::mutate(hline = 0,
                Var = stringr::str_to_title(stringr::str_extract(Var,"winter|spring|summer|fall")))
old$Var <- factor(ne_anom$Var, levels= c("Winter","Spring","Summer","Fall"))

ne_anom <- ecopull::seasonal_oisst_anom %>%
  dplyr::filter(EPU %in% c("GB","GOM")) %>%
  dplyr::mutate(hline = 0,
                Var = stringr::str_to_title(stringr::str_extract(Var,"winter|spring|summer|fall")))
ne_anom$Var <- factor(ne_anom$Var, levels= c("Winter","Spring","Summer","Fall"))

ne_anom_plt <- ggplot2::ggplot(data = old,
                               aes(x = Time, y = Value, color = EPU, group = EPU)) +

  ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
                    xmin = x.shade.min , xmax = x.shade.max,
                    ymin = -Inf, ymax = Inf) +
  ggplot2::geom_line()+
  ggplot2::geom_point()+
  ggplot2::geom_line(data = ne_anom, aes(x = Time, y = Value), color = "blue") +
  ggplot2::geom_point(data = ne_anom, aes(x = Time, y = Value), color = "blue") +
  ggplot2::ylim(-2,3)+
  #ecodata::geom_gls() +
  ggplot2::ylab(expression("SST Anomaly (C)")) +
  ggplot2::xlab(element_blank())+
  ggplot2::ggtitle("Gulf of Maine & Georges Bank SST Anomaly") +
  ggplot2::scale_color_manual(values = c("black","indianred"))+
  ggplot2::scale_x_continuous(expand = c(0.01, 0.01)) +
  ggplot2::geom_hline(aes(yintercept = hline),
                      size = hline.size,
                      alpha = hline.alpha,
                      linetype = hline.lty) +
  ggplot2::facet_wrap(Var ~., ncol = 2, scales = "free_y")+
  ecodata::theme_facet() +
  ggplot2::theme(strip.text=element_text(hjust=0),
                 plot.title = element_text(size = 12))+
  ecodata::theme_title()
ne_anom_plt


out_table<- ne_anom %>%
  rename(Value_new_workflow = Value) %>%
  left_join(old) %>%
  dplyr::mutate(Magnitude_difference = abs(Value - Value_new_workflow),
                Percent_difference = abs(Value - Value_new_workflow) / abs(Value) * 100)
out_table

write.csv(out_table, file = "comparative_table.csv")

plt <- ggplot2::ggplot(out_table,
                       ggplot2::aes(x = Magnitude_difference,
                                    fill = EPU))+
  ggplot2::geom_histogram(binwidth = 0.01,
                          color = "black") +
  ggplot2::theme_bw() +
  ggbreak::scale_x_break(c(0.14, 0.98), scales = 2)+
  ggplot2::xlim(c(0,1.25)) +
  ggplot2::xlab("Magnitude of difference") +
  ggplot2::ylim(c(0,35)) +
  ggplot2::labs(title = "Comparison of old and new OISST anomaly values, 2010-2021") +
  ggplot2::facet_grid(rows = ggplot2::vars(Var))
plt

ggsave("histogram_comparison.png", dpi = 72)

