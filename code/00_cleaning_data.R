library(aqp)
library(tidyverse)
library(readxl)
rm(list = ls())

# Load data from an MS Excel file ----------------------------------------------
# site data
site <- read_excel("data_input/Profiles_data.xlsx", sheet = "Soil_profile_loc")
# horizon data
hor_des <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_descr")
hor_chem <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_chem")
hor_phys <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_phys")
hor_mech <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_mech")
# metadata (not available)

# Clean data and select useful variables ---------------------------------------
# site 
site %>% View()

names(data)
data <- data %>% 
  select(-Depth, -SAR) %>% 
  rename(hid = Sample, pid = ProfileID, x = Longitude, y = Latitude,
         top = Upper, bottom = Lower, n = Horizon, sand = Sand, 
         silt = Silt, clay = Clay, ph = pH, ec = EC, oc = OC, na = `Na+`,
         esp = ESP)

horizons <- data %>% dplyr::select(idp, idh, top:esp)
site <- data %>% select(idp, x, y) %>% unique()

x <- horizons
depths(x) <- idp ~ top + bottom
site(x) <- site
aqp::coordinates(x) <- ~x+y
aqp::proj4string(x) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"

aqp::checkSPC(x)
aqp::checkHzDepthLogic(x)
aqp::fillHzGaps(x)

#### Spline

z <- aqp::spc2mpspline(x, var_name = "clay", hzdesgn = "n", d = c(0, 50))
plotSPC(z, color = "clay_spline", divide.hz = FALSE )

z@horizons %>% group_by(idp) %>% summarise(meanClay = mean(clay_splain, na.rm=T))
##### 
s <- aqp::slab(x, 
               fm = ~ clay + sand + ph + oc,
               slab.structure = 0:100,
               slab.fun = function(x) quantile(x, c(0.01, 0.5, 0.99), na.rm = TRUE))

ggplot(s, aes(x = top, y = X50.)) +
  # plot median
  geom_line() +
  # plot 10th & 90th quantiles
  geom_ribbon(aes(ymin = X1., ymax = X99., x = top), alpha = 0.2) +
  # invert depths
  xlim(c(100, 0)) +
  # flip axis
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x")


