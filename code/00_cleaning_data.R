library(aqp)
library(tidyverse)

data <- read_csv("data_input/soildata.csv")

names(data)
data <- data %>% 
  select(-Depth, -SAR) %>% 
  rename(idh = Sample, idp = ProfileID, x = Longitude, y = Latitude,
         top = Upper, bottom = Lower, n = Horizon, sand = Sand, 
         silt = Silt, clay = Clay, ph = pH, ec = EC, oc = OC, na = `Na+`,
         esp = ESP)

horizons <- data %>% dplyr::select(idp, idh, top:esp)
site <- data %>% select(idp, x, y) %>% unique()

x <- horizons
depths(x) <- idp ~ top + bottom
site(x) <- site
aqp::coordinates(x) <- ~x+y

aqp::checkSPC(x)
aqp::checkHzDepthLogic(x)
aqp::fillHzGaps(x)

d1 <- profile_compare(x, vars=c('clay','silt','ph','oc'),
                max_d=100, k=0.01, plot.depth.matrix=TRUE)
d1 <- cluster::diana(d1)
p <- as.phylo(as.hclust(d1))
plot(p, show.tip.label=FALSE)
tiplabels(sp2$surface, col=cutree(h, 3), bg=NA, cex=0.75)
