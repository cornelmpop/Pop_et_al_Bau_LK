# Simple script to generate convex hulls around exploited source areas
# TODO: Rewrite as a function.

source("../R/model_main.R")

# Prep:
library(rgdal)
sources_v <- readOGR("../data/sources_utm_abr")
sources_v <- data.table(sources_v@data[, c("SourceID", "Elevation",
                                           "x", "y", "z")])
names(sources_v)[1] <- "sid"
setkeyv(sources_v, "sid")
gis_data <- merge(glm_sel, sources_v)

### Convex hulls:
aid_poly <- list()
for (i in unique(gis_data[pot.used == 1]$aid)) {
  aid.data <- gis_data[aid == i, c("x", "y")]
  z <- chull(aid.data)
  aid_poly[[as.character(i)]] <- aid.data[c(z, z[1]), ]
}
saveRDS(aid_poly, "../data/aids.chulls.rds")
