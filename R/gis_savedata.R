# Loads GRASS data and exports it so that it can be worked with easily from
# within R
library(rgrass7)
library(rgdal)
library(raster)
initGRASS("~/soft/system/grass7_py2/grass-7.4.0",
          gisDbase = "/mnt/ramdisk/Vaucluse/GIS",
          location = "Vaucluse_UTM", mapset = "PERMANENT", override = T,
          use_g.dirseps.exe = TRUE)
get.GIS_LOCK()
set.GIS_LOCK()

#python gen_srcprobmap.py /mnt/ramdisk/Vaucluse/GIS Vaucluse_UTM sources_utm sites_utm -i 36000 -o 72000 6 -d SimRegion

# Make sure the contours file exists/is set:
execGRASS("r.contour", input = "coston_SimRegion@PERMANENT",
          output = "cost_timefromBau", levels = 1:5 * 3600)

# Set region to extent defined by the contours (e.g. 5 hrs walking time)
execGRASS("g.region", vector = "cost_timefromBau")

# Load rasters and vectors of interest, and save them:
bau_5hrs <- readRAST("SimRegion")
writeRaster(raster::raster(bau_5hrs), "../data/bau_5hrs.tif")

# Contours:
hrs_contour_fromBau <- readVECT("cost_timefromBau")
rgdal::writeOGR(hrs_contour_fromBau, "cost_timefromBau",
                dsn = "../data/time_fromBau_contours",
                driver = "ESRI Shapefile")

# Sites:
sites.v <- readVECT("sites_utm")
rgdal::writeOGR(sites.v, "sites_utm", dsn = "../data/sites_utm",
                driver = "ESRI Shapefile")

# Sources:
sources.v <- readVECT("sources_utm")
rgdal::writeOGR(sources.v, "sources_utm", dsn = "../data/sources_utm",
                driver = "ESRI Shapefile")


### Close GRASS connection
unset.GIS_LOCK()
unlink_.gislock()
remove_GISRC()
