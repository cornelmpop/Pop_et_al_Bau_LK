# DESCRIPTION:
# -----------
# TODO: Re-verify this script after clean-up.
#
# Compute times to all alternative sources from all landscape locations,
# verifying the data.
#
# PROVIDES:
# --------
# - src_cells, cells_to_srcs, costs_toBau, costs_fromBau, all saved to
#   ../data/times_cells_to_sources_verified.RData. Costs_from/to Bau can be
#   used to filter out cells from cells.to.src which lie beyond a certain time
#   radius.
# - src_issues: Sources for which there are issues with NA on cost rasters
#

# PART 1: Prep. work
# -------------------
# NOTE:  Make sure the following python script is run
# Process on inverted DEM, so we are looking at costs of moving from given
# location TO the source:
# - python gen_src_costmaps.py -m 72000 /mnt/ramdisk/Vaucluse/GIS Vaucluse_UTM sources_utm -d SimRegion_inv
library(rgrass7)
library(rgdal)
library(raster)
initGRASS("~/soft/system/grass7_py2/grass-7.4.0",
          gisDbase = "/mnt/ramdisk/Vaucluse/GIS",
          location = "Vaucluse_UTM", mapset = "PERMANENT", override = T,
          use_g.dirseps.exe = TRUE)
get.GIS_LOCK()
set.GIS_LOCK()

source("../R/model_main.R")
gis_sel <- glm_sel # Make a working copy of the source data.


# PART 2: Basic data compilation with validation
# ----------------------------------------------
src_cells <- list() # Map sources to cell 'coordinates' (i.e. row id)
check_exts <- list() # Checks for validation - all params. should be identical
# Times from cells to sources - From this <cell>, to that <target_sid>, takes
# <time.to.src> to walk.
src_info <- list()
for (s in gis.sel$sid) {
  sid_r <- readRAST(paste("source_", s, sep = ""))
  # Make sure all source files have absolutely identical parameters (e.g.
  # extents)
  check_exts[[s]] <- data.table(data.length = nrow(sid_r@data),
                                cc.off.x1 = sid_r@grid@cellcentre.offset[1],
                                cc.off.x2 = sid_r@grid@cellcentre.offset[2],
                                cell.size.x1 = sid_r@grid@cellsize[1],
                                cell.size.x2 = sid_r@grid@cellsize[2],
                                cell.dim.x1 = sid_r@grid@cells.dim[1],
                                cell.dim.x2 = sid_r@grid@cells.dim[2],
                                bb.1 = as.numeric(sid_r@bbox[1, 1]),
                                bb.2 = as.numeric(sid_r@bbox[2, 1]),
                                bb.3 = as.numeric(sid_r@bbox[1, 2]),
                                bb.4 = as.numeric(sid_r@bbox[2, 2]))

  # Record times to source from different locations (cells):
  src_info[[s]] <- data.table(target_sid = rep(s, nrow(sid_r@data)),
                              time.to.src = sid_r@data[, 1],
                              cell = as.numeric(rownames(sid_r@data)))

  # Where is the source (cell ID):
  sloc <- which(sid_r@data == 0)
  src_cells[[s]] <- data.table(src = s, cell = sloc)
}
rm(sid_r, sloc) # Clean

# Read in info on cells for Bau:
costs_toBau <- readRAST("coston_SimRegion_inv")
costs_toBau_info <- data.table(data.length = nrow(costs_toBau@data),
                             cc.off.x1 = costs_toBau@grid@cellcentre.offset[1],
                             cc.off.x2 = costs_toBau@grid@cellcentre.offset[2],
                             cell.size.x1 = costs_toBau@grid@cellsize[1],
                             cell.size.x2 = costs_toBau@grid@cellsize[2],
                             cell.dim.x1 = costs_toBau@grid@cells.dim[1],
                             cell.dim.x2 = costs_toBau@grid@cells.dim[2],
                             bb.1 = as.numeric(costs_toBau@bbox[1, 1]),
                             bb.2 = as.numeric(costs_toBau@bbox[2, 1]),
                             bb.3 = as.numeric(costs_toBau@bbox[1, 2]),
                             bb.4 = as.numeric(costs_toBau@bbox[2, 2]))
costs_toBau <- data.table(time_toBau = costs_toBau@data[, 1],
                          cell = as.numeric(rownames(costs_toBau@data)))

costs_fromBau <- readRAST("coston_SimRegion")
costs_fromBau_info <- data.table(data.length = nrow(costs_fromBau@data),
                            cc.off.x1 = costs_fromBau@grid@cellcentre.offset[1],
                            cc.off.x2 = costs_fromBau@grid@cellcentre.offset[2],
                            cell.size.x1 = costs_fromBau@grid@cellsize[1],
                            cell.size.x2 = costs_fromBau@grid@cellsize[2],
                            cell.dim.x1 = costs_fromBau@grid@cells.dim[1],
                            cell.dim.x2 = costs_fromBau@grid@cells.dim[2],
                            bb.1 = as.numeric(costs_fromBau@bbox[1, 1]),
                            bb.2 = as.numeric(costs_fromBau@bbox[2, 1]),
                            bb.3 = as.numeric(costs_fromBau@bbox[1, 2]),
                            bb.4 = as.numeric(costs_fromBau@bbox[2, 2]))
costs_fromBau <- data.table(time_fromBau = costs_fromBau@data[, 1],
                            cell = as.numeric(rownames(costs_fromBau@data)))


# CHECK 1: Verify that parameters for all source rasters are identical (no
#          missaligned cells, for whatever reason), and that the same applies to
#          Bau cost DEMs
check_exts_c <- rbindlist(check_exts)
check_exts_c <- rbind(check_exts_c, costs_toBau_info, costs_fromBau_info)

if (!length(unique(apply(check_exts_c, MAR = 2,
                        FUN = function(x) {length(unique(x))}))) == 1) {
  stop("Not all parameters are identical across rasters.")
}
rm(check_exts, check_exts_c, costs_toBau_info, costs_fromBau_info) # Clean.

# CHECK 2: Verify costs_toBau and costs_fromBau against glm data (gis.sel)
src_issues <- c()
for (s in glm_sel$sid) {
  s_cell <- src_cells[src == s]$cell
  toBau_res <- c(round(glm_sel[sid == s]$time_toBau, 2),
                round(costs_toBau[cell == s_cell]$time_toBau, 2))
  fromBau_res <- c(round(glm_sel[sid == s]$time_fromBau, 2),
                  round(costs_fromBau[cell == s_cell]$time_fromBau, 2))

  if (TRUE %in% is.na(toBau_res)) {
    src_issues <- c(src_issues, s)
  } else if (!identical(toBau_res[1], toBau_res[2])) {
    stop("Cost mistmatch between GLM data and cost raster map (to Bau)")
  }

  if (TRUE %in% is.na(fromBau_res)) {
    src_issues <- c(src_issues, s)
  } else if (!identical(fromBau_res[1], fromBau_res[2])) {
    stop("Cost mistmatch between GLM data and cost raster map (from Bau)")
  }
}
rm(s_cell, toBau_res, fromBau_res) # Clean


# Save:
src_cells <- src_cells.c
cells_to_srcs <- src_info.c
out_f <- "../data/times_cells_to_sources_verified.RData"
if (file.exists(out_f)) {
  stop("'file' already exists")
} else {
  save(src_cells, cells_to_srcs, costs_toBau, costs_fromBau,
       file = out_f, compress = FALSE) # Don't enable compress! MUCH slower!
}
rm(out_f)

out_f <- "../data/bau_classif_blank_fullext_verified.rds"
if (file.exists(out_f)) {
  stop("'file' already exists")
} else {
  saveRDS(costs_toBau, file = out_f, compress = FALSE)
}
rm(out_f)

####################################################

### Close GRASS connection
unset.GIS_LOCK()
unlink_.gislock()
remove_GISRC()