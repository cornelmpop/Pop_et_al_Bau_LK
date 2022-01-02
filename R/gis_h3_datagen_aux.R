# Process output of gis_h3_datagen into more manageable bits
# Can be sourced after sourcing gis_h3_datagen.R and model_main.R

#source("../R/model_main.R")
load("../data/times_cells_to_sources_verified.RData")
# Note: the following could have been done using the loaded costs_toBau too
cells_to_srcs_red <- merge(cells.to.srcs, glm_sel[, c("sid", "time_toBau")],
                           by.x = "target_sid", by.y = "sid")
rm(cells.to.srcs)
gc()
max_bs <- max(glm_sel$time_fromBau) + max(glm_sel$time_fromBau) * 0.05
cells_to_srcs_red$time <- cells_to_srcs_red$time.to.src + cells_to_srcs_red$time_toBau
cells_to_srcs_red <- cells_to_srcs_red[time < max(glm_sel$time_fromBau)]
gc()
cells_to_srcs_red$time.to.src <- NULL
cells_to_srcs_red$time_toBau <- NULL
cells_to_srcs_red$time.sqrt <- sqrt(cells_to_srcs_red$time)
cells_to_srcs_red$time <- NULL
cells_to_srcs_red <- merge(cells_to_srcs_red,
                           glm_sel[, c("sid", "qual.log", "ext.sqrt", "large_r")],
                           by.x = "target_sid", by.y = "sid")

#saveRDS(cells_to_srcs_red, "../data/cells_to_srcs_to_Bau.rds")

# Process eval_nodes for H3.rmd:
#eval_nodes <- readRDS("../data/cells_to_srcs_to_Bau.rds")
edge_w_bn <- readRDS("../data/costs_fromBau.rds") # Bau to new nodes edge w
## Restrict evaluation to nodes in 5hr radius:
edge_w_bn <- edge_w_bn[time_fromBau < 5 * 3600]
#eval_nodes <- eval_nodes[cell %in% edge_w_bn$cell]
eval_nodes <- cells_to_srcs_red[cell %in% edge_w_bn$cell]
saveRDS(eval_nodes, "../data/eval_nodes_5h.rds")
rm(cells_to_srcs_red)
gc()
