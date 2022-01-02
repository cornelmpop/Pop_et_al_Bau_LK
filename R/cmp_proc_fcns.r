# Misc data processing functions.
# Copyright (2020) Cornel Pop


#' Load and prepare data on edge weights to form network of sources (no other
#' nodes considered).
#' Note: Default data consists of minimum travel times (seconds) from node 1
#' (col 1) to node 2 (col 2)
form_source_net <- function(nw_data = "../data/srctimestoall.csv") {
  # Load GRASS/python generated data on minimum travel times between sources:
  edge_w <- data.table::data.table(read.csv(nw_data, header = F))
  edge_w$times <- as.vector(as.character(edge_w$V3))
  edge_w[which(edge_w$V3 == "*"), ]$times <- NA
  edge_w$times <- as.numeric(edge_w$times)

  # Make sure that all NA values correspond to no-data ("*")
  if (unique(as.character(edge_w$V3[which(is.na(edge_w$times))])) == "*") {
    edge_w$V3 <- NULL
  } else {
    stop("Re-check input data")
  }
  names(edge_w) <- c("sid", "target_sid", "edge_w_st")

  return(edge_w)
}
