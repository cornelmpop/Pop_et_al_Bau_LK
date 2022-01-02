# Misc. supporting functions. Please get in touch before using, since there may
# be undocumented gotchas.
# Copyright: 2020, Cornel Pop

#' Report p values in a sensible manner:
#' p = a given raw p value
#' cutoff = a given cutoff below which it makes no real sense to report
report_pvals <- function(p, cutoff = 0.001) {
  if (p < cutoff) {
    p <- "< .001"
  } else {
    p.dig <- nchar(strsplit(as.character(cutoff, options(scipen = 999)),
                           "[.]")[[1]][2])
    p <- paste("= ", round(p, p.dig), sep = "")
  }
  return(p)
}


#' This function predicts fits from a logistic model when keeping
#' all but one variable constant (i.e. type != 'all'), constant value is 0
#' (assumes all variables are z-transformed, so zero represents their mean).
predict_logistic_fits <- function(model, var_name = NA, var_vals = NA,
                                  var_df = NA, vopt = NA, vopt_val = NA,
                                  type = "raw") {
  pn <- 100 # Number of points for plots
  mod_vars <- names(model$coefficients)[2:length(model$coefficients)]
  v_idx <- which(mod_vars == var_name)

  if (type != "all") {
    if (is.na(var_name) || is.na(var_vals)) {
      stop("var_name and var_vals must be specified when type is not 'all'")
      }
  }

  if (type == "all") {
    pred_data <- var_df[, mod_vars]
  } else if (type == "raw") {
    pred_data <- data.frame(matrix(ncol = length(mod_vars),
                                  nrow = length(var_vals)))
    names(pred_data) <- mod_vars

    pred_data[, v_idx] <- var_vals
    pred_data[, -v_idx] <- 0

    if (!is.na(vopt)) {
      vo.idx <- which(mod_vars == vopt)
      pred_data[, vo.idx] <- vopt_val
    }

  } else if (type == "plot") {
    pred_data <- data.frame(matrix(ncol = length(mod_vars), nrow = pn))
    names(pred_data) <- mod_vars

    pred_data[, v_idx] <- seq(from = min(var_vals), to = max(var_vals),
                              length.out = pn)
    pred_data[, -v_idx] <- 0

    if (!is.na(vopt)) {
      vo.idx <- which(mod_vars == vopt)
      pred_data[, vo.idx] <- vopt_val
    }
  } else {
    stop("Type must be set appropriately - see function source.")
  }

  probs_out <- predict.glm(object = model, newdata = pred_data,
                           type = "link", se.fit = T)
  probs_out.df <- data.frame(fit = probs_out$fit,
                            lwr = probs_out$fit - probs_out$se.fit * abs(qt(p = 0.025, df = model$df.residual)),
                            upr = probs_out$fit + probs_out$se.fit * abs(qt(p = 0.025, df = model$df.residual)))
  probs_out.df <- exp(probs_out.df) / (1 + exp(probs_out.df))
  return(cbind(pred_data, probs_out.df))
}


#' Function for generating plots of fitted values w/ confidence intervals for
#' isolated predictors.
plot_binpred_inf <- function(model, var_df, var_name, resp_name,
                             var_name_unscaled=NA, vopt=NA, vopt_val=NA,
                             bin_freq=TRUE) {
  var_df_idx <- which(names(var_df) == var_name)
  resp_df_idx <- which(names(var_df) == resp_name)

  if (is.na(var_name_unscaled)) {
    var_name_unscaled <- substr(var_name, 1, nchar(var_name) - 2)
  }
  orig_df_idx <- which(names(var_df) == var_name_unscaled)

  # Get requested fits:
  pred_fits <- predict_logistic_fits(model, var_name = var_name,
                                      var_vals = var_df[, var_df_idx],
                                      vopt = vopt, vopt_val = vopt_val,
                                      type = "raw")
  pred_fits$response <- var_df[, resp_df_idx]
  names(pred_fits)[which(names(pred_fits) == var_name)] <- "targetPred__"

  # Bin frequencies of alternative responses:
  pts_pos_idx <- which(pred_fits$response == 1)

  pts_pos_sum <- data.frame(table(var_df[pts_pos_idx, var_name]))
  pts_pos_sum[, 1] <- as.numeric(as.character(pts_pos_sum[, 1]))
  pts_pos_sum[, 3] <- 1
  names(pts_pos_sum) <- c("x", "Freq", "y")

  pts_neg_sum <- data.frame(table(var_df[-pts_pos_idx, var_name]))
  pts_neg_sum[, 1] <- as.numeric(as.character(pts_neg_sum[, 1]))
  pts_neg_sum[, 3] <- 0
  names(pts_neg_sum) <- c("x", "Freq", "y")

  # Axis values:
  orig.v <- var_df[, orig_df_idx]
  alt.labels <- sort(unique(orig.v)) # Labels on unscaled variable
  alt.ticks <- (alt.labels - mean(orig.v)) / sd(orig.v)

  g <- ggplot(pred_fits, aes(x = targetPred__, y = fit)) +
    theme_bw() +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = var_name)

  if (bin_freq == TRUE) {
    g <- g + geom_point(data = pts_pos_sum, aes(x = x, y = y, size = Freq)) +
      geom_point(data = pts_neg_sum, aes(x = x, y = y, size = Freq))
  } else {
    g <- g + geom_point(data = pts_pos_sum, aes(x = x, y = y), size = 0.5) +
      geom_point(data = pts_neg_sum, aes(x = x, y = y), size = 0.5)
  }

  return(list(g = g, xscale = list(breaks = alt.ticks, labels = alt.labels)))
}