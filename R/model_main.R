# Baseline logistic model formulation and cross-validation. Supplies glm_sel
# data to other scripts
library(data.table)
library(caret)

# 1. Load and prepare data:
glm_sel <- readRDS("../data/bau_data.rds")
glm_sel$qual.log <- log(glm_sel$quality)
glm_sel$ext.sqrt <- sqrt(glm_sel$extent)
glm_sel$time.sqrt <- sqrt(glm_sel$time_fromBau)
glm_sel$pot.used.fac <- as.factor(glm_sel$pot.used)

# Seed random no. generator for replicability
set.seed(070281) # CMPs birthday

# Remove inaccessible sources (i.e., for which travel times cannot be computed)
# from consideration:
glm_sel <- glm_sel[-which(is.na(glm_sel$time_fromBau)), ]

# Scale to get comparable coefficients
glm_sel$qual.log.z <- as.vector(scale(glm_sel$qual.log))
glm_sel$ext.sqrt.z <- as.vector(scale(glm_sel$ext.sqrt))
glm_sel$time.sqrt.z <- as.vector(scale(glm_sel$time.sqrt))
glm_sel$large_r.z <- as.vector(scale(glm_sel$large_r))

# 2. Model formulation (with scaled and unscaled predictors):
mod <- glm(pot.used.fac ~ qual.log.z + ext.sqrt.z + time.sqrt.z + large_r.z,
           data = glm_sel, family = "binomial")
mod_ns <- glm(pot.used.fac ~ qual.log + ext.sqrt + time.sqrt + large_r,
           data = glm_sel, family = "binomial")

# 2a. Test significance:
mod_null <- glm(pot.used.fac ~ 1, data = glm_sel, family = "binomial")
res_mod_sig <- anova(mod_null, mod, test = "Chisq")

# 3. Fits for individual sources:
glm_sel$probs.full <- predict.glm(object = mod, type = "response")


# Generate cross-validated PS1 values
nfolds <- 5 # Number of folds
reps <- 100 # Number of repetitions

pred_cv_fits <- list() # Predicted fits, all folds for each repetition
mods_byfold <- list() # Keep track of all models (by fold, for all reps)
pred_cv_tc <- list() # Keep track of predicted fits when time is kept to min.
for (r in 1:reps) {
  # Get a list of potused and unused sources, and randomly divide their SIDs by
  # fold:
  data.potused <- sample(glm_sel[pot.used.fac == "1", ]$sid)
  data.nonused <- sample(glm_sel[pot.used.fac == "0", ]$sid)
  folds.potused <- data.table(sid = data.potused,
                              fold = cut(1:length(data.potused),
                                         breaks = nfolds, labels = 1:nfolds))
  folds.nonused <- data.table(sid = data.nonused,
                              fold = cut(1:length(data.nonused),
                                         breaks = nfolds, labels = 1:nfolds))

  # Loop through the list of folds, predicting values based on non-fold data
  pred.fit <- list() # List of predited fits
  sids.cont <- c()
  for (i in 1:nfolds) {
    glm_pred <- rbind(glm_sel[sid %in% folds.potused[fold == i, ]$sid],
                     glm_sel[sid %in% folds.nonused[fold == i, ]$sid])
    glm_train <- glm_sel[!sid %in% glm_pred$sid, ]

    mod.cv <- glm(pot.used.fac ~ qual.log + ext.sqrt + time.sqrt + large_r,
                 data = glm_train, family = "binomial")
    mods_byfold[[(r - 1) * nfolds + i]] <- mod.cv # Add current model


    # Predict fits and CI:
    pred.fit[[i]] <- glm_pred
    p.res.l <- predict.glm(object = mod.cv, newdata = glm_pred,
                          type = "link", se.fit = TRUE)
    pred.fit[[i]]$fit <- exp(p.res.l$fit) / (1 + exp(p.res.l$fit))
    pred.fit[[i]]$l.f.raw <- p.res.l$fit - p.res.l$se.fit * abs(qt(p = 0.025,
                                                      df = mod.cv$df.residual))
    pred.fit[[i]]$u.f.raw <- p.res.l$fit + p.res.l$se.fit * abs(qt(p = 0.025,
                                                      df = mod.cv$df.residual))
    pred.fit[[i]]$lwr_fit <- exp(pred.fit[[i]]$l.f.raw) /
                             (1 + exp(pred.fit[[i]]$l.f.raw))
    pred.fit[[i]]$upr_fit <- exp(pred.fit[[i]]$u.f.raw) /
                             (1 + exp(pred.fit[[i]]$u.f.raw))

    # Predict with time kept constant at minimum obs. for Bau - some
    # extrapolating unavoidable because minimum values will vary by fold.
    pred.fold.tc <- glm_pred[, c("sid", "aid", "qual.log", "ext.sqrt",
                          "time.sqrt", "large_r")]
    # Set minimum to constant corresponding to actual dataset minimum.
    pred.fold.tc$time.sqrt <- min(glm_sel$time.sqrt)
    pred.fold.tc$fit.timemin <- predict.glm(object = mod.cv,
                                            newdata = pred.fold.tc,
                                            type = "response")
    pred_cv_tc[[(r - 1) * nfolds + i]] <- pred.fold.tc
  }
  pred_cv_fits[[r]] <- rbindlist(pred.fit)
}
# Aggregate individual predicted values
pred_cv_fits <- rbindlist(pred_cv_fits)
pred_cv_fits_agg <- pred_cv_fits[, .(mean_pred = mean(fit),
                                     med_pred = median(fit),
                                     obs = mean(pot.used)), by = c("sid")]
# Build confusion matrix using average predicted values per source:
cm_out <- caret::confusionMatrix(as.factor(as.numeric(pred_cv_fits_agg$mean_pred > .5)),
                                as.factor(pred_cv_fits_agg$obs), positive = "1")

# Cross-validated time-controlled fits.
pred_cv_tc_full <- rbindlist(pred_cv_tc)
# Aggregate data per source ID:
pred_cv_tc_full <- pred_cv_tc_full[, list(mean_fit_cv_tc = mean(fit.timemin),
                                          med_fit_cv_tc = median(fit.timemin)),
                                   by = c("sid")]
setkey(pred_cv_tc_full, "sid")
pred_cv_tc_full <- merge(glm_sel, pred_cv_tc_full)