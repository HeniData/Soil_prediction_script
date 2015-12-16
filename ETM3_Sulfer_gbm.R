#' Boosted regression predictions of nutrient mass balances with spatial covariates
#' EthioSIS Mehlich-3 extractable P,K,S,Ca & Mg data from 255 Woredas
#' M. Walsh, December 2015

# Required packages
# install.packages(c("devtools","caret","doParallel","plyr","gbm")), dependencies=TRUE)
require(devtools)
require(caret)
require(doParallel)
require(plyr)
require(gbm)
require(raster)

# Data setup --------------------------------------------------------------
# SourceURL <- "https://raw.githubusercontent.com/mgwalsh/Ethiopia/blob/master/ETM3_setup.R"
# source_url(SourceURL)
setwd("./script")
source("ETM3_setup_2.R")

# Mehlich-3 nutrient mass balance variables

S <- log(etm3_cal$S)



# Gridded covariates
GRIDSc <- etm3_cal[c(8:35)] ## gridded covariates for model calibration from 204 Woredas
GRIDSv <- etm3_val[c(8:35)] ## same for 51 randomly selected validation Woredas

# GBM models --------------------------------------------------------------
# Start foreach to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# Control setup
set.seed(1385321)
tc <- trainControl(method = "cv", number=10)

# V0 = ilr [P,K,S,Ca,Mg | Fv]
S.gbm <- train(GRIDSc, S, 
                method = "gbm", 
                preProc = c("center", "scale"),
                trControl = tc,
                tuneGrid = expand.grid(.n.trees=seq(50,500,by=50), 
                                       .interaction.depth = 3,
                                       .shrinkage = 0.1,
                                       .n.minobsinnode = 100))
print(S.gbm)
S.imp <- varImp(S.gbm)
plot(S.imp, top=28)

# # V3 = ilr [P,K | K,Ca,Mg]
# V3.gbm <- train(GRIDSc, V3, 
#                 method = "gbm", 
#                 preProc = c("center", "scale"),
#                 trControl = tc,
#                 tuneGrid = expand.grid(.n.trees=seq(50,500,by=50), 
#                                        .interaction.depth = 3,
#                                        .shrinkage = 0.1,
#                                        .n.minobsinnode = 100))
# print(V3.gbm)
# v3.imp <- varImp(V3.gbm)
# plot(v3.imp, top=28)
# 
# # V4 = ilr [K | Ca,Mg]
# V4.gbm <- train(GRIDSc, V4, 
#                 method = "gbm", 
#                 preProc = c("center", "scale"),
#                 trControl = tc,
#                 tuneGrid = expand.grid(.n.trees=seq(50,500,by=50), 
#                                        .interaction.depth = 3,
#                                        .shrinkage = 0.1,
#                                        .n.minobsinnode = 100))
# print(V4.gbm)
# v4.imp <- varImp(V4.gbm)
# plot(v4.imp, top=28)
# 
# # V5 = ilr [P | S]
# V5.gbm <- train(GRIDSc, V5, 
#                 method = "gbm", 
#                 preProc = c("center", "scale"),
#                 trControl = tc,
#                 tuneGrid = expand.grid(.n.trees=seq(50,500,by=50), 
#                                        .interaction.depth = 3,
#                                        .shrinkage = 0.1,
#                                        .n.minobsinnode = 100))
# print(V5.gbm)
# v5.imp <- varImp(V5.gbm)
# plot(v5.imp, top=28)
# 
# # V6 = ilr [Ca | Mg]
# V6.gbm <- train(GRIDSc, V6, 
#                 method = "gbm", 
#                 preProc = c("center", "scale"),
#                 trControl = tc,
#                 tuneGrid = expand.grid(.n.trees=seq(50,500,by=50), 
#                                        .interaction.depth = 3,
#                                        .shrinkage = 0.1,
#                                        .n.minobsinnode = 100))
# print(V6.gbm)
# v6.imp <- varImp(V6.gbm)
# plot(v6.imp, top=28)
# 
stopCluster(mc)

# Test set predictions ----------------------------------------------------
S_test_gbm <- predict(S.gbm, GRIDSv)
# V3_gbm <- predict(V3.gbm, GRIDSv)
# V4_gbm <- predict(V4.gbm, GRIDSv)
# V5_gbm <- predict(V5.gbm, GRIDSv)
# V6_gbm <- predict(V6.gbm, GRIDSv)
# pred <- cbind.data.frame(V0_gbm,V3_gbm,V4_gbm,V5_gbm,V6_gbm)
# test <- etm3_val[c("PID","V0","V3","V4","V5","V6")]
# gbm_eval <- cbind(test, pred)

# Gridded predictions -----------------------------------------------------
S_pred_gbm <- predict(grids, V0.gbm)
# V3_gbm <- predict(grids, V3.gbm)
# V4_gbm <- predict(grids, V4.gbm)
# V5_gbm <- predict(grids, V5.gbm)
# V6_gbm <- predict(grids, V6.gbm)
# gbm_pred <- stack(V0_gbm,V3_gbm,V4_gbm,V5_gbm,V6_gbm)
# names(gbm_pred) <- c("V0_gbm","V3_gbm","V4_gbm","V5_gbm","V6_gbm")
# plot(gbm_pred)

# Export Gtif's -----------------------------------------------------------
# Create a "Results" folder in current working directory
#dir.create("ETM3_results", showWarnings=F)

# Export Gtif's to "./ETM3_results"
