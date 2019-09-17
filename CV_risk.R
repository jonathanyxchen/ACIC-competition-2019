library(tidyverse)
devtools::load_all("acic2019bbd")
library(SuperLearner)

tmle_low_data = read_csv("tmle_low_results.csv")
# tmle_high_data = read_csv("tmle_high_results.csv")
file_names = tmle_low_data[tmle_low_data$DGPid == 22, ] %>% pull(dataset)
results=list() 
for (i in file_names) {
  dataset = paste("data/challenge_low/", i, ".csv", sep = '')
  data <- data.table::fread(dataset)
  data <- data[, lapply(.SD, as.double)]
  W_nodes <- grep("V", names(data), value = TRUE)
  node_list <- list(Y = "Y", A = "A", W = W_nodes)
  
  # define models for Q and g with screening
  #lrnr_glm <- make_learner(Lrnr_glm)
  #lrnr_mean <- make_learner(Lrnr_mean)
  #lrnr_glmnet <- make_learner(Lrnr_glmnet)
  #lrnr_glmnet5 <- make_learner(Lrnr_glmnet, alpha = .5)
  #lrnr_glmnet500 <- make_learner(Lrnr_glmnet, nlambda = 500)
  #lrnr_ranger <- make_learner(Lrnr_ranger)
  #lrnr_xgboost <- make_learner(Lrnr_xgboost)
  #lrnr_xgboost50 <- make_learner(Lrnr_xgboost, nrounds = 50)
  #lrnr_polymars <- Lrnr_pkg_SuperLearner$new("SL.polymars")
  #lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  lrnr_dbarts <- make_learner(Lrnr_dbarts)
  stack <- make_learner(Stack, 
                        #lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_glmnet5,
                        #lrnr_glmnet500, lrnr_ranger, lrnr_xgboost,
                        #lrnr_xgboost50, lrnr_polymars, lrnr_bayesglm, 
                        lrnr_dbarts)
  
  screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
  screen_rf <- Lrnr_pkg_SuperLearner_screener$new("screen.randomForest")
  cor_pipeline <- make_learner(Pipeline, screen_cor, stack)
  rf_pipeline <- make_learner(Pipeline, screen_rf, stack)
  
  stackz <- make_learner(Stack, cor_pipeline, rf_pipeline)
  
  logit_metalearner <- make_learner(Lrnr_solnp, metalearner_logistic_binomial,
                                    loss_squared_error)
  Q_learner <- make_learner(Lrnr_sl, stackz, logit_metalearner)
  g_learner <- make_learner(Lrnr_sl, stackz, logit_metalearner)
  learner_list <- list(Y = Q_learner, A = g_learner)
  
  data2 <- data.table::copy(data)
  # fit ATE
  # TODO: make confidence intervals bounded by range of data
  ate_spec <- tmle3::tmle_ATE(treatment_level = 1, control_level = 0)
  tmle_fit <- tmle3::tmle3(ate_spec, data2, node_list, learner_list)
  
  # format results
  ests <- tmle_fit$summary
  dataset_name <- stringr::str_remove(basename(dataset), ".csv")
  result <- list(dataset = dataset_name, ATE = ests$psi_transformed,
                 lb = ests$lower_transformed, ub = ests$upper_transformed)
  # Q_fit_cv_table = tmle_fit$likelihood$factor_list[["Y"]]$learner$cv_risk(loss_fun = loss_squared_error)
  # g_fit_cv_tabl = tmle_fit$likelihood$factor_list[["A"]]$learner$cv_risk(loss_fun = loss_squared_error)
  file_name = paste(i, "_BART",".RData")
  save(tmle_fit, file = file_name)
  print(i)
  print("done!")
}
