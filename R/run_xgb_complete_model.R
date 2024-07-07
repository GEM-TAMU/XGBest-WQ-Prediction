generate_xgb_models <- function(sample_data) {
  pars <- unique(sample_data$Par)
  hucs <- unique(substr(sample_data$HUC12, 1, 2))
  combinations <- expand.grid(Par = pars, HUC12 = hucs)
  
  cl <- makeCluster(10)
  registerDoParallel(cl)
  
  output <- foreach(i = 1:nrow(combinations),.packages = c("tidyverse","xgboost","foreach","SHAPforxgboost")) %dopar% {
    par <- combinations$Par[i]
    huc <- combinations$HUC12[i]
    train_data <- sample_data%>%
      filter(Par == par & substr(HUC12, 1, 2) == huc)
    
    x <- as.matrix(train_data%>%
                     select(-c("StationID", "HUC12", "date", "Par", "Q","ConcAve")))
    y <- as.numeric(log(unlist(train_data %>% select(ConcAve))))
    
    best_param = list()
    best_seednumber = 42
    best_test_rmse = Inf
    best_test_rmse_index = 0
    
    for (iter in 1:50) {
      param <- list(objective = "reg:squarederror",
                    max_depth = sample(5:20, 1),
                    eta = runif(1, .01, .3),
                    gamma = runif(1, 0.0, 0.2), 
                    subsample = runif(1, .6, .9),
                    colsample_bytree = runif(1, .5, .8), 
                    min_child_weight = sample(1:10, 1),
                    max_delta_step = sample(1:10, 1)
      )
      cv.nround = 500
      cv.nfold = 5
      seed.number = sample.int(10000, 1)[[1]]
      set.seed(seed.number)
      mdcv <- xgb.cv(data = x, label = y, params = param, nthread=6, 
                     nfold=cv.nfold, nrounds=cv.nround,
                     verbose = F, early_stop_round=20, 
                     maximize=FALSE)
      min_test_rmse = min(mdcv$evaluation_log$test_rmse_mean)
      min_test_rmse_index = which.min(mdcv$evaluation_log$test_rmse_mean)
      
      if (min_test_rmse < best_test_rmse) {
        best_test_rmse = min_test_rmse
        best_test_rmse_index = min_test_rmse_index
        best_seednumber = seed.number
        best_param = param
      }
      print(iter)
    }
    
    nround = best_test_rmse_index
    set.seed(best_seednumber)
    
    model <- xgboost(data = x, label = y,
                     nround = nround,
                     params = best_param,
                     nthread=6)
    shap_long <- shap.prep(xgb_model = model, X_train = as.matrix(x))
    
    list(Par = par, huc = huc, model = model,shap_long = shap_long)
  }
  stopCluster(cl)
  return(output)
}

################################################################################
#Run models#####################################################################
xgb_models <- generate_xgb_models(combined_data[["Sample"]]) 

#Variable Importance############################################################

#XBG SHAP importance #Not run
xgb_importance <- data.frame()
for(i in 1:length(xgb_models)){
  par <- xgb_models[[i]]$Par
  huc <- xgb_models[[i]]$huc
  model <- xgb_models[[i]]$shap_long
  importance_matrix <- shap.importance(model)
  importance_matrix$Par <- par
  importance_matrix$Region <- huc 
  xgb_importance <- rbind(xgb_importance, importance_matrix)
}

rm(i,par,huc,model,importance_matrix)

#Final hyperparameters of XGB model#############################################
xgb_hyperparameters_final <- data.frame()
for(i in 1:9){
  model <- xgb_models[[i]]$model
  df <- as.data.frame(model$params)
  df$Par <- xgb_models[[i]]$Par
  df$HUC02 <- xgb_models[[i]]$huc
  xgb_hyperparameters_final <- rbind(xgb_hyperparameters_final,df)
}

xgb_hyperparameters_final <- xgb_hyperparameters_final%>%
  select(Par, HUC02, max_depth, eta, gamma, subsample,colsample_bytree,min_child_weight,max_delta_step)
write.csv(xgb_hyperparameters_final,"./outputs/xgb_hyperparameters.csv")

#SHAP for all predictions#######################################################
shap_values_for_predictions <- list("TN" = list(),"TP" = list(), "TSS" = list())
for(i in 1:length(xgb_models)){
  par <- xgb_models[[i]]$Par
  huc <- xgb_models[[i]]$huc
  shap_values_for_predictions[[par]][[paste0(huc)]] <- list()
}

numCores <- detectCores()
cl <- makeCluster(numCores-5)
registerDoParallel(cl)

for(i in 1:length(xgb_models)){
  par <- xgb_models[[i]]$Par
  huc <- xgb_models[[i]]$huc
  model <- xgb_models[[i]][["model"]]
  df_train <- combined_data[["Daily"]]%>%
                  filter(substr(HUC12,1,2) == huc)
  stations <- unique(combined_data[["Sample"]]%>%filter(Par == par & substr(HUC12,1,2) == huc))
  stations <- unique(stations$StationID)
  shap_values_for_predictions[[par]][[huc]] <- foreach(j = stations, .packages = c('tidyverse', 'xgboost', 'SHAPforxgboost')) %dopar% {
    df <- df_train%>%
              filter(StationID == j)%>%
              ungroup()%>%
              select(-c("StationID", "HUC12", "date", "Q"))
    shap_long <- shap.prep(xgb_model = model, X_train = as.matrix(df))
    shap_long <- shap_long%>%filter(variable %in% c("logQ"))
    shap_long$StationID <- j
    return(shap_long)
  }
}
stopCluster()

rm(i,par,huc,numCores,cl,df,df_train,stations)
