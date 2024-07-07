#cv ML model
xgb_cv <- function(sample_data) {
  pars <- unique(sample_data$Par)
  hucs <- unique(substr(sample_data$HUC12, 1, 2))
  combinations <- expand.grid(Par = pars, HUC12 = hucs)
  numCores2 <- 9
  cl2 <- makeCluster(numCores2)
  registerDoParallel(cl2)
  output <- foreach(j = 1:nrow(combinations), .combine = rbind,.packages = c("tidyverse","foreach","xgboost")) %dopar% {
    par <- combinations$Par[j]
    huc <- combinations$HUC12[j]
    df <- sample_data%>%
      filter(Par == par & substr(HUC12, 1, 2) == huc)
    train_data <- df%>%
      filter(fold == "train")%>%
      select(-c("StationID", "HUC12", "fold", "date", "Par", "Q"))
    x <- as.matrix(train_data%>%
                     select(-"ConcAve"))
    y <- as.numeric(log(unlist(train_data %>% select(ConcAve))))
    
    best_param = list()
    best_seednumber = 42
    best_test_rmse = Inf
    best_test_rmse_index = 0
    
    for (iter in 1:20) {
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
    
    df$yHat <- exp(predict(model,newdata = as.matrix(df%>%select(-c("StationID", "HUC12", "fold", "date", "Par", "Q","ConcAve")))))
    df <- df%>%
      select("StationID","HUC12","Par","date","Q","ConcAve","fold","yHat")
    print(paste0(par, " ", huc, " complete"))
    df
  }
  return(output)
}

numCores <- 2
cl <- makeCluster(numCores)
registerDoParallel(cl)
ML_cv_output <- foreach(i = 1:length(combined_data_splits), .packages = c("tidyverse","xgboost","foreach","parallel","doParallel")) %dopar% {
  result <- xgb_cv(combined_data_splits[[i]])
  print(paste("Iteration", i, "complete"))
  return(result)
}
stopCluster(cl)

rm(cl)


#Bias correction################################################################
ML_cv_output_bias_corrected <- ML_cv_output
for(i in 1:length(ML_cv_output_bias_corrected)){
  df <- ML_cv_output_bias_corrected[[i]]
  df_out <- data.frame()
  for(j in unique(df$Par)){
    for(huc in unique(substr(df$HUC12,1,2))){
      df2 <- df%>%filter(Par == j & substr(HUC12,1,2) == huc)
      df3 <- df2%>%filter(fold == "train")
      model <- lm(log(df3$ConcAve)~log(df3$yHat))
      m_roe <- cov(log(df3$yHat),log(df3$ConcAve))/var(log(df3$yHat))
      b_roe <- mean(log(df3$ConcAve)) - m_roe*mean(log(df3$yHat))
      df2$yHat <- log(df2$yHat)*m_roe + b_roe
      df3$yHat <- log(df3$yHat)*m_roe + b_roe
      df2$yHat <- exp(df2$yHat)*mean(exp((df3$yHat) - log(df3$ConcAve)))
      df_out <- rbind(df_out, df2)
    }
  }
  ML_cv_output_bias_corrected[[i]] <- df_out
}

rm(i,df,huc,j,df_out,df2,df3,m_roe,b_roe,model)
