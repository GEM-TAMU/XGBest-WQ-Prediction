get_days_in_month <- function(year, month) {
  date <- make_date(year, month, 1)
  days_in_month(date)
}

#Create a lookup table to identify model number in list of complete models
xgb_model_lookup <- data.frame()
for(i in 1:length(xgb_models)){
  df <- data.frame(Par = xgb_models[[i]]$Par, HUC = xgb_models[[i]]$huc, Model = i)
  xgb_model_lookup <- rbind(xgb_model_lookup,df)
}

all_flows <- all_data[["flows"]]%>%
  select("StationID","date","Q")%>%
  filter(year(date) < 2021 & year(date) > 1995)

for(i in 1:nrow(xgb_model_lookup)){
  par <- xgb_models[[i]]$Par
  huc <- xgb_models[[i]]$huc
  model <- xgb_models[[i]]$model
  
  par2 <- par
  if(par2 == "TSS"){
    par2 <- "SED"
  }
  
  if (!dir.exists(paste0("./outputs/XGB_monthly_fluxes/","R",huc,"_MLWQ_monthly/"))) {
    dir.create(paste0("./outputs/XGB_monthly_fluxes/","R",huc,"_MLWQ_monthly/"))
  }
  if (!dir.exists(paste0("./outputs/XGB_monthly_fluxes/","R",huc,"_MLWQ_monthly/","/",par2))) {
    dir.create(paste0("./outputs/XGB_monthly_fluxes/","R",huc,"_MLWQ_monthly/",par2))
  }
  train <- combined_data[["Sample"]]%>%filter(Par == par & substr(HUC12,1,2) == huc)
  train$yHat <- exp(predict(model,newdata = as.matrix(train%>%select(-c("HUC12","StationID", "date", "Q","ConcAve","Par")))))
  roe_model <- lm(log(train$ConcAve)~log(train$yHat))
  m_roe <- cov(log(train$yHat),log(train$ConcAve))/var(log(train$yHat))
  b_roe <- mean(log(train$ConcAve)) - m_roe*mean(log(train$yHat))
  train$yHat <- log(train$yHat)*m_roe + b_roe
  multiply_factor <- mean(exp((train$yHat) - log(train$ConcAve)))
  train$yHat <- exp(train$yHat)*multiply_factor
  
  for(station in unique(train$StationID)){
    train_station <- train%>%
      filter(StationID %in% station)
    
    daily <- combined_data[["Daily"]]%>%filter(StationID %in% station)%>%ungroup()
    HUC12 <- unique(daily$HUC12)
    daily$yHat <- exp(predict(model,newdata = as.matrix(daily%>%select(-c("StationID", "HUC12", "date", "Q")))))
    daily$yHat <- log(daily$yHat)*m_roe + b_roe
    daily$yHat <- exp(daily$yHat)*multiply_factor
    
    daily <- daily%>%
      select(c("StationID","date","yHat"))
    flows <- all_flows%>%filter(StationID %in% station)  #As daily df only contains flows > 0 
    flows <- flows%>%left_join(daily, by = c("StationID","date"))
    
    flows$yHat[flows$Q == 0] <- 0  #MAKE CONCENTRATION EQUAL TO ZERO WHERE FLOW = 0 
    
    if(par %in% c("TN","TP")){
      flows$flux_day <- flows$Q*flows$yHat*2.44657     #kg/day    
    }else if(par %in% c("TSS")){
      flows$flux_day <-  flows$Q*flows$yHat*2.44657*10^-3     #metric tons/day
    }
    
    flux_monthly <- flows%>%
      mutate(year = year(date), month = month(date))%>%
      group_by(year,month)%>%
      summarise(num_obs = n(), mean_load = mean(flux_day))
    flux_monthly <- flux_monthly[order(flux_monthly$year, flux_monthly$month), ]
    flux_monthly$mean_load = round(flux_monthly$mean_load,6)
    if(max(flux_monthly$year) - min(flux_monthly$year) > 10){          #if number of years > 10 then use 25 days atleast per month otherwise 20
      flux_monthly <- flux_monthly%>%
                            filter(num_obs >=25)
    } else if(max(flux_monthly$year) - min(flux_monthly$year) <= 10){
      flux_monthly <- flux_monthly%>%
                          filter(num_obs >=20)
    }
    
    #Monthly loads
    flux_monthly$mean_load <- flux_monthly$mean_load * as.numeric(get_days_in_month(flux_monthly$year, flux_monthly$month)) 
    
    
    if(nrow(flux_monthly) >= 10){           #At least 10 months of data
      file_name <- paste("outputs/XGB_monthly_fluxes/R",huc,"_MLWQ_monthly/",par2,"/",par2,"_", station,"_XGB",
                         "_trKGE_",round(accuracy_ML_bias_corrected$train_KGE[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par]*100,0),
                         "_trNSE_",round(accuracy_ML_bias_corrected$train_NSE[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par]*100,0),
                         "_trPBIAS_",round(accuracy_ML_bias_corrected$train_pbias[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par],1),
                         "_testKGE_",round(accuracy_ML_bias_corrected$test_KGE[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par]*100,0),
                         "_testNSE_",round(accuracy_ML_bias_corrected$test_NSE[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par]*100,0),
                         "_testPBIAS_",round(accuracy_ML_bias_corrected$test_pbias[accuracy_ML_bias_corrected$StationID == station & accuracy_ML_bias_corrected$Par == par],1),
                         "_fKGE_",round(KGE(train_station$yHat,train_station$ConcAve)*100,0),
                         "_fNSE_",round(NSE(train_station$yHat,train_station$ConcAve)*100,0),
                         "_fPBIAS_",round(pbias(train_station$yHat*train_station$Q,train_station$ConcAve*train_station$Q),1),
                         ".txt", sep = "")
      write.table(flux_monthly, file_name, sep = " ",quote = FALSE,row.names = FALSE)
    }
  }
  
}

rm(b_roe,huc,HUC12,i,file_name,m_roe,multiply_factor,par,station,train,train_station,xgb_model_lookup,model,roe_model,daily,flows,flux_monthly,all_flows)

