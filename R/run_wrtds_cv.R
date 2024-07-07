modelEstimationSimplified <- function(eList,windowY = 10, windowQ = 2, windowS = 0.5){
  eList <- setUpEstimation(eList = eList, 
                           windowY = windowY, windowQ = windowQ, windowS = windowS,
                           minNumObs = 10, minNumUncen = 10,
                           edgeAdjust = TRUE, verbose = TRUE)
  surfaces1 <- estSurfaces(eList, 
                           windowY = windowY, windowQ=windowQ, windowS=windowS,
                           minNumObs = 10, minNumUncen = 10, edgeAdjust = TRUE,
                           verbose = TRUE, run.parallel = FALSE)
  
  eList$surfaces <- surfaces1
  localDaily <- getSurfaceEstimates(eList, localsurfaces=NA, localDaily = NA)
  eList$Daily <- localDaily
  return(eList)
}


################################################################################
combinations <- data.frame(unique(combined_data_splits[[1]]%>%select(StationID,Par)))
combinations <- bind_rows(replicate(20, combinations, simplify = FALSE), .id = "Iteration")
combinations$Iteration <- as.numeric(combinations$Iteration)

numCores <- detectCores()-5
cl <- makeCluster(numCores)
registerDoParallel(cl)

completed <- list.files("./outputs/wrtds_cv_outputs/", full.names = FALSE)
remaining <- c()
for(i in 1:nrow(combinations)){
  if(!paste0(combinations$Iteration[i],"_",combinations$Par[i],"_",combinations$StationID[i],".csv") %in% completed){
    remaining <- c(remaining,i)
  }
}

wrtds_cv_outputs <- foreach(i = sample(remaining),.packages = c("tidyverse","EGRET")) %do% {
  #Get the data for iteration and all daily flows
  iteration <- as.numeric(combinations$Iteration[i])
  Sample_i <- combined_data_splits[[iteration]]
  Daily_i <- combined_data[["Daily"]]
  
  #Filter data for station i 
  station <- combinations$StationID[i]
  par <- combinations$Par[i]
  Sample <- Sample_i%>%
              filter(Par == par & StationID == station)
  Daily <- Daily_i%>%
              ungroup%>%
              filter(StationID == station)%>%
              filter(date %in% Sample$date)
           
  Sample_train <- Sample%>%
                    filter(fold == "train")
  
  #EGRET WRTDS
  train_data <- write_egret_list(Sample_train,Daily)
  preds <- modelEstimationSimplified(train_data,windowY = 10, windowQ = 2, windowS = 0.5)
  daily_out <- preds[["Daily"]]
  sample_out <- daily_out%>%
                  filter(Date %in% Sample$date)%>%
                  left_join(Sample%>%select(c("StationID","HUC12","Par","date","ConcAve","fold")),by = c("Date" = "date"))%>%
                  select("StationID","HUC12","Par","Date","Q","ConcAve","fold","ConcDay")%>%
                  rename(yHat = "ConcDay", date = "Date")
  
  # daily_out$StationID <- station
  # daily_out$Par <- par
  # daily_out$HUC12 <- unique(sample_out$HUC12)
  # daily_out <- daily_out%>%
  #                 select("StationID","HUC12","Par","Date","Q","ConcDay")%>%
  #                 rename(yHat = "ConcDay", date = "Date")
  
  #Write results in a list
  write_csv(sample_out, file = paste0("./outputs/wrtds_cv_outputs/",iteration,"_",par,"_",station,".csv"))
  wrtds_cv_outputs <- list(Iteration = iteration, StationID = station, Par = par, Sample = sample_out)
}

stopCluster(cl)

