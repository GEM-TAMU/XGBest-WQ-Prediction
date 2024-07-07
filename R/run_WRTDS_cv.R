#1. WRTDS cross validation #####################################################
WRTDS_cv <- function(station_data){
  sample <- station_data[["Sample"]]
  daily <- station_data[["Daily"]]
  daily <- daily%>%filter(Date %in% sample$Date)
  sample <- sample%>%
    filter(fold == "train")
  station_train <- station_data
  station_train[["Sample"]] <- sample
  station_train <- mergeReport(station_data$INFO, daily,sample)
  model <- modelEstimation_simplified(station_train)
  sample <- station_data[["Sample"]]
  pred <- sample%>%
    select("Date","fold","Q","ConcAve")%>%
    left_join(model[["Daily"]]%>%select("Date","ConcDay"), by = "Date")%>%
    rename("yHat" = "ConcDay")%>%
    mutate(StationID = station_data[["INFO"]]$StationID,
           HUC12 = station_data[["INFO"]]$HUC12,
           Par = station_data[["INFO"]]$Par)
  return(pred)
}

#Run############################################################################

combinations <- data.frame(unique(combined_data_splits[[1]]%>%select(StationID,Par)))
combinations <- bind_rows(replicate(20, combinations, simplify = FALSE), .id = "Iteration")
combinations$Iteration <- as.numeric(combinations$Iteration)

for(j in 1:1000){
  tryCatch({
    completed <- list.files("./outputs/WRTDS/", full.names = FALSE)
    remaining <- c()
    for(i in 1:nrow(combinations)){
      if(!paste0(combinations$Iteration[i],"_",combinations$Par[i],"_",combinations$StationID[i],".csv") %in% completed){
        remaining <- c(remaining,i)
      }
    }
    numCores <- 10
    cl <- makeCluster(numCores)
    registerDoParallel(cl)
    output <- foreach(i = sample(remaining), .combine = rbind,.packages = c("tidyverse","EGRET")) %dopar% {
      iteration <- as.numeric(combinations$Iteration[i])
      Sample_i <- combined_data_splits[[iteration]]
      Daily_i <- combined_data[["Daily"]]
      Sample <- Sample_i%>%
        filter(Par == combinations$Par[i] & StationID == combinations$StationID[i])
      Daily <- Daily_i%>%
        ungroup%>%
        filter(StationID == combinations$StationID[i])
      
      df <- write_egret_list(Sample,Daily)
      tryCatch({
        preds <- WRTDS_cv(df)
        station <- df[["INFO"]]$StationID
        par <- df[["INFO"]]$Par
        write_csv(preds, file = paste0("./outputs/WRTDS/",iteration,"_",par,"_",station,".csv"))
      }, error = function(e){
      })
    }
    Sys.sleep(30)
  }, error = function(e){
    print(paste("trying again"))
  })
}

#Read outputs
output_folder <- "./outputs/WRTDS/"
csv_files <- list.files(output_folder,pattern = "\\.csv$",full.names = TRUE)
combined_df <- bind_rows(lapply(csv_files, read_wrtds_outputs))
WRTDS_cv_output <- split(combined_df,combined_df$Iteration)

rm(remaining,i,j,numCores,cl,combinations,completed,output_folder,csv_files,combined_df)
