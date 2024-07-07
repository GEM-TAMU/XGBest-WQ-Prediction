#1.Get routing HUC12 subwatersheds##############################################
get_routing_subwatersheds <- function(huc12,routing_table) {
  current <- huc12
  all_upstream <- c()
  while (TRUE) {
    upstream <- routing_table %>%
      filter(InletHUC %in% current) %>%
      pull(OutletHUC)
    all_upstream <- c(all_upstream, current)
    if (length(upstream) == 0) {
      break
    }
    current <- upstream
  }
  return(unique(all_upstream))
}

#2. Remove Outliers#############################################################
rm_outliers <- function(Sample,columns = c("logQ","ConcAve","Tot_L")){
  variables_for_mahalanobis <- Sample[, columns]
  mean_cov <- cov(variables_for_mahalanobis)
  mahalanobis_dist <- mahalanobis(variables_for_mahalanobis, center = colMeans(variables_for_mahalanobis), cov = mean_cov)
  threshold <- quantile(mahalanobis_dist, 0.999)
  return(Sample[-which(mahalanobis_dist > threshold),])
}

#3. Train test split############################################################
split_data <- function(df) {
  set.seed(NULL)  # Reset the seed for each call to ensure randomness
  df %>%
    group_by(StationID, Par) %>%
    mutate(fold = sample(c(rep("train", round(0.8 * n())), rep("test", n() - round(0.8 * n()))))) %>%
    ungroup()
}

#4. ############################################################################
r2 <- function(sim, obs) {
  mean_obs <- mean(obs)
  mean_sim <- mean(sim)
  numerator <- sum((obs - mean_obs) * (sim - mean_sim))^2
  denominator <- sum((obs - mean_obs)^2) * sum((sim - mean_sim)^2)
  r_squared <- numerator / denominator
  return(r_squared)
}

#5. ############################################################################
check_accuracy <- function(data){
  data_i <- do.call(rbind,data)
  output <- data.frame(StationID = character(),par = character(),
                       train_NSE = numeric(), test_NSE = numeric(),
                       train_pbias = numeric(), test_pbias = numeric(),
                       train_KGE = numeric(), test_KGE = numeric(),
                       train_r2 = numeric(), test_r2 = numeric(),
                       train_mae = numeric(), test_mae = numeric())
  for(par in unique(data_i$Par)){
    data_i_Par <- data_i%>%filter(Par == par)
    for(station in unique(data_i_Par$StationID)) {
      df <- data_i%>%filter(Par == par & StationID == station)
      df_train <- df%>%
        filter(fold == "train")
      df_test <- df%>%
        filter(fold == "test")
      results <- data.frame(StationID = as.character(station),Par = as.character(par), 
                            train_NSE = NSE(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_NSE = NSE(sim = df_test$yHat, obs = df_test$ConcAve),
                            train_pbias = pbias(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_pbias = pbias(sim = df_test$yHat, obs = df_test$ConcAve),
                            train_KGE = KGE(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_KGE = KGE(sim = df_test$yHat, obs = df_test$ConcAve),
                            train_r2 = r2(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_r2 = r2(sim = df_test$yHat, obs = df_test$ConcAve),
                            train_mae = mae(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_mae = mae(sim = df_test$yHat, obs = df_test$ConcAve),
                            train_rmse = rmse(sim = df_train$yHat, obs = df_train$ConcAve),
                            test_rmse = rmse(sim = df_test$yHat, obs = df_test$ConcAve))
      output <- rbind(output,results)
    }
  }
  return(output)
}

#6. Summarize accuracy##########################################################
summarize_accuracy <- function(data){
  output <- data%>%
    group_by(Par)%>%
    summarize(Total = n(),
              test_NSE_gt_0.3 = sum(test_NSE > 0.3, na.rm = TRUE),
              test_KGE_gt_0.3 = sum(test_KGE > 0.3, na.rm = TRUE),
              test_pbias_15 = sum(abs(test_pbias) < 15, na.rm = TRUE),
              test_r2_gt_0.3 = sum(test_r2 > 0.3, na.rm = TRUE),
              combined_kge_pbias = sum(test_KGE > 0.3 & abs(test_pbias) <= 15, na.rm =TRUE))
  return(output)
}

#7.Write EGRET format files#####################################################
write_egret_list <- function(Sample, Daily){
  if(!file.exists("temp")){
    dir.create("temp")
  }
  INFO  <- Sample[1,]%>%
    select("StationID", "HUC12", "bfi","Tot_L","Par")
  Sample_new <- Sample%>%
    mutate(remark = "")%>%
    select("date","remark","ConcAve")%>%
    mutate(date = as.character(format(date,"%m/%d/%Y")))
  write.csv(Sample_new,paste0("./temp/sample_temp",INFO$StationID,".txt"),row.names = FALSE)
  Sample_new <- readUserSample(filePath = "./temp",fileName = paste0("sample_temp",INFO$StationID,".txt"))
  Sample_new <- Sample_new%>%
    left_join(Sample%>%select(date,fold), join_by("Date" == "date"))
  Daily <- Daily%>%
    select("date","Q")%>%
    mutate(date = as.character(format(date,"%m/%d/%Y")))
  write.csv(Daily,paste0("./temp/daily_temp",INFO$StationID,".txt"),row.names = FALSE)
  Daily <- readUserDaily(filePath = "./temp", fileName = paste0("daily_temp",INFO$StationID,".txt"))
  eList <- mergeReport(INFO,Daily,Sample_new)
  return(eList)
}

#7. Read WRTDS outputs##########################################################
read_wrtds_outputs <- function(csv_file){
  df <- read.csv(csv_file)
  parts <- unlist(strsplit(basename(csv_file), "_"))
  iteration <- as.numeric(parts[1])
  parameter <- parts[2]
  station_id <- as.numeric(sub("\\.csv$", "", parts[3]))
  df$Iteration <- iteration
  df$Par <- parameter
  df$StationID <- station_id
  return(df)
}
