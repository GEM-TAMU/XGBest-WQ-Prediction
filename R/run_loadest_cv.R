loadest_cv_outputs <- list()
loadest_best_models <- data.frame()
for(i in 1:20){
  output <- data.frame()
  data_i <- combined_data_splits[[i]]
  for(par in unique(data_i$Par)){
    data_i_Par <- data_i%>%filter(Par == par)
    for(station in unique(data_i_Par$StationID)){
      data <- data_i_Par%>%filter(StationID == station)
      train <- data%>%filter(fold == "train")
      tryCatch({
        model <- rloadest::selBestModel("ConcAve", data = data.frame(train), flow = "Q",dates = "date", conc.units="mg/L")
        loadest_best_models <- rbind(loadest_best_models,data.frame(StationID = station,Par = par, iteration = i,Model = model$model.no))
        pred <- rloadest::predConc(model,data)
        out <- data%>%
          select("StationID","HUC12","Par","date","Q","ConcAve","fold")%>%
          left_join(pred%>%select("Date","Conc"), join_by("date"=="Date"))%>%
          rename(yHat = Conc)
        output <- rbind(output,out)
      }, error = function(e){
        print(paste("station", station, "error"))
      })
      print(paste(station,"complete"))
    }
  }
  print(paste("iteration", i, "complete"))
  loadest_cv_outputs[[i]] <- output
}

rm(i,data_i,data_i_Par,out,output,model, pred, train,data,par,station,out,pred)
