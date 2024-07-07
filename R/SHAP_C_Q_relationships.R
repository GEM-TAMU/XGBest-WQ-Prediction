c_q_modalities <- data.frame()
for(par in names(shap_values_for_predictions)){
  df <- shap_values_for_predictions[[par]]
  df_actual <- accuracy_ML%>%filter(Par == par)
  for(huc in names(df)){
    df2 <- df[[huc]]
    for(i in 1:length(df2)){
      df3 <- df2[[i]]
      station <- unique(df3$StationID)
      if(station %in% df_actual$StationID){
        flows <- all_data[["flows"]]%>%filter(StationID == station)
        median_flow <- log(median(flows$Q))
        
        c_up <- df3%>%filter(rfvalue >= median_flow)
        c_down <- df3%>%filter(rfvalue <= median_flow)
        lm_c_up <- lm(value~rfvalue, data = c_up)
        lm_c_down <- lm(value~rfvalue, data = c_down)
        cor_test_c_up <- cor.test(c_up$value,c_up$rfvalue)
        cor_test_c_down <- cor.test(c_down$value,c_down$rfvalue)
        df4 <- data.frame(StationID = station, Par = par, huc = huc,median_flow = median_flow, 
                          a_up = as.numeric(lm_c_up$coefficients[1]), b_up = as.numeric(lm_c_up$coefficients[2]),
                          a_down = as.numeric(lm_c_down$coefficients[1]), b_down = as.numeric(lm_c_down$coefficients[2]),
                          pvalue_up = as.numeric(cor_test_c_up$p.value), pvalue_down = as.numeric(cor_test_c_down$p.value))
        
        if(df4$pvalue_up[1] < 0.05 & df4$b_up[1] > 0){
          df4$mod_up <- "Inc"
        }
        if(df4$pvalue_up[1] < 0.05 & df4$b_up[1] < 0){
          df4$mod_up <- "Dec"
        }
        if(df4$pvalue_up[1] >= 0.05){
          df4$mod_up <- "Flat"
        }
        if(df4$pvalue_down[1] < 0.05 & df4$b_down[1] > 0){
          df4$mod_down <- "Inc"
        }
        if(df4$pvalue_down[1] < 0.05 & df4$b_down[1] < 0){
          df4$mod_down <- "Dec"
        }
        if(df4$pvalue_down[1] >= 0.05){
          df4$mod_down <- "Flat"
        }
        c_q_modalities <- rbind(c_q_modalities, df4)
        
        png(paste0("./outputs/modalities/SHAP_",station,"_",par,"_",df4$mod_down[1],"_",df4$mod_up[1],".png"),height = 4, width = 4, res = 300, units = "in")
        print(ggplot(df3)+
          geom_point(aes(x = rfvalue, y = value), size = 0.5, color = "darkgrey")+
          geom_vline(xintercept = median_flow, linetype="longdash",
                     color = "blue", size=.5)+
          geom_segment(aes(x = median_flow, y = predict(lm_c_up, newdata = data.frame(rfvalue = median_flow)),
                           xend = max(rfvalue), yend = predict(lm_c_up, newdata = data.frame(rfvalue = max(rfvalue)))),
                       color = "black", linetype = "solid",size = 1) +
          geom_segment(aes(x = median_flow, y = predict(lm_c_down, newdata = data.frame(rfvalue = median_flow)),
                           xend = min(rfvalue), yend = predict(lm_c_down, newdata = data.frame(rfvalue = min(rfvalue)))),
                       color = "black", linetype = "solid", size = 1) +
          theme_bw()+
          xlab("ln(Q)")+
          ylab("SHAP value for ln(Q)")+
          labs(subtitle = paste0("USGS - 0",station))+
          theme(
            plot.title = element_text(color = "black"),
            plot.subtitle = element_text(color = "black", size = 12),
            axis.title.x = element_text(color = "black", size = 12),
            axis.title.y = element_text(color = "black", size = 12),
            axis.text = element_text(color = "black", size = 12),
            legend.title = element_text(color = "black"),
            legend.text = element_text(color = "black")
          ))
        
        dev.off()
      }
    }
  }
}

#Combine
c_q_modalities$modality <- paste0(c_q_modalities$mod_down,"-",c_q_modalities$mod_up)

df <- c_q_modalities %>%
  group_by(Par, modality) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(Par) %>%
  mutate(percentage = n / sum(n) * 100)

#Number of daily samples
df2 <- combined_data$Daily
df2 <- df2%>%
        group_by(StationID)%>%
        summarise(days = n())
c_q_modalities <- c_q_modalities%>%left_join(df2, by = "StationID")
c_q_modalities2 <- c_q_modalities%>%filter(days > 365*5)
