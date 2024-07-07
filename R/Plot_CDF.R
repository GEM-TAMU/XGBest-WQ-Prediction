accuracy_df <- rbind(accuracy_loadest,accuracy_WRTDS,accuracy_ML_bias_corrected)
accuracy_df <- accuracy_df%>%
  mutate(Par = ifelse(Par == "SED", "TSS", Par))

accuracy_df <- accuracy_df%>%
  select(c("Par","StationID","train_NSE","test_NSE","Model"))%>%
  rename("Train" = "train_NSE", "Validation" = "test_NSE")%>%
  pivot_longer(-c("StationID","Par","Model"),values_to = "NSE", names_to = "type")%>%
  mutate(Model = case_when(
    Model == "XGB_bias_corrected" ~ "XGB",
    TRUE ~ Model))
accuracy_df$type <- factor(accuracy_df$type, levels = c("Train","Validation"))

#CDF############################################################################
png(filename = "./outputs/cdf.png", height = 5,width = 6.5, res = 300, units = "in")
ggplot()+
  stat_ecdf(data = accuracy_df,aes(x = NSE, color = Model, linetype = Model),geom = "step", size = 0.4)+
  coord_cartesian(xlim = c(0,1))+
  facet_grid(type~Par)+
  scale_x_continuous( expand = c(0, 0)) +
  xlab("NSE")+
  ylab("CDF")+
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(colour = "black", fill = "white"),
        legend.background = element_blank(),
        panel.spacing = unit(1.5, "lines"))
dev.off()

rm(accuracy_df)
