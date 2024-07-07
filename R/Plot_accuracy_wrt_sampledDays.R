accuracy_df <- rbind(accuracy_loadest,accuracy_ML_bias_corrected,accuracy_WRTDS)
accuracy_df <- accuracy_df%>%
  mutate(Par = ifelse(Par == "SED", "TSS", Par))

accuracy_df <- accuracy_df%>%
  select(c("Par","StationID","train_NSE","test_NSE","Model"))%>%
  rename("Train" = "train_NSE", "Test" = "test_NSE")%>%
  pivot_longer(-c("StationID","Par","Model"),values_to = "NSE", names_to = "type")%>%
  mutate(Model = case_when(
    Model == "XGB_bias_corrected" ~ "XGB",
    TRUE ~ Model))
accuracy_df$type <- factor(accuracy_df$type, levels = c("Train","Test"))
df <- combined_data_splits[[1]]
df <- df%>%
  group_by(Par,StationID)%>%
  summarise(n = n())
df$Par[df$Par == "SED"] <- "TSS"
df <- accuracy_df%>%
  left_join(df, by = c("Par","StationID"))
df2 <- df%>%filter(type == "Test")
df2 <- df2%>%
  mutate(value_range = cut(n, breaks = c(-Inf, 50, 100, 200, Inf), 
                           labels = c("20-50", "50-100", "100-200", ">200")))

summary_df <- df2%>%
  group_by(Model, value_range, Par) %>%
  summarise(count = sum(NSE > 0.3)*100/n(), .groups = 'drop') %>%
  complete(Model, value_range, Par, fill = list(count = 0.1))%>%
  filter(!is.na(value_range))

summary_df$Model <- factor(summary_df$Model, level = c("LOADEST","WRTDS","XGB"))

png("outputs/performance_vs_samples.png", height = 6,width = 6.5, res = 300, units = "in")
ggplot(summary_df, aes(x = value_range, y = count, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~Par) +
  labs(x = "Number of samples per site", y = "Percent of sites with validation NSE > 0.3") +
  scale_fill_manual(values = c("LOADEST" = "#1b9e77", "WRTDS" = 'darkgrey', "XGB" = "#7570b3")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.title = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )+
  ylim(0,85)
dev.off()

rm(summary_df,df,df2,accuracy_df)
