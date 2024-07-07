df_top10 <- xgb_importance %>%
  mutate(region_par = paste(Region,Par))%>%
  group_by(Region, Par) %>%
  top_n(10, mean_abs_shap) %>%
  ungroup() %>%
  arrange(Region, Par, desc(mean_abs_shap))

df_top10$Region <- factor(df_top10$Region, levels = c("01","02","03"))
df_top10$Par <- factor(df_top10$Par, levels = c("TN","TP","TSS"))
df_top10$variable <- as.character(df_top10$variable)
df_top10$variable[df_top10$variable %in% variables_replace] <- names(variables_replace)[match(df_top10$variable, variables_replace)]
p <- ggplot(df_top10, aes(x = mean_abs_shap, y = reorder_within(variable, mean_abs_shap,region_par))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(Region~Par, scales = "free")+
  theme_minimal() +
  scale_y_reordered()+
  theme(strip.text = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.position = "none"
  )+
  labs(x = "Variable Importance (mean(|SHAP values|))") +
  theme(text = element_text(size = 8))+
  scale_x_continuous(labels = function(x) format(x, digits = 1, scientific = FALSE))

p
png("./outputs/importance.png", height = 7, width = 6.4, res = 300, units = "in")
print(p)
dev.off()

rm(df_top10,p)
