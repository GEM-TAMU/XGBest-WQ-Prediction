accuracy_loadest <- check_accuracy(loadest_cv_outputs)
accuracy_loadest$Model <- "LOADEST"
summary_loadest <- summarize_accuracy(accuracy_loadest)
summary_loadest$Model <- "LOADEST"

accuracy_ML <- check_accuracy(ML_cv_output)
summary_ML <- summarize_accuracy(accuracy_ML)
accuracy_ML$Model <- "XGB"
summary_ML$Model <- "XGB"

accuracy_ML_bias_corrected <- check_accuracy(ML_cv_output_bias_corrected)
summary_ML_bias_corrected <- summarize_accuracy(accuracy_ML_bias_corrected)
accuracy_ML_bias_corrected$Model <- "XGB_bias_corrected"
summary_ML_bias_corrected$Model <- "XGB_bias_corrected"

accuracy_WRTDS <- check_accuracy(WRTDS_cv_output)
summary_WRTDS <- summarize_accuracy(accuracy_WRTDS)
accuracy_WRTDS$Model <- "WRTDS"
summary_WRTDS$Model <- "WRTDS"

summary_table <- rbind(summary_loadest,summary_WRTDS,summary_ML,summary_ML_bias_corrected)
summary_table$Par <- factor(summary_table$Par, levels = c("TP","TN","TSS"))
summary_table <- summary_table[order(summary_table$Par),]

rm(summary_loadest,summary_ML,summary_ML_bias_corrected,summary_WRTDS)

summary_table <- summary_table%>%
                    filter(Model %in% c("LOADEST", "WRTDS","XGB_bias_corrected"))%>%
                    select(Par,test_NSE_gt_0.3, test_KGE_gt_0.3,test_pbias_25)

write.table(summary_table, "./outputs/summary_table.csv")
