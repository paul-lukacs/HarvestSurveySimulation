
summary_stats <- dplyr::select(all_ests, scen, resp_bias, resp_rate, mean_SE, MARE, RRMSE)

write.csv(summary_stats, file = "sim_summary_stats.csv", row.names = F)


bybias <- summary_stats %>% 
  group_by(scen, resp_bias) %>% 
  summarise(SE = mean(mean_SE))

write.csv(bybias, file = "bybias.csv", row.names = F)
