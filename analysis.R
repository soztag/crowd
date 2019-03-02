plot_battery <- function(m, long) {
  # name long y labels, so they can be used as named vectors to replace existing short labels in the below
  long %>% 
    set_names(value = colnames(crowddata$exp_work)) %>% 
    stringr::str_wrap(width = 65) %>% 
    {.} -> long
  
  # munge data
  m %>% 
    as_tibble() %>% 
    add_column(study = crowddata$study) %>% 
    gather(key = "item", value = "score", -study) %>% 
    ggplot(
      mapping = aes(
        x = as.factor(score), 
        y = item, 
        fill = study
      )
    ) +
    stat_bin_2d(mapping = aes(alpha = stat(count))) + 
    stat_bin_2d(geom = "text", mapping = aes(label = ..count..)) + 
    facet_wrap(vars(study), nrow = 1) + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "bottom"
    ) +
    xlab("Agreement with Statement") +
    ylab(label = NULL) +
    scale_fill_discrete(name = "Platforms") + 
    guides(fill = FALSE) + 
    scale_y_discrete(labels = long)
}