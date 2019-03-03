plot_battery <- function(m, items = NULL, condition = "expect_crowd", lang = "en", width = 80) {
  # name long y labels, so they can be used as named vectors to replace existing short labels in the below
  if (is.null(items)) {
    items <- colnames(m)
  }
  
  if (lang == "de") {
    # take german short titles in this case
    shorts <- quest %>% 
      dplyr::filter(section == "expect_crowd") %>% 
      dplyr::filter(var %in% items) %>% 
      pull(short_german)
  } else {
    shorts <- items
  }
  
  quest %>% 
    dplyr::filter(section == condition) %>% 
    dplyr::filter(var %in% items) %>% 
    pull(var_german) %>% 
    stringr::str_wrap(width = width) %>%
    paste0(shorts, ":\n", .) %>%
    set_names(value = items) %>% 
    {.} -> long
  
  # munge data
  m[, items] %>% 
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
