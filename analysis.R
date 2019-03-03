plot_battery <- function(m, items = NULL, condition = "expect_crowd", lang = "en", width = 80, diff = FALSE) {
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
    ylab(label = NULL) +
    scale_fill_discrete(name = "Platforms") + 
    guides(fill = FALSE) + 
    scale_y_discrete(labels = long) %>% 
    {.} -> g
  
  if (lang == "de") {
    g <- g + labs(alpha = "Anzahl der Antworten")
  } else {
    g <- g + labs(alpha = "Count")
  }
  
  if (diff) {
    g <- g + labs(
      subtitle = "In meiner Erwerbsarbeit / In meiner Tätigkeit als CrowdworkerIn ist es mir wichtig, dass ..."
      # subtitle = as_vector(quest[quest$section == "expect_crowd", "section_intro_german"])[1]
    )
    if (lang == "de") {
      g <- g + xlab("Unterschied in der Zustimmung")
      
    } else {
      g <- g + xlab("Difference in Agreement")
    }
  } else {
    g <- g + labs(
      subtitle = "Bei meiner Tätigkeit als CrowdworkerIn ist es mir wichtig, dass ..."
      # subtitle = as_vector(quest[quest$section == "expect_crowd", "section_intro_german"])[1]
    )
    g <- g + xlab(NULL)
    g <- g + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    if (lang == "de") {
      g <- g + scale_x_discrete(labels = c(
        "Trifft gar nicht zu",
        "Trifft wenig zu",
        "Teils, teils",
        "Trifft ziemlich zu",
        "Trifft völlig zu"
      ))
    } else {
      g <- g + scale_x_discrete(labels = c(
        "Strongly disagree",
        "Disagree",
        "Undecided",
        "Agree",
        "Strongly agree"
      ))
    }
  }
  
  g
}

get_item <- function(item) {
  assert_choice(x = item, choices = colnames(crowddata$exp_crowd))
  quest %>% 
    dplyr::filter(section == "expect_crowd") %>% 
    dplyr::filter(var == item) %>% 
    pull(short_german) %>% 
    purrr::as_vector() %>% 
    {.} -> handle_de
  quest %>% 
    dplyr::filter(section == "expect_crowd") %>% 
    dplyr::filter(var == item) %>% 
    pull(var_german) %>% 
    purrr::as_vector() %>% 
    {.} -> wording_de
  
  
  glue::glue("`{handle_de}`^[*'Bei meiner Tätigkeit ist es mir wichtig, dass {wording_de}'*]")
}

get_items <- function(items) {
  purrr::map_chr(.x = items, .f = get_item) %>% 
    glue::glue_collapse(sep = ", ", last = " und ")
}