plot_battery <- function(m, items = NULL, condition = "expect_crowd", lang = "en", width = 80, diff = FALSE, concepts = NULL) {
  # name long y labels, so they can be used as named vectors to replace existing short labels in the below
  if (is.null(items)) {
    items <- colnames(m)
  }
  
  quest %>% 
    dplyr::filter(section == condition) %>% 
    dplyr::filter(var %in% items) %>% 
    dplyr::mutate(var_german = stringr::str_wrap(string = var_german, width = width)) %>% 
    {.} -> df
  long <- glue::glue_data(
    .x = df, 
    "...{var_german}", 
    "({short_german}/{var})", 
    .sep = "\n"
  )
  long <- rlang::set_names(x = long, nm = df$var)
  
  # munge data
  m[, items] %>% 
    as_tibble() %>% 
    add_column(study = crowddata$study) %>% 
    gather(key = "item", value = "score", -study) %>% 
    {.} -> m
  if (!is.null(concepts)) {
    concepts <- reshape2::melt(interesting)  # helpful to melt existing list
    colnames(concepts) <- c("item", "concept")
    concepts$item <- as.character(concepts$item)
    m <- dplyr::inner_join(x = m, y = concepts, by = "item")
  } else {
    # ugly hack
    m$concept <- m$study
  }
  
  # plotting
  g <- ggplot(
    data = m,
    mapping = aes(
      x = as.factor(score), 
      y = item,
      fill = concept
    )
  )
  g <- g + stat_bin_2d(mapping = aes(alpha = stat(count)))
  g <- g + stat_bin_2d(geom = "text", mapping = aes(label = ..count..))
  g <- g + facet_wrap(vars(study), nrow = 1)
  g <- g + theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  )
  g <- g + scale_y_discrete(labels = long, limits = items)
  g <- g + ylab(NULL)
  
  if (is.null(concepts)) {
    # color by platform, but no guide
    g <- g + scale_fill_discrete(name = "Platforms") + guides(fill = FALSE) 
  } else {
    g <- g + scale_fill_discrete(name = "Dimension der Leistungsgerechtigkeit")
  }
  
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