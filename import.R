library(tidyverse)
library(checkmate)
library(dplyr)
library(magrittr)
library(purrr)

# Import full data from TresorIt ====

# binding *right* to *one* data frame does not work, because the underlying bind_rows (or sth) inside map_dfr will kill all spss attributes
rawdat <- purrr::map(
  .x = list(atizo = "atizo", applause = "applause", crowdguru = "crowdguru"),
  .f = function(x) {
    df <- haven::read_sav(
      file = fs::path("../../Tresors/Crowddaten/data/", x, ext = "sav"),
      user_na = FALSE
    )
    
    # the last 6 cols have unique (hashes?) column names, to avoid confusion we delete them
    # they seem to measure some time spent *between* the different survey pages, not very important to keep
    df[(ncol(df) - 6):ncol(df)] <- NULL
      
    # Sadly, the below columns are string inputs in the survey, and include some *strings* instead of integers ("four" instead of 4 etc.).
    # These offending cells have been fixed by hand in excel, and commited in the below csv.
    # It appeared easier to do this in excel than with individual subsetting edits.
    df[, c("v_35", "v_38", "v_45", "v_44")] <- readr::read_delim(
      file = fs::path("data/manual_corrections/", x, ext = "csv"), 
      delim = ";",
      col_names = TRUE,
      col_types = c("iiii")
    )
    df
  }
)

# there are no meaningful short variable names available anywhere in the raw data, so we came up with these for easier handling.
# first, we need to test that all the separate studies use the same variable names
map_dfr(.x = rawdat, .f = names) %>% 
  pmap_lgl(
    .f = function(atizo, applause, crowdguru) {
      length(unique(c(atizo, applause,crowdguru))) == 1
    }
  ) %>% 
  assert_true(x = all(.), na.ok = FALSE)

# re-write spss attributes into proper R factors
rawdat <- map(.x = rawdat, .f = haven::as_factor, only_labeled = TRUE)

# bind everything into a df
# notice: because there are plenty of inconsistencies between the factor levels of the three studies, binding causes a lot of warnings and factors are coerced to character vectors
crowddata <- suppressWarnings(dplyr::bind_rows(rawdat, .id = "study"))

# test some variables before they get thrown out, just to be sure
# this was probably used to mark test-runs, of which there appear none left in the data
assert_subset(x = as.character(crowddata$tester), choices = "Kein Tester")
# this was probably the state of the user session
assert_subset(x = as.character(x = crowddata$dispcode), choices = c("Beendet (31)", "Beendet nach Unterbrechung (32)"))
# this was apparently the time to completion
# there are some -1s in here, which does not make sense, but the data seems otherwise ok, so this should be NA
crowddata[crowddata$duration < 0, "duration"] <- NA
assert_integerish(
  x = crowddata$duration,
  lower = 1,
  upper = 20000,
  any.missing = TRUE
)
# all other non-questionnaire variables at the end are uninteresting or empty

crowddata %<>%
  dplyr::select(c(
    study = study,
    birth = v_1,
    gender = v_2,
    education = v_7,
    disability_care = v_12,
    children = v_17,
    employment = v_22,
    sum_employer = v_35,
    profession_dev = v_37,
    sum_platforms = v_38,
    platforms = v_39,
    h_month = v_45,
    h_platform = v_44,
    time_of_day = v_46,
    time_of_week = v_52,
    workspace = v_53,
    perm_contract = v_203,
    interesting_work = v_175,
    learning = v_176,
    cooperation = v_177,
    support = v_178,
    expectations = v_179,
    codecide = v_180,
    autonomy = v_181,
    deadline = v_182,
    quantity = v_183,
    evaluation = v_184,
    transparent_eval = v_185,
    planning = v_186,
    enough_time = v_187,
    training = v_188,
    credit_chef = v_189,
    credit_col = v_190,
    fair = v_191,
    adequate = v_192,
    safe_job = v_193,
    balance_loc = v_194,
    balance_time = v_195,
    separation = v_196,
    transparent_tasks = v_197,
    influence_eval = v_198,
    consistent_eval = v_199,
    dissent_eval = v_200,
    wage = v_201,
    wage_organisation = v_202,
    interesting_work_cw = v_119,
    learning_cw = v_120,
    cooperation_cw = v_121,
    support_cw = v_122,
    expectations_cw = v_123,
    codecide_cw = v_124,
    autonomy_cw = v_125,
    deadline_cw = v_126,
    quantity_cw = v_127,
    evaluation_cw = v_128,
    transparent_eval_cw = v_129,
    planning_cw = v_130,
    enough_time_cw = v_131,
    training_cw = v_132,
    credit_chef_cw = v_133,
    credit_col_cw = v_134,
    fair_cw = v_135,
    adequate_cw = v_136,
    safe_job_cw = v_137,
    balance_loc_cw = v_138,
    balance_time_cw = v_139,
    separation_cw = v_140,
    transparent_tasks_cw = v_141,
    influence_eval_cw = v_142,
    consistent_eval_cw = v_143,
    dissent_eval_cw = v_144,
    wage_cw = v_145,
    wage_organisation_cw = v_146
  ))

# tests and assertions

crowddata$study %<>% as_factor(ordered = FALSE)

# this is how we currently reconstruct attributes, we just trust that atizo is correct
# this actually only reconstructs *one* attribute, only prompt
for (cur_col in names(crowddata)) {
  new_prompt <- attributes(x =  rawdat$atizo[[cur_col]])$label
  attr(x = crowddata[[cur_col]], which = "prompt") <- new_prompt
}

crowddata$birth %<>% 
  as.integer(crowddata$birth) # %>%
  # TODO this looses the above attributes again
  # assert_integer(lower = 1900, upper = 2005, any.missing = TRUE)
# TODO pipe below if possible
attr(x = crowddata$birth, which = "prompt") <- "In welchem Jahr wurden Sie geboren?"

assert_factor(x = crowddata$gender, levels = c("Männlich", "Weiblich"), ordered = FALSE, empty.levels.ok = FALSE, any.missing = TRUE, all.missing = FALSE)
attr(x = crowddata$gender, which = "prompt") <- "Sind Sie..."

crowddata$education %<>%
  as_factor(ordered = FALSE) %>%
  fct_recode(NULL = "0") %>%
  assert_factor(ordered = FALSE, n.levels = 6) %>%
  fct_explicit_na(na_level = "(Keine Angabe)")
  # TODO use purrr at the end to mark all factors wth fct_explicit_nas

# crowddata[15:73][crowddata[15:73] == 0] <- NA