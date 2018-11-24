library(tidyverse)
library(checkmate)
library(dplyr)
library(magrittr)
library(purrr)
library(forcats)

# Import full data from TresorIt ====

# binding *right* to *one* data frame does not work, because the underlying bind_rows (or sth) inside map_dfr will kill all spss attributes
rawdat <- purrr::map(
  .x = list(atizo = "atizo", applause = "applause", crowdguru = "crowdguru"),
  .f = function(x) {
    df <- haven::read_sav(
      file = fs::path("../../Tresors/Crowddaten/data/", x, ext = "sav"),
      user_na = FALSE  # turns out, the spss file does not actually include properly coded user-defined missings, so this won't help either way
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

# store full prompts
prompts <- tibble::tibble(var = character(), german = character())
# TODO this would be place to also add english prompts
# TODO however, that would require all the below factor levels etc. to *also* be translated, and we're avoiding that for now.

# tests, assertions, metadat  ====

crowddata$birth %<>% 
  as.integer(crowddata$birth) %>%
  na_if(y = -99) %>% 
  assert_integer(lower = 1900, upper = 2005, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "birth",
    german = "In welchem Jahr wurden Sie geboren?"
  )

crowddata$gender %<>%
  assert_factor(
    levels = c("Männlich", "Weiblich"), 
    ordered = FALSE, 
    empty.levels.ok = FALSE, 
    any.missing = TRUE, 
    all.missing = FALSE
  )
prompts %<>%
  add_row(
    var = "gender",
    german = "Sind Sie ..."
  )

crowddata$education %<>%
  as_factor(ordered = FALSE) %>%
  fct_recode(NULL = "0") %>%
  assert_factor(ordered = FALSE, n.levels = 6)
prompts %<>%
  add_row(
    var = "education",
    german = "Welchen Abschluss haben Sie gemacht?"
  )

crowddata$disability_care %<>%
  recode_factor(ja = TRUE, nein = FALSE) %>% 
  as.logical() %>% 
  assert_logical(any.missing = TRUE)
prompts %<>%
  add_row(
    var = "disability_care",
    german = "Gibt es in Ihrem Haushalt jemand, der aus Alters-oder Krankheitsgründen oder wegen einer Behinderung hilfe-oder pflegebedürftig ist?"
  )

crowddata$children %<>%
  recode_factor(Ja = TRUE, Nein = FALSE) %>% 
  as.logical() %>% 
  assert_logical(any.missing = TRUE)
prompts %<>%
  add_row(
    var = "children",
    german = "Gibt es in Ihrem Haushalt Kinder, die erst 2000 oder später geboren sind?"
  )

crowddata$employment %<>%
  as_factor(ordered = FALSE) %>%
  fct_recode(NULL = "0") %>% 
  # several levels are missing
  lvls_expand(new_levels = c(
    "Voll erwerbstätig",
    "In Teilzeitbeschäftigung",
    "In betrieblicher Ausbildung/Lehre/oder betrieblicher Umschulung",
    "Geringfügig oder unregelmäßig erwerbstätig",
    "In Altersteilzeit mit Arbeitszeit null",
    "Freiwilliger Wehrdienst",
    "Freiwilliges soziales/ökologisches Jahr, Bundesfreiwilligendienst",
    "Nicht erwerbstätig"
  )) %>% 
  assert_factor(ordered = FALSE, n.levels = 8)
prompts %<>%
  add_row(
    var = "employment",
    german = "Üben Sie derzeit eine Erwerbstätigkeit aus? Was trifft für Sie zu?"
  )

crowddata$sum_employer %<>%
  as.integer() %>% 
  replace(. > 50, NA) %>%  # these seem too high, probably in error
  replace(. == 0, NA) %>%  # these seem too low
  assert_integer(lower = 1)
prompts %<>%
  add_row(
    var = "sum_employer",
    german = "Bei wie vielen verschiedenen Arbeitgebern waren Sie seit dem Sie erstmals eine berufliche Tätigkeit aufgenommen haben beschäftigt, einschließlich Ihrer heutigen Beschäftigung? Phasen der Selbstständigkeit und der Beschäftigung bei einer Arbeitszeitfirma zählen wie ein Arbeitgeber."
  )

crowddata$profession_dev %<>%
  as_factor(ordered = FALSE) %>% 
  fct_recode(NULL = "0") %>% 
  assert_factor(n.levels = 3, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "profession_dev",
    german = "Wenn Sie Ihr ganzes Berufsleben betrachten, würden Sie sagen, Sie haben einen beruflichen Aufstieg, einen Abstieg, keine wesentliche Veränderung oder war das eher ein Auf und Ab?"
  )

crowddata$sum_platforms %<>%
  as.integer() %>% 
  na_if(y = 99) %>% # this was probably the NA value
  na_if(y = 17) %>%   # this just seems too high, typo perhaps
  # 0 was retained, because perhaps as per the question wording respondent wasn't *currently* working on the platform
  assert_integer(lower = 0)
prompts %<>%
  add_row(
    var = "sum_platforms",
    german = "Auf wie vielen Crowdworking-Plattformen arbeiten Sie zur Zeit?"
  )

crowddata$platforms %<>%
  as_factor(ordered = FALSE) %>% 
  assert_factor(n.levels = 5, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "platforms",
    german = "Auf welcher Art von Crowdworking-Plattformen sind Sie überwiegend tätig?"
  )

crowddata$h_month %<>% 
  as.integer() %>% 
  assert_integer(lower = 0, upper = 400)
prompts %<>%
  add_row(
    var = "h_month",
    german = "Wie viele Stunden im Monat arbeiten Sie auf Crowdworking-Plattformen insgesamt?"
  )

crowddata$h_platform %>% 
  as.integer() %>% 
  assert_integer(lower = 0, upper = 400)
prompts %<>%
  add_row(
    var = "h_platform",
    german = "Wie viele Stunden im Monat arbeiten Sie auf dieser Crowdworking-Plattform?"
  )

# TODO: Test that h_platform is >= h_month ?

crowddata$time_of_day %<>%
  as_factor(ordered = FALSE) %>% 
  assert_factor(n.levels = 6, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "time_of_day",
    german = "Zu welcher Tageszeit sind Sie als CrowdworkerIn tätig?"
  )

crowddata$time_of_week %<>%
  as_factor(ordered = FALSE) %>% 
  assert_factor(n.levels = 3, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "time_of_week",
    german = "Sind Sie überwiegend unter der Woche, am Wochenende oder die ganze Woche über auf Crowdworking-Plattformen tätig?"
  )

crowddata$workspace %<>%
  as_factor(ordered = FALSE) %>% 
  assert_factor(n.levels = 3, any.missing = TRUE)
prompts %<>%
  add_row(
    var = "workspace",
    german = "Von welchem Ort aus sind Sie auf Crowdworking-Plattformen tätig?"
  )

crowddata$perm_contract %<>%
  recode_factor(Ja = TRUE, Nein = FALSE) %>% 
  as.logical() %>% 
  assert_logical(any.missing = TRUE)
prompts %<>%
  add_row(
    var = "perm_contract",
    german = "Würden Sie Ihre aktuelle Crowdworking-Tätigkeit gerne im Rahmen einer unbefristeten Vollzeitstelle ausüben?"
  )
