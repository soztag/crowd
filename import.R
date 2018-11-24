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


# 1 Welcome ====

# Sehr geehrte Damen und Herren,
# wir freuen uns, dass Sie an der Befragung "Crowdworking - Ansprüche an Arbeit" im Rahmen eines Foschungsprojektes des Lehrstuhls für Soziologie der Universität Hohenheim teilnehmen. Mit dieser Studie wollen wir in Erfahrung bringen, welche Ansprüche, Einstellungen und Aspirationen Sie zum einen an Erwerbsarbeit und zum anderen an Crowdarbeit haben. Den Fragebogen auszufüllen wird etwa 20 Minuten in Anspruch nehmen.
# Die Befragung ist anonym. Es werden keine personenbezogenen Daten erhoben, anhand derer Sie als Person indentifiziert werden können. Sämtliche Daten werden ausschließlich im Rahmen des Forschungsprojekts verwendet.
# Wir bedanken uns für Ihre Unterstützung!


crowddata %>% mutate(
  # 2 Personal Information ====
  study = study,
  birth = {
    as.integer(v_1) %>%
      na_if(y = -99) %>% 
      assert_integer(lower = 1900, upper = 2005, any.missing = TRUE)
  },
  gender = {
    assert_factor(
      x = v_2,
      levels = c("Männlich", "Weiblich"), 
      ordered = FALSE, 
      empty.levels.ok = FALSE, 
      any.missing = TRUE, 
      all.missing = FALSE
    )
  },
  education = {
    as_factor(x = v_7, ordered = FALSE) %>%
      fct_recode(NULL = "0") %>%
      assert_factor(ordered = FALSE, n.levels = 6)
  },
  disability_care = {
    recode_factor(.x = v_12, ja = TRUE, nein = FALSE) %>% 
      as.logical() %>% 
      assert_logical(any.missing = TRUE)
  },
  children = {
    recode_factor(.x = v_17, Ja = TRUE, Nein = FALSE) %>% 
      as.logical() %>% 
      assert_logical(any.missing = TRUE)
  },
  employment = {
    as_factor(x = v_22, ordered = FALSE) %>%
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
  },
  sum_employer = {
    as.integer(v_35) %>% 
      replace(. > 50, NA) %>%  # these seem too high, probably in error
      replace(. == 0, NA) %>%  # these seem too low
      assert_integer(lower = 1)
  },
  profession_dev = {
    as_factor(x = v_37, ordered = FALSE) %>% 
      fct_recode(NULL = "0") %>% 
      assert_factor(n.levels = 3, any.missing = TRUE)
  }
) %>% 
  
  # 3 current job ====
  mutate(
    sum_platforms = {
      as.integer(v_38) %>% 
        na_if(y = 99) %>% # this was probably the NA value
        na_if(y = 17) %>%   # this just seems too high, typo perhaps
        # 0 was retained, because perhaps as per the question wording respondent wasn't *currently* working on the platform
        assert_integer(lower = 0)
    },  
    platforms = {
      as_factor(x = v_39, ordered = FALSE) %>% 
        assert_factor(n.levels = 5, any.missing = TRUE)
    },
    h_month = {
      as.integer(v_45) %>% 
        assert_integer(lower = 0, upper = 400)
    },
    h_platform = {
      as.integer(v_44) %>% 
        assert_integer(lower = 0, upper = 400)
    },
    time_of_day = {
      as_factor(x = v_46, ordered = FALSE) %>% 
        assert_factor(n.levels = 6, any.missing = TRUE)
    },
    time_of_week = {
      as_factor(x = v_52, ordered = FALSE) %>% 
        assert_factor(n.levels = 3, any.missing = TRUE)
    },
    workspace = {
      as_factor(x = v_53, ordered = FALSE) %>% 
        assert_factor(n.levels = 3, any.missing = TRUE)
    },
    perm_contract = {
      recode_factor(.x = v_203, Ja = TRUE, Nein = FALSE) %>% 
        as.logical() %>% 
        assert_logical(any.missing = TRUE)
    }
  ) %>%
  # h_platform must, by definition, be less than h_month, but it is not for these
  mutate_at(
    .vars = c("h_month", "h_platform"),
    .funs = funs(
      if_else(
        condition = h_platform <= h_month,
        true = .,
        false = NA_integer_
      )
    )
  ) %>% 
  
  # 4 Expectations Work ====
  mutate(
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
  ) %>% 
  
  # 5 Expectations Crowdwork ====
  mutate(
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
  ) %>% 
  {.} -> crowddata

# store full quest
quest <- tibble::tibble(
  var = character(), 
  var_german = character(),
  section = character(),
  section_intro_german = character()
)
# TODO this would be place to also add english quest
# TODO however, that would require all the below factor levels etc. to *also* be translated, and we're avoiding that for now.

quest %<>%
  add_row(
    var = "birth",
    var_german = "In welchem Jahr wurden Sie geboren?"
  )

quest %<>%
  add_row(
    var = "gender",
    var_german = "Sind Sie ..."
  )

quest %<>%
  add_row(
    var = "education",
    var_german = "Welchen Abschluss haben Sie gemacht?"
  )

quest %<>%
  add_row(
    var = "disability_care",
    var_german = "Gibt es in Ihrem Haushalt jemand, der aus Alters-oder Krankheitsgründen oder wegen einer Behinderung hilfe-oder pflegebedürftig ist?"
  )

quest %<>%
  add_row(
    var = "children",
    var_german = "Gibt es in Ihrem Haushalt Kinder, die erst 2000 oder später geboren sind?"
  )

quest %<>%
  add_row(
    var = "employment",
    var_german = "Üben Sie derzeit eine Erwerbstätigkeit aus? Was trifft für Sie zu?"
  )

quest %<>%
  add_row(
    var = "sum_employer",
    var_german = "Bei wie vielen verschiedenen Arbeitgebern waren Sie seit dem Sie erstmals eine berufliche Tätigkeit aufgenommen haben beschäftigt, einschließlich Ihrer heutigen Beschäftigung? Phasen der Selbstständigkeit und der Beschäftigung bei einer Arbeitszeitfirma zählen wie ein Arbeitgeber."
  )

quest %<>%
  add_row(
    var = "profession_dev",
    var_german = "Wenn Sie Ihr ganzes Berufsleben betrachten, würden Sie sagen, Sie haben einen beruflichen Aufstieg, einen Abstieg, keine wesentliche Veränderung oder war das eher ein Auf und Ab?"
  )

quest[1:8, c("section")] <- "personal_info"
quest[1:8, c("section_intro_german")] <- "Zunächst würden wir gerne einige Fragen zu Ihrer Person und zu Ihrem bisherigen Berufsleben stellen."


quest %<>%
  add_row(
    var = "sum_platforms",
    var_german = "Auf wie vielen Crowdworking-Plattformen arbeiten Sie zur Zeit?"
  )

quest %<>%
  add_row(
    var = "platforms",
    var_german = "Auf welcher Art von Crowdworking-Plattformen sind Sie überwiegend tätig?"
  )

quest %<>%
  add_row(
    var = "h_month",
    var_german = "Wie viele Stunden im Monat arbeiten Sie auf Crowdworking-Plattformen insgesamt?"
  )

quest %<>%
  add_row(
    var = "h_platform",
    var_german = "Wie viele Stunden im Monat arbeiten Sie auf dieser Crowdworking-Plattform?"
    # the wording is actually slightly inconsistent for this one; some questionnaires ask for, say "Atizo" by name, others just refer to "this platform".
  )

quest %<>%
  add_row(
    var = "time_of_day",
    var_german = "Zu welcher Tageszeit sind Sie als CrowdworkerIn tätig?"
  )

quest %<>%
  add_row(
    var = "time_of_week",
    var_german = "Sind Sie überwiegend unter der Woche, am Wochenende oder die ganze Woche über auf Crowdworking-Plattformen tätig?"
  )

quest %<>%
  add_row(
    var = "workspace",
    var_german = "Von welchem Ort aus sind Sie auf Crowdworking-Plattformen tätig?"
  )

quest %<>%
  add_row(
    var = "perm_contract",
    var_german = "Würden Sie Ihre aktuelle Crowdworking-Tätigkeit gerne im Rahmen einer unbefristeten Vollzeitstelle ausüben?"
  )

quest[9:16, c("section")] <- "current_occ"
quest[9:16, c("section_intro_german")] <- "Nun würden wir gerne etwas über Ihre aktuelle Tätigkeit als CrowdworkerIn erfahren."


quest %<>%
  add_row(
    var = names(crowddata)[18:45],
    section = "expect_work",
    section_intro_german = "Bitte denken Sie an Ihre aktuelle Erwerbstätigkeit. Sollten Sie zur Zeit keiner Erwerbstätigkeit nachgehen, so denken Sie bitte an Ihren letzten Job. Sollten Sie bisher noch nicht als ArbeitnehmerIn tätig gewesen sein, so stellen Sie sich bitte Ihren künftigen Job vor. Die folgenden Fragen beziehen sich ausschließlich auf diese Tätigkeit und fokussieren Ihre subjektiven Erwartungen an Arbeit. \n In meiner Erwerbsarbeit ist es mir wichtig, dass... \n Die Stärke Ihrer Zustimmung erfolgt auf einer 6-stufigen Skala."
  )
quest[quest$section == "expect_work", "var_german"] <- rawdat$atizo %>% 
  select(v_175:v_202) %>% 
  map_chr(.f = function(x) {
    attr(x = x, which = "label")
  })


quest %>%
  add_row(
    var = names(crowddata)[46:(46+27)],
    section = "expect_crowd",
    section_intro_german = "Bitte denken Sie nun an Ihre Aktivität auf Internet-Plattformen. Die folgenden Fragen beziehen sich ausschließlich (!) auf Ihre Tätigkeit als CrowdworkerIn. Dabei interessiert uns wieder, wie wichtig Ihnen folgende Ausssagen zu Ansprüchen an Crowdarbeit sind. \n Bei meiner Tätigketit als CrowdwokerIn ist es mir wichtig, dass..."
  ) %>% tail()
# quest[quest$section == "expect_crowd", "var_german"] <- 
# rawdat$atizo %>% 
#   select(v_119:v_146) %>% 
#   ncol(.)
# 
#   map_chr(.f = function(x) {
#     attr(x = x, which = "label")
#   })
