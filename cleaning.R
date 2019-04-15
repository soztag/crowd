rawdat <- readr::read_rds(path = "data/rawdat.rds")

# bind everything into a df
# notice: because there are plenty of inconsistencies between the factor levels of the three studies, binding causes a lot of warnings and factors are coerced to character vectors
crowddata <- suppressWarnings(dplyr::bind_rows(rawdat, .id = "study"))

# 1 Welcome ====

# Sehr geehrte Damen und Herren,
# wir freuen uns, dass Sie an der Befragung "Crowdworking - Ansprüche an Arbeit" im Rahmen eines Foschungsprojektes des Lehrstuhls für Soziologie der Universität Hohenheim teilnehmen. Mit dieser Studie wollen wir in Erfahrung bringen, welche Ansprüche, Einstellungen und Aspirationen Sie zum einen an Erwerbsarbeit und zum anderen an Crowdarbeit haben. Den Fragebogen auszufüllen wird etwa 20 Minuten in Anspruch nehmen.
# Die Befragung ist anonym. Es werden keine personenbezogenen Daten erhoben, anhand derer Sie als Person indentifiziert werden können. Sämtliche Daten werden ausschließlich im Rahmen des Forschungsprojekts verwendet.
# Wir bedanken uns für Ihre Unterstützung!


crowddata %>% mutate(
  # 2 Personal Information ====
  study = study,
  birth = {
    as.integer(birth) %>%
      na_if(y = -99) %>% 
      assert_integer(lower = 1900, upper = 2005, any.missing = TRUE)
  },
  gender = {
    assert_factor(
      x = gender,
      levels = c("Männlich", "Weiblich"), 
      ordered = FALSE, 
      empty.levels.ok = FALSE, 
      any.missing = TRUE, 
      all.missing = FALSE
    )
  },
  education = {
    as_factor(x = education, ordered = FALSE) %>%
      fct_recode(NULL = "0") %>%
      assert_factor(ordered = FALSE, n.levels = 6)
  },
  disability_care = {
    recode_factor(.x = disability_care, ja = TRUE, nein = FALSE) %>% 
      as.logical() %>% 
      assert_logical(any.missing = TRUE)
  },
  children = {
    recode_factor(.x = children, Ja = TRUE, Nein = FALSE) %>% 
      as.logical() %>% 
      assert_logical(any.missing = TRUE)
  },
  employment = {
    as_factor(x = employment, ordered = FALSE) %>%
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
    as.integer(sum_employer) %>% 
      replace(. > 50, NA) %>%  # these seem too high, probably in error
      replace(. == 0, NA) %>%  # these seem too low
      assert_integer(lower = 1)
  },
  profession_dev = {
    as_factor(x = profession_dev, ordered = FALSE) %>% 
      fct_recode(NULL = "0") %>% 
      assert_factor(n.levels = 3, any.missing = TRUE)
  }
) %>% 
  
  # 3 current job ====
mutate(
  sum_platforms = {
    as.integer(sum_platforms) %>% 
      na_if(y = 99) %>% # this was probably the NA value
      na_if(y = 17) %>%   # this just seems too high, typo perhaps
      # 0 was retained, because perhaps as per the question wording respondent wasn't *currently* working on the platform
      assert_integer(lower = 0)
  },  
  platforms = {
    as_factor(x = platforms, ordered = FALSE) %>% 
      assert_factor(n.levels = 5, any.missing = TRUE)
  },
  h_month = {
    as.integer(h_month) %>% 
      assert_integer(lower = 0, upper = 400)
  },
  h_platform = {
    as.integer(h_platform) %>% 
      assert_integer(lower = 0, upper = 400)
  },
  time_of_day = {
    as_factor(x = time_of_day, ordered = FALSE) %>% 
      assert_factor(n.levels = 6, any.missing = TRUE)
  },
  time_of_week = {
    as_factor(x = time_of_week, ordered = FALSE) %>% 
      assert_factor(n.levels = 3, any.missing = TRUE)
  },
  workspace = {
    as_factor(x = workspace, ordered = FALSE) %>% 
      assert_factor(n.levels = 3, any.missing = TRUE)
  },
  perm_contract = {
    recode_factor(.x = perm_contract, Ja = TRUE, Nein = FALSE) %>% 
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
  {.} -> crowddata
  
# 4 Expectations Work ====
crowddata$exp_work <- as.matrix(crowddata[, names(crowddata)[18:45]])
crowddata %<>% select(
  -c(18:45)
)
crowddata$exp_work <- apply(
  X = crowddata$exp_work,
  MARGIN = c(1,2),
  FUN = function(x) {
    switch(
      EXPR = x,
      `Trifft gar nicht zu` = -2,
      `Trifft wenig zu` = -1,
      `Teils. teils` = 0,
      `Trifft ziemlich zu` = 1,
      `Trifft völlig zu` = 2,
      `0` = NA,
      `Kann ich nicht beurteilen` = NA,
      `NA` = NA
    )
  }
)

# 5 Expectations Crowdwork ====
crowddata$exp_crowd <- as.matrix(crowddata[, names(crowddata)[18:45]])
# dont be surprised; above indices are the same because in the meantime, we have deleted the first battery, so the indices changed!
crowddata %<>% select(
  -c(18:45)
)
crowddata$exp_crowd <- apply(
  X = crowddata$exp_crowd,
  MARGIN = c(1,2),
  FUN = function(x) {
    switch(
      EXPR = x,
      `Trifft gar nicht zu` = -2,
      `Trifft wenig zu` = -1,
      # careful here; below is typed slightly differently then above version!
      `Teils, teils` = 0,
      `Trifft ziemlich zu` = 1,
      `Trifft völlig zu` = 2,
      `0` = NA,
      `Kann ich nicht beurteilen` = NA,
      `NA` = NA
    )
  }
)
colnames(crowddata$exp_crowd) <- colnames(crowddata$exp_work)


# store full questionnaire ====

# TODO this would be place to also add english quest
# TODO however, that would require all the below factor levels etc. to *also* be translated, and we're avoiding that for now.
tibble::tibble(
  var = character(), 
  var_german = character(),
  var_english = character(),
  short_german = character(),
  section = character(),
  section_intro_german = character(),
  section_intro_english = character()
) %>%  

  # 2 Personal Information
  add_row(
    var = "birth",
    var_german = "In welchem Jahr wurden Sie geboren?",
    var_english = "In what year were you born?"
  ) %>% 
  add_row(
    var = "gender",
    var_german = "Sind Sie ...",
    var_english = "What is your gender?"
  ) %>% 
  add_row(
    var = "education",
    var_german = "Welchen Abschluss haben Sie gemacht?",
    var_english = "What is your highest educational degree?"
  ) %>% 
  add_row(
    var = "disability_care",
    var_german = "Gibt es in Ihrem Haushalt jemand, der aus Alters-oder Krankheitsgründen oder wegen einer Behinderung hilfe-oder pflegebedürftig ist?",
    var_english = "Is there someone in your houshold who needs special care due to age, illness or disability?"
  ) %>% 
  add_row(
    var = "children",
    var_german = "Gibt es in Ihrem Haushalt Kinder, die erst 2000 oder später geboren sind?",
    var_english = "Are ther children in your household born in the year 2000 or later?"
  ) %>% 
  add_row(
    var = "employment",
    var_german = "Üben Sie derzeit eine Erwerbstätigkeit aus? Was trifft für Sie zu?",
    var_english = "Are you currently employed?"
  ) %>% 
  add_row(
    var = "sum_employer",
    var_german = "Bei wie vielen verschiedenen Arbeitgebern waren Sie seit dem Sie erstmals eine berufliche Tätigkeit aufgenommen haben beschäftigt, einschließlich Ihrer heutigen Beschäftigung? Phasen der Selbstständigkeit und der Beschäftigung bei einer Arbeitszeitfirma zählen wie ein Arbeitgeber.",
    var_english = "How many employers have you had, including your current one? Periods if self-employment or working for a temp agency count as one employer."
  ) %>% 
  add_row(
    var = "profession_dev",
    var_german = "Wenn Sie Ihr ganzes Berufsleben betrachten, würden Sie sagen, Sie haben einen beruflichen Aufstieg, einen Abstieg, keine wesentliche Veränderung oder war das eher ein Auf und Ab?",
    var_english = "Considering your entire career, would you say that you have move up the ladder, down, no change or where there ups and downs?"
  ) %>% 
  {.} -> quest
quest[1:8, c("section")] <- "personal_info"
quest[1:8, c("section_intro_german")] <- "Zunächst würden wir gerne einige Fragen zu Ihrer Person und zu Ihrem bisherigen Berufsleben stellen."
quest[1:8, c("section_intro_english")] <- "Please answer some questions about your career."

# 3 Current Job
quest %>% 
  add_row(
    var = "sum_platforms",
    var_german = "Auf wie vielen Crowdworking-Plattformen arbeiten Sie zur Zeit?",
    var_english = "How many crowdworking-platforms are you working on at this time?"
  ) %>% 
  add_row(
    var = "platforms",
    var_german = "Auf welcher Art von Crowdworking-Plattformen sind Sie überwiegend tätig?",
    var_english = "What kinds of crowdworking-platforms are you using primarily?"
  ) %>% 
  add_row(
    var = "h_month",
    var_german = "Wie viele Stunden im Monat arbeiten Sie auf Crowdworking-Plattformen insgesamt?",
    var_english = "How many hours a month are you working on crowdworking-platforms?"
  ) %>% 
  add_row(
    var = "h_platform",
    var_german = "Wie viele Stunden im Monat arbeiten Sie auf dieser Crowdworking-Plattform?",
    var_english = "how many hours per month are you working on this crowdworking platform?"
    # the wording is actually slightly inconsistent for this one; some questionnaires ask for, say "Atizo" by name, others just refer to "this platform".
  ) %>% 
  add_row(
    var = "time_of_day",
    var_german = "Zu welcher Tageszeit sind Sie als CrowdworkerIn tätig?",
    var_english = "At what time of day do you do crowdwork?"
  ) %>% 
  add_row(
    var = "time_of_week",
    var_german = "Sind Sie überwiegend unter der Woche, am Wochenende oder die ganze Woche über auf Crowdworking-Plattformen tätig?",
    var_english = "Are you primarily working during the week, on the weekend or the entire week for the crowdworking platform?"
  ) %>% 
  add_row(
    var = "workspace",
    var_german = "Von welchem Ort aus sind Sie auf Crowdworking-Plattformen tätig?",
    var_english = "From where do you work on the crowdworking platforms?"
  ) %>% 
  add_row(
    var = "perm_contract",
    var_german = "Würden Sie Ihre aktuelle Crowdworking-Tätigkeit gerne im Rahmen einer unbefristeten Vollzeitstelle ausüben?",
    var_english = "Would you like to do your current crowdworking job as a full-time job?"
  ) %>% 
  {.} -> quest
quest[9:16, c("section")] <- "current_occ"
quest[9:16, c("section_intro_german")] <- "Nun würden wir gerne etwas über Ihre aktuelle Tätigkeit als CrowdworkerIn erfahren."
quest[9:16, c("section_intro_english")] <- "Please answer some questions about your current job as a crowdworker."


# 4 Expectations Work
quest %<>%
  add_row(
    var = colnames(crowddata$exp_work),
    short_german = c(
      "interessante_arbeit",
      "lernen",
      "kooperation",
      "unterstuetzung",
      "erwartungen",
      "mitbestimmung",
      "autonomie",
      "zeitpunkt",
      "arbeitsmenge",
      "kriterien_einfluss",
      "kriterien_transparent",
      "realistische_planung",
      "genug_zeit",
      "ausbildung",
      "anerkennung_chef",
      "anerkennung_kollegen",
      "gerecht",
      "angemessen",
      "sicherheit",
      "anpassen_ort",
      "anpassen_zeit",
      "trennung",
      "klare_aufgaben",
      "bewertung_einfluss",
      "bewertung_konsistent",
      "bewertung_widerspruch",
      "entlohnung_angemessen",
      "entlohnung_leistung"
    ),
    section = "expect_work",
    section_intro_german = "Bitte denken Sie an Ihre aktuelle Erwerbstätigkeit. Sollten Sie zur Zeit keiner Erwerbstätigkeit nachgehen, so denken Sie bitte an Ihren letzten Job. Sollten Sie bisher noch nicht als ArbeitnehmerIn tätig gewesen sein, so stellen Sie sich bitte Ihren künftigen Job vor. Die folgenden Fragen beziehen sich ausschließlich auf diese Tätigkeit und fokussieren Ihre subjektiven Erwartungen an Arbeit. \n In meiner Erwerbsarbeit ist es mir wichtig, dass... \n Die Stärke Ihrer Zustimmung erfolgt auf einer 6-stufigen Skala."
  )
quest[quest$section == "expect_work", "var_german"] <- rawdat$atizo %>% 
  select(interesting_work:wage_organisation) %>% 
  map_chr(.f = function(x) {
    attr(x = x, which = "label")
  })

# 5 Expectations Crowdwork
quest %<>%
  add_row(
    var = colnames(crowddata$exp_work),
    short_german = as_vector(quest[quest$section == "expect_work", "short_german"]),
    section = "expect_crowd",
    section_intro_german = "Bitte denken Sie nun an Ihre Aktivität auf Internet-Plattformen. Die folgenden Fragen beziehen sich ausschließlich (!) auf Ihre Tätigkeit als CrowdworkerIn. Dabei interessiert uns wieder, wie wichtig Ihnen folgende Ausssagen zu Ansprüchen an Crowdarbeit sind. \n Bei meiner Tätigketit als CrowdwokerIn ist es mir wichtig, dass..."
  )
quest[quest$section == "expect_crowd", "var_german"] <- rawdat$atizo %>%
  select(interesting_work_cw:wage_organisation_cw) %>% 
  map_chr(.f = function(x) {
    attr(x = x, which = "label")
  })

# make sure that the above typed vars are the same as in crowddata
assert_set_equal(x = names(crowddata)[2:17], y = quest$var[1:16], ordered = TRUE)
