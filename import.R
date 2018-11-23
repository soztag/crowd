library(tidyverse)
library(checkmate)
library(haven)
library(dplyr)
library(magrittr)
library(purrr)

# Import full data from TresorIt
rawdat <- NULL
rawdat$atizo <- read_sav("../../Tresors/Crowddaten/data/atizo.sav",
                  user_na = FALSE)
# TODO: haven, NAs (see -99 als NA, row 191) VK: What??
# Sadly, the survey had some free input fields. I changed the columns manually in excel.
# TODO add apply function (or something for tibbles?) to check data
rawdat$atizo[, c("v_35", "v_38", "v_45", "v_44")] <- read_csv2("data/atizoc.csv", col_names = TRUE)
rawdat$atizo %<>% 
  select(c(birth = v_1,
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

rawdat$applause <- read_sav("../../Tresors/crowd/data/applause.sav",
                     user_na = FALSE)
rawdat$applause[, c("v_35", "v_38", "v_45", "v_44")]  <- read_csv2("data/applausec.csv", col_names = TRUE)
rawdat$applause %<>% 
  select(c(birth = v_1,
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

rawdat$crowdguru <- read_sav("../../Tresors/crowd/data/crowdguru.sav",
                      user_na = FALSE)
rawdat$crowdguru[, c("v_35", "v_38", "v_45", "v_44")]  <- read_csv2("data/crowdguruc.csv", col_names = TRUE)
rawdat$crowdguru %<>% 
  select(c(birth = v_1,
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

rawdat <- map(.x = rawdat, .f = as_factor, only_labeled = TRUE)

# Append crowdguru and applause to atizo.
crowddata <- dplyr::bind_rows(rawdat, .id = "study")

crowddata$study %<>% as_factor(ordered = FALSE)

for (cur_col in names(crowddata)) {
  new_prompt <- attributes(x =  rawdat$atizo[[cur_col]])$label
  attr(x = crowddata[[cur_col]], which = "prompt") <- new_prompt
}

crowddata$birth %<>% 
  as.integer(crowddata$birth) # %>%
  # assert_integer(lower = 1900, upper = 2005, any.missing = TRUE)
# TODO pipe below if possible
attr(x = crowddata$birth, which = "prompt") <- "In welchem Jahr wurden Sie geboren?"

crowddata$gender %<>%
  assert_factor(levels = c("Männlich", "Weiblich"), ordered = FALSE, empty.levels.ok = FALSE, any.missing = TRUE, all.missing = FALSE)
attr(x = crowddata$gender, which = "prompt") <- "Sind Sie..."

crowddata$education %<>%
  as_factor(ordered = FALSE) %>%
  fct_recode(NULL = "0") %>%
  assert_factor(ordered = FALSE, n.levels = 6) %>%
  fct_explicit_na(na_level = "(Keine Angabe)")
  # TODO use purrr at the end to mark all factors wth fct_explicit_nas

# crowddata[15:73][crowddata[15:73] == 0] <- NA

# crowddata$disability_care %>% 

