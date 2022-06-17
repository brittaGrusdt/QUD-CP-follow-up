library(boot)
library(brms)
library(here)
library(latex2exp)
library(scales)
library(tidybayes)
library(tidyverse)

# Load experimental data
# test-data:
# target_dir = here("results", "pretests")
# fn <- file.path(target_dir, "results_82_CommunicationBlocks2_BG_last_test.csv")

# real-data:
target_dir = here("results")
fn <- file.path(target_dir, "results_82_CommunicationBlocks2_BG.csv")

data <- read.csv(fn, sep=",") %>% as_tibble() %>% group_by(prolific_id)

# Attention-check for Prolific

# We don't pay participants who selected a single, but the wrong picture in the 
# attention check trial; if they selected two pictures, including the correct one, 
# they were paid 
attention_checks = data %>% filter(type=="attention-check") %>% 
  dplyr::select(submission_id, prolific_id, selected_pic, expected) %>% 
  mutate(correct = case_when(expected == selected_pic ~ TRUE, T ~ FALSE), 
         fail = !str_detect(selected_pic, expected)) 

message("participants that are not paid:")
attention_checks %>% filter(fail)

# 1. However, we do not include data from participants who selected several 
# pictures in the attention check trial 
ids_out.attention = attention_checks %>% filter(!correct) %>% 
  pull(prolific_id) %>% unique()

# 2. At least 1 selection of control picture in test trials
df.critical = data %>% filter(type == "critical")
ids_out.control_scene = df.critical %>% select(prolific_id, response) %>% 
  filter(str_detect(response, "contrast")) %>% pull(prolific_id) %>% unique()


# 3. Participants' responses to questions shown in the end
ids_out.check_ann = data %>% dplyr::select(prolific_id, check_ann) %>% 
  distinct() %>% 
  filter(str_detect(check_ann, "same") | str_detect(check_ann, "ignored")) %>% 
  pull(prolific_id) %>% unique()

# 4. Participants' comments
comments <- data %>% select(comments, prolific_id) %>% distinct() %>% 
  filter(comments != "")
comments$comments

comments[c(4, 9, 14), ]
# participant mentioning problem in second to last trial can be included as this
# was a filler trial anyway
data %>% filter(prolific_id == "5f0874db34894f07bd04f436") %>% 
  dplyr::select(trial_number, type, block) %>% 
  filter(type != "training" & block != "practice")

# participant mentioning Ann's question is excluded anyway due to response to
# questions shown in the end and participant who seemed to have problems to 
# understand the experiment is also excluded due to control scene
ids_out.comments = comments[c(9, 14), ] %>% pull(prolific_id)

# 5. Reaction times
ids_out.rts = df.critical %>% dplyr::select(prolific_id, RT, id) %>% 
  filter(RT < 6000) %>% pull(prolific_id) %>% unique()

df.out = tibble()
if(length(ids_out.rts) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.rts, cause = "RT"))
if(length(ids_out.comments) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.comments, cause = "comments"))
if(length(ids_out.check_ann) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.check_ann, cause = "qud-not-processed"))                                         
if(length(ids_out.control_scene) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.control_scene, cause = "control-scene"))                           
if(length(ids_out.attention) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.attention, cause = "attention-check"))                   

ids_out = df.out$id %>% unique()  
data_cleaned = data %>% filter(!prolific_id %in% ids_out) %>% group_by(prolific_id)

write_csv(data_cleaned, here("results", "data_cleaned.csv"))
write_csv(df.out, here("results", "excluded-participants.csv"))

# also save excluded data
excluded.all = data %>% filter(prolific_id %in% ids_out) %>% group_by(prolific_id)
write_csv(excluded.all, here("results", "data_excluded_all.csv"))

excluded.control = data %>% filter(prolific_id %in% ids_out.control_scene) %>%
  group_by(prolific_id)
write_csv(excluded.control, here("results", "data_excluded_control.csv"))

excluded.check_ann = data %>% filter(prolific_id %in% ids_out.check_ann) %>%
  group_by(prolific_id)
write_csv(excluded.check_ann, here("results", "data_excluded_check_ann.csv"))

excluded.attention = data %>% filter(prolific_id %in% ids_out.attention) %>%
  group_by(prolific_id)
write_csv(excluded.attention, here("results", "data_excluded_attention.csv"))

excluded.rts = data %>% filter(prolific_id %in% ids_out.rts) %>%
  group_by(prolific_id)
write_csv(excluded.rts, here("results", "data_excluded_rts.csv"))


n_in = data_cleaned %>% distinct_at(vars(c(prolific_id))) %>% nrow()
n_out = df.out %>% distinct_at(vars(c(id))) %>% nrow()

ratio = round(n_in/(n_in + n_out), 2) * 100
message(paste(ratio, "% included.", sep=""))
message(paste(n_in, " participants included.", sep=""))



