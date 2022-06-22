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
data.test <- data %>% filter(str_detect(block, "test"))
# Attention-check for Prolific

# We don't want to pay participants who selected a single, but the wrong picture
# in the attention check trial; if they selected two pictures, including the
# correct one, they were paid
attention_checks = data %>% filter(type=="attention-check") %>% 
  dplyr::select(submission_id, prolific_id, selected_pic, expected, startDate) %>% 
  mutate(correct = case_when(expected == selected_pic ~ TRUE, T ~ FALSE), 
         fail = !str_detect(selected_pic, expected)) 

message("participants that are not paid:")
attention_checks %>% filter(fail)

# 1. However, we do not include data from participants who selected several 
# pictures in the attention check trial 
ids_out.attention = attention_checks %>% filter(!correct) %>% 
  pull(prolific_id) %>% unique()

# 2. At least 1 selection of control picture in test trials (excluding practice)
#  trial 10 two scenes were called contrast, but just one meant to be the control
#  scene (pic1) used for exclusion!
# attention-check trial is considered separately
out.control_not_trial10 = data.test %>% filter(type != "attention-check") %>%
  dplyr::select(prolific_id, response, id, selected_pic) %>% 
  filter(id != "trial10" & str_detect(response, "contrast")) 
ids_out.control_not_trial10  = out.control_not_trial10 %>% 
  pull(prolific_id) %>% unique()

out.control_trial10 = data.test %>% filter(type != "attention-check") %>% 
  dplyr::select(prolific_id, response, id, selected_pic) %>% 
  filter(id == "trial10") %>% 
  filter(str_detect(response, "contrast") & str_detect(selected_pic, "pic1"))
ids_out.control_trial10 = out.control_trial10 %>% pull(prolific_id) %>% unique()

out.control = bind_rows(out.control_trial10, out.control_not_trial10)
ids_out.control = c(ids_out.control_not_trial10, ids_out.control_trial10) %>% 
  unique()


# 3. Participants' responses to questions shown in the end
out.check_ann = data %>% dplyr::select(prolific_id, check_ann) %>% distinct() 
ids_out.check_ann = out.check_ann %>% 
  filter(str_detect(check_ann, "same") | str_detect(check_ann, "ignored")) %>% 
  pull(prolific_id) %>% unique()

# 4. Participants' comments
comments <- data %>% select(comments, prolific_id, startDate) %>% distinct() %>% 
  filter(comments != "")
comments$comments
comments[c(69, 62, 31, 7),]

# 1. participant mentioning problem in second to last trial can be included as 
# this was a filler trial anyway ("5f0874db34894f07bd04f436")
data %>% filter(prolific_id == "5f0874db34894f07bd04f436") %>% 
  dplyr::select(trial_number, type, block) %>% 
  filter(type != "training" & block != "practice")

# 2. participant mentioning Ann's question is excluded anyway due to response to
# questions shown in the end ("6128a3db67305ef94021fcd7")
# 3. participant who seemed to have problems to understand the experiment is also 
# excluded anyway due to control scene ("5e9132c5df10df4620035695")
# 4. participant mentioning problems (made no sense) also excluded anyway due to
# selection of control scene ("906540d3bc4eed4c35a0")

ids_out.comments = c("6128a3db67305ef94021fcd7", "5e9132c5df10df4620035695",
                     "906540d3bc4eed4c35a0")
# double check again:
comments %>% filter(prolific_id %in% ids_out.comments)

# 5. Reaction times
out.rts = data.test %>% dplyr::select(prolific_id, RT, id) %>% 
  filter(RT < 6000) %>% distinct() %>% arrange(RT) %>% 
  summarize(n=n(), min = min(RT))
ids_out.rts = out.rts %>% filter(n>1) %>% pull(prolific_id) %>% unique()

df.out = tibble()
if(length(ids_out.rts) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.rts, cause = "RT"))
if(length(ids_out.comments) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.comments, cause = "comments"))
if(length(ids_out.check_ann) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.check_ann, cause = "qud-not-processed"))                                         
if(length(ids_out.control) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.control, cause = "control-scene"))                           
if(length(ids_out.attention) > 0) df.out = bind_rows(df.out, tibble(id = ids_out.attention, cause = "attention-check"))                   

ids_out = df.out$id %>% unique()  
data_cleaned = data %>% filter(!prolific_id %in% ids_out) %>% group_by(prolific_id)

write_csv(data_cleaned, here("results", "data_cleaned.csv"))
write_csv(df.out, here("results", "excluded-participants.csv"))


# Save excluded data as well ----------------------------------------------
excluded.all = data %>% filter(prolific_id %in% ids_out) %>% group_by(prolific_id)
write_csv(excluded.all, here("results", "data_excluded_all.csv"))

excluded.control = data %>% filter(prolific_id %in% ids_out.control) %>%
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



