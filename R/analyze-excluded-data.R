library(boot)
library(brms)
library(here)
library(latex2exp)
library(scales)
library(tidybayes)
library(tidyverse)
library(xtable)

df.out <- read_csv(here("results", "excluded-participants.csv"))
data <- read_csv(here("results", "data_excluded_all.csv")) %>% 
  group_by(prolific_id)

ids_out.control = df.out %>% filter(cause == "control-scene") %>% pull(id)
ids_out.attention = df.out %>% filter(cause == "attention-check") %>% pull(id)
ids_out.comments = df.out %>% filter(cause == "comments") %>% pull(id)
ids_out.rts = df.out %>% filter(cause == "RT") %>% pull(id)
ids_out.check_ann = df.out %>% filter(cause == "qud-not-processed") %>% pull(id)

ids_out = data$prolific_id %>% unique()

# Information about excluded data -----------------------------------------
df.out %>% group_by(cause) %>% summarize(n = n(), .groups = "drop_last") 

all_control = left_join(tibble(id = data$prolific_id) %>% distinct(), 
                        tibble(id = ids_out.control, control = TRUE))
control_qud = left_join(all_control, tibble(id = ids_out.check_ann, qud = TRUE))
control_qud_attention = left_join(control_qud, 
                                  tibble(id = ids_out.attention, attention = T))
control_qud_attention_rt = left_join(control_qud_attention,
                                     tibble(id = ids_out.rts, rt = T))
df.out.all = left_join(control_qud_attention_rt,
                       tibble(id=ids_out.comments, comment = T)) %>% 
  replace_na(list(control = F, qud = F, attention = F, rt = F, comment = F)) %>% 
  unite("control_qud_attention_rt_comment", 
        control, qud, attention, rt, comment) %>% 
  group_by(control_qud_attention_rt_comment) %>% 
  filter(str_detect(control_qud_attention_rt_comment, "TRUE")) %>% 
  dplyr::count() %>% 
  separate("control_qud_attention_rt_comment", 
           into = c("control", "qud", "attention", "rt", "comment"), sep = "_",
           convert = TRUE) %>% 
  arrange(desc(n)) %>% mutate(n_criteria = rowSums(across(where(is.logical))))
xtable(df.out.all)
df.out.all

# excluded *only* because of control trial:
out_only_control = data %>% filter(str_detect(block, "test")) %>% 
  filter(prolific_id %in% ids_out.control & 
           !prolific_id %in% ids_out.attention &
           !prolific_id %in% ids_out.comments & 
           !prolific_id %in% ids_out.rts &
           !prolific_id %in% ids_out.check_ann) %>% 
  dplyr::select(prolific_id, response, type, id, selected_pic, pic1) 

out_only_control.summary = out_only_control %>% 
  filter(str_detect(response, "contrast")) %>% 
  dplyr::count(name = "n_control") %>% arrange(desc(n_control)) 

out_only_control.summary %>% group_by(n_control) %>% 
  dplyr::count(name = "n_participants")
ids_out.only_control_once = out_only_control.summary %>% 
  filter(n_control == 1) %>% pull(prolific_id)

# those who are only excluded because of a single selection of control scene:
out_only_control %>% filter(prolific_id %in% ids_out.only_control_once) %>% 
  filter(str_detect(response, "contrast")) %>% group_by(type, id) %>% 
  dplyr::count() %>% arrange(desc(n))

# because of trial21 (by far most often single control scene)
ids_out.only_control_trial21 = out_only_control %>% 
  filter(prolific_id %in% ids_out.only_control_once &
           str_detect(response, "contrast") & id == "trial21") %>% 
  pull(prolific_id) %>% unique()

# in critical trials 1-3, if they always choose the option without the yellow
# block, they should choose exhaustive in trial1 + trial2, and non-exhaustive 
# in trial3 ---> does not seem to be the case
data %>% filter(prolific_id %in% ids_out.only_control_trial21) %>% 
  dplyr::select(prolific_id, response, id, type, selected_pic, pic1, pic2, pic3) %>% 
  filter(id %in% c("trial1", "trial2", "trial3")) %>% 
  group_by(id, response) %>% dplyr::count() %>% arrange(id, desc(n))

# look at exact responses in trial21 for these participant
data %>% filter(prolific_id %in% ids_out.only_control_trial21) %>% 
  dplyr::select(prolific_id, response, id, type, selected_pic) %>% 
  filter(id == "trial21") %>%
  mutate(nb_selected = str_count(selected_pic, "_") + 1) %>% 
  group_by(nb_selected) %>% dplyr::count()

# Nb of criteria in which participants failed
out.by_id = df.out %>% group_by(id) %>% summarize(n=n()) %>% arrange(desc(n))
out.by_id %>% ggplot(aes(x=n)) + geom_bar(stat="count") + 
  labs(x = "# criteria failed in", y = "# participants", title = "excluded data")

out.several = out.by_id %>% filter(n > 1) %>% pull(id) %>% unique()
out.one_cause = out.by_id %>% filter(n == 1) %>% pull(id) %>% unique()

ratio = length(out.several) / length(ids_out)
message(paste("from those that were excluded, ", round(ratio*100, 2),
              "% were excluded because failed in multiple criteria", sep=""))


# reasons for those who are excluded just because of one criteria
ids_out.one_cause = out.by_id %>% filter(n==1) %>% pull(id) 
df.out %>% filter(id %in% out.one_cause) %>% 
  group_by(cause) %>% summarize(n=n())

# 1. single cause is QUD
out.single_cause_qud = df.out %>% 
  filter(id %in% out.one_cause & cause == "qud-not-processed") %>% pull(id)
out.check_ann = data %>% dplyr::select(prolific_id, check_ann) %>% distinct() 
out.check_ann %>% filter(prolific_id %in% out.single_cause_qud) %>% 
  group_by(check_ann) %>% dplyr::count() %>% arrange(desc(n))

# 2. single cause is selection of control scene
out.single_cause_control = df.out %>% 
  filter(id %in% out.one_cause & cause == "control-scene") %>% pull(id)

out.control = data %>% 
  dplyr::select(prolific_id, response, id, selected_pic, block, type) %>% 
  filter(prolific_id %in% ids_out.control & str_detect(block, "test") & 
           type != "attention-check")

out.control %>% filter(prolific_id %in% out.single_cause_control) %>% 
  filter(str_detect(response, "contrast")) %>% 
  dplyr::count(name = "n_control") %>% arrange(desc(n_control)) %>% 
  group_by(n_control) %>% dplyr::count(name = "n_participants")


