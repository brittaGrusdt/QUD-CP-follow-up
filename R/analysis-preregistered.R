library(boot)
library(brms)
library(here)
library(latex2exp)
library(scales)
library(tidybayes)
library(tidyverse)
source(here("R", "utils.R"))

theme_set(theme_minimal(base_size=12) + theme(legend.position = "top"))

# Get & Prepare data ------------------------------------------------------
data = read_csv(here("results", "data_cleaned.csv")) %>% group_by(prolific_id)

data.critical = data %>% 
  filter(type=="critical") %>% 
  dplyr::select(submission_id, prolific_id, id, type, QUD, response, 
                selected_pic, pic2, pic3, RT) %>% 
  mutate(pic2=str_replace(pic2, ".png", ""), pic2=str_replace(pic2, "_horiz", ""),
         pic3 = str_replace(pic3, ".png", ""), pic3=str_replace(pic3, "_horiz", "")) %>% 
  mutate(picPair=paste(pic2, pic3, sep=" & "),
         pic_condition = 
           case_when(picPair == "if2_unn & if2_unu" ~ "D. withDistractor-external", 
                     picPair == "if1_un & if2_unu" ~ "B. w/oDistractor-external",
                     picPair == "if1_un & if1_uu" ~ "A. w/oDistractor-internal",
                     picPair == "if2_unn & if1_uu" ~ "C. withDistractor-internal")) %>% 
  dplyr::select(-pic2, -pic3) %>%
  mutate(stimulus = substr(pic_condition, 1, 1),
         pic_condition = substr(pic_condition, 4, nchar(pic_condition)), 
         response = case_when(response == "exhaustive_non-exhaustive" ~ "both",
                              T ~ response),
         QUD = case_when(QUD == "willq" ~ "will-q",
                         QUD == "ifp" ~ "if-p", 
                         T ~ QUD))

data.critical.model = data.critical %>% 
  dplyr::select(-picPair, -selected_pic, -submission_id, -type, -id) %>% 
  mutate(picPair = pic_condition, 
         nb_selected = str_count(response, "_") + 1,
         response = case_when(nb_selected == 3 ~ "all", T ~ response),
         response = str_replace(response, "non-exhaustive", "NE"),
         response = str_replace(response, "exhaustive", "E"), 
         response = str_replace(response, "contrast", "C"), 
         response = str_replace(response, "E_NE", "both"), 
         QUD = as.factor(QUD)) %>% 
  separate(col = pic_condition, into = c("exh", "nonExh"), sep="-") %>% 
  mutate(exh = case_when(exh == "w/oDistractor" ~ "woD", 
                         exh == "withDistractor" ~ "withD"),
         exh = factor(exh, levels = c("woD", "withD")), 
         nonExh = case_when(nonExh == "internal" ~ "int", 
                            nonExh == "external" ~ "ext"),
         nonExh = factor(nonExh, levels = c("int", "ext")))


# reference predictor categories are: QUD:if-p, exh:withD, nonExh:ext
df.ordinal <- data.critical.model %>%
  mutate(response = factor(response, levels = c("NE", "both", "E"), ordered=T), 
         exh = factor(exh, levels = c("withD", "woD")),
         nonExh = factor(nonExh, levels = c("ext", "int")), 
         QUD = factor(QUD, levels = c("if-p", "will-q"))
         )
contrasts(df.ordinal$response) <- contr.treatment(3)

ordinal_model <- brm(data = df.ordinal,
                     family = cumulative("probit"),
                     formula =  response ~ 1 + QUD * exh + exh * nonExh + 
                       (1 + exh + nonExh + QUD | prolific_id),
                       # (1 + exh + nonExh + QUD + QUD*exh + exh * nonExh | prolific_id),
                     seed = 1, chains = 4, cores = 4, iter = 3000,
                     control = list(adapt_delta = 0.9))

conds <- make_conditions(df.ordinal, c("exh", "nonExh"))
effects <- conditional_effects(ordinal_model, "QUD", categorical = TRUE, 
                               conditions = conds, robust = T, 
                               re_formula = NA)
# effects$`QUD:cats__`
effects

summary(ordinal_model)

hypothesis(ordinal_model, "QUDwillMq > 0")


# Exploratory analysis ----------------------------------------------------
# Thresholds
tau1 = fixef(ordinal_model)["Intercept[1]", "Estimate"]
tau2 = fixef(ordinal_model)["Intercept[2]", "Estimate"]
posterior_draws = tidy_draws(ordinal_model)

# reference category: QUD:if-p, exh:withD, nonExh:ext
posterior_preds = function(qud, exh, non_exh, tau1=tau1, tau2=tau2, 
                           draws=posterior_draws) {
  
  qud = str_replace(qud, "-", "") # to catch if-p/will-q inputs as well
  samples <- draws %>% dplyr::select(starts_with("b_")) %>% 
    rename(tau1 = `b_Intercept[1]`, tau2 = `b_Intercept[2]`) %>% 
    mutate(
      eta_willq_withD_ext = `b_QUDwillMq`,
      eta_ifp_withD_int = `b_nonExhint`,
      eta_ifp_woD_ext = `b_exhwoD`,
      
      eta_ifp_woD_int = `b_exhwoD` + `b_nonExhint` + `b_exhwoD:nonExhint`,
      eta_willq_woD_ext = `b_QUDwillMq` + `b_exhwoD` + `b_QUDwillMq:exhwoD`
    )
  eta = paste("eta", qud, exh, non_exh, sep = "_")
  
  if(eta == "eta_ifp_withD_ext") {
    # predictions for reference category
    predictions = samples %>% 
      transmute(NE = pnorm(tau1), both = pnorm(tau2) - pnorm(tau1), 
                E = 1 - pnorm(tau2))
  } else {
    predictions = samples %>% 
      transmute(NE = pnorm(tau1 - .data[[eta]]), 
                both = pnorm(tau2 - .data[[eta]]) - 
                  pnorm(tau1 - .data[[eta]]),
                E = 1 - pnorm(tau2 - .data[[eta]])) 
  }
  
  predictions %>% add_column(QUD=qud, exhaustive=exh, non_exhaustive=non_exh)
}

samples_posterior_probs =
  bind_rows(posterior_preds("willq", "withD", "ext"),
            posterior_preds("willq", "woD", "ext"),
            posterior_preds("ifp", "withD", "ext"),
            posterior_preds("ifp", "woD", "ext"),
            posterior_preds("ifp", "withD", "int"),
            posterior_preds("ifp", "woD", "int"))


# H2: P(E | QUD=willq) > P(both | QUD=willq)
samples_posterior_probs %>% filter(QUD == "willq") %>% 
  mutate(h2 = E > both) %>% summarize(p_h2 = mean(h2))

# H3: P(both | QUD=ifp) > P(E | QUD=ifp)
samples_posterior_probs %>% filter(QUD == "ifp") %>% 
  mutate(h3 = both > E) %>% summarize(p_h3 = mean(h3))


