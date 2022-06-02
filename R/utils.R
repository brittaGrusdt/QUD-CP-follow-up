get_mean_selection_rate <- function(dat, indices) {
  return(dat[indices, ] %>% pull(response) %>% mean())
}
# returns the statistics calculated for each of N bootstrap samples
get_bootstrap_statistics = function(dat, response, N=1000) {
  bootstrap_samples <- group_map(dat, function(df, df.group) {
    bootstrap = boot(df, statistic = get_mean_selection_rate, R=N)$t
    samples = tibble(rate=bootstrap[,1]) 
    samples.ordered = left_join(df.group, samples, by=character()) %>%
      arrange(rate) %>% rowid_to_column("idx")
    return(samples.ordered)
  }) %>% bind_rows() %>% add_column(response = response)
  return(bootstrap_samples)
}
get_bootstrap_cis = function(bootstrap_samples, N=1000) {
  CI_bounds = c(low = ceiling(0.025 * N), up = ceiling(0.975 * N))
  bootstrapped_cis <- bootstrap_samples %>% 
    group_by(across(c(-idx, -rate, -response))) %>% 
    mutate(CI = case_when(idx == CI_bounds[["low"]] ~ "ci.low",
                          idx == CI_bounds[["up"]] ~ "ci.up")) %>% 
    filter(!is.na(CI)) %>% dplyr::select(-idx) %>% 
    pivot_wider(names_from = "CI", values_from = "rate")
  return(bootstrapped_cis)
}
