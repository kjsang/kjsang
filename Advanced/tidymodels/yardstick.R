# package loading ---------------------------------------------------------
options(scipen=999)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, # helper packages
               tidymodels) # modeling packages including yardstick
two_class_example
# metrics: General Function to Estimate Performance
yardstick::metrics(two_class_example, truth, predicted)
two_class_example %>% 
  roc_auc(truth, Class1)

hpc_cv %>% 
  as_tibble() %>% 
  precision(obs, pred, estimator = "micro")

hpc_cv %>%
  as_tibble() %>% 
  group_by(Resample) %>%
  metrics(obs, pred, VF:L)

class_metrics <- metric_set(accuracy, kap)
hpc_cv %>%
  group_by(Resample) %>%
  class_metrics(obs, estimate = pred)
hpc_cv %>% 
  as_tibble() %>% 
  filter(Resample == "Fold01") %>% 
  conf_mat(obs, pred)
hpc_cv %>% 
  group_by(Resample) %>% 
  conf_mat(obs, pred) %>% 
  mutate(tidied = map(conf_mat, tidy)) %>% 
  unnest(tidied) -> cells_per_resample

hpc_cv %>%
  group_by(Resample) %>%
  summarize(total = n()) %>%
  left_join(cells_per_resample, by = "Resample") %>%
  # Compute the proportions
  mutate(prop = value/total) %>%
  group_by(name) %>%
  # Average
  summarize(prop = mean(prop)) -> counts_per_resample

mean_cmat <- matrix(counts_per_resample$prop, byrow = TRUE, ncol = 4)
rownames(mean_cmat) <- levels(hpc_cv$obs)
colnames(mean_cmat) <- levels(hpc_cv$obs)
round(mean_cmat, 3)

hpc_cv %>% 
  conf_mat(obs, pred) -> cm
ggplot2::autoplot(cm, type = "heatmap")
