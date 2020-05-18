library(tidyverse)

idx_vars <- c("FinitePool_2", "Personal_8", "Friends_8", "CanadaQ_3", "CanadaQ_2", "CanadaQ_1")

max_lbls <- read_csv("data/lbl_df_en_sv.csv") %>% 
  filter(var %in% idx_vars) %>% 
  group_by(var) %>% 
  summarise(max_code=max(code))


qs <- read_csv("data/q_df_en_se.csv") %>% 
  filter(var %in% idx_vars, row_id ==2)

df <- read_csv("data/df_long_raw.csv") %>% 
  semi_join(qs, by="var")

rescaled_df <- df %>% 
  filter(!is.na(code)) %>% 
  left_join(max_lbls, by="var") %>% 
  mutate(scaled_code=(code-1)/(max_code-1),
         rescaled_code7=scaled_code*6+1) %>% 
  group_by(rowid, Residency) %>% 
  summarise(mean_code = mean(rescaled_code7))

ggplot(rescaled_df)+
  geom_density(aes(x=mean_code))+
  facet_wrap(vars(Residency))




