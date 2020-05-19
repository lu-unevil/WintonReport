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
  # The rescaling from 1-5 to 1-7 is done using 1.5*(old value) - 0.5
  mutate(rev_code=ifelse(var=="CanadaQ_3", 6-code, code),
         scaled_code=ifelse(max_code==5, 1.5*rev_code-0.5, rev_code)) %>% 
  group_by(rowid, Residency) %>% 
  summarise(mean_code = mean(scaled_code))

ggplot(rescaled_df)+
  geom_density(aes(x=mean_code))+
  facet_wrap(vars(Residency), ncol=2)




