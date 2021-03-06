## @knitr prepare_data
########## BEGIN ############
library(tidyverse)
library(hrbrthemes)
library(tidybayes)
#remotes::install_github("hadley/emo")
library(emo)
library(gt)

#dt <- read_csv("data/WintonCentreCleaned_covid_8country_labelled.csv")
dt <- read_csv("data/WintonCentreCleaned_covid_11country_plusUK2_labelled.csv")

# Make a small df with codes corresponding to missing values
msng_df <- tibble::tribble(~var, ~code, ~msng,
                           "Ethnic min", "4",  TRUE,
                           "Govrestrict_1","8", TRUE,
                           "Govrestrict_2","8", TRUE,
                           "Govrestrict_3","8", TRUE )

country_df <- tibble::tribble( ~Residency, ~ji_name, ~countryname_en, ~countryname_se,
                               "AU", "australia", "Australia", "Australien",
                               "DE", "de", "Germany", "Tyskland",
                               "ES", "es", "Spain", "Spanien",
                               "UK", "gb", "UK", "Storbritannien",
                               "IT", "it", "Italy", "Italien",
                               "MX", "mexico","Mexico", "Mexico",
                               "SE", "sweden", "Sweden", "Sverige",
                               "KR", "kr", "South Korea", "Sydkorea",
                               "JP", "jp", "Japan", "Japan",
                               "CN", "cn", "China", "Kina",
                               "US", "us", "USA", "USA" )

country_df <- country_df %>% 
  pivot_longer(starts_with("countryname_"), names_to = "language", 
               names_prefix = "countryname_", values_to = "countryname") %>% 
  filter(language==lang) %>% 
  select(-language) %>% 
  mutate(countryflag=map_chr(ji_name, emo::ji))

wrap_txt <- function(x, w=80){
  paste(strwrap(x,
                width = w
  ),
  collapse = "\n")
}

# Prepare labels for annotating plots
q_df_raw <- dt %>% slice(1:3) %>% select(-version) %>% 
  tibble::rowid_to_column(var = "row_id") %>% 
  pivot_longer(-row_id, names_to = "var", values_to = "txt_en") %>% 
  arrange(var, row_id)

write_csv(q_df_raw, "data/q_df_raw_en.csv")
q_df <- read_csv("data/q_df_en_se.csv")

q_df <- q_df %>% 
  pivot_longer(starts_with("txt"), names_to = "language", 
               names_prefix = "txt_", values_to = "question") %>% 
  filter(language==lang)%>% 
  select(-language) %>% 
  replace_na(list(question=""))

lbl_df_raw <- dt %>% slice(4) %>% select(-version) %>% 
  map(str_replace, " to (?=\\d)| - (?=\\d)", ", ") %>% 
  map(str_replace_all, "(?<=\\d\\s)\\(", " = ") %>% 
  map_df(~enframe(str_split(.x, ",\\s+(?=\\d)")[[1]] ), .id = "var") %>%
  select(-name) %>% 
  separate(value, sep="\\s?+=\\s?+", into=c("code", "label"), extra="merge", convert=TRUE) %>% 
  mutate(label=str_replace(label, "\\)$", ""),
         label=str_replace_all(label, "'", ""),
         label=trimws(label)) %>% 
  mutate(code=as.character(code))

write_csv(lbl_df_raw, "data/lbl_df_raw_en.csv")
lbl_df <- read_csv("data/lbl_df_en_sv.csv")

lbl_df <- lbl_df %>% 
  pivot_longer(starts_with("label"), names_to = "language", 
               names_prefix = "label_", values_to = "label") %>% 
  filter(language==lang)%>% 
  select(-language) %>% 
  anti_join(type_convert(msng_df), by=c("var", "code"))

# Prepare data
df_lng_raw <- dt %>% 
  slice(-1:-4) %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(cols = GenSocTrust:Politics, names_to = "var", values_to = "code") %>% 
  left_join(msng_df, by=c("var", "code")) %>% 
  replace_na(list(msng=FALSE))

write_csv(df_lng_raw, "data/df_long_raw.csv")

data_df <- df_lng_raw %>% 
  mutate(code=ifelse(msng, NA, code),
         code_num = parse_number(code)) %>% 
  count(var, Residency, code, code_num) %>% 
  group_by(var) %>% 
  mutate(mid_category_num = ceiling(mean(code_num, na.rm = TRUE))) %>% 
  group_by(var, Residency) %>% 
  mutate(nna_n=ifelse(!is.na(code), n, NA),
         pct=n/sum(n),
         nna_pct = nna_n/sum(nna_n, na.rm=TRUE),
         sgnd_pct=ifelse(code_num >= mid_category_num, pct, -pct),
         nna_sgnd_pct=ifelse(code_num >= mid_category_num, nna_pct, -nna_pct),
         sum_pct_low = -sum(pct * (code_num < mid_category_num), na.rm = TRUE),
         sum_pct_hi = sum(pct * (code_num > mid_category_num), na.rm = TRUE),
         panel=case_when(
           is.na(code_num) ~ "Missing",
           code_num == mid_category_num ~ "Neutral",
           TRUE ~ "Responses") 
  ) %>% 
  ungroup() %>% 
  left_join(country_df, by="Residency")
##########  END  ############

## @knitr service_functions
########## BEGIN ###########
# Service function for single questions
plot_single <- function(var_name, q_dfa=q_df, lbl_dfa=lbl_df, data_dfa=data_df){
  lbl_var_df <- lbl_dfa %>%
    filter(var==var_name) %>%
    arrange(desc(code)) %>% # desc(code)
    #mutate(x01=(1:n())-1)
    mutate(x01=1-(code-min(code))/(max(code)-min(code)))
  
  q_var_df <- q_dfa %>% 
    filter(var==var_name)
  
  clrs <- RColorBrewer::brewer.pal(max(lbl_var_df$code), "BrBG")
  lbl_var_df$clr <- clrs[as.numeric(lbl_var_df$code)]
  
  df_plot <- data_dfa %>% 
    filter(var==var_name)
  
  df_plot %>%
    filter(!is.na(code)) %>% 
    ggplot(mapping=aes(y=fct_rev(countryflag)))+
    geom_col(mapping=aes(x=nna_pct, fill=(code)),
             position = "fill") +
    geom_text(data=lbl_var_df,
              aes(x=x01, y=Inf, color=I(clr), label=label),
              family="Roboto Condensed",
              hjust="inward", vjust=1) +
    scale_fill_manual(values=clrs ) +
    theme_ipsum_rc(grid=FALSE)+
    scale_y_discrete(expand =  expansion(add = c(0,1)))+
    scale_x_continuous(labels = scales::percent, expand = expansion(add=c(0.01, 0.02)),
                       sec.axis = sec_axis( ~1-., labels = scales::percent))+
    labs(title=wrap_txt(with(q_var_df, question[row_id==1]), 75), 
         #subtitle = with(q_var_df, question[row_id==2]), 
         y=NULL, x=NULL) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=16),
          plot.title = element_text(lineheight = 1.1, face="bold", size=14),
          legend.position = 'none')
}

# Service function for group questions
plot_group <- function(var_group_name, q_dfa=q_df, lbl_dfa=lbl_df, data_dfa=data_df) {
  q_var_df <- q_dfa %>% 
    filter(var_group==var_group_name, row_id==2) %>% 
    select(var, question)
  
  q_title <- q_dfa %>% 
    filter(var_group==var_group_name, row_id==0) %>% 
    pull(question)
  
  var_names_df <- distinct(q_var_df, var)
  
  lbl_var_df <- lbl_dfa %>%
    semi_join(var_names_df, by="var") %>% 
    group_by(code) %>% 
    summarise(label=first(label)) %>% 
    arrange(desc(code)) %>% 
    mutate(x01=1-(code-min(code))/(max(code)-min(code)))
  
  clrs <- RColorBrewer::brewer.pal(max(lbl_var_df$code), "BrBG")
  lbl_var_df$clr <- clrs[as.numeric(lbl_var_df$code)]
  
  
  df_plot <- data_dfa %>% 
    semi_join(var_names_df, by="var") %>% 
    left_join(q_var_df, by="var")
  
  df_plot %>%
    filter(!is.na(code)) %>% 
    mutate(question=map_chr(question, wrap_txt, 75)) %>% 
    ggplot(mapping=aes(y=fct_rev(countryflag)))+
    geom_col(mapping=aes(x=nna_pct, fill=code),
             position = "fill") +
    geom_text(data=lbl_var_df,
              aes(x=x01, y=0.4, color=I(clr), label=label),
              family="Roboto Condensed",
              hjust="inward", vjust=1) +
    facet_wrap(vars(question), ncol = 2)+
    scale_fill_manual(values=clrs) +
    theme_ipsum_rc(grid=FALSE)+
    scale_y_discrete(expand =  expansion(add = c(1,0)))+
    scale_x_continuous(labels = scales::percent, expand = expansion(add=c(0.01, 0.02)),
                       sec.axis = sec_axis( ~1-., labels = scales::percent))+
    labs(title=wrap_txt(q_title,105), 
         y=NULL, x=NULL) + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size=20),
          plot.title = element_text(lineheight = 1.1, face="bold"),
          strip.text = element_text(size=12),
          legend.position = 'none')
}

# Service function for categorical plots by country
plot_categorical <- function(var_name, q_dfa=q_df, lbl_dfa=lbl_df, data_dfa=data_df){
  
  lbl_var_df <- lbl_dfa %>%
    filter(var==var_name) %>%
    arrange(desc(code)) %>% 
    select(-var) %>% 
    mutate(code=as.character(code))
  
  q_var_df <- q_dfa %>% 
    filter(var==var_name)
  
  clrs <- RColorBrewer::brewer.pal(7, "BrBG")[7]
  lbl_var_df$clr <- clrs[as.numeric(lbl_var_df$code)]
  
  
  df_plot <- data_dfa %>% 
    filter(var==var_name)
  
  df_plot %>% 
    filter(!is.na(code)) %>%
    left_join(lbl_var_df, by="code") %>% 
    ggplot(mapping=aes(y=fct_rev(label)))+
    geom_col(mapping=aes(x=nna_pct), fill=clrs, show.legend = FALSE)+
    facet_wrap(vars(paste(countryflag, countryname)), ncol = 2)+
    theme_ipsum_rc(grid = FALSE)+
    scale_x_continuous(labels = scales::percent, limits=c(0,1))+
    labs(title=with(q_var_df, question[row_id==1]),
         subtitle = with(q_var_df, question[row_id==2]),           
         y=NULL, x=NULL) + 
    theme(axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(lineheight = 1.1, face="bold"),
          strip.text = element_text(size=14),
          legend.position = 'none')
}

plot_binary <- function(var_name, q_dfa=q_df, lbl_dfa=lbl_df, data_dfa=data_df){
  lbl_var_df <- lbl_dfa %>%
    filter(var==var_name, code==1) %>% 
    mutate(code=as.character(code))
  
  q_var_df <- q_dfa %>% 
    filter(var==var_name)
  
  clrs <- RColorBrewer::brewer.pal(7, "BrBG")[7]
  lbl_var_df$clr <- clrs
  
  df_plot <- data_dfa %>% 
    filter(var==var_name) %>% 
    semi_join(lbl_var_df, by=c("var", "code"))
  
  df_plot %>%
    filter(!is.na(code)) %>% 
    ggplot(mapping=aes(y=fct_rev(countryflag)))+
    geom_col(mapping=aes(x=nna_pct, fill=(code))) +
    geom_text(data=lbl_var_df,
              aes(x=0, y=Inf, color=I(clr), label=label),
              family="Roboto Condensed",
              hjust="inward", vjust=1) +
    scale_fill_manual(values=clrs ) +
    theme_ipsum_rc(grid=FALSE)+
    scale_y_discrete(expand =  expansion(add = c(0,1)))+
    scale_x_continuous(labels = scales::percent, limits=c(0,1))+
    labs(title=wrap_txt(with(q_var_df, question[row_id==1]), 75), 
         y=NULL, x=NULL) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=16),
          plot.title = element_text(lineheight = 1.1, face="bold", size=14),
          legend.position = 'none')
}

plot_binary_group <- function(var_group_name, q_dfa=q_df, lbl_dfa=lbl_df, data_dfa=data_df){
  
  q_var_df <- q_dfa %>% 
    filter(var_group==var_group_name, row_id==2) %>% 
    select(var, question)
  
  q_title <- q_dfa %>% 
    filter(var_group==var_group_name, row_id==0) %>% 
    pull(question)
  
  var_names_df <- distinct(q_var_df, var)
  
  lbl_var_df <- lbl_dfa %>%
    semi_join(var_names_df, by="var") %>% 
    group_by(code) %>% 
    summarise(label=first(label)) %>% 
    filter(code==1) %>% 
    mutate(code=as.character(code))
  
  clrs <- RColorBrewer::brewer.pal(7, "BrBG")[7]
  lbl_var_df$clr <- clrs
  
  df_plot <- data_dfa %>% 
    semi_join(var_names_df, by="var") %>% 
    left_join(q_var_df, by="var") %>% 
    semi_join(lbl_var_df, "code")
  
  df_plot %>%
    filter(!is.na(code)) %>% 
    mutate(question=map_chr(question, wrap_txt, 75)) %>% 
    ggplot(mapping=aes(y=fct_rev(countryflag)))+
    geom_col(mapping=aes(x=nna_pct, fill=code)) +
    geom_text(data=lbl_var_df,
              aes(x=0, y=0.4, color=I(clr), label=label),
              family="Roboto Condensed",
              hjust="inward", vjust=1) +
    facet_wrap(vars(question), ncol = 2)+
    scale_fill_manual(values=clrs) +
    theme_ipsum_rc(grid=FALSE)+
    scale_y_discrete(expand =  expansion(add = c(1,0)))+
    scale_x_continuous(labels = scales::percent, limits=c(0,1),
                       expand = expansion(add=c(0.01, 0.02)))+
    labs(title=wrap_txt(q_title,105), 
         y=NULL, x=NULL) + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size=20),
          plot.title = element_text(lineheight = 1.1, face="bold"),
          strip.text = element_text(size=12),
          legend.position = 'none')
}
##########  END  ############

## @knitr trust_1
########## BEGIN ###########

##########  END  ###########

## @knitr trust_1
########## BEGIN ###########
plot_single("GenSocTrust")
##########  END  ###########

## @knitr trust_2
########## BEGIN ###########
plot_group("Trustingroups")
##########  END  ###########

## @knitr covid_1
########## BEGIN ###########
plot_categorical("COVIDexp")
##########  END  ###########

## @knitr covid_2
########## BEGIN ###########
plot_group("COVIDeffect")
##########  END  ###########

## @knitr sars_1
########## BEGIN ###########
plot_binary("SARS")
##########  END  ###########

## @knitr cultcog_1
########## BEGIN ###########
plot_group("CultCog")
##########  END  ###########

## @knitr prosocial_1
########## BEGIN ###########
plot_single("prosocial")
##########  END  ###########

## @knitr canadaq_1
########## BEGIN ###########
plot_group("CanadaQ")
##########  END  ###########

## @knitr finitepool_1
########## BEGIN ###########
plot_group("FinitePool")
##########  END  ###########

## @knitr longitude_1
########## BEGIN ###########
plot_group("Longitude")
##########  END  ###########

## @knitr personal_1
########## BEGIN ###########
plot_group("Personal")
##########  END  ###########

## @knitr friends_1
########## BEGIN ###########
plot_group("Friends")
##########  END  ###########

## @knitr mediaexp_1
########## BEGIN ###########
plot_binary_group("MediaExp")
##########  END  ###########

## @knitr mediaexp_2
########## BEGIN ###########
plot_group("MediaTrust")
##########  END  ###########

## @knitr soughtinfo_1
########## BEGIN ###########
plot_binary("Soughtinfo")
##########  END  ###########

## @knitr prep_1
########## BEGIN ###########
## Prep code will come here
##########  END  ###########

## @knitr govresponse_1
########## BEGIN ###########
plot_group("Govresponse")
##########  END  ###########

## @knitr govresponse_2
########## BEGIN ###########
plot_group("Govrestrict")
##########  END  ###########

## @knitr sciunderstand_1
########## BEGIN ###########
plot_single("Sciunderstand")
##########  END  ###########

## @knitr sciunderstand_2
########## BEGIN ###########
plot_group("KnowledgeCert")
##########  END  ###########

## @knitr vaccine_1
########## BEGIN ###########
plot_binary_group("Vaccine")
##########  END  ###########

## @knitr idx_var_1
########## BEGIN ###########
idx_vars <- c("FinitePool_2", "Personal_8", "Friends_8", "CanadaQ_3", "CanadaQ_2", "CanadaQ_1")

max_lbls <- lbl_df %>% 
  filter(var %in% idx_vars) %>% 
  arrange(var, code) %>% 
  mutate(code_label=paste(label, "(", code, ")", sep="")) %>% 
  group_by(var) %>% 
  summarise(
    codelabel_range=paste(code_label, collapse=".."),
    max_code=max(code))

var_grps <- q_df %>% 
  filter(var %in% idx_vars) %>% 
  distinct(var_group)

qs1 <- q_df %>% 
  semi_join(var_grps, by="var_group") %>% 
  filter(row_id==0) %>% 
  select(var_group, question1=question)

qs <- q_df %>% 
  filter(var %in% idx_vars, row_id==2) %>%
  left_join(qs1, by="var_group")

r_idx_df <- df_lng_raw %>% 
  semi_join(qs, by="var") %>% 
  filter(!is.na(code)) %>% 
  mutate(code=parse_number(code)) %>% 
  left_join(max_lbls, by="var") %>% 
  mutate(rev_code=ifelse(var=="CanadaQ_3", 6-code, code),
         scaled_code=ifelse(max_code==5, 1.5*rev_code-0.5, rev_code)) %>% 
  group_by(rowid, Residency) %>% 
  summarise(mean_code = mean(scaled_code)) %>% 
  left_join(country_df, by="Residency")

tbl_hdr_df <- tibble::tribble(~language, ~var, ~txt,
                              "se", "question_all", "Fråga",
                              "se", "codelabel_range", "Val",
                              "en", "question_all", "Question",
                              "en", "codelabel_range", "Choices") %>% 
  filter(language==lang)

qs %>% 
  left_join(max_lbls, by="var") %>% 
  mutate(question_all=paste("<em>",question1,"</em><br>", question, sep ="")) %>% 
  select(question_all, codelabel_range) %>% 
  gt() %>% 
  fmt_markdown(vars(question_all)) %>% 
  cols_label(question_all=with(tbl_hdr_df, txt[var=="question_all"]),
             codelabel_range=with(tbl_hdr_df, txt[var=="codelabel_range"])) %>% 
  tab_options(column_labels.background.color = "#666666",
              column_labels.font.weight = "bold",
              table.font.size = pct(80),
              data_row.padding = px(3),
              table.width = pct(100)
              )
##########  END  ###########

## @knitr idx_var_2
########## BEGIN ###########

idx_plt_df <- tibble::tribble(~language, ~item, ~txt,
                              "en", "title", "Risk Perception Index by country",
                              "en", "x", "Risk Perception Index (distribution)",
                              "se", "title", "Riskuppfattningsindex per land",
                              "se", "x", "Riskuppfattningsindex (fördelning)") %>% 
  filter(language==lang)

r_idx_df %>% 
  ggplot()+
  stat_halfeyeh(aes(x=mean_code, 
                    y=fct_reorder(countryflag, mean_code)))+
  scale_x_continuous(limits = c(1,7), expand = c(0,0))+
  labs(title=with(idx_plt_df, txt[item=="title"]),
       x=with(idx_plt_df, txt[item=="x"]), 
       y=NULL)+
  theme_ipsum_rc(grid=FALSE)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16),
        plot.title = element_text(lineheight = 1.1, face="bold"),
        strip.text = element_text(size=12),
        legend.position = 'none')
##########  END  ###########
