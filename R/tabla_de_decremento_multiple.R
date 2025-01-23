library(tidyverse)
library(readr)
library(readxl)
library(patchwork)
library(viridis)

load("Databases/base0_mort.RData")
source(file  = "R/tab_mort_func.R")
load("Databases/df_causes2019.RData")

tab_mort <- tab_mort_func(base = base0)
tmort19  <- tab_mort %>% 
  filter(edo == "República Mexicana",
         year == 2019) %>% 
  select(age, sex, px) %>% 
  mutate(id = paste(age, sex, sep = ""))

df_causes2 <- df_causes1 %>% 
  mutate(
    sex = case_when(
      sexo == 1 ~ "males",
      sexo == 2 ~ "females"
    ),
    dos_causas = ifelse(
      disease_group == "Externas", disease_group,"Resto"
    )
  ) %>% 
  group_by(sex, age, dos_causas) %>% 
  summarise(defs = sum(defs), .groups = "drop") %>% 
  group_by(sex, age) %>% 
  mutate(R_i = 1 - defs / sum(defs)) %>% 
  ungroup() %>% 
  mutate(id = paste(age, sex, sep ="")) %>% 
  na.omit()

tde <- full_join(
  tmort19,
  df_causes2,
  by = "id"
) %>% 
  na.omit() %>% 
  select(age = age.x, sex = sex.x,
         px, R_i) %>% 
  mutate(px_i = px^R_i,
         qx_i = 1 - px_i,
         lx_i = c(1, cumprod(px_i))[-length(px_i)],
         dx_i = lx_i * qx_i, 
         )

