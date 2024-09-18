library(tidyverse)
library(readxl)
library(readr)
library(stringr)
 
## Fuentes/Censos/input/censo2020.xls
data <- read_xlsx("input/Censo2020.xlsx",
                  skip = 11, col_names = F
                  )

names(data)<-
  c("id","edo","age","both","males","females")

base_mx <- data %>% 
  filter(is.na(edo) == FALSE) %>% 
  select(-c("id", "both")) %>%
  na.omit() %>%
  filter(
    age != "Total",
    !grepl("De", age),
    !grepl("85 años", age)
  ) %>%
  mutate(age = str_extract(age,"\\d+")) %>%
  gather(key = sex, value = pob, -age, -edo) %>% 
  type_convert() %>% 
  mutate(pob2 = ifelse(sex == "females", pob, -pob))
  
save(base_mx, file = "input/base_mx.RData")

load("input/base_mx.RData")
library(tidyverse)
base_mx %>%
  filter(edo == "Total", is.na(age) == FALSE) %>%
  ggplot() +
  geom_bar(aes(x = age, y = pob2/1000000, fill = age),
           stat = "identity",
           show.legend = F
           ) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    limits = c(-1.25, 1.25),
    breaks = seq(-1.25, 1.25, 0.25),
    labels = as.character(
      c(
        seq(1.25, 0, -0.25),
        seq(0.25, 1.25, 0.25)
      ))
  ) +
  scale_x_continuous(
    limits = c(-1, 101), breaks = seq(0, 100, 5),
    labels = seq(0, 100, 5)
  )+
  annotate(
    geom = "text", x = 95, y = -1, label = "Hombres",
    color = "black", size = 3
  ) +
  annotate(
    geom = "text", x = 95, y = 1, label = "Mujeres",
    color = "black", size = 3
  ) +
  theme_light() +
  scale_fill_viridis_c(option = "A", guide = guide_colorbar()) +
  labs(y = "Población (millones)", x = "Edad", fill = "Edad") 
 scale_fill_viridis_c(option = "A") 
 ggsave(filename = "output/piramide1.png",dpi = 320)

 #Tarea 
 left_join(
   base_mx %>% 
     filter(edo == "Total", is.na(age) == F) %>% 
     group_by(sex) %>% 
     summarise(pob = sum(pob), .groups = "drop"),
   base_mx %>% 
     filter(edo== "Total", is.na(age) == T) %>% 
     select(sex, pob_na = pob),
   by = "sex"
 ) %>% 
   mutate(rat = 100 * pob_na / pob)
 
base_mx_pror <- left_join(
  base_mx %>% 
    filter(edo == "Total") %>% 
    group_by(sex) %>% 
    mutate(prop = pob / sum(pob)) %>% 
    filter(is.na(age) == F) %>%
    ungroup(),
  base_mx %>% 
    filter(edo == "Total", is.na(age) == T) %>% 
    select(sex, pob_na = pob),
  by = "sex"
)  %>% 
  mutate(pob_fin = pob + pob_na * prop) %>% 
  select(age, sex, pob = pob_fin)

# indice de wipple 26 de Agosto

left_join(
  base_mx_pror %>% 
    filter(
      age %in% c(25:60),
      age %% 10 == 0 | age %% 10 == 5
    ) %>% 
    group_by(sex) %>% 
    summarise(pob_num = sum(pob), .groups = "drop"),
  base_mx_pror %>% 
    filter(age %in% c(23:62)) %>% 
    group_by(sex) %>% 
    summarise(pob_den = sum(pob), ,groups = "drop"),
  by = "sex"
) %>% 
   mutate(W = 5 * pob_num / pob_den)

## indice de Myers 28 de Agosto

s <- base_mx_pror %>% 
  filter(age %in% c(10:80)) %>% 
  mutate(digit = age %% 10) %>% 
  group_by(digit) %>% 
  summarise(step1 = sum(pob), .groups = "drop") %>% 
  mutate(
    weights  = c(1:10),
    tot_pop1 = step1*weights
  )
s
t <-  base_mx_pror %>% 
  filter(age %in% c(10:80)) %>% 
  mutate(digit = age %% 10) %>% 
  filter(!age %in% c(10:19)) %>% 
  group_by(digit) %>% 
  summarise(step2 = sum(pob), .groups = "drop") %>% 
  mutate (
    weights = c(9:0),
    tot_pop2 = step2*weights
  )
t

myers_tab <- left_join(s,t,by = "digit") %>% 
  select(digit,tot_pop1,tot_pop2) %>% 
  mutate(blended = tot_pop1 + tot_pop2,
         blend_perc = 100*blended/sum(blended),
deviat = abs(blend_perc-10 ))

myers_tab %>% 
  summarise(myers = sum(deviat)/2)
## agrupaciones en edades quinquenales

# Define the bins for the age groups 
bins <- seq(0,max(base_mx_pror$age)+5,by = 5)
  
# create labels for the age group
labels <- paste(bins[-length(bins)],
               bins[-1]-1,
               sep="-")
# Create a new colum for the age group
 base_mx_pror <- base_mx_pror %>% 
   mutate(age_group = cut(age,breaks = bins,
                          labels = labels, right = FALSE))
 
 # Group by age group and sum the population
 base_mx_5 <- base_mx_pror %>% 
   group_by(sex,age_group) %>% 
   summarise(pob = sum(pob,na.rm=TRUE),
             .groups = "drop") %>% 
   mutate(pob2 = ifelse(sex=="males",-pob,pob))

 base_mx_5 %>% 
   ggplot() +
   geom_bar(aes(x=age_group,y=pob2/1000000,fill=age_group),
            stat = "identity",
            show.legend = F
            ) +
   coord_flip() +
   geom_hline(yintercept = 0) +
   scale_y_continuous(
     limits = c(-6,6),breaks = seq(-6,6,1),
     labels = as.character(c(
       seq(6,0,-1),
       seq(1,6,1)
     ))
   ) +
   annotate(
     geom = "text",x=20,y=-3,label="Hombres",size=3
   ) +
   annotate(
     geom = "text",x=20,y=3,label = "Mujeres",color = "black", size=3
   )+
   theme_light()+
   scale_fill_viridis_d(option = "C") 
 
 library(DemoTools)
 library(ungroup)
 pclm(x=bins, )
 
 base_mx_5H <- base_mx_5 %>% 
   filter(sex=="males")
 
 base_mx_5M <- base_mx_5 %>% 
   filter(sex=="females")

 base_mx_1_H <- pclm(x=bins[-length(bins)],
                     base_mx_5H$pob, nlast = 10) 
 base_mx_1_M <- pclm(x=bins[-length(bins)],
                     base_mx_5M$pob, nlast = 10) 
 
 base_mx_1 <- rbind(
 enframe(base_mx_1_H$fitted,
         name="age",value = "pob") %>% 
   mutate(age=c(0:109),sex="males",
          .before = pob)
 ,
 enframe(base_mx_1_M$fitted,
         name="age",value = "pob") %>% 
   mutate(age=c(0:109),sex="females",
          .before = pob)
 
 ) %>% 
   mutate(pob2 = ifelse(sex=="males",-pob,pob))
 #grafca suave
 base_mx_1 %>% 
   ggplot() +
   geom_bar(aes(x=age,y=pob2/1000000,fill=age),
            stat = "identity",
            show.legend = F
   ) +
   coord_flip() +
   geom_hline(yintercept = 0) +
   scale_y_continuous(
     limits = c(-1.25,1.25),breaks = seq(-1.25,1.25,0.25),
     labels = as.character(c(
       seq(1.25,0,-0.25),
       seq(0.25,1.25,0.25)
     ))
   ) +
   scale_x_continuous(
     limits = c(-1,110),
     breaks = seq(0,110,5),
     labels = seq(0,110,5)
   ) +
   annotate(
     geom = "text",x=95,y=-1,label="Hombres",size=3
   ) +
   annotate(
     geom = "text",x=95,y=1,label = "Mujeres",color = "black", size=3
   )+
   theme_light()+
   scale_fill_viridis_d(option = "C") 
 ggsave(filename = "output/piramide2.png",dpi = 320)       

 
 
 
 
 
 
 
 
 
 base_mx_5 <- base_mx_5 %>% 
   separate(age_group, into = c("digi1","digi2"),sep="-")
 type_convert() %>% 
   select(age=digi1,sex,pob)
 
 base_mx_5 <-  nase_mx_5 %>% 
   filter()
   
   
