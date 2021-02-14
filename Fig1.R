library(tidyverse)
library(extrafont)
font_import()
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

AUS <- read.csv("Data/AUSraw.csv", header = T)
ESP <- read.csv("Data/ESPraw.csv", header = T)
IR  <- read.csv("Data/IRraw.csv", header = T)
SK  <- read.csv("Data/SKraw.csv", header = T)
FIN <- read.csv("Data/FINraw.csv", header = T)
HUN <- read.csv("Data/HUNraw.csv", header = T)

AUS <- AUS %>% 
  as.data.frame() %>% 
  mutate(Country = "Australia")

ESP <- ESP %>% 
  as.data.frame() %>% 
  mutate(Country = "Spain")

IR <- IR %>% 
  as.data.frame() %>% 
  mutate(Country = "Ireland") %>% 
  select(-c(X, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12, X.13, X.14))

SK <- SK %>% 
  as.data.frame() %>% 
  mutate(Country = "South Korea") %>% 
  select(-Total)

FIN <- FIN %>% 
  as.data.frame() %>% 
  mutate(Country = "Finland")

HUN <- HUN %>% 
  as.data.frame() %>% 
  mutate(Country = "Hungary")

data <- AUS %>% 
  bind_rows(ESP, IR, SK, FIN, HUN)

Prop_edu <- data %>% 
  as.data.frame() %>% 
  mutate(Total = parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + 
           parity_5 + parity_6 + parity_7 + parity_8p) %>% 
  select(cohort, edu, Country, Total) %>% 
  group_by(cohort, edu, Country) %>% 
  mutate(T_edu = sum(Total)) %>% 
  ungroup(cohort, edu, Country) %>% 
  group_by(cohort, Country) %>% 
  mutate(T_bc = sum(Total)) %>% 
  ungroup(cohort, Country) %>% 
  mutate(Prop_edu = T_edu / T_bc,
         edu = factor(edu, levels = c("High", "Medium", "Low")))

Fig_Prop_edu <- Prop_edu %>% 
  ggplot(aes(x = cohort, y = Prop_edu, group = edu, fill = edu)) +
  geom_area(alpha = 0.8) +
  facet_wrap(~ Country) +
  theme_bw(base_family = "Times New Roman") +  
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c(Mycol[1], Mycol[2], Mycol[3]), name = "") +
  scale_x_continuous(breaks = seq(1940, 1970, 5)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  labs(y = "Composition by educational attainment", x = "Birth cohort")
ggsave("Output/Fig1.eps", Fig_Prop_edu, device = cairo_ps, width = 7.5, height = 6)
