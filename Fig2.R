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

data_edu <- data %>% 
  as.data.frame() %>% 
  mutate(W = parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B0 = W,
         B1 = parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B2 = parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B3 = parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B4 = parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B5 = parity_5 + parity_6 + parity_7 + parity_8p,
         B6 = parity_6 + parity_7 + parity_8p,
         B7 = parity_7 + parity_8p,
         B8 = parity_8p,
         B3p = B3 + B4 + B5 + B6 + B7 + B8,
         B  = B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8) %>% 
  select(cohort, edu, Country, W, B0, B1, B2, B3p, B)

data_all <- data_edu %>% 
  group_by(cohort, Country) %>% 
  summarise(W = sum(W),
            B0 = sum(B0),
            B1 = sum(B1),
            B2 = sum(B2),
            B3p = sum(B3p),
            B = sum(B)) %>% 
  ungroup(cohort, Country) %>% 
  mutate(edu = "All") %>% 
  bind_rows(data_edu) %>% 
  mutate(CCF = B / W,
         edu = factor(edu, levels = c("All", "High", "Medium", "Low")))
  
p <- data_all %>%
  ggplot(aes(x = cohort, y = CCF, group = edu, colour = edu, linetype = edu)) +
  geom_line(size = 1.5) +
  facet_wrap(~ Country) +
  theme_bw(base_family = "Times New Roman") +  
  scale_colour_manual(values = c("black", Mycol[1], Mycol[2], Mycol[3]), name = "") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed", "twodash")) +
  scale_x_continuous(breaks = seq(1940, 1970, 5)) +
  ylim(1.3, 4) +
  labs(y = "Completed cohort fertility", x = "Birth cohort")

p <- p + guides(linetype = FALSE) +
  theme(legend.position = "bottom")

ggsave("Output/Fig2.eps", p, device = cairo_ps, width = 7.5, height = 6)