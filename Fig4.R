library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)

Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
bc <- c("1940 vs 1945", "1945 vs 1950", "1950 vs 1955", "1955 vs 1960", "1960 vs 1965", "1965 vs 1970")
Country <- c("AUS", "ESP", "IR", "SK", "FIN", "HUN")

Fig4_fun <- function(Country){
  data <- read.csv(paste("Data/", Country, "raw.csv", sep = ""), header = T)
  
  Country_full <- ifelse(Country == "AUS", "Australia",
                         ifelse(Country == "ESP", "Spain",
                                ifelse(Country == "IR", "Ireland",
                                       ifelse(Country == "SK", "South Korea",
                                              ifelse(Country == "FIN", "Finland", "Hungary")))))
  
  data_edu <- data %>% 
    as.data.frame() %>% 
    filter(cohort %in% c(1940, 1945, 1950, 1955, 1960, 1965, 1970)) %>% 
    mutate(We = parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B1 = parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B2 = parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B3 = parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B4 = parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B5 = parity_5 + parity_6 + parity_7 + parity_8p,
           B6 = parity_6 + parity_7 + parity_8p,
           B7 = parity_7 + parity_8p,
           B8 = parity_8p,
           B3p = B3 + B4 + B5 + B6 + B7 + B8,
           B  = B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8,
           K1 = B1 / We,
           K2 = B2 / We,
           K3 = B3p / We) %>% 
    select(cohort, edu, We, B, B1, B2, B3p, K1, K2, K3) %>% 
    mutate_if(is.integer, as.numeric)
  
  deriv_edu <- function(Edu){
    D <- data_edu %>% 
      filter(edu == Edu) %>% 
      select(-cohort, -edu)
    
    cohort_t1 <- D[1:6, ]
    cohort_t2 <- D[2:7, ]
    
    mid <- (cohort_t2 * cohort_t1) ^ 0.5
    
    h <- 5
    deriv_K1 <- (log(cohort_t2$K1 / cohort_t1$K1) / h) * mid$K1
    deriv_K2 <- (log(cohort_t2$K2 / cohort_t1$K2) / h) * mid$K2
    deriv_K3 <- (log(cohort_t2$K3 / cohort_t1$K3) / h) * mid$K3
    
    T_cont <- cbind(bc, deriv_K1, deriv_K2, deriv_K3)
    
    T_cont <- T_cont %>% 
      as.data.frame() %>%
      mutate(deriv_K1 = as.numeric(as.character(deriv_K1)),
             deriv_K2 = as.numeric(as.character(deriv_K2)),
             deriv_K3 = as.numeric(as.character(deriv_K3)),
             edu = Edu)
    
    return(T_cont)
  }
  
  deriv_low    <- deriv_edu("Low")
  deriv_medium <- deriv_edu("Medium")
  deriv_high   <- deriv_edu("High")
  
  D <- deriv_low %>% 
    bind_rows(deriv_medium, deriv_high) %>% 
    gather(key = Parity, value = Cont, -c(bc, edu)) %>% 
    mutate(Parity = case_when(Parity == "deriv_K1" ~ "One",
                              Parity == "deriv_K2" ~ "Two",
                              Parity == "deriv_K3" ~ "Three +"),
           Parity = factor(Parity, levels = c("One", "Two", "Three +")),
           edu = factor(edu, levels = c("Low", "Medium", "High")),
           Cont = Cont * 100)
  
  Fig4 <- D %>% 
    ggplot(aes(x = bc, y = Cont, fill = Parity, group = 1)) +
    geom_col() +
    facet_wrap(~ edu) +
    scale_fill_manual(values = c(Mycol[1], Mycol[2], Mycol[3]), 
                      name = "Parity") +
    theme_bw(base_family = "Times New Roman") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = Country_full) +
    ylim(-12, 2)
  
  return(Fig4)
}

AUS <- Fig4_fun(Country[1])
ESP <- Fig4_fun(Country[2])
IR  <- Fig4_fun(Country[3])
SK  <- Fig4_fun(Country[4])
FIN <- Fig4_fun(Country[5])
HUN <- Fig4_fun(Country[6])

legend <- get_legend(AUS + guides(color = guide_legend(nrow = 1)) +
                           theme(legend.position = "bottom"))

main <- plot_grid(AUS, FIN, HUN, IR, SK, ESP)
y.grob <- textGrob("Variation in completed cohort fertility", rot = 90, 
                   gp = gpar(fontfamily = "Times New Roman"))
x.grob <- textGrob("Birth cohort", gp = gpar(fontfamily = "Times New Roman"))
main_title <- grid.arrange(arrangeGrob(main, left = y.grob, bottom = x.grob))

legend <- plot_grid(legend)

Fig4 <- plot_grid(main_title, legend, ncol = 1, rel_heights = c(1, 0.1))

ggsave("Output/Fig4.eps", Fig4, device = cairo_ps, width = 8, height = 7)