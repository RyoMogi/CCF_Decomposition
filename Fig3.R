Country <- c("AUS", "ESP", "IR", "SK", "FIN", "HUN")

D_Fig3_fun <- function(Country){
  
  data <- read.csv(paste("Data/", Country, "raw.csv", sep = ""), header = T)
  
  Country_full <- ifelse(Country == "AUS", "Australia",
                         ifelse(Country == "ESP", "Spain",
                                ifelse(Country == "IR", "Ireland",
                                       ifelse(Country == "SK", "South Korea",
                                              ifelse(Country == "FIN", "Finland", "Hungary")))))
  
  data_edu <- data %>% 
    as.data.frame() %>% 
    mutate(We = parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
           B0 = We,
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
    select(cohort, edu, We, B0, B1, B2, B3, B4, B5, B6, B7, B8, B3p, B)
  
  data_edu_all <- data_edu %>% 
    group_by(cohort) %>% 
    summarise(W = sum(We)) %>% 
    #ungroup(cohort) %>% 
    left_join(data_edu, by = "cohort") %>%
    filter(cohort %in% seq(1940, 1970, 5)) %>%
    mutate(E = We / W,
           F1e = B1 / We,
           F2e = B2 / We,
           F3e = B3 / We,
           F4e = B4 / We,
           F5e = B5 / We,
           F6e = B6 / We,
           F7e = B7 / We,
           F8e = B8 / We)
  
  decomp_CCF <- function(Edu){
    D <- data_edu_all %>% 
      filter(edu == Edu) %>% 
      mutate_if(is.integer, as.numeric) %>% 
      select(B0, B1, B2, B3, B4, B5, B6, B7, B8, B, We, W, E, 
             F1e, F2e, F3e, F4e, F5e, F6e, F7e, F8e)
    
    cohort_t1 <- D[1:6, ]
    cohort_t2 <- D[2:7, ]
    
    # midpoint of each factor
    mid <- (cohort_t2 * cohort_t1) ^ 0.5
    
    h <- 5
    
    # derivative
    D_deriv <- D %>% 
      select(F1e, F2e, F3e, F4e, F5e, F6e, F7e, F8e, E)
    
    cohort_t1_deriv <- D_deriv[1:6, ]
    cohort_t2_deriv <- D_deriv[2:7, ]
    
    deriv <- (log(cohort_t2_deriv / cohort_t1_deriv) / h) * mid[, c("F1e", "F2e", "F3e", "F4e", "F5e", "F6e", "F7e", "F8e", "E")]
    
    for(i in 1:ncol(deriv)){
      deriv[, i] <- ifelse(is.na(deriv[, i]), 0, deriv[, i])
    }
    
    
    # Compute contribution of each element to CCF
    F <- deriv[, 1:8] * mid[, "E"] * 100
    E <- mid[, 14:21] * deriv[, "E"] * 100
    
    cont <- cbind(F, E)
    colnames(cont) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8",
                        "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
    
    rownames(cont) <- c("1940-45", "1945-50", "1950-55", "1955-60", "1960-65", "1965-70")
    
    F_cont <- rowSums(cont[, 1:8])
    E_cont <- rowSums(cont[, 9:16])
    
    T_cont <- cbind(F_cont, E_cont, cont)
    
    return(T_cont)
  }
  
  comp_low    <- decomp_CCF("Low")
  comp_medium <- decomp_CCF("Medium")
  comp_high   <- decomp_CCF("High")
  
  F <- comp_low$F_cont + comp_medium$F_cont + comp_high$F_cont
  E <- comp_low$E_cont + comp_medium$E_cont + comp_high$E_cont
  
  bc <- c("1940 vs 1945", "1945 vs 1950", "1950 vs 1955", "1955 vs 1960", "1960 vs 1965", "1965 vs 1970")
  
  D_cont <- cbind(bc, F, E, Country_full)
  
  D_cont <- as.data.frame(D_cont)
  
  return(D_cont)
}

AUS <- D_Fig3_fun(Country[1])
ESP <- D_Fig3_fun(Country[2])
IR  <- D_Fig3_fun(Country[3])
SK  <- D_Fig3_fun(Country[4])
FIN <- D_Fig3_fun(Country[5])
HUN <- D_Fig3_fun(Country[6])

D_Fig3 <- AUS %>% 
  bind_rows(ESP, IR, SK, FIN, HUN) %>% 
  mutate(F = as.numeric(as.character(F)),
         E = as.numeric(as.character(E))) %>% 
  gather(key = Factor, value = Cont, -c(bc, Country_full)) %>% 
  mutate(Factor = case_when(Factor == "E" ~ "Educational composition (E)",
                            Factor == "F" ~ "Education-specific fertility (F)"),
         Factor = factor(Factor, levels = c("Educational composition (E)",
                                            "Education-specific fertility (F)")))

D_Fig3_T <- D_Fig3 %>% 
  group_by(bc, Country_full) %>% 
  summarise(Total = sum(Cont))

Fig3 <- D_Fig3 %>% 
  ggplot() +
  geom_bar(aes(x = bc, y = Cont, fill = Factor, group = 1), stat = "identity") +
  geom_line(data = D_Fig3_T, aes(x = bc, y = Total, group = 1), linetype = "longdash") +
  facet_wrap(~ Country_full) +
  scale_fill_manual(values = c(Mycol[1], Mycol[3]), name = "") +
  labs(x = "Birth cohort", y = "Contribution to change in completed cohort fertility") +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("Output/Fig3.eps", Fig3, device = cairo_ps, width = 7.5, height = 6)