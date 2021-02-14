library(ggrepel)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

#sel_bc_Fig5 <- reactive({
#  switch(input$dataset_Fig5,
#         "1940-1970" = 1, 
#         "1945-1970" = 2, 
#         "1950-1970" = 3, 
#         "1955-1970" = 4, 
#         "1960-1970" = 5, 
#         "1965-1970" = 6
#  )
#})

sel_bc_Fig5 <- 6

Country <- c("AUS", "CR", "FIN", "GR", "HUN", "ITA", "IR", "RUS", "SER", "SK", "ESP")
CCF_all <- c()
Prop_F_all <- c()

for(i in 1:length(Country)){
  data_Fig5 <- read.csv(paste("Data/", Country[i], "raw.csv", sep = ""), header = T)
  
  data_edu_Fig5 <- data_Fig5 %>% 
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
  
  if(Country[i] == "ITA"){
    bc_min <- case_when(sel_bc_Fig5 == 1 ~ 1938,
                        sel_bc_Fig5 == 2 ~ 1942,
                        sel_bc_Fig5 == 3 ~ 1948,
                        sel_bc_Fig5 == 4 ~ 1953,
                        sel_bc_Fig5 == 5 ~ 1958,
                        sel_bc_Fig5 == 6 ~ 1963)
    
    bc_max <- bc_min + 5
    
  } else {
    
    bc_min <- case_when(sel_bc_Fig5 == 1 ~ 1940,
                        sel_bc_Fig5 == 2 ~ 1945,
                        sel_bc_Fig5 == 3 ~ 1950,
                        sel_bc_Fig5 == 4 ~ 1955,
                        sel_bc_Fig5 == 5 ~ 1960,
                        sel_bc_Fig5 == 6 ~ 1965)
    
    bc_max <- bc_min + 5
  }
  
  ## To get CCF
  CCF <- data_edu_Fig5 %>%
    #filter(cohort >= bc_min & cohort <= bc_max) %>% 
    filter(cohort >= 1965 & cohort <= 1970) %>% 
    summarise(W = sum(We),
              B = sum(B)) %>% 
    mutate(CCF = B / W)
  
  CCF <- CCF$CCF
  
  
  ## To get the proportion of F
  if(Country[i] == "ITA"){
    
    data_edu_all <- data_edu_Fig5 %>% 
      group_by(cohort) %>% 
      summarise(W = sum(We)) %>% 
      left_join(data_edu_Fig5, by = "cohort") %>%
      filter(cohort != 1933) %>% 
      mutate(E = We / W,
             F1e = B1 / We,
             F2e = B2 / We,
             F3e = B3 / We,
             F4e = B4 / We,
             F5e = B5 / We,
             F6e = B6 / We,
             F7e = B7 / We,
             F8e = B8 / We)
  } else {
    
    data_edu_all <- data_edu_Fig5 %>% 
      group_by(cohort) %>% 
      summarise(W = sum(We)) %>% 
      left_join(data_edu_Fig5, by = "cohort") %>%
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
  }
  
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
    F <- deriv[, 1:8] * mid[, "E"]
    E <- mid[, 14:21] * deriv[, "E"]
    
    cont <- cbind(F, E)
    colnames(cont) <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8",
                        "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8")
    
    if(Country[i] == "ITA"){
      
      rownames(cont) <- c("1938-1942", "1942-1948", "1948-1953", "1953-1958", "1958-1963", "1963-1968")
    } else {
      
      rownames(cont) <- c("1940-45", "1945-50", "1950-55", "1955-60", "1960-65", "1965-70")
    }
    
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
  
  if(Country[i] == "ITA"){
    
    bc <- c("1938-1942", "1942-1948", "1948-1953", "1953-1958", "1958-1963", "1963-1968")
    bc5_sel <- bc[sel_bc_Fig5]
    
  } else {
    
    bc <- c("1940-1945", "1945-1950", "1950-1955", "1955-1960", "1960-1965", "1965-1970")
    bc5_sel <- bc[sel_bc_Fig5]
  }
  
  D_cont <- cbind(bc, F, E)
  
  Prop_F <- D_cont %>% 
    as.data.frame() %>% 
    mutate(F = as.numeric(as.character(F)),
           E = as.numeric(as.character(E)),
           Abs_F = ifelse(F < 0, -F, F),
           Abs_E = ifelse(E < 0, -E, E),
           Abs_Total = Abs_F + Abs_E,
           Prop_F = Abs_F / Abs_Total,
           cumsum_Abs_F = rev(cumsum(rev(Abs_F))),
           cumsum_Abs_E = rev(cumsum(rev(Abs_E))),
           cumsum_Abs_Total = cumsum_Abs_F + cumsum_Abs_E,
           cumsum_Prop_F = cumsum_Abs_F / cumsum_Abs_Total) %>% 
    filter(bc == bc5_sel)
  
  Prop_F <- Prop_F$cumsum_Prop_F
  
  CCF_all <- c(CCF_all, CCF)
  Prop_F_all <- c(Prop_F_all, Prop_F)
}

D_Fig5 <- cbind(Country, CCF_all, Prop_F_all)

Fig5 <- D_Fig5 %>% 
  as.data.frame() %>% 
  mutate(CCF_all = as.numeric(as.character(CCF_all)),
         Prop_F_all = as.numeric(as.character(Prop_F_all)),
         Country = case_when(Country == "AUS" ~ "Australia",
                             Country == "CR" ~ "Croatia",
                             Country == "FIN" ~ "Finland",
                             Country == "GR" ~ "Greece",
                             Country == "HUN" ~ "Hungary",
                             Country == "ITA" ~ "Italy",
                             Country == "IR" ~ "Ireland",
                             Country == "RUS" ~ "Russia",
                             Country == "SER" ~ "Serbia",
                             Country == "SK" ~ "South Korea",
                             Country == "ESP" ~ "Spain"),
         Country_sel = ifelse(Country %in% c("Australia", "Finland", "Hungary", "Ireland", "South Korea", "Spain"), "1", "0")) %>% 
  ggplot(aes(x = CCF_all, y = Prop_F_all, group = Country, colour = Country_sel)) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = Country)) +
  theme_bw(base_family = "Times New Roman") +
  scale_colour_manual(values = c(Mycol[1], Mycol[3]), name = "") +
  labs(x = "Completed cohort fertility rate", 
       y = "Average contribution of the component F to changes in completed cohort fertility") +
  theme(legend.position = "none")
ggsave("Output/Fig5_dot.eps", Fig5, device = cairo_ps, width = 8.5, height = 6)