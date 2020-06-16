install.packages()
install.packages("Matrix")

library(bacondecomp)
library(tidyverse)
library(haven)
library(lfe)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

castle <- read_data("castle.dta")

#--- global variables
crime1 <- c("jhcitizen_c", "jhpolice_c", 
            "murder", "homicide", 
            "robbery", "assault", "burglary",
            "larceny", "motor", "robbery_gun_r")

demo <- c("emo", "blackm_15_24", "whitem_15_24", 
          "blackm_25_44", "whitem_25_44")

# variables dropped to prevent colinearity
dropped_vars <- c("r20004", "r20014",
                  "r20024", "r20034",
                  "r20044", "r20054",
                  "r20064", "r20074",
                  "r20084", "r20094",
                  "r20101", "r20102", "r20103",
                  "r20104", "trend_9", "trend_46",
                  "trend_49", "trend_50", "trend_51"
)

lintrend <- castle %>%
  select(starts_with("trend")) %>% 
  colnames %>% 
  # remove due to colinearity
  subset(.,! . %in% dropped_vars) 

region <- castle %>%
  select(starts_with("r20")) %>% 
  colnames %>% 
  # remove due to colinearity
  subset(.,! . %in% dropped_vars) 


exocrime <- c("l_lacerny", "l_motor")
spending <- c("l_exp_subsidy", "l_exp_pubwelfare")


xvar <- c(
  "blackm_15_24", "whitem_15_24", "blackm_25_44", "whitem_25_44",
  "l_exp_subsidy", "l_exp_pubwelfare",
  "l_police", "unemployrt", "poverty", 
  "l_income", "l_prisoner", "l_lagprisoner"
)

law <- c("cdl")

dd_formula <- as.formula(
  paste("l_homicide ~ ",
        paste(
          paste(xvar, collapse = " + "),
          paste(region, collapse = " + "),
          paste(lintrend, collapse = " + "),
          paste("post", collapse = " + "), sep = " + "),
        "| year + sid | 0 | sid"
  )
)

#Fixed effect regression using post as treatment variable 
dd_reg <- felm(dd_formula, weights = castle$popwt, data = castle)
summary(dd_reg)

##CASTLE 2
castle <- castle %>%
  mutate(
    time_til = year - treatment_date,
    lead1 = case_when(time_til == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(time_til == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(time_til == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(time_til == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(time_til == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(time_til == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(time_til == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(time_til == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(time_til == -9 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(time_til == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(time_til == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(time_til == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(time_til == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(time_til == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(time_til == 5 ~ 1, TRUE ~ 0)
  )

event_study_formula <- as.formula(
  paste("l_homicide ~ + ",
        paste(
          paste(region, collapse = " + "),
          paste(paste("lead", 1:9, sep = ""), collapse = " + "),
          paste(paste("lag", 1:5, sep = ""), collapse = " + "), sep = " + "),
        "| year + state | 0 | sid"
  ),
)

event_study_reg <- felm(event_study_formula, weights = castle$popwt, data = castle)
summary(event_study_reg)

##CASTLE 3

# order of the coefficients for the plot
plot_order <- c("lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

# grab the clustered standard errors
# and average coefficient estimates
# from the regression, label them accordingly
# add a zero'th lag for plotting purposes
leadslags_plot <- tibble(
  sd = c(event_study_reg$cse[plot_order], 0),
  mean = c(coef(event_study_reg)[plot_order], 0),
  label = c(-9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

# This version has a point-range at each
# estimated lead or lag
# comes down to stylistic preference at the
# end of the day!
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before and after castle doctrine expansion") +
  ylab("log(Homicide Rate)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

##CASTLE 4

# This version includes
# an interval that traces the confidence intervals
# of your coefficients
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  # this creates a red horizontal line
  geom_hline(yintercept = 0.035169444, color = "red") +
  geom_line() + 
  geom_point() +
  geom_ribbon(alpha = 0.2) +
  theme_minimal() +
  # Important to have informative axes labels!
  xlab("Years before and after castle doctrine expansion") +
  ylab("log(Homicide Rate)") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

##CASTLE 5

library(bacondecomp)
library(lfe)

df_bacon <- bacon(l_homicide ~ post,
                  data = castle, id_var = "state",
                  time_var = "year")

# Diff-in-diff estimate is the weighted average of 
# individual 2x2 estimates
dd_estimate <- sum(df_bacon$estimate*df_bacon$weight)

# 2x2 Decomposition Plot
bacon_plot <- ggplot(data = df_bacon) +
  geom_point(aes(x = weight, y = estimate, 
                 color = type, shape = type), size = 2) +
  xlab("Weight") +
  ylab("2x2 DD Estimate") +
  geom_hline(yintercept = dd_estimate, color = "red") +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.background = element_rect(
      fill="white", linetype="solid"),
    legend.justification=c(1,1), 
    legend.position=c(1,1)
  )

bacon_plot

# create formula
bacon_dd_formula <- as.formula(
  'l_homicide ~ post | year + sid | 0 | sid')

# Simple diff-in-diff regression
bacon_dd_reg <- felm(formula = bacon_dd_formula, data = castle)
summary(bacon_dd_reg)

# Note that the estimate from earlier equals the 
# coefficient on post
dd_estimate
