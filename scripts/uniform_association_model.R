# Uniform association model
# Duncan, Otis Dudley. 1979. “How Destination Depends on Origin in the Occupational Mobility Table.” The American Journal of Sociology 84(4):793–803.

# packages
library(tidyverse)
library(MASS)
library(magrittr)
library(knitr)
library(vcdExtra)

# Data (p. 795)
tab <- matrix(
  c(50, 19, 26, 8, 7, 11, 6, 2,
    16, 40, 34, 18, 11, 20, 8, 3, 
    12, 35, 65, 66, 35, 88, 23, 21, 
    11, 20, 58, 110, 40, 183, 64, 32, 
    2, 8, 12, 23, 25, 46, 28, 12,
    12, 28, 102, 162, 90, 554, 230, 177,
    0, 6, 19, 40, 21, 158, 143, 71, 
    0, 3, 14, 32, 15, 126, 91, 106
    ),
  nrow = 8, 
  byrow = TRUE, 
  dimnames = list(1:8,1:8)) %>% 
  as.table() 
df <- as.data.frame(tab)
names(df) <- c("O","D","Freq")

# Row and column scores
df$U <- as.numeric(df$O)
df$V <- as.numeric(df$D)

# Diagonal parameters
df %<>% mutate(Diag = ifelse(U == V, U, 0) %>% factor())

# (1) Independence
fit_1 <- glm(Freq ~ O + D, data = df, family = poisson)
summary(fit_1)

# (2) Row effects
fit_2 <- glm(Freq ~ O + D + O:V, data = df, family = poisson)
summary(fit_2)

# (3) Quasi independence, diagonal omitted
fit_3 <- glm(Freq ~ O + D + Diag, data = df, family = poisson)
summary(fit_3)

# (4) Uniform association, diagonal omitted
fit_4 <- glm(Freq ~  O + D + U:V + Diag, data = df, family = poisson)
summary(fit_4)
# log-odds
fit_4$coefficients["U:V"] %>% exp()

# (5) Row effects, diagonal omitted* .
fit_5 <- glm(Freq ~ O + D + O:V + Diag, data = df, family = poisson)
summary(fit_5)
names(fit_5)
names(summary(fit_5))

# Table 2
Model <- c("Independence","Row effects","Quasi independence, diagonal omitted","Uniform association, diagonal omitted","Row effects, diagonal omitted")
df <- c(fit_1$df.residual,fit_2$df.residual,fit_3$df.residual,fit_4$df.residual,fit_5$df.residual)
X2 <- c(fit_1$deviance,fit_2$deviance,fit_3$deviance,fit_4$deviance,fit_5$deviance)
tibble(Model, df, X2) %>% 
  kable(digits = 1)

# Figure 2
b_4 <- fit_4$coefficients["U:V"] %>% "*"(7:0) %>% exp() 

b_5 <- fit_5$coefficients[grep("V", names(fit_5$coefficients))]
b_5 %<>% "*"(-1) %>% exp() %>% dplyr::recode(.missing = 1)

b_4 <- data.frame(y = b_4, model = 4, x = 1:8)
b_5 <- data.frame(y = b_5, model = 5, x = 1:8)
df <- bind_rows(b_4,b_5)
df$x <- factor(df$x)
df$model <- factor(df$model)

df %>% 
  mutate(x = factor(x, levels = 8:1)) %>%
  ggplot(aes(x = x, y = y, group = model, linetype = model)) + 
  geom_line() + 
  ylim(1,3) + 
  scale_linetype_manual(values = c("longdash","solid")) +
  labs(x = "FATHER'S OCCUPATION (i)", y = "b_i/b_8") +
  theme_classic() 

# mosaic plot
mosaic(fit_1, ~O+D, residuals_type = "rstandard")
mosaic(fit_2, ~O+D, residuals_type = "rstandard")
mosaic(fit_3, ~O+D, residuals_type = "rstandard")
mosaic(fit_4, ~O+D, residuals_type = "rstandard")
mosaic(fit_5, ~O+D, residuals_type = "rstandard")

