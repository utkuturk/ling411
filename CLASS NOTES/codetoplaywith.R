# TO PLAY WITH INTERACTONS AND ALL OTHER NUMBERS.

library(magrittr)

# use sum contrasts (-.5 / .5) for three factors
contrasts <- list()
contrasts$a_mainef <- c(-1, -1,  1,  1) * 0.5
contrasts$b_mainef <- c(-1,  1, -1,  1) * 0.5
contrasts$ab_interaction <- with(contrasts, a_mainef * b_mainef)

labels <- list()
labels$A <- ifelse(contrasts$a_mainef == -.5, "A1", "A2")
labels$B <- ifelse(contrasts$b_mainef == -.5, "B1", "B2")

coefs <- list(a_mainef = 0, b_mainef = 1, ab_interaction = -10)

effects_byrow <- data.frame(a_mainef = contrasts$a_mainef * coefs$a_mainef, 
                            b_mainef = contrasts$b_mainef * coefs$b_mainef,
                            ab_interaction = contrasts$ab_interaction * coefs$ab_interaction
                            )
effects_byrow$y_pred <- rowSums(effects_byrow)

effects_byrow$A <- labels$A %>% as.factor
effects_byrow$B <- labels$B %>% as.factor 


library(ggplot2)

ggplot(effects_byrow, aes(x = A, y = y_pred, color = B, group = B)) + geom_point() + 
  geom_line() + scale_y_continuous(breaks = seq(-5, 5 , .5))




library(brms)
library(dplyr)
library(languageR)
dative_data <- languageR::dative %>% group_by(AnimacyOfRec, AnimacyOfTheme) %>%
                summarize(N = length(RealizationOfRecipient), N_NP = sum(RealizationOfRecipient == "NP"))
dative_data %<>% within({
  cAnimacyOfRec <- ifelse( AnimacyOfRec == "animate", .5, -.5)
  cAnimacyOfTheme <- ifelse( AnimacyOfTheme == "animate", .5, -.5)
})

# serial
m <- brms::brm(cbind(N, N_NP) ~ cAnimacyOfTheme + cAnimacyOfRec, dative_data, chain = 1, family = "binomial")
m
