### Data analysis

source("R/ImportData.R")

subset <- survey %>%
  filter(Question == "6) interpret tables and figures")

survey %>% 
  group_by(Question)
  
mod <- MASS::polr(AnswerEnglish ~ 1, data = subset, Hess = TRUE)
summary(mod)
coeffs <- coef(summary(mod))
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))
