### Data analysis
library("broom")

source("R/ImportData.R")

# Only quesitons
survey %>% 
  group_by(Question) %>% 
  nest() %>%
  mutate(mod = map(data, ~ MASS::polr(AnswerEnglish ~ 1, data = .x, , Hess = TRUE)), 
         result = map(mod, tidy),
         sum = map(mod, summary),
         coeffs = map(sum, coef)) %>% 
  unnest(coeffs)
  

# Sex, age and education
survey %>% 
  group_by(Question) %>% 
  nest() %>%
  mutate(mod = map(data, ~ MASS::polr(AnswerEnglish ~ sex + age + year_higher_education, data = .x, , Hess = TRUE)), 
         result = map(mod, tidy)) %>% 
  unnest(result) %>% 
  mutate(p.value = pnorm(abs(statistic), lower.tail = FALSE) * 2,
         CI.lower = estimate - 1.96 * std.error,
         CI.higher = estimate + 1.96 * std.error,
         OverlapZero = case_when(CI.lower > 0 & CI.higher > 0 ~ "no",
                                 CI.lower < 0 & CI.higher < 0 ~ "no",
                                 TRUE ~ "yes")) %>% 
  filter(OverlapZero == "no")
  
# 6) sexmale
# 4) year
# 7) year
# 14) age + year


survey %>% 
  filter(Question == "6) interpret tables and figures") %>% 
  ggplot(aes(x = AnswerEnglish)) +
  geom_bar(show.legend = FALSE) +
  facet_grid(~ sex)

survey %>% 
  filter(Question == "4) analyse data") %>% 
  ggplot(aes(x = AnswerEnglish)) +
  geom_bar(show.legend = FALSE) +
  facet_grid(~ year_higher_education)

survey %>% 
  filter(Question == "7) present results written or oral") %>% 
  ggplot(aes(x = AnswerEnglish)) +
  geom_bar(show.legend = FALSE) +
  facet_grid(~ year_higher_education)

survey %>% 
  filter(Question == "14) I would use a digital stats platform to become better") %>% 
  ggplot(aes(x = AnswerEnglish)) +
  geom_bar(show.legend = FALSE) +
  facet_grid(age ~ year_higher_education)


# initial analysis
subset <- survey %>%
  filter(Question == "6) interpret tables and figures")
mod <- MASS::polr(AnswerEnglish ~ sex + age + year_higher_education, data = subset, Hess = TRUE)
summary(mod)
tidy(mod)
confint(mod)
coeffs <- coef(summary(mod))
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))
