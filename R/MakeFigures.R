#### MAKE FIGURES ####
source("R/ImportData.R")

General <- survey %>% 
  filter(Bulk == "general") %>% 
  ungroup() %>% 
  #mutate(Question = factor(Question, levels = c("Idenitfy ressources", "Collect data", "Understand likelihood", "Analyse data", "Table and figure", "Present data", "Write result"))) %>% 
  # iris %>% rename(Answer = Species) %>% 
  ggplot(aes(x = Answer, fill = factor(Answer), group = Answer)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(1,4,3,2,5)]) +
  scale_x_continuous(breaks = 1:5, labels = scale1$EnglishText) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of students") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()
ggsave(filename = "General.png", device = "png", width = 6, height = 5, dpi = 300)


Attitude <- survey %>% 
  filter(Bulk == "attitude") %>% 
  mutate(Question = factor(Question, levels = c("8) I will benefit from stats",  "9) My stats will be better if I take more bio courses",  "10) My present understanding limits me to do well in biology",  "11) Stat problems make me nervous", "12) Stats will be important for my scientific career",  "13) Stats will be important for a daily bases",   "14) I would use a digital stats platform to become better", "15) I wish for more stats ressources to become better"))) %>% 
  ggplot(aes(x = Answer, fill = factor(Answer), group = Answer)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = wes_palette("Darjeeling1")[c(1,4,3,2,5)]) +
  scale_x_continuous(breaks = 1:5, labels = scale1$EnglishText) +
  scale_y_continuous(breaks = c(0, 20, 40)) +
  labs(x = "", y = "Number of students") +
  facet_wrap(~ Question, ncol = 1) +
  theme_bw()
ggsave(filename = "Attitude.png", device = "png", width = 6, height = 5, dpi = 300)
