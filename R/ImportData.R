#### IMPORT DATA ####

library("tidyverse")
library("wesanderson")

data <- read_csv2(file = "data/dataset_uten brukernavn.csv", skip_empty_rows = TRUE)
scale <- read.csv2(file = "data/labels.csv", header = FALSE)


scale1 <- scale %>% 
  filter(V1 %in% c("s_3")) %>% 
  select(-V1) %>% 
  rename("Numeric" = "V2", "Text" = "V3") %>% 
  mutate(EnglishText = c("Not at all", "Little", "Moderately", "Very much", "Completely"))


questions <- tibble(ID = c("s_8", "s_60", "s_10", "s_11",  "s_64",  "s_63",  "s_12",  
                           "s_65",  "s_66",  "s_67",  "s_68",  "s_69",  "s_70",  "s_71",  "s_72"),
       Question = c("6) interpret tables and figures", "5) present data in a figure", "2) collect scientific data", "4) analyse data",  "1) idenitfy statistical ressources to interpret data",  "3) understand the term likelihood",  "7) present results written or oral",  
                    "10) My present understanding limits me to do well in biology",  "8) I will benefit from stats",  "12) Stats will be important for my scientific career",  "13) Stats will be important for a daily bases",  "11) Stat problems make me nervous",  "9) My stats will be better if I take more bio courses",  "15) I wish for more stats ressources to become better",  "14) I would use a digital stats platform to become better"), 
       Bulk = c(rep("general", 7), rep("attitude", 8)))

survey <- data %>% 
  select(s_8, s_60, s_10, s_11:s_12, s_65:s_72, s_57:stato_5) %>% 
  drop_na() %>% 
  mutate(s_57 = case_when(s_57 %in% c("Kvinne", "kvinne", "Jente", "Dame", "Hunkjonn") ~ "female",
                          s_57 %in% c("Mann", "mann", "Mannj", "Gutt", "Hannkjonn") ~ "male",
                          TRUE ~ as.character(s_57))) %>% 
  rowid_to_column(var = "StudentID") %>% 
  pivot_longer(cols = c(s_8:s_72), names_to = c("ID"), values_to = c("Answer")) %>%
  left_join(questions, by = "ID") %>% 
  left_join(scale1, by = c("Answer" = "Numeric")) %>% 
  select(StudentID, ID:EnglishText, s_57:stato_5) %>% 
  rename("AnswerEnglish" = "EnglishText", "AnswerNorw" = "Text" , "sex" = "s_57", "age" = "s_37", "year_higher_education" = "s_38", "Nr_parents_higher_education" = "s_58", "Grades_ok" = "s_39", "Status_new" = "stato_1", "Status_distributed" = "stato_2", "Status_some" = "stato_3", "Status_finished" = "stato_4", "Status_lost" = "stato_5") %>% 
  mutate(AnswerEnglish = factor(AnswerEnglish, levels = c("Not at all", "Little", "Moderately", "Very much", "Completely")),
         AnswerNorw = factor(AnswerNorw, levels = c("Ikke i det hele tatt", "Litt", "Moderat", "Veldig", "Ekstremt"))) 



survey %>% 
  filter(Bulk == "meta") %>% 
  ggplot(aes(y = Answer, x = Question)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "")

meta <- survey %>% 
  filter(Bulk == "meta") %>% 
  ungroup()

meta %>% distinct(StudentID) # 66 participants

meta %>% filter(Question == "sex") %>% count(Answer) # 34 Femal, 32 Male
meta %>% filter(Question == "age") %>% count(Answer) # between 19 and 31 years
meta %>% filter(Question == "Nr of parents with higher education") %>% count(Answer) # 

# Education
# 1/første: 53
# 2 3
# 3 3
# 4 3
# 5 1
# NA 3

