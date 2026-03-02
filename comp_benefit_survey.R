# Compensation and Benefits Survey

library(dplyr)
library(ggplot2)
library(tidyverse)

#load in data
survey = read.table("C:/Users/sav61051/OneDrive - University of Georgia/Desktop/comp_benefit_survey.txt",
                    sep = "\t", header = T)
survey$N = 1

#clean up data
survey = survey %>% 
  mutate(College = case_when(College == "" ~ "Unknown",
                             .default = as.character(College)))

#basic demographics ####
survey_demo = survey %>% 
  group_by(Degree, AssistYN, College) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Percent = (N/sum(N))*100)
survey_demo.d = survey %>% 
  group_by(Degree) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Percent = (N/sum(N))*100)
survey_demo.c = survey %>% 
  group_by(College) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Percent = (N/sum(N))*100)
survey_demo.a = survey %>% 
  group_by(AssistYN, Assistantship) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(Percent = (N/sum(N))*100)

survey_demo.d = survey_demo.d %>% 
  mutate(Degree = case_when(Degree == "" ~ "Unknown",
                            .default = as.character(Degree)))

#make a pi chart
survey_demo.c = survey_demo.c %>% 
  mutate(College = case_when(College == "Unknown" ~ "Other",
                             .default = as.character(College)))

ggplot(survey_demo.c, aes(x = "", y = N, fill = College)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(title = "Respondents by College") +
  theme_void()

#basic averages for all scale questions ####
survey_sum = survey %>% 
   summarize(Q1 = mean(Q1),
            Q2 = mean(Q2),
            Q3 = mean(Q3),
            Q4 = mean(Q4),
            Q5 = mean(Q5),
            Q6 = mean(Q6),
            Q7 = mean(Q7),
            Q8 = mean(Q8))

#make a chart
survey_q.l = survey[,1:9] %>% 
  pivot_longer(cols = Q1:Q8)
  
ggplot(survey_q.l, aes(x = name, y = value)) +
  geom_boxplot() +
  labs(title = "Responses Overall (n = 449)",
       x = "Question",
       y = "Scale") +
  theme_bw()

#by degree
survey_sum.d = survey %>% 
  group_by(Degree) %>% 
  summarize(Q1 = mean(Q1),
            Q2 = mean(Q2),
            Q3 = mean(Q3),
            Q4 = mean(Q4),
            Q5 = mean(Q5),
            Q6 = mean(Q6),
            Q7 = mean(Q7),
            Q8 = mean(Q8),
            N = sum(N))

#by assistantship
survey_sum.a = survey %>% 
  group_by(AssistYN) %>% 
  summarize(Q1 = mean(Q1),
            Q2 = mean(Q2),
            Q3 = mean(Q3),
            Q4 = mean(Q4),
            Q5 = mean(Q5),
            Q6 = mean(Q6),
            Q7 = mean(Q7),
            Q8 = mean(Q8),
            N = sum(N))
