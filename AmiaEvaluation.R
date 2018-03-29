rm(list=ls())
######################################################################
# load library
######################################################################
library(dplyr)
library(tidyr)
library(janitor)
library(data.table)
library(ggplot2)
######################################################################
# define function
######################################################################
source("TrialHunterFunctions.R")
simulateInput <- function(EE1){
  name = unique(EE1$Name)
  class = unique(EE1$Class)
  cui = unique(EE1$CUI)
  
  input = list()
  # sample header
  input$header = sample(c("Ignore","Yes","No"),1,prob = c(0.0,0.5,0.5))
  if(class %in% c("Measurement","Demographic")){
    input$header = "Yes"
  }
  
  # sample status
  status_set = as.character(unlist(unique(EE1 %>% filter(Relation == "modified_by") %>% select(Value))))
  if(sum(!is.na(status_set)) > 0){
    status_set = status_set[!is.na(status_set)]
    status_null = sample(x = c(TRUE,FALSE),size = 1,prob = c(0.0,1.0))
    if(status_null){
      input$status = NULL
    }else{
      input$status = sample(x = status_set,size = sample(1:length(status_set))[1],replace = F)
    }
  }else{
    input$status = NULL
  }
  
  
  # sample value
  value_set <- as.character(unlist(unique(EE1 %>% filter(Relation == "has_TempMea") %>% select(Value))))
  if(sum(!is.na(value_set)) > 0){
    value_set = value_set[!is.na(value_set)]
    value_range = as.numeric(unlist(strsplit(x = value_set,split = ":")))
    value_range = value_range[value_range != Inf & value_range != -Inf]
    value_null = sample(x = c(TRUE,FALSE),size = 1,prob = c(0.2,0.8))
    if(value_null){
      input$value = NULL
    }else{
      value_min = min(value_range) - 10
      value_max = max(value_range) + 10
      input$value = runif(n = 1,min = value_min,max = value_max)
    }
  }else{
    input$value = NULL
  }
  
  # sample time
  time_set <- as.character(unlist(unique(EE1 %>% filter(Relation == "has_TempMea") %>% select(Value))))
  if(sum(!is.na(time_set)) > 0){
    time_set = time_set[!is.na(time_set)]
    time_range = as.numeric(unlist(strsplit(x = time_set,split = ":")))
    time_range = time_range[time_range != Inf & time_range != -Inf]
    
    time_null = sample(x = c(TRUE,FALSE),size = 1,prob = c(0.2,0.8))
    if(time_null){
      input$time = NULL
    }else{
      time_min = min(time_range) - 10
      time_max = max(time_range) + 10
      input$time = runif(n = 1,min = time_min,max = time_max)
      input$time_unit = "days"
    }
  }else{
    input$time = NULL
  }
  
  return(input)
}
calculateTopQuestionsRandom <- function(v){
  # Caculate the next question randomly.
  #
  # v: a list contain matrix(current matrix), trial(remaining trial) 
  # and EE1(top candidate).
  
  # cat("Calculate Top Questions\n")
  
  #[1]Trial[2]Class[3]Name[4]CUI[5]Relation[6]Value[7]EI[8]p
  # Each criteria is repeated three times for Relation.
  # p is the probability the user's answer is yes.
  EE = v$matrix %>% 
    group_by(.dots=c("Class","Name","CUI")) %>%
    summarise(YES_exclude = 1, NO_exclude = 1, p = 1, expect_exclude = 1)
  
  index = sample(1:dim(EE)[1])[1]
  # select the top one criteria.
  EE1 <- v$matrix %>% 
    right_join(EE[index,],by = c("Class","Name","CUI"))
  
  # EE1 %>% filter(!is.na(Value)) %>% print(n = 10)
  return(EE1)
}

######################################################################
# load data.
######################################################################
load("umls_table.rda")
######################################################################
# simulate q-a-u iteration.
######################################################################
boot = 0
set.seed(1)
remain_trial_1 = tibble(q_num = integer(),trial_num = integer())
remain_trial_2 = tibble(q_num = integer(),trial_num = integer())

while(boot < 3){
  boot = boot + 1
  print(boot)
  
  # top questions.
  v1 = list()
  v1$matrix <- umls_table %>%
    mutate(p = 0.5)
  v1$trial = data.frame(Trial = unique(v1$matrix$Trial))
  v1$remaining = dim(v1$trial)[1]
  
  i = 0
  while(v1$remaining > 0 & dim(v1$matrix)[1] > 0){
    i = i + 1
    v1$EE1 = calculateTopQuestions(v = v1)
    input = simulateInput(v1$EE1)
    v1 = updateMatrix(v1,input)
    v1$remaining = dim(v1$trial)[1]
    remain_trial_1 = tibble(q_num = i, trial_num = v1$remaining) %>%
      bind_rows(remain_trial_1)
  }
  
  # random questions.
  v2 = list()
  v2$matrix <- umls_table %>%
    mutate(p = 0.5)
  v2$trial = data.frame(Trial = unique(v2$matrix$Trial))
  v2$remaining = dim(v2$trial)[1]
  
  i = 0
  while(v2$remaining > 0 & dim(v2$matrix)[1] > 0){
    i = i + 1
    v2$EE1 = calculateTopQuestionsRandom(v = v2)
    input = simulateInput(v2$EE1)
    v2 = updateMatrix(v2,input)
    v2$remaining = dim(v2$trial)[1]
    remain_trial_2 = tibble(q_num = i, trial_num = v2$remaining) %>%
      bind_rows(remain_trial_2)
  }
}

######################################################################
# plot result
######################################################################
input_1 = remain_trial_1 %>% 
  group_by(q_num) %>% 
  summarise(mean = mean(trial_num),sd = sd(trial_num)) %>%
  mutate(method = "TH") %>%
  arrange(q_num)

input_2 = remain_trial_2 %>% 
  group_by(q_num) %>% 
  summarise(mean = mean(trial_num),sd = sd(trial_num)) %>%
  mutate(method = "RND") %>%
  arrange(q_num)

plot_input = input_1 %>%
  bind_rows(input_2)
plot_input %>% ggplot(aes(x = q_num,y = mean,color = method)) + 
  geom_line()

plot_input %>% 
  select(q_num,mean,method) %>%
  spread(method,mean) %>%
  mutate(fc = RND/TH) %>%
  select(fc) %>%
  unlist() %>%
  mean()

plot_input %>% 
  select(q_num,mean,method) %>%
  spread(method,mean) %>%
  filter(q_num %in% c(1,2,3,4,seq(from = 5,to =40,by = 5))) %>%
  write.csv(file = "AmiaTable2.csv")