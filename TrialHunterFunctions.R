outputTrials <- function(output,v){
  # Print out remaining trials in the client browser.
  # Used with shinny server only.
  # output: shinny server output
  # v: a list contain matrix(current matrix), trial(remaining trial) 
  # and EE1(top candidate).  # cat("Output Trials\n")
  
  prepareTable <- v$trial %>%
    arrange(Trial) %>%
    mutate(Trial = paste0('<a href="https://clinicaltrials.gov/ct2/show/',Trial,'" target="_blank">',Trial,'</a>')) %>%
    select(Trial)
  output$responsesTable <- renderDT(prepareTable, escape = FALSE)
  output$num_of_trial <- renderText({
    paste("There are ",dim(v$trial)[1]," remainning","\n")
  })
}

updateMatrix <- function(v,input){
  # Update matrix based on the user input.
  # input: a list contain header(/Yes/NO), 
  # status(char), value(numeric), time(numeric) 
  # and time_unit(days/weeks/months/years)
  # v: a list contain matrix(current matrix), trial(remaining trial) 
  # and EE1(top candidate).
  # cat("Update Matrix\n")
  
  # print(paste("header",input$header))
  # print(paste("status",input$status))
  # print(paste("value",input$value))
  # print(paste("time",input$time))
  
  # init ex_id for excluded trials.
  ex_id = tibble(Trial = character())
  name = unique(v$EE1$Name)
  cui = unique(v$EE1$CUI)
  class = unique(v$EE1$Class)
  
  header = input$header
  status = input$status
  value = input$value
  time = input$time
  
  # patch for measurement.
  if(class == "Measurement" & header == "No"){
    header = "I don't know"
  }
  
  # patch for age
  if(name == "Age" & header == "No"){
    header = "I don't know"
  }
  
  if(header == "I don't know"){
    # cat("User ignore the criteria!\n")
  }else{
    if(header == "No"){
      ########################################################  
      ######## remove trials where EI is include.
      ########################################################
      ex_id = v$matrix %>%
        filter(Name == name & EI == "INC") %>%
        select(Trial) %>%
        union(ex_id)
    }else{
      ########################################################  
      ######## remove trials where EI is exclude.
      ########################################################
      exc_matrix =  v$matrix %>%
        filter(Name == name & EI == "EXC")
      
      # same time or time is NA.
      time_ex_id = exc_matrix %>% 
        filter(Relation == "has_TempMea" & is.na(Value)) %>%
        select(Trial)
      if(!is.null(time) && time !=-999){
        # user does input his own time.
        time_unit = input$time_unit
        time = switch (time_unit,
                       "days" = time,
                       "weeks" = time * 7,
                       "month" = time * 30,
                       "year" = time * 365
        )
        # Value in has_TempMea is min:max. (e.g. 3:Inf)
        time_ex_id = exc_matrix %>% 
          filter(Relation == "has_TempMea" & !is.na(Value)) %>%
          separate(Value,c("Min","Max",":")) %>%
          mutate(Min = as.numeric(Min),Max = as.numeric(Max)) %>%
          filter(Min < time & Max > time) %>%
          select(Trial) %>% 
          bind_rows(time_ex_id)
      }
      # same status or status is NA.
      status_ex_id = exc_matrix %>% 
        filter(Relation == "modified_by" & is.na(Value)) %>%
        select(Trial)
      if(!is.null(status)){
        # user does check at lease one checkbox
        status_ex_id = exc_matrix %>% 
          filter(Relation == "modified_by" & !is.na(Value) & Value %in% status) %>%
          select(Trial) %>% 
          bind_rows(status_ex_id)
      }
      # same value or value is NA.
      value_ex_id = exc_matrix %>% 
        filter(Relation == "has_value" & is.na(Value)) %>%
        select(Trial)
      if(!is.null(value) && value !=-999){
        # user does input his own value
        
        # Value in has_value is min:max. (e.g. -Inf:5)
        value_ex_id = exc_matrix %>% 
          filter(Relation == "has_value" & !is.na(Value)) %>%
          separate(Value,c("Min","Max"),":") %>%
          mutate(Min = as.numeric(Min),Max = as.numeric(Max)) %>%
          filter(Min < value & Max > value) %>%
          select(Trial) %>% 
          bind_rows(value_ex_id)
      }
      
      # exclude trials with matched exclusion criteria.
      ex_id = time_ex_id %>% 
        intersect(value_ex_id) %>%
        intersect(status_ex_id) %>%
        union(ex_id)
      
      ########################################################  
      ######## remove trials where EI is included
      ########################################################
      inc_matrix =  v$matrix %>%
        filter(Name == name & EI == "INC")
      
      # diff time and time is not NA.
      if(!is.null(time) && time != -999){
        time_unit = input$time_unit
        time = switch (time_unit,
                       "days" = time,
                       "weeks" = time*7,
                       "month" = time*30,
                       "year" = time*365
        )
        
        time_ex_id = inc_matrix %>% 
          filter(Relation == "has_TempMea" & !is.na(Value)) %>%
          separate(Value,c("Min","Max",":")) %>%
          mutate(Min = as.numeric(Min),Max = as.numeric(Max)) %>%
          filter(Min > time | Max < time) %>%
          select(Trial)
      }
      # diff status and status is not NA.
      if(!is.null(status)){
        status_ex_id = inc_matrix %>% 
          filter(Relation == "modified_by" & !is.na(Value) & !(Value %in% status)) %>%
          select(Trial)
      }
      # diff value and valus is not NA.
      if(!is.null(value) && value != -999){
        value_ex_id = inc_matrix %>% 
          filter(Relation == "has_value" & !is.na(Value)) %>%
          separate(Value,c("Min","Max"),":") %>%
          mutate(Min = as.numeric(Min),Max = as.numeric(Max)) %>%
          filter(Min > value | Max < value) %>%
          select(Trial)
      }
      
      # exclude trials with mis-matched inclusion criteria
      ex_id = time_ex_id %>% 
        union(value_ex_id) %>%
        union(status_ex_id) %>%
        union(ex_id)
      # ex_id %>% print(n=10)
    }
  }
  
  
  
  # update global trial.
  v$trial = v$trial %>%
    anti_join(ex_id, by = "Trial")
  
  # update matrix by removing criteria.
  remove_criteria <- v$matrix %>%
    filter(Class == class & Name == name & CUI == cui)
  v$matrix = v$matrix %>%
    anti_join(remove_criteria, by = c("Class","Name","CUI"))
  
  # update matrix by removing excluded trials.
  v$matrix = v$matrix %>%
    anti_join(ex_id, by = "Trial")
  return(v)
}

calculateTopQuestions <- function(v){
  # Caculate the next question based on 
  # maximize the expected number of trials
  # removed.
  # 
  # ++++++++++++++++++++++++++++++++++
  # 2018-02-24
  # In current implementation, p is 0.5 
  # for both include and exclude. 
  # Therefore the selection criteria is 
  # to select criteria occurs most frequently.
  # ++++++++++++++++++++++++++++++++++
  #
  # v: a list contain matrix(current matrix), trial(remaining trial) 
  # and EE1(top candidate).
  
  # cat("Calculate Top Questions\n")
  
  #[1]Trial[2]Class[3]Name[4]CUI[5]Relation[6]Value[7]EI[8]p
  # Each criteria is repeated three times for Relation.
  # p is the probability the user's answer is yes.
  EE = v$matrix %>%
    group_by(.dots=c("Class","Name","CUI")) %>%
    summarise(YES_exclude = sum(EI=="EXC")/3,
              NO_exclude = sum(EI=="INC")/3,
              p = mean(p)) %>%
    mutate(expect_exclude = YES_exclude * p + NO_exclude * (1-p)) %>%
    arrange(-expect_exclude)
  
  # select the top one criteria.
  EE1 <- v$matrix %>% 
    right_join(EE[1,],by = c("Class","Name","CUI"))
  
  # EE1 %>% filter(!is.na(Value)) %>% print(n = 10)
  return(EE1)
}

updateUiQuestions <- function(v){
  # Generate questions based on top selected criteria.
  # Used with shinny server only. 
  # v: a list contain matrix(current matrix), trial(remaining trial) 
  # and EE1(top candidate).
  
  # cat("Update Ui Questions\n")
  
  # extract information for top selected criteria.
  EE1 <- v$EE1
  name <- unique(EE1$Name)
  class <- unique(EE1$Class)
  cui <- unique(EE1$CUI)
  status <- as.character(unlist(unique(EE1 %>% filter(Relation == "modified_by") %>% select(Value))))
  value <- as.character(unlist(unique(EE1 %>% filter(Relation == "has_value") %>% select(Value))))
  time <- as.character(unlist(unique(EE1 %>% filter(Relation == "has_TempMea") %>% select(Value))))
  
  insertHeader <- function(name,class){
    insertUI(selector = "#placeholder",
             where = "beforeBegin",
             radioButtons(inputId = "header",label = paste0("Do you have ",name),choices = c("I don't know","Yes","No")),
             immediate = TRUE)
  }
  insertStatus <- function(status,class){
    insertUI(selector = "#placeholder",
             where = "beforeBegin",
             checkboxGroupInput(inputId = "status",label = "Check the description match your status",choices = status[!is.na(status)]),
             immediate = TRUE)
  }
  insertTime <- function(class){
    sentence = switch (class,
                       "Condition" = "How long have been with the observation ?",
                       "Observation" = "How long have been with the condition ?",
                       "Drug" = "How long have you used the drug ?",
                       "Procedure" = "How long ago did you go through the last procedure ?",
                       "Measurement" = "How long ago did you go through the last measurement ?"
    )
    insertUI(selector = "#placeholder",
             where = "beforeBegin",
             tags$div(
               tags$label(sentence),
               fluidRow(
                 column(6,
                        numericInput(inputId = "time", label = '',value = -999)
                 ),
                 column(6,
                        selectInput(inputId = "time_unit", label = '', choices = c("days","weeks","months","years"))
                 )
               )
             ),
             immediate = TRUE)
  }
  insertValue <- function(class){
    insertUI(selector = "#placeholder",
             where = "beforeBegin",
             numericInput(inputId = "value", label = "What is the value",value = -999),
             immediate = TRUE)
  }
  
  # ask question for demographic
  if(class == "Demographic"){
    if(name == "Age"){
      insertUI(selector = "#placeholder",
               where = "beforeBegin",
               radioButtons(inputId = "header",label = paste0("Do you want to tell us your ",name),choices = c("I don't know","Yes","No")),
               immediate = TRUE)
      insertUI(selector = "#placeholder",
               where = "beforeBegin",
               numericInput(inputId = "value",label = "What is your age",value = -999),
               immediate = TRUE)
    }
  }
  
  # ask question for condition and observation.
  if(class %in% c("Condition","Observation")){
    insertHeader(name = name,class = class)
    if(sum(!is.na(status)) > 0 ){
      insertStatus(status = status, class = class)
    }
    if(sum(!is.na(value)) > 0){
      insertValue(class = class)
    }
    if(sum(!is.na(time)) > 0 ){
      insertTime(class = class)
    }
  }
  
  # ask question for drug and procedure
  if(class %in% c("Drug","Procedure")){
    insertHeader(name = name,class = class)
    if(sum(!is.na(status)) > 0 ){
      insertStatus(status = status, class = class)
    }
    if(sum(!is.na(value)) > 0){
      insertValue(class = class)
    }
    if(sum(!is.na(time)) > 0 ){
      insertTime(class = class)
    }
  }
  
  # ask question for Measurement
  if(class == "Measurement"){
    insertHeader(name = name,class = class)
    if(sum(!is.na(status)) > 0 ){
      insertStatus(status = status, class = class)
    }
    if(sum(!is.na(value)) > 0){
      insertValue(class = class)
    }
    if(sum(!is.na(time)) > 0 ){
      insertTime(class = class)
    }
  }
}

refreshPlaceHolder <- function(session){
  # refresh the placeholder if restart is clicked.
  # used only for shinny server.
  # session: shinny session.
  # cat("Refresh PlaceHolder\n")
  removeUI(selector = "div#uiInput",immediate = TRUE)
  insertUI(selector = "#start",where = "beforeBegin",
           ui = tags$div(id = "uiInput",tags$div(id = "placeholder")),
           immediate = TRUE)
  
  # javascript used to reset the input value to avoid the input value 
  # could not be destroyed even after ui removed.
  session$sendCustomMessage(type = "resetValue", message = "header")
  session$sendCustomMessage(type = "resetValue", message = "time")
  session$sendCustomMessage(type = "resetValue", message = "status")
  session$sendCustomMessage(type = "resetValue", message = "value")
  session$sendCustomMessage(type = "resetValue", message = "time_unit")
  
}

actionButton <- function(inputId, label, btn.style = "" , css.class = "") {
  if ( btn.style %in% c("primary","info","success","warning","danger","inverse","link")) {
    btn.css.class <- paste("btn",btn.style,sep="-")
  } else btn.css.class = ""
  
  tags$button(id=inputId, type="button", class=paste("btn action-button",btn.css.class,css.class,collapse=" "), label)
}
