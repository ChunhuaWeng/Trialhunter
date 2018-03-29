rm(list=ls())
######################################################################
# load library
######################################################################
library(dplyr)
library(shiny)
library(stringr)
library(tidyr)
library(DT)

######################################################################
# load data.
######################################################################
load("umls_table.rda")
clean_matrix <- umls_table %>%
  mutate(p = 0.5)

# matrix
# [1] "Trial"    "Class"    "Name"     "CUI"      "Relation" "Value"    "EI"      
# [8] "p"
# p = 0.5 for most value, p = ? for cathy's report.
# if Relation is modified_by, value could be flexible.
# if Relation is has_TempMea, value is number:number (days).
# if Relation is has_value, value is number:number.
######################################################################
# define function
######################################################################
source("TrialHunterFunctions.R")
######################################################################
# Trial Hunter service.
######################################################################
ui = bootstrapPage(
  shinyjs::useShinyjs(),
  theme = "bootstrap.min.css",
  fluidRow(
    column(11,offset = 1,
           HTML("<h3><Strong>TrialHunter</Strong>: dynamic questionnaire generation for efficient patient search of trials</h3>"),
           fluidRow(
             column(6,
                    DTOutput('responsesTable')
             ),
             column(5,
                    fluidRow(
                      column(12,
                             tags$div(id = "uiInput",tags$div(id = "placeholder"))
                             )
                    ),
                    fluidRow(
                      column(12,offset = 1,
                             actionButton(inputId = "start",label = "Start",btn.style = "primary",css.class = "btn-large"),
                             actionButton(inputId = "neext",label = "Next",btn.style = "warning",css.class = "btn-large")
                             )
                      
                    )
           )
        )
    )
  ),
  # javascript embedded.
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
              Shiny.onInputChange(variableName, null);
              });
              ")
)

server = function(input, output, session) {
  # init global var.
  v <- reactiveValues(matrix = clean_matrix,
                      trial = data.frame(Trial = unique(clean_matrix$Trial)),
                      EE1 = NULL)
  
  observeEvent(input$start,{
    # restart the value.
    v$matrix = clean_matrix
    v$trial = data.frame(Trial = unique(clean_matrix$Trial))
    # refresh the placeholder.
    refreshPlaceHolder(session)
  })
  
  # implement for Next button.
  observeEvent(input$neext,{
    
    # update Matrix and trial list if not the first next.
    if(!is.null(input$header) | !is.null(input$value)){
      v <- updateMatrix(v,input)
    }
    
    # show candidate trial list.
    outputTrials(output,v)
    
    # calculate top questions.
    v$EE1 <- calculateTopQuestions(v)

    # refresh the placeholder.
    refreshPlaceHolder(session)
    
    # update ui questions.
    updateUiQuestions(v)

  })
  
}

shinyApp(ui = ui, server = server)
