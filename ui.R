#Initialize Shiny Package#
require(stringr)
require(rvest)
require(magrittr)
require(koRpus)
require(openNLP)
require(tm)
library(wordcloud)
library(shiny)

include = c("Yes" = "yes" , "No" = "no")

shinyUI(
  fluidPage(
    
    #Set Title Panel#
    titlePanel(
      "Beautiful Banana Job Search Engine"
    ),
    sidebarPanel(
      #Input the job field to search#
      h3("Field"),
      
      textInput("field", label = NULL, value = "", width = NULL),
      
      hr(),
      
      #Input the location to search
      h3("Location:"),
      
      textInput("loc", label = NULL, value = "", width = NULL),
      
      hr(),
      
      #Input the max number of word to include in the summary
      h3("Max Word:"),
      
      sliderInput("num", "Input the max number of words you want to show:",
                  min = 10, max = 100, value = 30),
      
      
      #To choose whether to inlcude a detailed table or not
      h3("Output Option"),
      
      selectInput("include", "Would you like to see the detailed word summary:", include)
    ),
      
    mainPanel(
      
      #Additional Results View#
      h3("Key Words"),
      hr(),
      #The panel to show the word cloud plot.
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("plot")) 
      ),
      
      #The panel to demonstrate the detailed table if user requires
      conditionalPanel(
        #If want summary table from Input, include the table#
        condition = "input.include == 'yes'",
        tabsetPanel(
          tabPanel("Detailed Word Summary",
                   tableOutput("table"))
        )
      )
    )
  )
)