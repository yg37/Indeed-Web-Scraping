#Initialize Shiny Package#
require(stringr)
require(rvest)
require(magrittr)
require(koRpus)
require(openNLP)
require(tm)
library(wordcloud)
library(shiny)

#Set conditional vectors for use in UI #
field = c("Statistics", "Computer Science", 
          "Management", "Accounting",
          "Marketing")
names(field) = field
states = read.table("states.txt",header = F, 
                    sep = "\n", stringsAsFactors = F) 
states = states$V1
names(states) = states

shinyUI(
  fluidPage(
    
    #Set Title Panel#
    titlePanel(
      "Beautiful Banana Job Search Engine"
    ),
    sidebarPanel(
      #Field Section#
      h3("Field"),
      
      #Selection Input for Prior on Number of Socks#
      selectInput("field_find", "Choose Your Interested Field", field),
      
      hr(),
      
      #Hyperparmeter Selection based on priors above#
      h3("Location:"),
      
      selectInput("loc_find", "Choose Your Interested States", states)
      
    ),
      
    mainPanel(
      
      #Additional Results View#
      h3("Key Words"),
      hr(),
      
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("plot")) 
      ),
      
      tabsetPanel(
        tabPanel("Table",
                 tableOutput("table"))
      )
    )
  )
)