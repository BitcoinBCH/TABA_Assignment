#Implemented By
#TABA Question 1
#11810057 Megha Goel
#11810099 Gaurav Borse
#11810036 Shariq Imam

if(!require(shiny)){install.packages("shiny")}
if(!require(shinydashboard)){install.packages("shinydashboard")}
if(!require(shinythemes)){install.packages("shinythemes")}
if(!require(udpipe)){install.packages("udpipe")}
if(!require(textrank)){install.packages("textrank")}
if(!require(lattice)){install.packages("lattice")}
if(!require(igraph)){install.packages("igraph")}
if(!require(ggraph)){install.packages("ggraph")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(stringr)){install.packages("stringr")}
if (!require(extrafont)){install.packages("extrafont")} 
if (!require(extrafontdb)){install.packages("extrafontdb")}
library(extrafont)
library(extrafontdb)
loadfonts(device = "win")
#font_import()
library(shiny)
library(shinydashboard)
library(shinythemes)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)

options(shiny.maxRequestSize=30*1024^2)

# Different language models 
# https://github.com/bnosac/udpipe.models.ud/tree/master/models

ui <- fluidPage(
  # Shiny theme
  theme = shinythemes::shinytheme("united"),
  # Application title
  titlePanel("Building a Shiny App around the UDPipe NLP workflow"),
  tags$style(type="text/css",
             ".shiny-output-error {visibility: hidden; }",
             ".shiny-output-error:before {visibility: hidden; }",
             ".sidebar-toggle{visibilty: hidden; }",
             ".skin-blue .main-header .navbar {background-color:#367fa9}",
             ".content-warpper, .right-side{ background-color:white ;}"
   ),
   tags$head(
     tags$title(
       ""
     )
  ),
  sidebarPanel(width =4,fileInput("file1","Choose txt file",
                                    multiple = TRUE,
                                    accept = c("*.txt")),
                 fileInput("model","Choose a model",
                           multiple = FALSE,
                           accept = ".udipipe"),
                 checkboxGroupInput("POS","Parts of Speech",
                                    c("1. Adjective (JJ)"="JJ",
                                      "2. Noun (NN)"="NN",
                                      "3. Proper Noun (NNP)"="NNP",
                                      "4. Adverb (RB)"="RB",
                                      "5. Verb (VB)"="VB",
                                      "6. Adjective"="ADJECTIVE",
                                      "7. Noun"="NOUN",
                                      "8. Proper Noun"="PROPER NOUN",
                                      "9. Adverb"="ADVERB",
                                      "10. Verb"="VERB"),selected = c("JJ","NN","NNP")),
                 sliderInput("max","Minimum frequency in the Co Occurence graph:",min = 0,max= 200,value =2)
            ),
      mainPanel(
          tabsetPanel(type = "tabs",
                              tabPanel("Overview",
                                       h4(p("Steps to use this App")),
                                       p("* This app supports only text file i.e. (.txt file), we've added this validation while we click on the Data Tab not in Model & Co Occurence graph.",align="justify"),
                                       p("* Please run the below command from R Console, which is responsible for loading the dependencies related to this App."),
                                       p(' source("https://raw.githubusercontent.com/BitcoinBCH/TABA_Assignment/master/dependencies.R")'),
                                       p("* Now browse the text file."),
                                       p("* Browse the UDPipe model. We've provided the UDPipe model for English, Hindi & Spanish in mail, we couldn't uploaded in the github account as the file size is more."),
                                       p("* While selecting the Part of Speech, you need select in the following ways"),
                                       p("* By Default XPOS will be selected, i.e. JJ,NN & NNP, but when you select the spanish text file and it's model, then you need select the normal Adjective, Noun, etc. UPOS from 6 point onwards."),
                                       p("* Select the number of word Frequency which we will appear on the Co Occurence graph"),
                                       p("Now traverse through the tabs within the application")
                              ),
                              tabPanel("Data",
                                       verbatimTextOutput("data")),
                              tabPanel("Model",
                                       verbatimTextOutput("model")),
                              tabPanel("Co Occurence graph", 
                                       plotOutput("occur"),
                                       verbatimTextOutput("info"))
                  ) # end of tabsetPanel
                )
)

server <- function (input,output ,session){
  
  readContent <- reactive({
    count <- nchar(input$file1$datapath)
    fileTextType <- substr(input$file1$datapath, (count - 2) , nchar(input$file1$datapath))
    fileTextname <- substr(input$file1$datapath, (count - 8) , nchar(input$file1$datapath))
    if (fileTextType != 'txt') {
      errorMessage = "Please upload the file which is of .txt extension"
      errorMessage
    } else {
      errorMessage = "";errorMessage;
      datacontent <- readLines(input$file1$datapath)
    }
  })
  
  datacollection <- reactive({
    req(input$file1)
    readContent()
    req(input$model)
    mod <- input$model
    ud_model <- udpipe_load_model(mod$datapath)
    datastore <- udpipe_annotate(ud_model,x=readContent())
    datastore <- as.data.frame(datastore)
    return(datastore)
  })
  
  datamodel <-reactive({
    
    getdata <- datacollection()
    language = gregexpr("spanish", input$file1$name,ignore.case = FALSE)
    if (language == 1) {
      collect <- subset(getdata, upos %in% input$POS)
    } else {
      collect <- subset(getdata, xpos %in% input$POS)
    }
    return (collect)
  })
  
  output$data <- renderText({     fileText <- paste(readContent(), collapse = "\n");     fileText   ;})
  output$model <- renderText({     fileText <- paste(datamodel(), collapse = "\n");     fileText   ;})
  output$occur <- renderPlot({
    readContent()
    subcontent = datamodel()
    occur <- cooccurrence( x= subcontent,
                          term = "lemma",
                          group = c("doc_id","paragraph_id","sentence_id"))
    wordnetwork <- head(occur,input$max)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
      ggraph(wordnetwork,layout= "fr")+
      geom_edge_link(aes(width=cooc,edge_alpha= cooc),edge_colour= "blue")+
      geom_node_text(aes(label=name),col= "darkgreen",size =5) +
      theme_graph(base_family = "Arial Narrow")+
      theme(legend.position = "none")+
      labs(title= "Coocurence grah of Words")
  })
}
shinyApp(ui=ui,server=server)