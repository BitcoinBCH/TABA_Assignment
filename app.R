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
                 checkboxGroupInput("POS","POS",
                                    c("Adjective"="JJ",
                                      "Noun"="NN",
                                      "Proper Noun"="NNP",
                                      "Adverb"="RB",
                                      "Verb"="VB"),selected = c("JJ","NN","NNP")),
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
                                       p("* Select the Part of Speech"),
                                       p("* Select the number of word Frequency which we will appear on the Co Occurence graph"),
                                       p("Now traverse through the tabs within the application")
                              ),
                              tabPanel("Data",
                                       verbatimTextOutput("x")),
                              tabPanel("Model",
                                       verbatimTextOutput("sub")),
                              tabPanel("Co Occurence graph", 
                                       plotOutput("cooc"),
                                       verbatimTextOutput("info"))
                  ) # end of tabsetPanel
                )
)

server <- function (input,output ,session){
  
  readContent <- reactive({
    count <- nchar(input$file1$datapath)
    fileTextType <- substr(input$file1$datapath, (count - 2) , nchar(input$file1$datapath))
    if (fileTextType != 'txt') {
      errorMessage = "Please upload the file which is of .txt extension"
      errorMessage
    } else {
      errorMessage = "";errorMessage;
      datacontent <- readLines(input$file1$datapath)
    }
  })
  
  x <- reactive({
    req(input$file1)
    readContent()
    req(input$model)
    mod <- input$model
    ud_model <- udpipe_load_model(mod$datapath)
    x <- udpipe_annotate(ud_model,x=readContent())
    x <- as.data.frame(x)
    return(x)
  })
  
  sub <-reactive({
    x <- x()
    sub <- subset(x, xpos %in% input$POS)
    sub
    return(sub)
  })
  
  output$x <- renderText({     fileText <- paste(readContent(), collapse = "\n");     fileText   ;})
  output$sub <- renderText({     fileText <- paste(sub(), collapse = "\n");     fileText   ;})
  output$cooc <- renderPlot({
    readContent()
    sub = sub()
    cooc <- cooccurrence( x= sub,
                          term = "lemma",
                          group = c("doc_id","paragraph_id","sentence_id"))
    wordnetwork <- head(cooc,input$max)
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