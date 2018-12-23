#Implemented By
#TABA Question 1
# PGID - 11810057 Name- Megha Goel
# PGID - 11810099 Name- Gaurav Borse
# PGID - 11810036 Name- Shariq Imam

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
               fileInput("languageModel","Choose a Model",
                         multiple = FALSE,
                         accept = ".udipipe"),
               checkboxGroupInput("POS","Parts of Speech",
                                  c("1. Adjective (JJ)"="JJ",
                                    "2. Noun (NN)"="NN",
                                    "3. Proper Noun (NNP)"="NNP",
                                    "4. Adverb (RB)"="RB",
                                    "5. Verb (VB)"="VB"),selected = c("JJ","NN","NNP")),
               sliderInput("max","Minimum frequency in the Co Occurence graph:",min = 0,max= 200,value =2)
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Overview",
                         h4(p("Implemented By")),
                         p('Megha Goel - 11810057'),
                         p('Gaurav Borse - 11810099'),
                         p('Shariq Imam - 11810036'),
                         br(),
                         h4(p("Steps to use this App")),
                         p("* This app supports only text file i.e. (.txt file), we've added this validation while we click on the Data Tab.",align="justify"),
                         p("* Please run the below command from R Console, which is responsible for loading the dependencies related to this App."),
                         p(' source("https://raw.githubusercontent.com/BitcoinBCH/TABA_Assignment/master/dependencies.R")'),
                         p('runGitHub("TABA_Assignment","BitcoinBCH")'),
                         br(),
                         p("* Now browse the text file."),
                         p("* Browse the UDPipe model. We've provided the UDPipe model for English, Hindi & Spanish in mail, we couldn't uploaded in the github account as the file size is more."),
                         p("* Select Parts of speech, For English and Hindi select from XPOS, and Remaining language select from UPOS"),
                         p("* Select the number of word Frequency which we will appear on the Co Occurence graph"),
                         p("Now traverse through the tabs within the application"),
                         p("In Word Cloud make the selection for verb & Adverb check box, then it'll appear. We've done only for Noun, Adverb and Verb")
                ),
                tabPanel("Data",
                         verbatimTextOutput("data")),
                tabPanel("Annotate",
                         dataTableOutput("languageModel")),
                tabPanel("Co Occurence graph", 
                         plotOutput("occur"),
                         verbatimTextOutput("info")),
                tabPanel("Word Cloud",
                         h4("Nouns"),
                         plotOutput('wordcloudnoun'),
                         h4("Verbs"),
                         plotOutput('wordcloudverb'),
                         h4("Adverb"),
                         plotOutput('wordcloudadverb'))
    ) # end of tabsetPanel
  )
)

server <- function (input,output ,session){
  observe({
    if(is.null(input$languageModel)){
      return(NULL)
    }
    else{
      languageModel = gregexpr("spanish", input$languageModel$name,ignore.case = FALSE)
      if (languageModel == 1) {
        updateCheckboxGroupInput(session, "POS",
                                 label = paste("Parts of Speech (UPOS)"),
                                 choices =c("1. Adjective (ADJ)"="ADJ",
                                            "2. Noun (NOUN)"="NOUN",
                                            "3. Proper Noun (PROPN)"="PROPN",
                                            "4. Adverb (RB)"="ADV",
                                            "5. Verb (VERB)"="VERB"),
                                 selected = c("ADJ","NOUN","PROPN"))
      }
      else{
        updateCheckboxGroupInput(session,"POS",
                                 label = paste("Parts of Speech (XPOS)"),
                                 choices =c("1. Adjective (JJ)"="JJ",
                                            "2. Noun (NN)"="NN",
                                            "3. Proper Noun (NNP)"="NNP",
                                            "4. Adverb (RB)"="RB",
                                            "5. Verb (VB)"="VB"),
                                 selected = c("JJ","NN","NNP"))
      }
    }
  })
  
  readContent <- reactive({
    count <- nchar(input$file1$datapath)
    fileTextType <- substr(input$file1$datapath, (count - 2) , nchar(input$file1$datapath))
    #fileTextname <- substr(input$file1$datapath, (count - 8) , nchar(input$file1$datapath))
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
    req(input$languageModel)
    mod <- input$languageModel
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
  output$languageModel <- renderDataTable({     fileText <- datamodel();     return(fileText)   ;})
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
  
  output$wordcloudnoun = renderPlot({
    if ('NOUN' %in% input$POS || 'NN' %in% input$POS) {
      languageModel = gregexpr("spanish", input$languageModel$name,ignore.case = FALSE)
      
        noun = datacollection() %>% subset(., upos %in% "NOUN")
      
      max_word = txt_freq(noun$lemma)
      
      wordcloud(words = max_word$key, 
                freq = max_word$freq, 
                min.freq = 0, 
                max.words = 50,
                random.order = FALSE, 
                colors = brewer.pal(6, "Dark2"))
    }
  }) 
  
  output$wordcloudverb = renderPlot({
    if ('VERB' %in% input$POS || 'VB' %in% input$POS) {
      languageModel = gregexpr("spanish", input$languageModel$name,ignore.case = FALSE)
      
      verb = datacollection() %>% subset(., upos %in% "VERB")
      
      max_word = txt_freq(verb$lemma)
      
      wordcloud(words = max_word$key, 
                freq = max_word$freq, 
                min.freq = 1, 
                max.words = 100,
                random.order = FALSE, 
                colors = brewer.pal(6, "Dark2"))
    }
  })
  output$wordcloudadverb = renderPlot({
    if ('ADV' %in% input$POS || 'RB' %in% input$POS) {
      languageModel = gregexpr("spanish", input$languageModel$name,ignore.case = FALSE)
      
      adverb = datacollection() %>% subset(., upos %in% "ADV")
      
      max_word = txt_freq(adverb$lemma)
      
      wordcloud(words = max_word$key, 
                freq = max_word$freq, 
                min.freq = 1, 
                max.words = 100,
                random.order = FALSE, 
                colors = brewer.pal(6, "Dark2"))
    }
  })
  
}
shinyApp(ui=ui,server=server)