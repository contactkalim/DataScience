#---------------------------------------------------------------------#
#              Shiny App                                              #
#This app performs Universal part-of-speech tagging on input text file#
#---------------------------------------------------------------------#
if (!require(shiny)){install.packages("shiny")}
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(ggplot2)){install.packages("ggplot2")}
if (!require(wordcloud)){install.packages("wordcloud")}
if (!require(readtext)){install.packages("readtext")}
if (!require(DT)){install.packages("DT")}
if (!require(stringr)){install.packages("stringr")}
if (!require(shinyjs)){install.packages("shinyjs")}
if (!require(toaster)){install.packages("toaster")}

library(DT)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(shiny)
library(readtext)
library(wordcloud)
library(shinyjs)
library(toaster)



shinyUI <-
  fluidPage(
    titlePanel("POS Tagging"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        tags$style(type="text/css", "
             #loadmessage {
                   position: fixed;
                   top: 0px;
                   left: 0px;
                   width: 100%;
                   padding: 5px 0px 5px 0px;
                   text-align: center;
                   font-weight: bold;
                   font-size: 100%;
                   color: #000000;
                   background-color: #CCFF66;
                   z-index: 105;
                   }
                   "),
        fileInput("file1", "Upload data (Any english text)"),
        checkboxGroupInput("selectionops", "Select part-of-speech tags",
                           c("adjective (ADJ)" ="ADJ", "noun (NOUN)" ="NOUN",
                             "proper noun (PROPN)" ="PROPN", "adverb (ADV)" = "ADV",
                             "verb (VERB)" = "VERB"), selected = c('ADJ', 'NOUN', 'PROPN')),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Please wait Loading data...",id="loadmessage"))),
      mainPanel(
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app performs Universal part-of-speech tagging on input text file.",align="justify"),
                             p("This app supports only English language text file.",align="justify"),
                             p("Please refer to the link below for sample text file."),
                             a(href="https://github.com/contactkalim/ShinyApp_POSTagging/The_Sack_of_Monte_Carlo.txt"
                               ,"Sample data input file"),   
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (text file with english text)")),
                               'and upload the text data file.')),
                    tabPanel("Annotated documents", br(), p("If running from github, the downloaded file will in folder similar to C:/Users/UserName/AppData/Local/Temp/Rtmp2RS6su/shinyapp186027c7a14/ShinyApp_POSTagging-master",align="justify"),
                             DT::dataTableOutput("selectedfile"), actionButton("downloadData", "Download as csv" )),
                    
                    tabPanel("WordClouds - nouns and verbs",br(),p("Word Cloud - Nouns.",align="justify"),
                             plotOutput('Wordcloudnouns'),br(),p("Word Cloud - Verbs",align="justify"),plotOutput('Wordcloudverbs')),
                    
                    tabPanel("Top-30 co-occurrences",br(),p("Top-30 co-occurrence for selected options.",align="justify"),
                             plotOutput('Coocgraph'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
#) # end of UI


shinyServer <- function(input, output) {
  Dataset <- reactive({
    
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return(NULL) } else{
        
        textfile <- readtext(input$file1$datapath)
        Data <- textfile$text[1]
        Data <- stringr::str_replace_all(Data,"[^a-zA-Z.\\s]", "") # keep only characters
        english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe") 
        x <- udpipe_annotate(english_model, Data) 
        x <- as.data.frame(x)
        x$sentence <- NULL 
        return(head(x,100))
      }
  })
  
  Datawithtext <- reactive({
    
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return(NULL) } else{
        
        textfile <- readtext(input$file1$datapath)
        Data <- textfile$text[1]
        Data <- stringr::str_replace_all(Data,"[^a-zA-Z.\\s]", "") # keep only characters
        english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe") 
        x <- udpipe_annotate(english_model, Data) 
        x <- as.data.frame(x)
        return(x)
      }
  })
  nouns <- reactive({
    x <- Datawithtext()
    all_nouns = x %>% subset(., upos %in% "NOUN")
    top_nouns = txt_freq(all_nouns$lemma)
    return(top_nouns)
  })
  verbs <- reactive({
    x <- Datawithtext()
    all_verbs = x %>% subset(., upos %in% "VERB") 
    top_verbs = txt_freq(all_verbs$lemma)
    return(top_verbs)
  })
  Coocsselected <- reactive({
    x <- Datawithtext()
    cooc <- cooccurrence(   	# try `?cooccurrence` for parm options
      x = subset(x, upos %in% Selections()), #c("ADJ", "NOUN", "PROPN")
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id"))  # 0.02 secs
    return(cooc)
  })
  Selections <- reactive({
    x <- vector()
    x <- c(x, input$selectionops)
    return(x)
  })
  
  
  # Calc and render plot  
  output$selectedfile <- DT::renderDataTable({
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return() } else{
        DT::datatable(Dataset(), options = list(orderClasses = TRUE))}
  })
  # output$downloadData <- downloadHandler(filename = function() {
  #         paste('data-', Sys.Date(), '.csv', sep='')
  #       },
  #       content = function(con) {
  #         write.csv(data, con)
  #       })
  observe({
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return(NULL) } else{
        if (input$downloadData>0){
          fname <- paste(gsub("\\..*","",input$file1$name), Sys.Date(), '.csv', sep='')
          datafordownload <- Datawithtext()
          datafordownload$sentence <- NULL
          write.csv(datafordownload,fname)
        }
      }
  })
  
  output$Wordcloudnouns <- renderPlot({
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return() } else{
        v <- nouns()
        createWordcloud(v$key, v$freq, scale=c(4,0.5),
                        minFreq = 2, maxWords=100,
                        palette = brewer.pal(8, "Dark2"),width = 4, height = 4, units = "in")
        }
  })
  output$Wordcloudverbs <- renderPlot({
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return() } else{
        v <- verbs()
        createWordcloud(v$key, v$freq, scale=c(4,0.5),
                        minFreq = 2, maxWords=100,
                        palette = brewer.pal(8, "Dark2"),width = 4, height = 4, units = "in")
        }
  })
  
  output$Coocgraph = renderPlot({
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
      return() } else{
        
        wordnetwork <- head(Coocsselected(), 50)
        wordnetwork <- igraph::graph_from_data_frame(wordnetwork) 
        
        ggraph(wordnetwork, layout = "fr") +  
          geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
          geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
          theme_graph(base_family = "Arial Narrow") +  
          theme(legend.position = "none") +
          labs(title = "Cooccurrences within 3 words distance")}
  })
  
}

shinyApp(shinyUI, shinyServer)


