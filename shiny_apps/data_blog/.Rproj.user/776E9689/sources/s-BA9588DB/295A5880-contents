library(shiny)
library(tm)
library(twitteR)
library(tidyr)
library(ggplot2)
library(ggmap)
library(wordcloud)



setup_twitter_oauth("e3jYwJZxRhpPhTIubnJyDPEhq", #consumer_key
"CLJPdUgytagatmPz89N3RxiWXCT68G6UW4NlNlgtREPSvlEaFS", #consumer_secret
"763105637140004865-ZjkWg8g2r80YLoI0tUEPmQPRcCCYAE4", #access_token
"VQOX93Wm737uO6lrBSuhjS0bzHZKHmDKpeWQDSpDToWT7") #access_secret


clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

ui = #shinyUI(
  fluidPage(
    textOutput(outputId = "tx1"),
    textOutput(outputId = "tx2"),
    textOutput(outputId = "tx3"),
    textInput(inputId = "handle", label = "Twitter Handle"), 
    actionButton(inputId = "but1", label = "Apply Changes"), 
    plotOutput("plot1")
  )
#)

server = function(input, output) {
  observeEvent(input$but1, { 
    handle = input$handle
   
    don = tryCatch(getUser(handle),
                   working <<- 1,
                   warning = function(w) {
                     working <<- 0
                     print("warning"); 
                     # handle warning here
                   },
                   error = function(e) {
                     working <<- 0
                     print("error");
                     # handle error her
                   }
    ) 
    print("boyo")
    #  }
    if(working == 1){
      output$tx1 = renderText({as.character(don$created)}) 
      output$tx3 = renderText({as.character(don$name)})
      dhist = userTimeline(don, n = 300)
      df = sapply(dhist, function(x) x$getText())
      df = sapply(df, function(row) iconv(row, "latin1", "ASCII", sub=""))
      dff = paste(clean.text(df), collapse = " ")
      dff = removeWords(dff, stopwords("english"))
      wrds = Corpus(VectorSource(dff)) 
      trms = as.matrix(TermDocumentMatrix(wrds))
      print("wtf")
      output$plot1 = renderPlot({
        wordcloud(row.names(trms), trms[,1])
      })
    } else {
      output$tx1 = renderText({"Not a valid twitter user"})
      output$tx3 = renderText({"try again with a valid username."})
    }
  })
}

shinyApp(ui = ui, server = server)

