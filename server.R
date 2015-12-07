shinyServer(
  function(input, output, session)
  {
    d=reactive(
    {
    get_indeed <- function(start,dest){
      url <- paste0("http://rss.indeed.com/rss?q=",input$field_find,"&l=",input$loc_find,"&start=",start)
      download.file(url,destfile=dest,method="wget")
    }
    
    dir.create("data/indeed/",recursive=TRUE,showWarnings=FALSE)
    
    for(i in seq(10,100,length.out=10)){
      dest = paste0("data/indeed/",i/10,".html")
      
      #Call function based on those values, a destination directory, and a specified limit#
      get_indeed(i, dest)
    }
    
    files <- dir("data/indeed/", pattern = "*.html", full.names = TRUE)
    
    descriptions <-c() #create a vector to store the results
    for (i in 1:length(files)){
      new <- read_xml(files[i]) %>%
        html_nodes("description") %>% html_text() 
      descriptions <- c(descriptions,new)
    }
    
    
    pos <- function(ele){
      sent_token_annotator <- Maxent_Sent_Token_Annotator()
      word_token_annotator <- Maxent_Word_Token_Annotator()
      annotations <- annotate(ele, list(sent_token_annotator, word_token_annotator))
      pos_tag_annotator <- Maxent_POS_Tag_Annotator()
      pos_tag_annotator
      pos <- annotate(ele, pos_tag_annotator, annotations)
      return (pos$features[[2]]$POS == "NN")
    }
    
    #get rid of the tags#
    for (i in 1:length(descriptions)){
      if (!is.na(str_extract(descriptions[i],".*\\."))){
        descriptions[i] = str_extract(descriptions[i],".*\\.")
      }
    }
    
    #remove duplicate entries
    descriptions <- unique(descriptions) 
    
    
    corp <- Corpus(VectorSource(descriptions))
    #dtm = DocumentTermMatrix(corp,control=list(removePunctuation=TRUE,removeNumbers=TRUE,stemming=TRUE))
    
    corp<-tm_map(corp,content_transformer(removeNumbers)) #convert to lower case#
    corp<-tm_map(corp,removePunctuation) #remove punctuation#
    #corp <- tm_map(corp, stemDocument)
    corp <-tm_map(corp, removeWords, stopwords('english'))
    
    dtm <- as.matrix(DocumentTermMatrix(corp))
    
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    annotations <- annotate(colnames(dtm), list(sent_token_annotator, word_token_annotator))
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    pos_tag_annotator
    pos <- annotate(colnames(dtm), pos_tag_annotator, annotations)
    
    non <- c()
    for (i in 2:length(pos)){
      if (pos$features[[i]]$POS != "NN"){
        non <- c(non,i-1)
      }
    }
    
    irrelevant <- c()
    for (i in 1:dim(dtm)[2]){
      if (str_detect(colnames(dtm)[i],"job")||str_detect(colnames(dtm)[i],"http")||str_detect(colnames(dtm)[i],"indeed")){
        irrelevant <- c(irrelevant,i)
      }
    }
    
    dtm <- dtm[,-c(non,irrelevant)]
    
    #Source: http://www.r-bloggers.com/text-mining/#
    v = sort(colSums(dtm), decreasing=TRUE);
    myNames = names(v);
    d = data.frame(word=myNames, freq=v) 
    row.names(d) = 1:nrow(d)
    d
    }
    )
    
    #Render Plots for Total Number of Socks and Proportion of Socks#
    output$plot = renderPlot(
      {
        wordcloud(d()$word, d()$freq, scale = c(4,1.5),
                  color=c("gold","darkturquoise","lightsalmon4","orange2"), min.freq = 2, max.words=30)
      }
    )
    
    output$table = renderTable(
      {
        table=data.frame(d()[1:15,],16:30,d()[16:30,])
        names(table)=c("Word","Frequncy","","Word","Frequncy")
        table
      }
    )
    
  }
)