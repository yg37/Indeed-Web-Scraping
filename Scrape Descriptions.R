require(stringr)
require(rvest)
require(magrittr)
require(koRpus)
require(openNLP)
require(tm)


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
wordcloud(d$word, colors=c(3,4), random.color=FALSE, d$freq,max.words=200)
