library(xml2)
library(stringr)
library(rvest)
library(magrittr)

get_indeed <- function(start,dest){
  url <- paste0("http://rss.indeed.com/rss?q=Statistics&l=California&start=",start)
  download.file(url,destfile=dest,method="wget")
}

dir.create("data/indeed/",recursive=TRUE,showWarnings=FALSE)

for(i in seq(10,100,length.out=10)){
  dest = paste0("data/indeed/",i/10,".html")
  
  #Call function based on those values, a destination directory, and a specified limit#
  get_indeed(i, dest)
}

