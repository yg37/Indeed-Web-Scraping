require(stringr)
require(rvest)
require(magrittr)

files <- dir("data/indeed/", pattern = "*.html", full.names = TRUE)
dir.create("data/links/",recursive=TRUE,showWarnings=FALSE)

for (i in 1:length(files)){
  links <- read_xml(files[i]) %>%
    html_nodes("link") %>% html_text() 
  
  counter = 0 #set a counter#
  for (link in links){
    dest = paste0("data/links/",i," ",counter,".html")
    download.file(link,destfile=dest,method="wget")
    counter = counter + 1
  }
}

htmls = dir("data/links",pattern = "*.html",full.names = TRUE)

job_summary <- NULL
num <- NULL

for (i in 1:length(htmls))
{
  html <- read_html(htmls[i])
  
  content <- html_nodes(html, "#job_summary") %>% html_text()
  
  if (length(content)==0) next
  
  content <- str_replace_all(tolower(content),"[^a-z0-9[:punct:] ]", " ")
  
  job_summary<- c(job_summary,content)
  
  num <- c(num,i)
}



qualifications <- NULL
responsibilities <- NULL
q1 <- q2 <- q3 <- q4 <- q5 <- q6 <- q7 <- r1 <- r2 <- r3 <- r4 <- NULL 

for(i in 1:length(job_summary))
{
  q1 <- str_extract_all(job_summary[i],"qualifi[a-z]+[: ]+.+? {2}") %>% unlist()
  q3 <- str_extract_all(job_summary[i],"require[a-z]+[: ]+.+? {2}") %>% unlist()
  q4 <- str_extract_all(job_summary[i],"demand[a-z]+[: ]+.+? {2}") %>% unlist()
  q5 <- str_extract_all(job_summary[i],"essentials[a-z]+[: ]+.+? {2}") %>% unlist()
  q6 <- str_extract_all(job_summary[i],"must[a-z]+[: ]+.+? {2}") %>% unlist()
  q7 <- str_extract_all(job_summary[i],"should[a-z]+[: ]+.+? {2}") %>% unlist()
  
  qua<-NULL
  
  if (length(q1)!=0|length(q2)!=0|length(q3)!=0)
  qua <- paste(q1, q2, q3, sep = " ") 
  
  if(length(q4)!=0|length(q5)!=0|length(q6)!=0|length(q7)!=0)
  qua <- ifelse(length(qua),qua,paste(q4, q5, q6, q7, sep = " "))
  
  qualifications <- c(qualifications,qua)
  
  r1 <- str_extract_all(job_summary[i],"responsi[a-z]+[: ]+.+? {2}") %>% unlist()
  r2 <- str_extract_all(job_summary[i],"accountab[a-z]+[: ]+.+? {2}") %>% unlist()
  r3 <- str_extract_all(job_summary[i],"duties[a-z]+[: ]+.+? {2}") %>% unlist()
  r4 <- str_extract_all(job_summary[i],"function[a-z]+[: ]+.+? {2}") %>% unlist()
  
  res <- NULL
  
  if (length(r1)!=0|length(r2)!=0|length(r3)!=0)
  res <- paste(r1, r2, r3, sep = " ")
  
  if(length(r4)!=0)
  res <- ifelse(length(res), res, r4)
  
  responsibilities <- c(responsibilities,res)
}
