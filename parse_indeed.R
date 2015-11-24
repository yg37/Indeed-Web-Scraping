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

job_summary <- rep(NA, length(htmls))

for (i in 1:length(htmls))
{
  html <- read_html(htmls[i])
  
  content <- html_nodes(html, "#job_summary") %>% html_text()
  
  job_summary[i] <- ifelse(length(content),tolower(content),NA)
  
}

for(i in 1:length(job_summary))
{
  qualifications <- str_extract_all(job_summary[i],"qualifications.*")
  
  responsibilities <- str_extract_all(job_summary[i],"responsibilities.*qualifications")
}

