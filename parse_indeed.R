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
