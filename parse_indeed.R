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

qualifications <- rep(NA, length(htmls))
responsibilities <- rep(NA, length(htmls))
q1 <- rep(NA, length(htmls))
q2 <- rep(NA, length(htmls))
q3 <- rep(NA, length(htmls))
q4 <- rep(NA, length(htmls))
q5 <- rep(NA, length(htmls))
q6 <- rep(NA, length(htmls))
q7 <- rep(NA, length(htmls))
r1 <- rep(NA, length(htmls))
r2 <- rep(NA, length(htmls))
r3 <- rep(NA, length(htmls))
r4 <- rep(NA, length(htmls))

for(i in 1:length(job_summary))
{
  q1[i] <- str_extract_all(job_summary[i],"qualifications?.*?\\n(\\n|[:blank:])?")
  q2[i] <- str_extract_all(job_summary[i],"qualified.*?\\n(\\n|[:blank:])?")
  q3[i] <- str_extract_all(job_summary[i],"requirements?.*?\\n(\\n|[:blank:])?")
  q4[i] <- str_extract_all(job_summary[i],"demands?.*?\\n(\\n|[:blank:])?")
  q5[i] <- str_extract_all(job_summary[i],"essentials?.*?\\n(\\n|[:blank:])?")
  q6[i] <- str_extract_all(job_summary[i],"must?.*?\\n(\\n|[:blank:])?")
  q7[i] <- str_extract_all(job_summary[i],"should?.*?\\n(\\n|[:blank:])?")
  qualifications[i] <- str_c(unlist(q1[i]), unlist(q2[i]), unlist(q3[i]), sep = " ")
  qualifications[i] <- ifelse(str_length(qualifications[i]), 
                              qualifications[i],
                              str_c(unlist(q4[i]), unlist(q5[i]), unlist(q6[i]), unlist(q7[i]), sep = " "))
  
  r1[i] <- str_extract_all(job_summary[i],"responsib.*?\\n(\\n|[:blank:])?")
  r2[i] <- str_extract_all(job_summary[i],"accountab.*?\\n(\\n|[:blank:])?")
  r3[i] <- str_extract_all(job_summary[i],"duties.*?\\n(\\n|[:blank:])?")
  r4[i] <- str_extract_all(job_summary[i],"functions?.*?\\n(\\n|[:blank:])?")
  responsibilities[i] <- str_c(unlist(r1[i]), unlist(r2[i]), unlist(r3[i]), sep = " ")
  responsibilities[i] <- ifelse(str_length(responsibilities[i]), responsibilities[i], r4[i])
}

