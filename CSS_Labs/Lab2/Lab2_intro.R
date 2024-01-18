
library(httr) 
library (tidyverse)
urls <- "https://www.government.se/search/?query=migration&ct=Article&ct=Information%20material&ct=Legal%20document&ct=Legal%20document&ct=Press%20release&ct=Speech&ct=Statement"
urls <- paste0("https://www.government.se/search/?page=", 1:4, "&query=migration&ct=Article&ct=Information%20material&ct=Legal%20document&ct=Legal%20document&ct=Press%20release&ct=Speech&ct=Statement")
print(urls)
#create an empty list container for html code
html_code <- vector ("list") 


# Loop through search pages and collect the html code that makes up the website
for (i in 1:length(urls)) { #go through each of the four pages
  #access the page to retrieve all kind of information (including meta-data about our request)
  page <- httr:: GET (urls[[i]]) 
  #extract content, meaning extracting the html code out of which the website is build
  page_content <- httr::content(page, "text") 
  #add raw page content to the list with future article urls
  html_code[[i]] <- page_content 
}

library(dplyr)
library(magrittr)
library(knitr)
library(stringr) 
library(httr) 
library (tidyverse)
urls_final <- vector ("list")
for (i in 1:length (html_code)) { 
    urls_final [[i]] <- str_extract_all (html_code[[i]],"<h3>(?s).*?</h3>") [[1]] %>% 
    #extract the link inside the url html tag to get rid of auxiliary characters
    str_extract_all (., "<a href=(?s).*?</a>") %>% 
    # In this step we want to change the object from a list to a flat vector
    unlist()%>%
    #delete <a href>
    str_remove_all (., '<a href=\"') %>% 
    #delete everything after "
    str_remove_all (., '\\".*')%>% 
    #paste "www.government.se" to get complete addresses
    paste0("https://www.government.se", .) 
  
}
urls_final
urls_final <- unlist (urls_final)
is.vector(urls_final)
#first column (variable) stores our article urls
article_df <- data.frame (url = urls_final, 
                          #variable 2: will store the text of the article, empty for now
                          text = NA,
                          #variable 3: will store the name of the agency that published the article, empty for now
                          agency = NA,
                          #variable 4: number of words in the article, empty for now
                          length = NA,
                          # by default the function data.frame() wants to convert any string data to factorial data. As we want to keep our strings as strings we will tell R manually to not transform string to factors. 
                          stringsAsFactors = F)
for (i in 1:nrow (article_df)) {
  print(i)
  raw_file <- httr:: GET (article_df [i, 1])
  article_content <- httr::content(raw_file, "text")
  #divdeki yerine git
  article_df [i,2] <-str_extract_all (article_content, '<div class=\\"has-wordExplanation\\">(?s).*?</div>')%>%
  #remove anything inside <> for the text body
  str_remove_all (., "<.*?>") %>% 
  #remove any extra whitespaces in the text body
  str_squish ()
  article_df [i,3] <- str_extract_all (article_content, '<div class=\\"categories-text\\">(?s).*?</a></div>')%>%
  #remove two last brackets in the string
  str_remove_all (., "</a></div>") %>% 
  #remove anything inside <> for the authoring agency
  str_remove_all (., "<.*>") 
  #calculate number of words in the article
  article_df [i,4] <-  str_count(article_df[i,2], '\\w+') 
  #ask the system to "fall asleep" for a few seconds, so that the website does not block us.
  Sys.sleep(1+sample(c(0.5,1,1.5,2),1)) 
}
df <- article_df[article_df$length >0,]
df <- df[df$text != "character(0)",]

table (df$agency)
summary (df$length)
plot(article_df$length)
ggplot(article_df, aes(x=agency, y=length)) +
  geom_bar(stat = "identity")
