library(dplyr)
library(magrittr)
library(knitr)
library(stringr) 
library(httr) 
library (tidyverse)
#Enis Mert Kuzu 
#I searched for freedom to search United Nations press releases about freedom.
urls<-paste0("https://press.un.org/en/sitesearch?search_api_fulltext=Freedom&sort_by=field_dated&page=",0:7)
#I wanted to get the first 8 pages as there are 10 articles on each page
print(urls)
html_code <- vector ("list")
for (i in 1:length(urls)) { 
  #go through each of the eight pages
  #access the page to retrieve all kind of information (including meta-data about our request)
  page <- httr:: GET (urls[[i]]) 
  #extract content, meaning extracting the html code out of which the website is build
  page_content <- httr::content(page, "text") 
  #add raw page content to the list with future article urls
  html_code[[i]] <- page_content 
}
urls_final <- vector ("list")
#check what one website page looks like.
html_code[[3]] 
for (i in 1:length (html_code)) {
  #Find the div of article links and scrap them
  urls_final [[i]] <- str_extract_all (html_code[[i]],'<div class="views-field views-field-title">(?s).*?</div>') [[1]] %>%
  str_extract_all (., "<a href=(?s).*?</a>") %>% 
  #change the object from a list to a flat vector
  unlist()%>% 
  #delete <a href> from beginning
  str_remove_all (., '<a href=\"') %>% 
  #delete everything after "
  str_remove_all (., '\\".*')%>%
  #add domain of website to get complete addresses
  paste0("https://press.un.org", .) 
}
#check what one url looks like.
urls_final [[1]]
# Transform the list into a flat vector 
urls_final <- unlist (urls_final)
#check is vector or not
is.vector(urls_final)

article_df <- data.frame (Url = urls_final, 
                          #variable 2: will store the text of the article,default empty
                          Text = NA,
                          #variable 3: will store type of the article,default empty
                          Type=NA,
                          #variable 4: will store the date that published the article,,default empty
                          Date = NA,
                          #variable 5: will store the only month of date,default empty
                          Month=NA,
                          #variable 6: will store the only year of date,default empty
                          Year=NA,
                          #variable 7: number of words in the article,default empty
                          Length = NA,
                          #tell R manually to not transform string to factors. 
                          stringsAsFactors = F) 
#This loop takes about 4 minutes to get datas
for (i in 1:nrow (article_df)){
  #access each article-page to retrieve all kind of information
  raw_file <- httr:: GET (article_df [i, 1])
  article_content <- httr::content(raw_file, "text")
  
  #extract text body from HTML whose div is found
  article_df [i,2] <-str_extract_all(article_content,'<div class=\\"field field--name-body field--type-text-with-summary field--label-hidden field__item\\">(?s).*?</div>')%>% 
  str_remove_all (., "<.*?>") %>% 
  #remove any extra whitespaces in the text body
  str_squish ()

  #extract type information from HTML
  type<-str_extract_all(article_content,'<div class=\\"field field--name-field-doctype field--type-entity-reference field--label-hidden field__item\\">(?s).*?</div>')%>%
  str_remove_all (., "<.*?>")
  article_df [i,3]<- type
  
  #extract date information from HTML
  article_df [i,4] <-str_extract_all(article_content,'<div class=\\"field field--name-field-dated field--type-datetime field--label-hidden field__item\\">(?s).*?</div>')%>%
  str_extract_all (., "<time datetime=(?s).*?</time>")%>%
  str_remove_all (., "<.*?>")
  
  #extract only month and year of date information
  date_object <- as.Date(article_df [i,4], format = "%d %B %Y")
  month <- format(date_object, "%m")
  year <- format(date_object, "%Y")
  
  #putting variables on dataframe
  article_df [i,5]<- month
  article_df [i,6]<- year
  article_df [i,7] <-str_count(article_df[i,2], '\\w+') 
  Sys.sleep(1+sample(c(0.5,1,1.5,2),1))
}
#get rid of empty observations,if it is any.
df <- article_df[article_df$Length >0,]
df <- df[df$Text != "character(0)",]
#looking at how many different dates there are from these articles. 
table (df$Date)
#looking at the statistics of articles using some summary statistics.
summary (df$Length)
#Plotting with the base R graphics x = article y = length 
plot(article_df$Length,ylab = "Article Length",xlab="Index of Article",col="red")

#finding frequency of each month in dataset
month_counts <- as.data.frame(table(df$Month))

# Plot the results of frequency of each month
ggplot(month_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Number of Articles") +
  ggtitle("Number of Articles by Month")

#finding frequency of each year in dataset
year_counts <- as.data.frame(table(df$Year))

# Plot the results of frequency of each year in dataset.
ggplot(year_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Number of Articles") +
  ggtitle("Number of Articles by Year")

#finding frequency of each type in dataset
type_counts <- as.data.frame(table(df$Type))

# Plot the results of frequency of each type in dataset.
ggplot(type_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("Type") +
  ylab("Number of Articles") +
  ggtitle("Number of Articles by Type")
