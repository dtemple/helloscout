library(dplyr)
library('XML')
library(rvest)

### SCRIPT FOR PAGE 1 ONLY ###
#html<-read_html("https://www.tripadvisor.com/Hotels-g60713-San_Francisco_California-Hotels.html")
#hotelrank<-html_nodes(html,'.p13n_imperfect .property_title , .p13n_imperfect .slim_ranking')
#hotels<-html_text(hotelrank)
#hotels

##GOAL##
# Get names of all hotels in a given city
# Get number of reviews
# Get city rank
# Save as a file

#parse html search result
page0_url<-read_html ("https://www.tripadvisor.com/Hotels-g60713-oa0-San_Francisco_California-Hotels.html")

# find the the lnumber of the last page listed in the bottom of the main page
npages<-page0_url%>% 
  html_nodes(" .pageNum ") %>% 
  html_attr(name="data-page-number") %>%
  tail(.,1) %>%
  as.numeric()

Hotel_Name<-vector(mode="character", length=30*npages)
Hotel_ReviewCount<-vector(mode="numeric", length=30*npages)
Hotel_Rank<-vector(mode="numeric", length=30*npages)

offset=0 #offset of page url
idx_s=0 #start index of the entries in the vectors

for (i in 1:7)
{
  #change page url in every iteration to go to the next page 
  page_url<-paste("https://www.tripadvisor.com/Hotels-g60713-oa",offset,
                  "-San_Francisco_California-Hotels.html",sep="")
  #parse HTML page
  link<-read_html(page_url)
  
  #get hotel names from this page
  H_names<-link %>%
    html_nodes(".p13n_imperfect .property_title") %>%
    html_text() %>%
    gsub('[\r\n\t]', '', .)
  
  #get the review count of the hotels in the page
  H_reviewCount<-link %>% 
    html_nodes(".p13n_imperfect .review_count a") %>% 
    html_text() %>%
    gsub(" Reviews", "", .)
    as.numeric()
  
  #get the rank of the hotels in the page
  H_rank<-link %>% 
    html_nodes(".p13n_imperfect .slim_ranking") %>% 
    html_text() %>%
    gsub("#", "", .) %>%
    gsub(" of 235 hotels in San Francisco", "", .) %>%
    as.numeric()

  
  #get the number of hotels in the page
  H_page<-length(H_names)
  review_page<-length(H_reviewCount)
  rank_page<-length(H_rank)
  
  Hotel_Name[(idx_s+1):(idx_s+H_page)]<-H_names
  Hotel_ReviewCount[(idx_s+1):(idx_s+review_page)]<-H_reviewCount
  Hotel_Rank[(idx_s+1):(idx_s+rank_page)]<-H_rank
  
  #increment the start index
  idx_s=idx_s+length(H_rank)
  
  #increment the offset to refer to the next page
  offset<-offset+30      
}

#remove empty values
Hotel_Name<-Hotel_Name[Hotel_Name!=""]
Hotel_ReviewCount<-Hotel_ReviewCount[Hotel_ReviewCount!=""]
Hotel_ReviewCount<-Hotel_ReviewCount[Hotel_ReviewCount!="0"]
Hotel_Rank<-Hotel_Rank[Hotel_Rank!=""]
Hotel_Rank<-Hotel_Rank[Hotel_Rank!="0"]

#get the total number of hotels
lenName=length(Hotel_Name)
lenReview=length(Hotel_ReviewCount)
lenRank=length(Hotel_Rank)

#create a data frame to from the vectors filled in the previous loop
ff<-data.frame(Hotel_Name,Hotel_ReviewCount,Hotel_Rank,stringsAsFactors=F)

#save in RDs file
save(ff,file="sf_hotels.Rds")

#Write data frame to a CSV file
write.table(ff,file="sf-hotels_test.csv",sep=",",row.names = F)