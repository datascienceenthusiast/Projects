start <- Sys.time()
library('rvest')
library('stringr')
library('xlsx')
library('XLConnect')
url <- 'https://www.tripadvisor.com/Restaurants-g55138-Knoxville_Tennessee.html'
wb <- loadWorkbook(filename = "D:\\3_Text Mining\\HW\\hw5\\TripAdvisor.xlsx", create = TRUE)

#Code to count all pages : Take top 5 pages for this assignment
#Get all page numbers
# pages <- complete_html %>%
#   html_nodes(".pageNumbers") %>% 
#   html_text()

# pages <- as.numeric(unlist(str_split(pages,"\n")))
# pages <- max(pages[!is.na(pages)])

rank <- 1
rest_names <- c()
cuisines <- c()
pricerange <- c()
meals <- c()
openhours <- c()
address <- c()
phoneno <- c()
avgrating <- c()
excellent <- c()
verygood <- c()
average <- c()
poor <- c()
terrible <- c()
food <- c()
service <- c()
value <- c()
atmosphere <-c()
families <- c()
couples <- c()
solo <- c()
business <- c()
friends <- c()

#Create placeholders in first 2 worksheets for the tripadvisor Restaurant info, ratings and customers
createSheet(wb, name = "Restaurant Information")
createSheet(wb, name = "Ratings and Customers")

#Get all the page links
restaurantlinks <- (url %>% read_html() %>% html_nodes(".pageNum") %>% html_attr("href"))[1:5]
restaurantlinks[1] <- url
restaurantlinks[2:5] <- sapply(restaurantlinks[2:5], function(x) paste0("https://www.tripadvisor.com",x))

for(link in restaurantlinks){
  
  #Get all restaurants on this page
  complete_html <- link %>%
    read_html()
  restaurants <- complete_html %>%
    html_nodes(".property_title") %>%
    html_attr("href")
  
  restaurants <- as.character(sapply(restaurants, function(x) str_c("https://www.tripadvisor.com",x)))
  
  
  for (i in 1:length(restaurants)){
    rest_url <- restaurants[i]
    whole_page <- rest_url %>%
      read_html()
    
    #Ger restaurant name
    rest_names <- c(rest_names, gsub("\n","",whole_page %>% html_node("#HEADING")%>% html_text()))
    
    #Get cuisines, meals, open hours and prices from the table_selection
    all_rows <- whole_page %>%
      html_node(".table_section") %>%
      html_nodes(".row")
    
    #Get Rating summary from the first row of the table
    food <- c(food, str_split_fixed((all_rows[1] %>% html_nodes("img") %>% html_attr("alt"))[1], " ",2)[1])
    service <- c(service, str_split_fixed((all_rows[1] %>% html_nodes("img") %>% html_attr("alt"))[2], " ",2)[1])
    value <- c(value, str_split_fixed((all_rows[1] %>% html_nodes("img") %>% html_attr("alt"))[3], " ",2)[1])
    atmosphere <- c(atmosphere, str_split_fixed((all_rows[1] %>% html_nodes("img") %>% html_attr("alt"))[4], " ",2)[1])
    
    ind_prices <- 0
    ind_cuisine <- 0
    ind_meals <- 0
    ind_openhours <- 0
    for (i in 1:length(all_rows)){
      row_i <- all_rows[[i]] %>% html_node(".title") %>% html_text()
      if(!is.na(row_i)){
        if(row_i == "\nAverage prices\n"){
          pricerange <- c(pricerange,gsub("\n","",all_rows[[i]] %>% html_node("span") %>% html_text()))
          ind_prices <- 1
        }
        if(row_i == "\nCuisine\n"){
          cuisines <- c(cuisines, gsub("\n","",all_rows[[i]] %>% html_node(".content") %>% html_text()))
          ind_cuisine <- 1
        }
        if(row_i == "\nMeals\n"){
          meals <- c(meals, gsub("\n","",all_rows[[i]] %>% html_node(".content") %>% html_text()))
          ind_meals <- 1
        }
        if(row_i == "\nOpen Hours\n"){
          openhours <- c(openhours, trimws(gsub("\n"," ",all_rows[[i]] %>% html_node(".hours.content") %>% html_text())))
          ind_openhours <- 1
        }
      }
    }
    
    if(ind_meals == 0){meals <- c(meals, "N/A")}
    if(ind_cuisine == 0){cuisines <- c(cuisines, "N/A")}
    if(ind_prices == 0){pricerange <- c(pricerange, "N/A")}
    if(ind_openhours == 0){openhours <- c(openhours, "N/A")}
    
    #Get Address
    temp1 <- whole_page %>%
      html_node(".street-address") %>%
      html_text()
    
    temp2 <- whole_page %>%
      html_node(".locality") %>%
      html_text()
    
    address <- c(address, str_c(temp1,temp2, sep = " "))
    
    #Get phone numbers
    temp <- whole_page %>%
      html_node(".phoneNumber") %>%
      html_text()
    
    phoneno <- c(phoneno,temp)
    
    #Get Average Rating
    avgrating <- c(avgrating, whole_page %>% html_node(".rate") %>% html_node("img") %>% html_attr("content"))
    
    #Get Traveler Rating Distribution
    excellent <- c(excellent, str_sub((whole_page %>% html_node("#ratingFilter") %>% 
                                         html_nodes("span.row_fill") %>% html_attr("style"))[1],7,-2))
    verygood <- c(verygood, str_sub((whole_page %>% html_node("#ratingFilter") %>% 
                                       html_nodes("span.row_fill") %>% html_attr("style"))[2],7,-2))
    average <- c(average, str_sub((whole_page %>% html_node("#ratingFilter") %>% 
                                     html_nodes("span.row_fill") %>% html_attr("style"))[3],7,-2))
    poor <- c(poor, str_sub((whole_page %>% html_node("#ratingFilter") %>% 
                               html_nodes("span.row_fill") %>% html_attr("style"))[4],7,-2))
    terrible <- c(terrible, str_sub((whole_page %>% html_node("#ratingFilter") %>% 
                                       html_nodes("span.row_fill") %>% html_attr("style"))[5],7,-2))
    
    #Get the Traveller type distribution
    temp <- whole_page %>%
      html_node(".col.segment") %>%
      html_nodes("label") %>%
      html_node("span") %>%
      html_text()
    families <- c(families, str_sub(temp[1],2,-2))
    couples <- c(couples, str_sub(temp[2],2,-2))
    solo <- c(solo, str_sub(temp[3],2,-2))
    business <- c(business, str_sub(temp[4],2,-2))
    friends <- c(friends, str_sub(temp[5],2,-2))
    
    
    #Get all reviews on the workbook sheet
    review_pages <- whole_page %>%
      html_nodes(".pageNumbers") %>% 
      html_text()
    
    review_pages <- as.numeric(unlist(str_split(review_pages,"\n")))
    review_pages <- max(review_pages[!is.na(review_pages)])
    
    temp_url <- unlist(str_split(rest_url,"Reviews"))
    reviewer_id <- c()
    reviewer_level <- c()
    num_reviews <- c()
    num_rest_reviews <- c()
    num_thanks <- c()
    rating_date <- c()
    review_text <- c()
    
    for (i in 1: review_pages){
      if (i==1){review_url <- paste0(temp_url[1],"Reviews",temp_url[2])}else{
        review_url <- paste0(temp_url[1],"Reviews-or",as.character((i-1)*10),temp_url[2])
      }
      reviews <- review_url %>% read_html() %>% html_nodes(".review")
      
      for (j in 1:length(reviews)){
        review <- reviews[j]
        temp <- gsub("\n","",review %>% html_node(".username.mo")%>%html_text())
        if (is.na(temp)){temp <- " "}
        reviewer_id <- c(reviewer_id, temp)
        
        temp <- str_sub(review %>% 
                          html_nodes(".levelBadge.badge") %>% 
                          html_attr("class"), start = -2)
        if (length(temp)==0 ){temp <- " "}
        reviewer_level <- c(reviewer_level, temp)
        
        temp <- review %>% html_nodes("span.badgeText")%>% html_text()
        a<-0
        b<-0
        c<-0
        if (length(temp) > 0){
          for (i in 1:length(temp)){
            if(grepl("reviews",temp[i]) & !(grepl("restaurant",temp[i]))){
              a<-1
              num_reviews <- c(num_reviews, str_sub(temp[i], end = -8))
            }
            if((grepl("restaurant",temp[i]))){
              b<-1
              num_rest_reviews <- c(num_rest_reviews, str_sub(temp[i], end = -19))
            }
            if(grepl("helpful",temp[i])){
              c<-1
              num_thanks <- c(num_thanks, str_sub(temp[i], start = 2, end = -15))
            }
          }
        }
        if (a==0){num_reviews <- c(num_reviews," ")}
        if (b==0){num_rest_reviews <- c(num_rest_reviews," ")}
        if (c==0){num_thanks <- c(num_thanks, " ")}
        
        #Get review dates
        temp <- review %>% html_node("span.ratingDate") %>% html_text()
        if (grepl("ago",temp)){
          rating_date <- c(rating_date,review %>% html_node("span.ratingDate") %>% html_attr("title"))
        }else{
          rating_date <- c(rating_date,str_sub(review %>% html_node("span.ratingDate") 
                                               %>% html_text(),start = 9, end = -2))
        }
      }
      review_text <- c(review_text, gsub("\n","",reviews %>% html_nodes(".entry") %>% 
                                           html_node("p") %>% html_text()))
    }
    
    df <- data.frame(reviewer_id, reviewer_level, num_reviews, num_rest_reviews, 
                     num_thanks, rating_date, review_text)
    createSheet(wb, name = as.character(rank))
    writeWorksheet(wb, data = df, sheet = as.character(rank), header = TRUE, rownames = NULL)
    rank <- rank + 1
  }
  

}

rank <- 1:(rank-1)
#Write the Restaurant information in the workbook
df <- data.frame(rank, rest_names, cuisines, address, phoneno, pricerange, openhours, meals)
writeWorksheet(wb, data = df, sheet = "Restaurant Information", header = TRUE, rownames = NULL)

#Write the ratings information in the workbook
df <- data.frame(rank, avgrating, excellent, verygood, average, poor, terrible, 
                 food, service, value, atmosphere, families, couples, solo, business, friends)
writeWorksheet(wb, data = df, sheet = "Ratings and Customers", header = TRUE, rownames = NULL)

saveWorkbook(wb)
end <- Sys.time()


