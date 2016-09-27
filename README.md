# Web Scraping TripAdvisor.com using R


This R code scrapes the TripAdvisor website. The input is a web url. 
The output is: • Restaurant Information 
o Cuisines 
o Address 
o Phone Number 
o Price Range 
o Hours of Operations 
o Meals offered

• Ratings and Customers 
o Average Review Rating 
o Traveler Rating Distribution (numbers of Good, Excellent, etc.) 
o Rating Summaries regarding Food, Service, Value and Atmosphere 
o Traveler Type Distribution (counts per category) 

• Reviews and reviewers information 
o Reviewer ID and their background/history (level, # reviews, etc.) 
o Review Date and Comments 
o Number of Thanks 

The output is in xlsx format in TripAdvisor.xlsx file. 
The first 2 tabs contain restaurant information and ratings and other info (on each restaurant level). Each restaurant is given a key named rank in these two sheets. This program produces all the reviews for each restaurant in first 5 pages of the website. The review information for each restaurant is displayed in different tabs with the restaurant's rank number as the sheet number of the xlsx file. 
Missing data is handled with N/A in the restaurant info level and blanks on review level. 
Any feedback is welcome. Thanks.
