# Download Required Packages
#install.packages("XML")
#install.packages("httr")

# Load required Packages
library(XML)
library(httr)

# Set working directory
setwd("C:/Users/james/Documents/UC Riverside Masters - Data Science/Class - Information Retrieval/Project - Build Search Engine")

# Read in a CSV file which has the list of all zip codes in the United States
# found a free dataset with zip information here - https://www.aggdata.com/node/86
zipcodes_df <- read.csv("us_postal_codes.csv")

# get zip codes only with length of five digits since Yelp only recognizes five digit zip codes
zipcodes_list <- zipcodes_df$Zip.Code[3004:nrow(zipcodes_df)]

# Using Yelp.com as the seed page - meaning the main page. prefix url to append to seed url to use GET to retrieve html text
seedUrl <- "https://www.yelp.com/"
prefixUrl_1 <- "search?find_loc="

# initialize data frame to store yelp data
yelpData_df <- data.frame()

# initialize counters for writing files to directory to save checkpoint
start <- 35226
end <- start+500
counter <- 0

# loop to get data from restaurants/businesses in Yelp.com
for (j in start:end){
        # increment counter
        counter <- counter + 1
        
        #1:length(zipcodes_list)
        # parse the webpages to get the entire html page text
        html <- GET(paste(seedUrl, prefixUrl_1, zipcodes_list[j], sep = ""))
        pageText <- htmlTreeParse(html, useInternalNodes = T)
        
        # intialize empty vector/list to store url of specific webpages
        prefixUrl_2 <- vector()
        
        # query a specific html tag to get the list of restaurants - if there are no restaurants continue to the next zip code
        restaurant_list <- xpathSApply(pageText, "//a[@class='biz-name js-analytics-click']")
        if (length(restaurant_list) == 0){
                next
        }
        
        # clean the restaurant_list to get the links to the actual restaurants
        for (i in 1:length(restaurant_list)){
                tempRestaurant <- as(restaurant_list[[i]], "character")
                tempRestaurant <- sub(".+?(?=/biz/)", "", tempRestaurant, perl = TRUE)
                tempRestaurant <- sub('\".+', "", tempRestaurant)
                prefixUrl_2 <- append(prefixUrl_2, tempRestaurant, after = length(prefixUrl_2))
        }
        
        # if there are no restaurants stored in restaurant_list then skip to the next iteration of the loop to next zip code, else store an error message in GetError so that we can move to the next zip code as well
        getError <- tryCatch({
                if (length(restaurant_list) == 0){
                        next
                }
        },
                error = function(e){
                        "Error"
                }
        )
        
        # move to next zip code if there was an error stored in getError
        if (!is.null(getError)){
                if (getError == "Error"){
                        next
                }
        }
        
        # loop over each of the restaurants to get data from each restaurant
        for (i in 1:length(prefixUrl_2)){
                #length(prefixUrl_2)
                # parse the webpages to get the entire html page text
                html2 <- GET(paste(seedUrl, prefixUrl_2[i], sep = ""))
                pageText2 <- htmlTreeParse(html2, useInternalNodes = T)
                
                # parse the webpages to get the entire html page text - this is for pictures
                html3 <- GET(paste(seedUrl, sub("/biz/", "/biz_photos/", prefixUrl_2[i]), sep = ""))
                pageText3 <- htmlTreeParse(html3, useInternalNodes = T)
                
                # Get the city information and state
                city <- xpathSApply(pageText2, "//small[@class='biz-city']")
                if (length(city) == 0){
                        next
                }
                city <- as(city[[1]], "character")
                city <- sub('<.+\">', "", city)
                city <- sub('<.+', "", city)
                cityfinal <- strsplit(city, split=", ")[[1]][1]
                statefinal <- strsplit(city, split=", ")[[1]][2]
                
                # Get the category of restaurant or general category of business
                category <- unique(xpathSApply(pageText2, "//span[@class='category-str-list']//a", xmlValue))[1:3]
                if (is.null(category)){
                        category1 <- "NA"
                        category2 <- "NA"
                        category3 <- "NA"
                } else {
                        category1 <- category[1]
                        category2 <- category[2]
                        category3 <- category[3]
                }
                
                # Get food image(s) - only get three food images (this may or may not include the logo or image of the restaurant)
                foodImg <- as.vector(xpathSApply(pageText3, "//div[@class='photo-box photo-box--interactive']//img/@src"))
                if (is.null(foodImg)){
                        foodIMG1 <- "NA"
                        foodIMG2 <- "NA"
                        foodIMG3 <- "NA"
                        foodIMG4 <- "NA"
                        foodIMG5 <- "NA"
                        foodIMG6 <- "NA"
                        foodIMG7 <- "NA"
                        foodIMG8 <- "NA"
                        foodIMG9 <- "NA"
                        foodIMG10 <- "NA"
                        foodIMG11 <- "NA"
                        foodIMG12 <- "NA"
                        foodIMG13 <- "NA"
                        foodIMG14 <- "NA"
                        foodIMG15 <- "NA"
                        foodIMG16 <- "NA"
                        foodIMG17 <- "NA"
                        foodIMG18 <- "NA"
                        foodIMG19 <- "NA"
                        foodIMG20 <- "NA"
                        foodIMG21 <- "NA"
                        foodIMG22 <- "NA"
                        foodIMG23 <- "NA"
                        foodIMG24 <- "NA"
                        foodIMG25 <- "NA"
                        foodIMG26 <- "NA"
                        foodIMG27 <- "NA"
                        foodIMG28 <- "NA"
                        foodIMG29 <- "NA"
                        foodIMG30 <- "NA"
                } else {
                        foodImg <- foodImg[1:30]
                        foodIMG1 <- foodImg[1]
                        foodIMG2 <- foodImg[2]
                        foodIMG3 <- foodImg[3]
                        foodIMG4 <- foodImg[4]
                        foodIMG5 <- foodImg[5]
                        foodIMG6 <- foodImg[6]
                        foodIMG7 <- foodImg[7]
                        foodIMG8 <- foodImg[8]
                        foodIMG9 <- foodImg[9]
                        foodIMG10 <- foodImg[10]
                        foodIMG11 <- foodImg[11]
                        foodIMG12 <- foodImg[12]
                        foodIMG13 <- foodImg[13]
                        foodIMG14 <- foodImg[14]
                        foodIMG15 <- foodImg[15]
                        foodIMG16 <- foodImg[16]
                        foodIMG17 <- foodImg[17]
                        foodIMG18 <- foodImg[18]
                        foodIMG19 <- foodImg[19]
                        foodIMG20 <- foodImg[20]
                        foodIMG21 <- foodImg[21]
                        foodIMG22 <- foodImg[22]
                        foodIMG23 <- foodImg[23]
                        foodIMG24 <- foodImg[24]
                        foodIMG25 <- foodImg[25]
                        foodIMG26 <- foodImg[26]
                        foodIMG27 <- foodImg[27]
                        foodIMG28 <- foodImg[28]
                        foodIMG29 <- foodImg[29]
                        foodIMG30 <- foodImg[30]
                }
                
                # Get the review information
                reviews_list <- xpathSApply(pageText2, "//p[@itemprop='description']")
                
                # initialize vector object to get reviews
                reviewstemp <- vector()
                
                # use tryCatch if error
                getError <- tryCatch({
                        
                        # loop to get reviews - concatenate reviews into one corpus document of text
                        for (x in 1:length(reviews_list)){
                                review <- as(reviews_list[[x]], "character")
                                review <- sub('<.+\">', "", review)
                                review <- gsub('\n', "", review)
                                review <- sub('</p>', "", review)
                                reviewstemp <- append(reviewstemp, review, after = length(reviewstemp))
                                review <- paste(reviewstemp, collapse = " ")
                        }
                        
                        
                        # rbind yelpData_df to append new records to the dataframe
                        temporary_df <- cbind("url" = paste(seedUrl, prefixUrl_2[i], sep = ""), "restaurant" = sub('/biz/',"", prefixUrl_2[i]), "city" = cityfinal, 
                                                                "zip_code"=zipcodes_list[j], "state" = statefinal, "category_one" = category1, "category_two" = category2, "category_three" = category3, "review" = review, 
                                                                "foodimg_one" = foodIMG1, "foodimg_two" = foodIMG2, "foodimg_three" = foodIMG3, "foodimg_four" = foodIMG4, "foodimg_five" = foodIMG5,
                                                                "foodimg_six" = foodIMG6, "foodimg_seven" = foodIMG7, "foodimg_eight" = foodIMG8, "foodimg_nine" = foodIMG9, "foodimg_ten" = foodIMG10,
                                                                "foodimg_eleven" = foodIMG11, "foodimg_twelve" = foodIMG12, "foodimg_thirteen" = foodIMG13, "foodimg_fourteen" = foodIMG14, "foodimg_fifteen" = foodIMG15,
                                                                "foodimg_sixteen" = foodIMG16, "foodimg_seventeen" = foodIMG17, "foodimg_eighteen" = foodIMG18, "foodimg_nineteen" = foodIMG19, "foodimg_twenty" = foodIMG20,
                                                                "foodimg_twentyone" = foodIMG21, "foodimg_twentytwo" = foodIMG22, "foodimg_twentythree" = foodIMG23, "foodimg_twentyfour" = foodIMG24, "foodimg_twentyfive" = foodIMG25,
                                                                "foodimg_twentysix" = foodIMG26, "foodimg_twentyseven" = foodIMG27, "foodimg_twentyeight" = foodIMG28, "foodimg_twentynine" = foodIMG29, "foodimg_thirty" = foodIMG30)
                        # as a precaution only append if the url is fully valid
                        if (grepl("https",temporary_df$url[1])) {
                                yelpData_df <- rbind(yelpData_df, temporary_df)
                        }
                        
                        
                },
                        error = function(e){
                                "Error"
                        })
                
        
                
                # move to next restaurant if there was an error stored in getError
                if (!is.null(getError)){
                        if (getError == "Error"){
                                next
                        }
                }
                

        }
        
        if (counter == 100){
                # write yelpData_df data to CSV file
                con <- file(description=paste("C:/Users/james/Documents/UC Riverside Masters - Data Science/Class - Information Retrieval/Project - Build Search Engine/YelpData_df", start, "-", start+(j - start), ".csv", sep = ""), "w")
                write.csv(yelpData_df, con, row.names = FALSE)
                close(con)
                
                # initialize data frame back empty
                yelpData_df <- data.frame()
                
                # reset counter
                counter <- 0
        }
}


