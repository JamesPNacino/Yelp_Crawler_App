# load required packages
library(shiny)
library(ggplot2)
library(XML)
library(httr)
source("Read_data.R", echo=TRUE)

# load in zip code data
#zipcodes_df <- read.csv("C:/Users/james/Documents/UC Riverside Masters - Data Science/Class - Information Retrieval/Project - Build Search Engi/us_postal_codes2.csv")

# Define UI for miles per gallon app ----
ui <- shinyUI(pageWithSidebar(
        
        headerPanel("Web App GUI to Crawl Yelp.com"),
        sidebarPanel(
                sliderInput('Restaurants', "Choose the number of restaurants to parse. When first loading the app, this will parse two restaurants. Wait till these restaurants have been parsed then you can change parameter and start to parse. It takes around 3 seconds per restaurant to parse so be patient please!",
                            2, min=1, max=5000, step=1, sep=""),
                submitButton(text="Start Parsing"),
                p("Only click the download button after data has finished parsing as shown on UI. Download parsed data from Yelp.com."),
                downloadButton("downloadData", "Download")
                
        ),
        mainPanel(
                tabsetPanel(
                        tabPanel("Data Table",
                                 h2("Data Scraped from Yelp.com, table view"),
                                 dataTableOutput("data_table")),
                        tabPanel("Chart Analysis",
                                 h2("Time chart (in seconds) to analyze how fast parsing takes"),
                                 h3("This chart is plotting the total time it has taken to finish parsing the desired amount of restaurants"),
                                 plotOutput("time_plot"),
                                 h3("This chart is plotting the time that each restaurant is getting parsed"),
                                 plotOutput("time_plot2"),
                                 h3("Summary Statistics"),
                                 tableOutput("summary"))
                )
        )))


# create parsing function
parse <- function(num_rst){
        #### User inputs here #####
        start <- 1   # this is the starting county
        end <- 1000   # this is the ending county to stop at
        how_many_restarants <- num_rst    # the number of restaurants to stop at, will take precedence over the ending county if the number of restauraunts has been reached first. Which will stop scraping/crawling
        
        ###########################
        
        # Read in a CSV file which has the list of all zip codes in the United States. I edited CSV before reading to only get top 1000 most populous zip codes
        zipcodes_list <- sample(zipcodes_df$Zip.Code.ZCTA)
        
        # Using Yelp.com as the seed page - meaning the main page. prefix url to append to seed url to use GET to retrieve html text
        seedUrl <- "https://www.yelp.com/"
        prefixUrl_1 <- "search?find_loc="
        
        # initialize data frame to store yelp data and time takes to get each restaurant
        yelpData_df <- data.frame()
        time_df <- data.frame()
        ptm <- proc.time()
        
        # initialize counters for writing files to directory to save checkpoint
        counter <- 0
        number_restaurants <- 0
        
        limit_breaker <- FALSE
        
        # loop to get data from restaurants/businesses in Yelp.com
        for (j in start:end){
                # break loop after getting desired amount of restaurants
                if (limit_breaker){
                        break
                }
                
                # increment counter
                counter <- counter + 1
                
                #1:length(zipcodes_list)
                # parse the webpages to get the entire html page text
                html <- GET(paste(seedUrl, prefixUrl_1, zipcodes_list[j], sep = ""))
                pageText <- htmlTreeParse(html, useInternalNodes = T)
                
                # intialize empty vector/list to store url of specific webpages
                prefixUrl_2 <- vector()
                
                # this gets urls for restaurants associated with the zip code
                more_restarauntsUrl <- paste(prefixUrl_1, zipcodes_list[j], sep = "")
                more_restarauntsUrl2 <- as.character(xpathSApply(pageText, "//div[@class='arrange_unit page-option']//a/@href")[1:3]) # getting more urls of businesses associated with zip code
                more_restarauntsUrl2 <- more_restarauntsUrl2[!is.na(more_restarauntsUrl2)]
                more_restarauntsUrl <- append(more_restarauntsUrl, more_restarauntsUrl2, after = length(more_restarauntsUrl))
                prefixUrl_2 <- vector()
                for (i in 1:length(more_restarauntsUrl)){
                        html <- GET(paste(seedUrl, more_restarauntsUrl[i], sep = ""))
                        pageText <- htmlTreeParse(html, useInternalNodes = T)
                        prefixUrl_2 <- append(prefixUrl_2, as.character(xpathSApply(pageText, "//a[@class='biz-name js-analytics-click']/@href")), after = length(prefixUrl_2))
                }
                
                # if there are no restaurants stored in prefixUrl_2 then skip to the next iteration of the loop to next zip code, else store an error message in GetError so that we can move to the next zip code as well
                getError <- tryCatch({
                        if (length(prefixUrl_2) == 0){
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
                                temporary_df <- data.frame(cbind("url" = paste(seedUrl, prefixUrl_2[i], sep = ""), "restaurant" = sub('/biz/',"", prefixUrl_2[i]), "city" = cityfinal, 
                                                                 "zip_code"=zipcodes_list[j], "state" = statefinal, "category_one" = category1, "category_two" = category2, "category_three" = category3, "review" = review, 
                                                                 "foodimg_one" = foodIMG1, "foodimg_two" = foodIMG2, "foodimg_three" = foodIMG3, "foodimg_four" = foodIMG4, "foodimg_five" = foodIMG5,
                                                                 "foodimg_six" = foodIMG6, "foodimg_seven" = foodIMG7, "foodimg_eight" = foodIMG8, "foodimg_nine" = foodIMG9, "foodimg_ten" = foodIMG10,
                                                                 "foodimg_eleven" = foodIMG11, "foodimg_twelve" = foodIMG12, "foodimg_thirteen" = foodIMG13, "foodimg_fourteen" = foodIMG14, "foodimg_fifteen" = foodIMG15,
                                                                 "foodimg_sixteen" = foodIMG16, "foodimg_seventeen" = foodIMG17, "foodimg_eighteen" = foodIMG18, "foodimg_nineteen" = foodIMG19, "foodimg_twenty" = foodIMG20,
                                                                 "foodimg_twentyone" = foodIMG21, "foodimg_twentytwo" = foodIMG22, "foodimg_twentythree" = foodIMG23, "foodimg_twentyfour" = foodIMG24, "foodimg_twentyfive" = foodIMG25,
                                                                 "foodimg_twentysix" = foodIMG26, "foodimg_twentyseven" = foodIMG27, "foodimg_twentyeight" = foodIMG28, "foodimg_twentynine" = foodIMG29, "foodimg_thirty" = foodIMG30))
                                
                                # as a precaution only append if the url is fully valid
                                if (grepl("https",temporary_df$url[1])) {
                                        yelpData_df <- rbind(yelpData_df, temporary_df)
                                        
                                        # record the time to get/parse each restaurant
                                        number_restaurants <- number_restaurants + 1
                                        time <- proc.time() - ptm
                                        if (number_restaurants == 1){
                                                time_df <- rbind(time_df, cbind("Num_Restaurants" = number_restaurants, "TotTimeElapsed_sec" = time[[3]], "ScrapeTime_sec" = time[[3]]))
                                                #print (time_df[number_restaurants,])
                                        } else {
                                                time_df <- rbind(time_df, cbind("Num_Restaurants" = number_restaurants, "TotTimeElapsed_sec" = time[[3]], "ScrapeTime_sec" = (time[[3]] - time_df$TotTimeElapsed_sec[number_restaurants-1])))
                                                #print (time_df[number_restaurants,])
                                        }
                                        
                                        # check to see if you should break out of loop
                                        if (number_restaurants == how_many_restarants){
                                                output <- list(yelpData_df, time_df)
                                                return (output)
                                                limit_breaker <- TRUE
                                                break
                                        }
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
                
        }
        
}





# Define server logic 
server <- function(input, output) {
        data <- reactive({
                data <- parse(input$Restaurants) 
                
        })
        
        output$data_table <- renderDataTable({data()[[2]]})
        
        
        
        output$time_plot <- renderPlot({
                ggplot(data=data()[[2]], aes(x=Num_Restaurants, y=TotTimeElapsed_sec)) +
                        geom_line(color="red")
                })
        
        output$time_plot2 <- renderPlot({
                ggplot(data=data()[[2]], aes(x=Num_Restaurants, y=ScrapeTime_sec)) +
                        geom_line(color="blue")
        })
        #output$data_table <- renderDataTable({parse(input$Restaurants)[[2]]})
        
        output$downloadData <- downloadHandler(
                filename = function() {
                        paste("Yelp_Data.csv", sep = "")
                },
                content = function(file) {
                        write.csv(data()[[1]], file, row.names = FALSE)
                }
        )
        
        output$summary <- renderTable({
                data.frame("Total_Restaurants" = nrow(data()[[2]]),"Average_parsing_time" = mean(data()[[2]]$ScrapeTime_sec))
        })
}

shinyApp(ui, server)

#runApp("C:\\Users\\james\\Documents\\UC Riverside Masters - Data Science\\Class - Information Retrieval\\Project - Build Search Engine")