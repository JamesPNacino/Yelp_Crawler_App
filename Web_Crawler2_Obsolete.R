install.packages("rCrawler")
library(Rcrawler)

seedUrl <- "https://www.yelp.com/"
directory = "C:/Users/james/Documents/UC Riverside Masters - Data Science/Class - Information Retrieval/Project - Build Search Engine/Data-method2"

Rcrawler(Website = seedUrl, no_cores=4, no_conn=4,  DIR=directory, urlregexfilter="/biz/.+")