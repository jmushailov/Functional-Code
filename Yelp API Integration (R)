rm(list=ls())

require(tidyverse)
require(httr)

# If necessary
use_proxy(url = "", auth = "basic")

client_id <- "xxxx"
#client_secret deprecated as of April 2019
client_key <- "xxxxx"


res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id))
 

# Before running this 
# TO USE:
# Set term
# Limit is capped at 50
# Radius is capped at 40000
# OffsetP - leave as is for first iteration. If results don't come up as NaN comment out offsetP first vector and use second vector.
# nameOfExport is what it will export as to desktop.
# Change saveP <- "C:/Users/Joe/Desktop/YelpData" at the bottom to your desktop file path


token <- content(res)$access_token

yelp <- "https://api.yelp.com"
term <- "sushi"
location <- "00000" #Enter a Zip
categories <- NULL
limit <- 50
radius <- 16000
offsetP <- c(1,51,101,151,201,251,301,351,401,451,501,551,601)
  # Extra offset
#offsetP <- c(651,701,751,801,851,901,951,1001)
nameOfExport <- paste0(term,location)

# Before using this intialize the below function! Then you can just do all your work using the code above and the below line!
YelpScr(offsetP)








YelpScr <- function(offset=offsetP){

for(i in 1:length(offsetP)){  
  
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit,
                               radius = radius
                               ,
                               # For next pages -> offset 51 for pg 2, 101 for pg 3.....
                                offset = offsetP[i]
                  ))

res <- GET(url, add_headers('Authorization' = paste("bearer", client_key)))

results <- content(res)

yelp_httr_parse <- function(x) {
  
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     zip_code = x$location$zip_code,
                     distance = x$distance,
                     phone = x$phone,
                     url = x$url
  )
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- tibble(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   city = parse_list$city, 
                   state = parse_list$state, 
                   zip_code = x$location$zip_code,
                   distance = x$distance,
                   phone = x$phone,
                   url = x$url)
  df
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)

payload <- do.call("rbind", results_list)


# Show analysis
ratingsCalc <- function(payload){
  x <- payload$rating * payload$review_count 
  x2 <- sum(x) / sum(payload$review_count)
  print(paste0("Average Yelp rating for stores within a radius of about ", radius, " feet of ", location, 
               ", using the search term ***", term,  "*** ", "is: ", x2, " stars. This is calculated using a total of ", sum(payload$review_count), " reviews"))
  
}

msg <- ratingsCalc(payload)
strsplit(msg, "\n")



saveP <- "C:/Users/Joe/Desktop/YelpData"
concat <- paste0(saveP, offsetP[i], nameOfExport, ".csv")
write.table(payload, concat)

if(sum(payload$review_count)==0){ break}
}}
