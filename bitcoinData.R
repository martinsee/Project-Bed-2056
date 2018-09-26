#kableExtra
#dplyr::do (summarise med flere verdier), lager liste, må bruke unnest() for å åpne lista. 

install.packages("gtrendsR")
install.packages("PxWebApiData")
library(gtrendsR)
library(PxWebApiData)
library(tidyr)
library(dplyr)
library(lubridate)

getwd()
bitcoin_data <- read.csv("data/bitcoinprice.csv")
summary(bitcoin_data)
names(bitcoin_data)
head(bitcoin_data)

bitcoin_price <- bitcoin_data[,c(4,9)]
str(bitcoin_price)
bitcoin_price$date <- ymd(bitcoin_price$date)

bitcoin_trends <- gtrends(keyword = "bitcoin", geo = "", time = "today+5-y",
        gprop = c("web", "news", "images", "froogle", "youtube"),
        category = 0, hl = "en-US", low_search_volume = FALSE,
        cookie_url = "http://trends.google.com/Cookies/NID")

#unnest(bitcoin_trends) error, no applicable method for unnest
interest_world <- bitcoin_trends[[1]]
countries <- na.omit(bitcoin_trends[[2]])
related_words <- bitcoin_trends[[6]] #relevant?
related_queries <- bitcoin_trends[[7]]

head(interest)
head(bitcoin_data)

#Interest over time
#Numbers represent search interest relative to the highest point on the chart for the given region and time.
#A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular. 
#A score of 0 means there was not enough data for this term.


