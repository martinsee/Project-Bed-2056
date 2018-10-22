library(dplyr)

getwd()

eur_usd <- read.csv("data/EUR_USD Historical Data.csv")

eur_usd <- eur_usd %>%
  select(1,2)

names(eur_usd) <- c("date", "price")

#eur_usd <- eur_usd %>%
#  arrange(desc())

head(eur_usd)

#plot(eur_usd$date, eur_usd$price)

#må fikse dato-kolonna

nok_usd <- read.csv("data/NOK_USD Historical Data.csv")

nok_usd <- nok_usd %>%
  select(1,2)

names(nok_usd) <- c("date", "price")

head(nok_usd)



