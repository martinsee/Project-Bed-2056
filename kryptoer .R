# krypto valutaer 

bitcoin_day <- read_csv("data/BTC_USD Bitfinex Historical Data.csv", 
                        col_types = cols(Date = col_date(format = "%b %d, %Y")))

bitcoin_day <- bitcoin_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  