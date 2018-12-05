
daily_btc <- read_csv("data/BTC_USD Bitfinex Historical Data.csv") #kan oppdateres
glimpse(daily_btc)

daily_btc$Date <- mdy(daily_btc$Date)

daily_btc %>%
  plot_ly(x = ~Date, type = "ohlc",
          open = ~Open, high = ~High,
          low = ~Low, close = ~Price) %>%
  layout(title = "Bitcoin")


daily_btc <- daily_btc %>%
  select(Date, Price) %>%
  rename(Dato = "Date", btc_usd = "Price")

daily_osebx <- osebx_xl <- read_excel("data/osebx.xlsx")

daily_osebx <- daily_osebx %>%
  select(OSEBX, Siste, 'Offisielt omsatt (NOK)') %>%
  rename()

