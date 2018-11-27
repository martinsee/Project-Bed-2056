library(readxl)
library(ggplot2)
library(tidyverse)
library(mosaic)
library(lubridate)

#Hente kursdata for equinor
eqnr <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=EQNR.OSE&csv_format=csv",
                 col_types = cols(quote_date = col_date(format = "%Y%m%d")))

eqnr <- eqnr %>%
  select(quote_date, close) %>%
  rename(Dato = "quote_date", EQNR = "close") %>%
  filter(Dato > "1996-01-01")

#Henter kursdata for hovedindeksen OSEBX
osebx <- read_excel("data/osebx.xlsx")

osebx <- osebx %>%
  rename(Dato = "OSEBX", OSEBX = "Siste") %>%
  select(Dato, OSEBX)

osebx$Dato <- ymd(osebx$Dato)

#Henter prisdata for bitcoin i USD
bitcoin <- read_csv("data/BTC_USD Bitfinex Historical Data.csv")
glimpse(bitcoin)

bitcoin <- bitcoin %>%
  select(Date, Price, "Change %") %>%
  rename(Dato = "Date", btc_usd = "Price", btc_change = "Change %")

bitcoin$Dato <- mdy(bitcoin$Dato)

head(bitcoin) #tidligste dato er 2. feb 2012 
tail(bitcoin) #akkurat nå er change % class character

#Henter prisdata for gull i USD
gull <- read_csv("data/monthly_csv.csv")
glimpse(gull)

gull$Date <- parse_date_time(gull$Date, orders = "Ym")
gull <- gull %>% 
  filter(Date >= "1996-01-01") %>%
  rename(Dato = "Date", Gull_usd = "Price")

#Henter kursdata for USD/NOK
usd_nok <- read_csv("data/USD_NOK Historical Data.csv")
usd_nok <- usd_nok %>%
  select(Date, Price, "Change %") %>%
  rename(Dato = "Date", usd_kurs = "Price", usd_change = "Change %")

glimpse(usd_nok)
Sys.setlocale("LC_TIME", "C") #Slet med norske månedforkortelser i as.Date()

usd_nok$Dato <- usd_nok$Dato %>%
  as.character() %>%
  parse_date_time(.,orders = "Omy")

#Joiner dataene med left_join på dato og fjerner de som ikke har match.lurt?
#akkurat nå er enkelte class 'POSIXct' og andre 'Date', gjør om POSIXct til Date
gull$Dato <- as.Date(gull$Dato)
usd_nok$Dato <- as.Date(usd_nok$Dato)

compare <- osebx %>%
  left_join(gull, by = "Dato") %>% 
  left_join(usd_nok, by = "Dato") %>%
  left_join(eqnr, by = "Dato") %>%
  na.omit() #nødvendig dersom inner_join?

#'compare <- compare %>%
#'left_join(bitcoin, by = "Dato") #egen på grunn av få datoer + daglige vs månedlige

compare <- compare %>%
 mutate(Gull_nok = Gull_usd*usd_kurs)#, BTC_nok = btc_usd*usd_kurs)

compare2 <- compare %>%
  gather(key = "investering", value = "verdi",-Dato)

summary(compare2)
compare2$investering <- as.factor(compare2$investering)
compare2$verdi <- as.numeric(compare2$verdi)

glimpse(compare2)

ggplot(compare2, aes(x=Dato, y=verdi, col = investering)) + geom_line()

#logs <- compare2 %>%
 # mutate()


#Samvariasjon mellom equinor hovedindeksen?
fit <- lm(OSEBX~EQNR, data=compare)
summary(fit)
#Multiple R-squared = 0.5772
plotModel(fit)

cor(compare$OSEBX, compare$EQNR)
#korrelasjon = 0.76

#Samvariasjon mellom gull/nok og USD/nok
fit2 <- lm(Gull_nok~usd_kurs, data=compare)
summary(fit2)

cor(compare$Gull_nok, compare$usd_kurs)
#korrelasjon = 0.20

#Samvariasjon mellom BTC/nok og USD/nok
fit3 <- lm(BTC_nok~usd_kurs, data=compare)
summary(fit3)
cor(compare$BTC_nok, compare$usd_kurs, na.rm = TRUE)
#na.rm unused argument?
