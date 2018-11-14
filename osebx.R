library(readxl)
library(ggplot2)
library(tidyverse)
library(anytime)
library(stringr)
library(mosaic)

#Hente kursdata for equinor
eqnr <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=EQNR.OSE&csv_format=csv",
                 col_types = cols(quote_date = col_date(format = "%Y%m%d")))

eqnr <- eqnr %>%
  select(quote_date, close) %>%
  rename(Dato = "quote_date", EQNR = "close") %>%
  filter(Dato > "1996-01-01")

#Henter kursdata for hovedindeksen OSEBX
osebx <- read_excel("data/osebx.xlsx")
names(osebx)

osebx <- osebx %>%
  rename(Dato = "OSEBX", OSEBX = "Siste") %>%
  select(Dato, OSEBX)

osebx$Dato <- anydate(osebx$Dato)

#Henter prisdata for bitcoin i USD
bitcoin <- read.csv("data/BTC_USD Bitfinex Historical Data.csv")
glimpse(bitcoin)

bitcoin <- bitcoin %>%
  select(ï..Date, Price, Change..) %>%
  rename(Dato = "ï..Date", btc_usd = "Price", btc_change = "Change..")

bitcoin$Dato <- anydate(bitcoin$Dato)
bitcoin$btc_usd <- as.numeric(bitcoin$btc_usd)

head(bitcoin) #tidligste dato er 2. feb 2012 
tail(bitcoin)

#Henter prisdata for gull i USD
gull <- read.csv("data/monthly_csv.csv")
str(gull)

gull$Date <- anydate(gull$Date)
gull <- gull %>% 
  filter(Date >= "1996-01-01") %>%
  rename(Dato = "Date", Gull_usd = "Price")

#Henter kursdata for USD/NOK
usd_nok <- read.csv("data/USD_NOK Historical Data.csv")
usd_nok <- usd_nok %>%
  select(ï..Date, Price, Change..) %>%
  rename(Dato = "ï..Date", usd_kurs = "Price", usd_change = "Change..")

Sys.setlocale("LC_TIME", "C") #Slet med norske månedforkortelser i as.Date()

glimpse(usd_nok)
#alt dette på grunn av monthly data, forskjell?? ønskelig?
usd_nok$Dato <- usd_nok$Dato %>%
  as.character() %>%
  str_replace(" ", "-")%>%
  paste("01-", ., sep = "") %>%
  as.Date(format = "%d-%h-%y")

#Joiner dataene med left_join på dato og fjerner de som ikke har match.lurt?
compare <- osebx %>%
  left_join(gull, by = "Dato") %>% 
  left_join(usd_nok, by = "Dato") %>%
  left_join(eqnr, by = "Dato") %>%
  na.omit() #nødvendig dersom inner_join?

compare <- compare %>%
  left_join(bitcoin, by = "Dato") #egen på grunn av få datoer + daglige vs månedlige

compare <- compare %>%
  mutate(Gull_nok = Gull_usd*usd_kurs, BTC_nok = btc_usd*usd_kurs)

ggplot(compare) +
  geom_line(aes(x=Dato, y=Kurs, color = "Børs")) + 
  geom_line(aes(x=Dato, y=EQNR, color = "EQNR")) +
  geom_line(aes(x=Dato, y=Gull_nok, color = "Gull i NOK")) +
  geom_line(aes(x=Dato, y=BTC_nok, color = "BTC i NOK")) +
  ggtitle("Investeringsaltnernativ siden 1996")

summary(compare)
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
