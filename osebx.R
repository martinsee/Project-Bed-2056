library(tidyverse)
library(readxl)
library(lubridate)
library(mosaic)

#Hente kursdata for equinor
eqnr <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=EQNR.OSE&csv_format=csv",
                 col_types = cols(quote_date = col_date(format = "%Y%m%d")))

eqnr <- eqnr %>%
  select(quote_date, close) %>%
  rename(Dato = "quote_date", EQNR = "close")

monthly_eqnr <- eqnr %>%
  group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(EQNR = mean(EQNR))

#Henter kursdata for hovedindeksen OSEBX
osebx <- read_excel("data/osebx.xlsx")             #kan oppdateres

osebx <- osebx %>%
  rename(Dato = "OSEBX", OSEBX = "Siste") %>%
  select(Dato, OSEBX)

osebx$Dato <- ymd(osebx$Dato)

monthly_osebx <- osebx %>%
  group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(OSEBX = mean(OSEBX))

#Henter prisdata for bitcoin i USD
bitcoin <- read_csv("data/BTC_USD Bitfinex Historical Data.csv") #kan oppdateres
glimpse(bitcoin)

bitcoin <- bitcoin %>%
  select(Date, Price) %>%
  rename(Dato = "Date", btc_usd = "Price")

bitcoin$Dato <- mdy(bitcoin$Dato)

head(bitcoin) #tidligste dato er 2. feb 2012 
tail(bitcoin) 

#Henter prisdata for gull i USD
gull <- read_csv("data/monthly_csv.csv")                      #kanskje oppdateres?

names(gull) <- c("Dato", "Gull_usd")
gull$Dato <- parse_date_time(gull$Dato, orders = "Ym")

#Henter kursdata for USD/NOK                                  #kan oppdateres
usd_nok <- read_csv("data/USD_NOK Historical Data.csv")
usd_nok <- usd_nok %>%
  select(Date, Price) %>% 
  rename(Dato = "Date", USD_NOK = "Price")

glimpse(usd_nok)
Sys.setlocale("LC_TIME", "C") #Slet med norske månedforkortelser i as.Date()

usd_nok$Dato <- usd_nok$Dato %>%
  as.character() %>%
  parse_date_time(.,orders = "Omy")

#Joiner dataene med left_join på dato og fjerner de som ikke har match.lurt?
#akkurat nå er enkelte class 'POSIXct' og andre 'Date', gjør om POSIXct til Date
gull$Dato <- as.Date(gull$Dato)
usd_nok$Dato <- as.Date(usd_nok$Dato)

monthly_joined <- monthly_osebx %>%
  inner_join(gull, by = "Dato") %>% 
  inner_join(usd_nok, by = "Dato") %>%
  inner_join(monthly_eqnr, by = "Dato")

full <- osebx %>%
  full_join(gull, by = "Dato") %>% 
  full_join(usd_nok, by = "Dato") %>%
  full_join(eqnr, by = "Dato")

#' monthly_joined <- monthly_joined %>%
#'left_join(bitcoin, by = "Dato") #egen på grunn av få datoer + daglige vs månedlige

monthly_joined <- monthly_joined %>%
 mutate(Gull_nok = Gull_usd*USD_NOK)#, BTC_nok = btc_usd*usd_kurs)

monthly_gathered <- monthly_joined %>%
  gather(key = "investering", value = "verdi",-Dato)

summary(monthly_gathered)
monthly_gathered <- monthly_gathered %>%
  filter(investering != "Gull_usd")

monthly_gathered$investering <- as.factor(monthly_gathered$investering)
monthly_gathered$verdi <- as.numeric(monthly_gathered$verdi)

summary(monthly_gathered)

ggplot(monthly_gathered, aes(x=Dato, y=verdi, col = investering)) + geom_line()

logs <- monthly_gathered %>%
  group_by(investering) %>%
  arrange(investering, Dato) %>%
  mutate(returns = c(NA, diff(log(verdi))))

logs <- logs %>%
  na.omit() %>%
  group_by(investering) %>%
  mutate(cum_returns = cumsum(returns))

ggplot(logs, aes(x=Dato, y= cum_returns, col = investering)) + geom_line()


#Samvariasjon mellom equinor hovedindeksen?
fit <- lm(OSEBX~EQNR, data=monthly_joined)
summary(fit)
#Multiple R-squared = 0.5772
plotModel(fit)

cor(monthly_joined$OSEBX, monthly_joined$EQNR)
#korrelasjon = 0.76

#lager funksjon for osebx(eqnr)
f <- makeFun(fit)
f(200)


#Samvariasjon mellom gull/nok og USD/nok
fit2 <- lm(Gull_nok~USD_NOK, data=monthly_joined)
summary(fit2)

cor(monthly_joined$Gull_nok, monthly_joined$USD_NOK)
#korrelasjon = 0.14

#Samvariasjon mellom BTC/nok og USD/nok
fit3 <- lm(BTC_nok~USD_NOK, data=monthly_joined)
summary(fit3)
cor(monthly_joined$BTC_nok, monthly_joined$usd_kurs)
#na.rm unused argument?
