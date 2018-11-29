library(tidyverse)
library(readxl)
library(lubridate)
library(mosaic)
library(formattable)

#Henter data for eiendomspriser
oecd.hus <- read_csv("DP_LIVE_27112018111628959.csv")

oecd.hus <- oecd.hus %>%
  filter(SUBJECT == "NOMINAL", FREQUENCY == "Q") %>%
  select(TIME, Value)

oecd.hus$TIME <- oecd.hus$TIME %>%
  str_replace_all(c("Q1" = "01-01", "Q2" = "04-01", "Q3" = "07-01", "Q4" = "10-01")) %>%
  ymd()

names(oecd.hus) <- c("Dato", "Bolig")

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

bitcoin <- bitcoin %>% group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(BTC_usd = mean(btc_usd))

#Henter prisdata for gull i USD
gull <- read_csv("data/monthly_csv.csv")                   #kanskje oppdateres?

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

#akkurat nå er enkelte class 'POSIXct' og andre 'Date', gjør om POSIXct til Date
gull$Dato <- as.Date(gull$Dato)
usd_nok$Dato <- as.Date(usd_nok$Dato)

monthly_joined <- monthly_osebx %>%
  inner_join(gull, by = "Dato") %>% 
  inner_join(usd_nok, by = "Dato") %>%
  inner_join(monthly_eqnr, by = "Dato")

quarterly_joined <- oecd.hus %>%
  left_join(monthly_osebx, by = "Dato") %>% 
  left_join(gull, by = "Dato") %>% 
  left_join(usd_nok, by = "Dato") %>%
  left_join(monthly_eqnr, by = "Dato")

full <- osebx %>%
  full_join(gull, by = "Dato") %>% 
  full_join(usd_nok, by = "Dato") %>%
  full_join(eqnr, by = "Dato") %>%
  full_join(oecd.hus, by = "Dato") %>%
  full_join(bitcoin, by = "Dato")

monthly_joined <- monthly_joined %>%
 mutate(Gull_nok = Gull_usd*USD_NOK)

bitcoin_joined <- bitcoin %>% #egen på grunn av få måneder med data
  left_join(usd_nok, by = "Dato") %>%
  mutate(BTC_nok = BTC_usd*USD_NOK, returns = c(NA, diff(log(BTC_nok)))) %>%
  na.omit() %>%
  mutate(cumsum = cumsum(returns))

monthly_gathered <- monthly_joined %>%
  gather(key = "investering", value = "verdi",-Dato)

quarterly_gathered <- quarterly_joined %>%
  gather(key = "investering", value = "verdi",-Dato)

summary(monthly_gathered)
monthly_gathered <- monthly_gathered %>%
  filter(investering != "Gull_usd")

monthly_gathered$investering <- as.factor(monthly_gathered$investering)
monthly_gathered$verdi <- as.numeric(monthly_gathered$verdi)

summary(monthly_gathered) #sjekker for NA

monthly_returns <- monthly_gathered %>%
  group_by(investering) %>%
  arrange(investering, Dato) %>%
  mutate(returns = c(NA, exp(diff(log(verdi)))-1)) %>%
  na.omit()

summary(monthly_returns) 

quarterly_returns<- quarterly_gathered %>%
  group_by(investering) %>%
  arrange(investering, Dato) %>%
  mutate(returns = c(NA, exp(diff(log(verdi)))-1)) %>%
  na.omit()

cumsum2 <- quarterly_returns %>%
  group_by(investering) %>%
  mutate(cum_returns = cumsum(returns))

ggplot(cumsum2, aes(x=Dato, y= cum_returns, col = investering)) + geom_line()


monthly_returns %>% 
  group_by(investering, year=floor_date(Dato, "year")) %>%
  summarise(sd = sd(returns))

monthly_returns %>% 
  group_by(investering, year=floor_date(Dato, "year")) %>%
  summarise(sum = sum(returns))

sum <- monthly_returns %>%
  group_by(investering, year=floor_date(Dato, "year")) %>%
  summarise(total_returns = sum(returns), mean_price = mean(verdi), sd_price = sd(verdi), mean_returns = mean(returns), sd_returns = sd(returns)) %>%
  arrange(year)
#sd på verdi eller returns? kan de sammenlignes

sum$total_returns <- percent(sum$total_returns)
sum$mean_returns <- percent(sum$mean_returns)
sum$sd_returns <- percent(sum$sd_returns)

cumsum <- monthly_returns %>%
  group_by(investering) %>%
  mutate(cum_returns = cumsum(returns))

ggplot(cumsum, aes(x=Dato, y= cum_returns, col = investering)) + geom_line()

ggplot()+
geom_line(data = cumsum, aes(x=Dato, y= cum_returns, col = investering)) +
geom_line(data = bitcoin_joined, aes(x=Dato, y=cumsum, col = "BTC"))
# stemmer returns på bitcoin, skal ha gått ned med 75% siden topp.

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
