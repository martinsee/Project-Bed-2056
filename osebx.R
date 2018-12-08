library(tidyverse)
library(readxl)
library(lubridate)
library(mosaic)
library(formattable)
library(tidyquant)
library(xts)
library(PerformanceAnalytics)
library(plotly)
library(tsibble)


#Hente kursdata for equinor
eqnr <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=EQNR.OSE&csv_format=csv",
                 col_types = cols(quote_date = col_date(format = "%Y%m%d")))

eqnr_candle <- eqnr %>%
  filter(quote_date > "2016-01-01") %>%
  select(quote_date, open, close, high, low) %>%
  rename(Dato = "quote_date")


eqnr <- eqnr %>%
  select(quote_date, close) %>%
  rename(Dato = "quote_date", EQNR = "close")


eqnr_candle %>%
plot_ly(x = ~Dato, type = "ohlc",
        open = ~open, high = ~high,
        low = ~low, close = ~close) %>%
  layout(title = "Equinor")

monthly_eqnr <- eqnr %>%
  group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(EQNR = mean(EQNR))


xts_eqnr_m <- to.monthly(as.xts(eqnr, order.by = eqnr$Dato), OHLC = FALSE)
eqnr_monthly_prices <- as.tibble(xts_eqnr_m)
rownames(eqnr_monthly_prices) <- c()

xts_eqnr2 <- xts(eqnr$Dato, eqnr$EQNR, order.by = eqnr$Dato)
colnames(xts_eqnr2) <- paste0("EQNR") #uten dato_kolonne, men mister desimal.

returns_xts <- Return.calculate(xts_eqnr2)
Return.annualized(returns_xts, scale = 365)*100 

eqnr_returns <- eqnr %>% 
  mutate(returns = c(NA, diff(log(EQNR)))) %>%
  na.omit()
  
#Henter kursdata for hovedindeksen OSEBX
osebx_xl <- read_excel("data/osebx.xlsx")             

osebx_xl <- osebx_xl %>%
rename(Dato = "OSEBX", OSEBX = "Siste") %>%
  select(Dato, OSEBX)

osebx$Dato <- ymd(osebx$Dato)

osebx <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=OSEBX.OSE&csv_format=csv",
                       col_types = cols(quote_date = col_date(format = "%Y%m%d")))

osebx <- osebx %>%
select(quote_date, close) %>%
  rename(Dato = "quote_date", OSEBX = "close")

monthly_osebx <- osebx %>%
  group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(OSEBX = mean(OSEBX))
#to.monthly?

market_returns <- osebx_xl %>% 
  filter(Dato > "2001-06-17") %>% #matcher data for equinor
  mutate(returns = c(NA, diff(log(OSEBX)))) %>%
  na.omit()

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
eiendom$Dato <- as.Date(eiendom$Dato)

monthly_joined <- monthly_osebx %>%
  inner_join(gull, by = "Dato") %>% 
  inner_join(usd_nok, by = "Dato") %>%
  inner_join(monthly_eqnr, by = "Dato") %>%
  inner_join(eiendom, by = "Dato")

#quarterly_joined <- oecd.hus %>%
 # left_join(monthly_osebx, by = "Dato") %>% 
#  left_join(gull, by = "Dato") %>% 
#  left_join(usd_nok, by = "Dato") %>%
#  left_join(monthly_eqnr, by = "Dato")

full <- osebx %>%
  full_join(gull, by = "Dato") %>% 
  full_join(usd_nok, by = "Dato") %>%
  full_join(eqnr, by = "Dato") %>%
  full_join(bitcoin, by = "Dato") %>%
  full_join(eiendom, by = "Dato")

monthly_joined <- monthly_joined %>%
 mutate(Gull_nok = Gull_usd*USD_NOK)

bitcoin_joined <- bitcoin %>% #egen på grunn av få måneder med data
  left_join(usd_nok, by = "Dato") %>%
  mutate(BTC_nok = BTC_usd*USD_NOK, returns = c(NA, diff(log(BTC_nok)))) %>%
  na.omit() %>%
  mutate(cumsum = cumsum(returns))

monthly_gathered <- monthly_joined %>%
  gather(key = "investering", value = "verdi",-Dato)

#quarterly_gathered <- quarterly_joined %>%
 # gather(key = "investering", value = "verdi",-Dato)

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

tidy_monthly_returns <- monthly_gathered %>%
  group_by(investering) %>%
  tq_transmute(mutate_fun = periodReturn, period = "monthly")

total_monthly_returns <- tidy_monthly_returns %>%
  mutate(cumulative.returns = cumsum(monthly.returns))

wide_monthly_returns <- tidy_monthly_returns %>%
  spread(investering, monthly.returns)

monthly_returns_xts <- as.xts(wide_monthly_returns, order.by = wide_monthly_returns$Dato)      
monthly_returns_xts <- monthly_returns_xts[,2:6] #husk å oppdatere

sd_returns <- StdDev(monthly_returns_xts)

annualized_sd <- StdDev.annualized(monthly_returns_xts, scale = 12) #should be viewed with suspicion
Return.annualized(monthly_returns_xts, scale =12) #funker ikke, se på to.monthly

cumsum_test <- monthly_returns_xts %>%
  cumsum() %>%
  tail(n=1)

plot(StdDev(monthly_returns_xts), cumsum_test)
  
ggplot(total_monthly_returns, aes(x=Dato, y = cumulative.returns, color = investering))+
  geom_line()

summary(monthly_returns) 

names(monthly_returns_xts)
equal_weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
higher_stocks = c(0.4, 0.1, 0.1, 0.4, 0)

#for some reason returns are class character 
storage.mode(monthly_returns_xts) <- "numeric"

StdDev(monthly_returns_xts, weights = equal_weights) # sd = 0.022
StdDev(monthly_returns_xts, weights = higher_stocks) # sd = 0.041

Return.portfolio(monthly_returns_xts, weights = equal_weights)
sum(Return.portfolio(monthly_returns_xts, weights = equal_weights))
Return.portfolio(monthly_returns_xts, weights = higher_stocks)
sum(Return.portfolio(monthly_returns_xts, weights = higher_stocks))

#quarterly_returns<- quarterly_gathered %>%
#  group_by(investering) %>%
#  arrange(investering, Dato) %>%
#  mutate(returns = c(NA, exp(diff(log(verdi)))-1)) %>%
#  na.omit()

#cumsum2 <- quarterly_returns %>%
#  group_by(investering) %>%
#  mutate(cum_returns = cumsum(returns))

#ggplot(cumsum2, aes(x=Dato, y= cum_returns, col = investering)) + geom_line()


monthly_returns %>% 
  group_by(investering, year=floor_date(Dato, "year")) %>%
  summarise(sd = sd(returns))

monthly_returns %>% 
  group_by(investering) %>% #, year=floor_date(Dato, "year")) %>%
  summarise(sum_monthly_returns = sum(returns), sd_monthly_returns = sd(returns))

sum <- monthly_returns %>%
  group_by(investering, year=floor_date(Dato, "year")) %>%
  summarise(total_returns = sum(returns), mean_price = mean(verdi), sd_price = sd(verdi), mean_returns = mean(returns), sd_returns = sd(returns)) %>%
  arrange(year)

ggplot(sum, aes(x=sd_returns, y= total_returns, color = investering)) +
  geom_point()
#ingen tydelig sammenheng mellom sd og årlige returns


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
# stemmer returns på bitcoin? skal ha gått ned med 75% siden topp.

#Samvariasjon mellom equinor hovedindeksen?
fit <- lm(OSEBX~EQNR, data=monthly_joined)
summary(fit)
#Multiple R-squared = 0.6067
plotModel(fit)

cor(monthly_joined$OSEBX, monthly_joined$EQNR)
#korrelasjon = 0.78

#noe med oljepris?

#beta for equinor (market = osebx)
beta <- cov(eqnr_returns$returns, market_returns$returns)/var(market_returns$returns)
beta #1.01

#Samvariasjon mellom gull/nok og USD/nok
fit2 <- lm(Gull_nok~USD_NOK, data=monthly_joined)
summary(fit2)

cor(monthly_joined$Gull_nok, monthly_joined$USD_NOK)
#korrelasjon = 0.14

#Samvariasjon mellom BTC/nok og USD/nok
fit3 <- lm(BTC_nok~USD_NOK, data=monthly_joined)
summary(fit3)
cor(monthly_joined$BTC_nok, monthly_joined$usd_kurs)

market <- market_returns %>%
  full_join(eqnr_returns, by = "Dato")

marketNA <- market %>%
filter(is.na(.$OSEBX))
#osebx har 3 færre observasjonser, mangler data for 25. okt 2005, 25 apr 2003 og 27 mai 2002.