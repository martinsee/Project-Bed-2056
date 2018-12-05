

#tanker/ting å gjøre
#se på effekt i omsatt volum på børser, se på moving correlation 2017-18 eller lignende.
#lag ett datasett med rolling_cor
#undersøke hvorfor korrelasjon med olje svinger så kraftig mellom år.


### Henter og rydder bitcoin-data


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

btc2017 <- daily_btc %>%
  filter(Dato > "2017-01-01")


#### Henter og rydder data fra OSEBX


daily_osebx <- read_excel("data/osebx.xlsx")

daily_osebx <- daily_osebx %>%
  select(OSEBX, Siste, 'Offisielt omsatt (NOK)') %>%
  rename(Dato = "OSEBX", OSEBX = "Siste", osebx_Volum = "Offisielt omsatt (NOK)")
daily_osebx$Dato <- ymd(daily_osebx$Dato)



#### Joiner bitcoin og osebx



ukedager <- daily_osebx %>%
  inner_join(daily_btc, by = "Dato")

plot(ukedager$OSEBX, ukedager$btc_usd)

ukedager2017 <- daily_osebx %>%
  inner_join(btc2017, by = "Dato")

plot(ukedager2017$OSEBX, ukedager2017$btc_usd)


##### Lager linæer modell, OSEBX avhengig variabel, bitcoin uavhengig

fit <- lm(OSEBX~btc_usd, data = ukedager)
summary(fit)
plotModel(fit)

fit2 <- lm(OSEBX~btc_usd, data = ukedager2017)
summary(fit2)
plotModel(fit2)

#Undersøker korrelasjon i ulike tidsintervaller.

rolling_cor <- ukedager %>% 
  mutate(cor_100 = slide2(OSEBX, btc_usd, cor, .size = 100, .align = "right")) %>% 
  unnest() %>%
  na.omit()

mean.cor <- cor(rolling_cor$OSEBX,rolling_cor$btc_usd)
mean.100.cor <- mean(rolling_cor$cor_100)

ggplot(data = rolling_cor) + geom_line(aes(x = Dato, y = cor_100)) + 
  geom_hline(yintercept=mean.cor, linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=mean.100.cor, linetype="dashed", color = "blue", size=1)


##### Data fra amerikansk aksjemarkedet (per nå kun S&P 500)

sp500 <- read_csv("data/^GSPC.csv")
#S&P 500 er en indeks som består av de 500 største selskapene på NYSE (?)

sp500 <- sp500 %>%  
  select(Date, Close) %>%
  rename(Dato = "Date", sp500 = "Close")

ukedager <- ukedager %>%
  inner_join(sp500, by = "Dato")

ukedager_long <- ukedager%>%
  gather(børs, verdi, -Dato, -btc_usd, -osebx_Volum)

ggplot(ukedager_long, aes(x=verdi, y = btc_usd)) +
  geom_point() +
  facet_wrap(~børs)

ggplot(ukedager_long, aes(x=verdi, y = btc_usd, col = børs)) +
  geom_point() +

#logaritmisk scale på bitcoin
ggplot(ukedager_long, aes(x=verdi, y = btc_usd, col = børs)) +
  geom_point() +
  scale_y_log10()


rolling_cor_sp <- ukedager %>% 
  mutate(sp_cor_100 = slide2(sp500, btc_usd, cor, .size = 100, .align = "right")) %>% 
  unnest() %>%
  na.omit()

mean.cor.sp <- cor(rolling_cor_sp$sp500,rolling_cor_sp$btc_usd)
mean.100.cor.sp <- mean(rolling_cor_sp$sp_cor_100)

ggplot(data = rolling_cor_sp) + geom_line(aes(x = Dato, y = sp_cor_100)) + 
  geom_hline(yintercept=mean.cor.sp, linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=mean.100.cor.sp, linetype="dashed", color = "blue", size=1)


#####
##### Analysis on monthly data.
#####


monthly_osebx <- daily_osebx %>%
  group_by(Dato=floor_date(Dato, "month")) %>%
  summarise(OSEBX = mean(OSEBX))

mnd <- monthly_osebx %>%
  inner_join(oil, by = "Dato") #%>%
  #filter(Dato > "2015-01-01")

oil <- read_csv("data/brent-monthly_csv.csv")
oil$Date <- oil$Date %>%
  str_replace_all("-15", "-01") %>%
  ymd()
names(oil) <- c("Dato", "oil")

plot(mnd$oil, mnd$OSEBX)

fit3 <- lm(OSEBX~oil, data = mnd)
summary(fit3)
plotModel(fit3)

m_rolling_cor <- mnd %>% 
  mutate(cor_3m = slide2(OSEBX, oil, cor, .size = 3, .align = "right")) %>% 
  unnest() %>%
  na.omit()

mean.cor.oil <- cor(m_rolling_cor$OSEBX,m_rolling_cor$oil)
mean.3m.cor <- mean(m_rolling_cor$cor_3m)

ggplot(data = m_rolling_cor) + geom_line(aes(x = Dato, y = cor_3m)) + 
  geom_hline(yintercept=mean.cor.oil, linetype="dashed", color = "orange", size=1) +
  geom_hline(yintercept=mean.3m.cor, linetype="dashed", color = "green", size=1)

#Konklusjon, olje har større korelleasjon med det norske aksjemarkedet enn bitcoin har.
#stemmer dette med vår analyse?