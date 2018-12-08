

#tanker/ting å gjøre
#se på effekt i omsatt volum på børser, se på moving correlation 2017-18 eller lignende.
#lag ett datasett med rolling_cor
#undersøke hvorfor korrelasjon med olje svinger så kraftig mellom år.


### Henter og rydder bitcoin-data


daily_BTC <- read_csv("data/BTC_USD Bitfinex Historical Data.csv") #kan oppdateres
glimpse(daily_BTC)

daily_BTC$Date <- mdy(daily_BTC$Date)

daily_BTC %>%
  plot_ly(x = ~Date, type = "ohlc",
          open = ~Open, high = ~High,
          low = ~Low, close = ~Price) %>%
  layout(title = "Bitcoin")

daily_BTC <- daily_BTC %>%
  select(Date, Price) %>%
  rename(Dato = "Date", BTC_usd = "Price")

BTC2017 <- daily_BTC %>%
  filter(Dato > "2017-01-01")


#### Henter og rydder data fra OSEBX


daily_osebx <- read_excel("data/osebx.xlsx")

daily_osebx <- daily_osebx %>%
  select(OSEBX, Siste, 'Offisielt omsatt (NOK)') %>%
  rename(Dato = "OSEBX", OSEBX = "Siste", osebx_Volum = "Offisielt omsatt (NOK)")
daily_osebx$Dato <- ymd(daily_osebx$Dato)



#### Joiner bitcoin og osebx


ukedager <- daily_osebx %>%
  inner_join(daily_BTC, by = "Dato")

ggplot(ukedager, aes(x = BTC_usd, y = OSEBX)) +
  geom_point()
#non-linear and overplotting

ggplot(ukedager, aes(x = BTC_usd, y = OSEBX)) +
  geom_point(alpha = 0.2)

ggplot(ukedager, aes(x = BTC_usd, y = OSEBX)) +
      geom_point(alpha = 0.3) +
      scale_x_log10()  
#strong linear positive relationship, a few outliers (negative BTC)

ggplot(ukedager, aes(x=cut(BTC_usd, breaks = 7), y=OSEBX)) +
  geom_boxplot()
#noe med x_labs eller scipen digits?
#hva betyr boxplots igjen?
#shows again the relationship is not linear


ukedager2017 <- daily_osebx %>%
  inner_join(BTC2017, by = "Dato")

plot(ukedager2017$OSEBX, ukedager2017$BTC_usd)


##### Lager linæer modell, OSEBX avhengig variabel, bitcoin uavhengig

fit <- lm(OSEBX~BTC_usd, data = ukedager)
summary(fit)
plotModel(fit)

fit2 <- lm(OSEBX~BTC_usd, data = ukedager2017)
summary(fit2)
plotModel(fit2)

#Undersøker korrelasjon i ulike tidsintervaller.

rolling_cor <- ukedager %>% 
  mutate(cor_100 = slide2(OSEBX, BTC_usd, cor, .size = 100, .align = "right")) %>% 
  unnest() %>%
  na.omit()

mean.cor <- cor(rolling_cor$OSEBX,rolling_cor$BTC_usd)
mean.100.cor <- mean(rolling_cor$cor_100)

ggplot(data = rolling_cor) + geom_line(aes(x = Dato, y = cor_100)) + 
  geom_hline(yintercept=mean.cor, linetype="dashed", color = "red", size=1) +
  geom_hline(yintercept=mean.100.cor, linetype="dashed", color = "blue", size=1)


##### Data fra amerikansk aksjemarkedet (per nå kun S&P 500)

sp_500 <- read_csv("data/^GSPC.csv")
#S&P 500 er en indeks som består av de 500 største selskapene på NYSE (?)

sp_500 <- sp_500 %>%  
  select(Date, Close) %>%
  rename(Dato = "Date", sp500 = "Close")


######
#Joiner sp500 til osebx og btc

ukedager <- ukedager %>%
  inner_join(sp_500, by = "Dato")

ggplot(ukedager, aes(x = BTC_usd, y = sp500)) +
  geom_point()
#non-linear and overplotting

ggplot(ukedager, aes(x = BTC_usd, y = sp500)) +
  geom_point(alpha = 0.2)

ggplot(ukedager, aes(x = BTC_usd, y = sp500)) +
  geom_point(alpha = 0.3) +
  scale_x_log10()  
#strong linear positive relationship, a few outliers (negative BTC)

#####
#Fra wide til long

ukedager_long <- ukedager%>%
  gather(børs, verdi, -Dato, -BTC_usd, -osebx_Volum)

ggplot(ukedager_long, aes(x=verdi, y = BTC_usd)) +
  geom_point() +
  facet_wrap(~børs)

ggplot(ukedager_long, aes(x=verdi, y = BTC_usd, col = børs)) +
  geom_point()

#logaritmisk scale på bitcoin
ggplot(ukedager_long, aes(x=verdi, y = BTC_usd, col = børs)) +
  geom_point() +
  scale_y_log10()


rolling_cor_sp <- ukedager %>% 
  mutate(sp_cor_100 = slide2(sp500, BTC_usd, cor, .size = 100, .align = "right")) %>% 
  unnest() %>%
  na.omit()

mean.cor.sp <- cor(rolling_cor_sp$sp500,rolling_cor_sp$BTC_usd)
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