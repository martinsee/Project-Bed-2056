## The trade war comparison 

cod <- ("2001-11-11")

## Oil wti 

oilwti <- read_csv("data/Crude Oil WTI Futures Historical Data.csv", 
                   col_types = cols(Date = col_date(format = "%b %d, %Y")))

oilwti <- oilwti %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))


## USA 3m Bond rate

usa3m <- read_csv("data/United States 3-Month Bond Yield Historical Data.csv", 
                  col_types = cols(Date = col_date(format = "%b %d, %Y")))

usa3m <- usa3m %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))


## Dow J. indeks 

dowj <- read_csv("data/Dow Jones Industrial Average Historical Data.csv",
                 col_types = cols(Date = col_date(format = "%b %d, %Y")))

dowj <- dowj %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))

## SP500 aksje indeks 

sp500 <- read_csv("data/^GSPC.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

sp500 <- sp500 %>%  
  select(Date, Close) %>%
  filter(Date >= as.Date(cod))


## Shanghai, fra 2011. 


shanghai <- read_csv("data/Shanghai Composite Historical Data.csv", 
                                               col_types = cols(Date = col_date(format = "%b %d, %Y"), 
                                                                Price = col_number()))

shanghai <- shanghai %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))



## Shenzhen 

shen <- read_csv("data/SZSE Component Historical Data.csv", 
                                           col_types = cols(Date = col_date(format = "%b %d, %Y")))

shen <- shen %>%
  select(Date, Price ) %>%
  filter(Date >= as.Date(cod))


## China 1Y Bond 

china_1y <- read_csv("data/China 1-Year Bond Yield Historical Data.csv", 
                                                    col_types = cols(Date = col_date(format = "%b %d, %Y")))

china_1y <- china_1y %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))

## Bitcoin 

bitcoin_day <- read_csv("data/BTC_USD Bitfinex Historical Data.csv", 
                        col_types = cols(Date = col_date(format = "%b %d, %Y")))

bitcoin_day <- bitcoin_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  


## Nasdaq 

nasdaq <- read_csv("data/NASDAQ Composite Historical Data.csv", 
                   col_types = cols(Date = col_date(format = "%b %d, %Y")))

nasdaq <- nasdaq %>%
  select(Date, Price) %>%
  filter(Date >= as.Date(cod))


## Xpr 

xpr_day <- read_csv("data/XRP Historical Data - Investing.com.csv", 
                    col_types = cols(Date = col_date(format = "%b %d, %Y")))

xpr_day <- xpr_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  


#### Felles datasett 


UC <- china_1y %>%
  full_join(sp500, by = "Date") %>% 
  full_join(oilwti, by = "Date") %>%
  full_join(shen, by = "Date") %>%
  full_join(usa3m, by = "Date") %>%
  full_join(shanghai, by = "Date") %>%
  full_join(bitcoin_day, by = "Date") %>% 
  full_join(nasdaq, by = "Date") %>%
  rename(china_bond = "Price.x", sp500 = "Close.x", oilwti = "Price.y", shen = "Price.x.x", usa_bond = "Price.y.y", shanghai = "Price.x.x.x",
         Bitcoin = "Close.y", Nasdaq = "Price.y.y.y") %>% 
      na.omit()


## Gjøre data om til lang    
  

UC_long  <- UC %>% 
  select(Date, china_bond, sp500, oilwti, shen, usa_bond, shanghai, Bitcoin, Nasdaq) %>%
  gather(key= "asset", value = "Close", -Date)

## Lage plot 

ggplot(UC_long, aes(x=Date, y=Close, col=asset ))+
  geom_line()+
  scale_y_log10(name = "Close")+
  ggtitle("USA og Kina") 


## Cor 

library(corrplot)

UC_cor  <- UC %>% 
  select(china_bond, sp500, oilwti, shen, usa_bond, shanghai, Bitcoin, Nasdaq)

res <- cor(UC_cor)
round(res, 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Den negative korrelasjon i Kina er nok et ressultat av at markedet toppet i 2015,
# hvor shanghai borsen lå på 4600, men den idag ligger 2600 som er en nedgang på 43% 
# Siden korrelasjons analyser krevet data på samme tidspunkt begrenser det vår muligheter lengre ti

#### Lengre tids analyse uten krypto

UC_u <- china_1y %>%
  full_join(oilwti, by = "Date") %>%
  full_join(sp500, by = "Date") %>% 
  full_join(usa3m, by = "Date") %>%
  full_join(shanghai, by = "Date")%>%
  rename( china_bond = "Price.x",  oilwti = "Price.y", sp500 = "Close", usa_bond = "Price.x.x", shanghai = "Price.y.y") %>% 
  na.omit()


UC_u_long  <- UC_u %>% 
  select(Date, china_bond, sp500, oilwti,  usa_bond, shanghai) %>%
  gather(key= "asset", value = "Close", -Date)

## Lage plot 

ggplot(UC_u_long, aes(x=Date, y=Close, col=asset ))+
  geom_line()+
  scale_y_log10(name = "Close")+
  ggtitle("USA og Kina") 


UC_u_cor  <- UC_u %>% 
  select(china_bond, sp500, oilwti, usa_bond, shanghai)

res2 <- cor(UC_u_cor)
round(res2, 2)

corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

### Differanse matrix 

res3

res2

res3 <- res[c(1, 2, 3, 5, 6), c(1, 2, 3, 5, 6)]

res4 <- res3 - res2 

corrplot(res4, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

res4

# Når vi endrer tidsintervallet ser vi at i differanse marixen at korrelasjonene er mye svakere ved et lengre tids



#### Undersøke de korrelasjonene som har stor differanse mellom tidsintervaller 

sp_bond <- lm(sp500~usa_bond, data = UC_u)
summary(sp_bond)
plotModel(sp_bond)

oil_shang <- lm(oilwti~shanghai, data = UC)
summary(oil_shang)
plotModel(oil_shang)

bond_shang <- lm(shanghai~china_bond, data = UC)
summary(bond_shang)
plotModel(bond_shang)


