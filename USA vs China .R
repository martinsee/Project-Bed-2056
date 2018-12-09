## The trade war comparison 

cod <- ("2002-01-03")

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
  full_join(dowj, by = "Date") %>% 
  full_join(oilwti, by = "Date") %>%
  full_join(shen, by = "Date") %>%
  full_join(usa3m, by = "Date") %>%
  full_join(shanghai, by = "Date") %>%
  full_join(xpr_day, by = "Date") %>% 
  full_join(bitcoin_day, by = "Date") %>% 
  full_join(nasdaq, by = "Date")%>%
  rename(china_bond = "Price.x", dowj = "Close.x", oilwti = "Price.y", shen = "Price.x.x", usa_bond = "Price.y.y", shanghai = "Price",
         Ripple = "Close.x.x", Bitcoin = "Close.y", Nasdaq = "Close.y.y")



## Gjøre data om til lang    
  

UC_long  <- UC %>% 
  select(Date, china_bond, dowj, oilwti, shen, usa_bond, shanghai, Ripple, Bitcoin, Nasdaq) %>%
  gather(key= "asset", value = "Close", -Date)

## Lage plot 

ggplot(UC_long, aes(x=Date, y=Close, col=asset ))+
  geom_line()+
  scale_y_log10(name = "Close")+
  ggtitle("USA og Kina") 


## Cor 

library(corrplot)

UC_cor  <- UC %>% 
  select(china_bond, dowj, oilwti, shen, usa_bond, shanghai, Ripple, Bitcoin, Nasdaq)

res <- cor(UC_cor)
round(res, 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#### Lengre tids analyse uten krypto

UC_u <- china_1y %>%
  full_join(oilwti, by = "Date") %>%
  full_join(usa3m, by = "Date") %>%
  full_join(shanghai, by = "Date") %>%
  full_join(nasdaq, by = "Date") %>% 
  rename( china_bond = "Price.x",  oilwti = "Price.y", usa_bond = "Price.x.x", shanghai = "Price.y.y", nasdaq = "Price") %>% 
  na.omit()


UC_u_long  <- UC_u %>% 
  select(Date, china_bond, oilwti,  usa_bond, shanghai, nasdaq) %>%
  gather(key= "asset", value = "Close", -Date)

## Lage plot 

ggplot(UC_u_long, aes(x=Date, y=Close, col=asset ))+
  geom_line()+
  scale_y_log10(name = "Close")+
  ggtitle("USA og Kina") 


UC_u_cor  <- UC_u %>% 
  select(china_bond, oilwti, usa_bond, shanghai, nasdaq)

res <- cor(UC_u_cor)
round(res, 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


