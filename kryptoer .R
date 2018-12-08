# krypto valutaer 

cod <- "2017-01-03"

#### Bitcoin 

bitcoin_day <- read_csv("data/BTC_USD Bitfinex Historical Data.csv", 
                        col_types = cols(Date = col_date(format = "%b %d, %Y")))

bitcoin_day <- bitcoin_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  


#### Etherum 

eth_day <- read_csv("data/Ethereum Historical Data - Investing.com.csv", 
                                                   col_types = cols(Date = col_date(format = "%b %d, %Y")))


eth_day <- eth_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  


##### XPR - Ripple  


xpr_day <- read_csv("data/XRP Historical Data - Investing.com.csv", 
                                              col_types = cols(Date = col_date(format = "%b %d, %Y")))

xpr_day <- xpr_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  


#### Litcoin 


lite_day <- read_csv("data/Litecoin Historical Data - Investing.com.csv", 
                                                   col_types = cols(Date = col_date(format = "%b %d, %Y")))

lite_day <- lite_day %>%
  select(Date, Price) %>%
  rename(Close = "Price") %>%
  filter(Date >= as.Date(cod))  



### Felles datasett 


krypto <- bitcoin_day %>%
  full_join(eth_day, by = "Date") %>% 
  full_join(xpr_day, by = "Date") %>%
  full_join(lite_day, by = "Date") %>%
  rename(Bitcoin = "Close.x", Etherum = "Close.y", Ripple = "Close.x.x", Litecoin = "Close.y.y") %>% 
  na.omit()

krypto_long  <- krypto %>% 
  select(Date, Bitcoin, Etherum, Ripple, Litecoin) %>%
  gather(key= "asset", value = "Close", -Date)


ggplot(krypto_long, aes(x=Date, y=Close, col=asset ))+
  geom_line()+
  scale_y_log10(name = "Close")+
  ggtitle("Krypto markedet") 


#### Korrelasjon matrix 

# install.packages("corrplot")
library(corrplot)

krypto_cor  <- krypto %>% 
  select(Bitcoin, Etherum, Ripple, Litecoin)

res <- cor(krypto_cor)
round(res, 2)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




