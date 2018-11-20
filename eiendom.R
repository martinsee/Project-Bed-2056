library(dplyr)
library(readxl)
library(tidyr)

# Laste ned dataen fra SSB 

boligpris <- read.csv("http://data.ssb.no/api/v0/dataset/25151.csv?lang=no", sep = ";", stringsAsFactors = TRUE)

# Justere data slik at man sprer kolonnen statistikk variabel 

boligpris <- spread(boligpris, statistikkvariabel , Gjennomsnittlig.kvadratmeterpris..etter.region..type.enebolig...r.og.statistikkvariabel)


boligpris <- boligpris %>%
  rename(Year = "Dato")
 
# 20.11 innser nå at data er i år men resten er å måneder, skal sjekke om jeg finner andre kilder, 
# eller har jeg lastet opp feil? trudde de var kvartalsmessig, sjekk ssb 