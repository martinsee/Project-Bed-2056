library(dplyr)
library(readxl)
library(tidyr)

# Laste ned dataen fra SSB 

boligpris <- read.csv("http://data.ssb.no/api/v0/dataset/25151.csv?lang=no", sep = ";", stringsAsFactors = TRUE)

# Justere data slik at man sprer kolonnen statistikk variabel 

boligpris <- spread(boligpris, statistikkvariabel , Gjennomsnittlig.kvadratmeterpris..etter.region..type.enebolig...r.og.statistikkvariabel)


