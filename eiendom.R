library(dplyr)
library(readxl)
library(tidyr)

# Laste ned dataen fra SSB 

boligpris <- read.csv("http://data.ssb.no/api/v0/dataset/25151.csv?lang=no", sep = ";", stringsAsFactors = TRUE)

?read.csv


boligpris

### De ble ikke korrekt justert, sjekk arbeidskravet hvor vi skulle laste hotelovernatting hvordan jeg gjorde det, evnt DC import (2) 


boligpris <- spread(boligpris, statistikkvariabel , Gjennomsnittlig.kvadratmeterpris..etter.region.type.enebolig...r.og.statistikkvariabel, )

?spread
