library(dplyr)
library(readxl)
library(tidyr)
library(readr)
library(zoo)
library(stringr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)


##### OECD Tall 

oecd.hus <- read_csv("DP_LIVE_27112018111628959.csv")

oecd.hus <- filter(oecd.hus, SUBJECT == "NOMINAL")

oecd.hus <- filter(oecd.hus, FREQUENCY == "Q")

oecd.hus$TIME <- str_replace(oecd.hus$TIME, "Q1", "01-01") 
oecd.hus$TIME <- str_replace(oecd.hus$TIME, "Q2", "01-04") 
oecd.hus$TIME <- str_replace(oecd.hus$TIME, "Q3", "01-07") 
oecd.hus$TIME <- str_replace(oecd.hus$TIME, "Q4", "01-10") 

oecd.hus$TIME <- as.Date(oecd.hus$TIME, "%Y-%m-%d")

ggplot(oecd.hus) +
geom_line(aes(x=TIME, y= Value)) 


#oecd.hus$TIME <- str_remove(oecd.hus$TIME, "Q")    #### Fjerne Q 

# oecd.hus$TIME <- as.numeric(oecd.hus$TIME)

# oecd.hus$time <- as.yearqtr(oecd.hus$TIME) 

# oecd.hus$TIME <- as.Date(as.yearqtr("oecd.hus"), frac = 1)

# oecd.hus$TIME <- parse_date_time('oecd.hus$TIME',orders = "Yq")

# oecd.hus$TIME <- parse_date_time('oecd.hus$TIME',orders = "%Y%q")


##### SSB - vanskelig å bruke 

# boligpris <- read.csv("http://data.ssb.no/api/v0/dataset/25151.csv?lang=no", sep = ";", stringsAsFactors = TRUE)

# Justere data slik at man sprer kolonnen statistikk variabel 

# boligpris <- spread(boligpris, statistikkvariabel , Gjennomsnittlig.kvadratmeterpris..etter.region..type.enebolig...r.og.statistikkvariabel)

#boligpris <- boligpris %>%
#  rename(Year = "Dato")

# 20.11 innser nå at data er i år men resten er å måneder, skal sjekke om jeg finner andre kilder, 
# eller har jeg lastet opp feil? trudde de var kvartalsmessig, sjekk ssb 














