library(readxl)#
library(dplyr)#
library(xts)#
library(tibble)
library(lubridate)#
library(forecast)#
library(DataCombine)
in_path <-"/Users/alfloeffler/Documents/Luftqualitaet/Daten/Reutlingen/Reutlingen_NO2_2016bis2019.xlsx"
REUTLINGEN76364_2016bis2019_total <- read_excel(in_path)
REUTLINGEN76364_2016bis2019 <- REUTLINGEN76364_2016bis2019_total[,c(4,5)] %>% na.locf()
dim(REUTLINGEN76364_2016bis2019) #27984 Werte
colnames(REUTLINGEN76364_2016bis2019) <- c("Datum","NO2")
# erste Messung
first(REUTLINGEN76364_2016bis2019$Datum) # erste Wert 2016-02-24 01:00
last(REUTLINGEN76364_2016bis2019$Datum)# 2019-05-07 24:00
mean(REUTLINGEN76364_2016bis2019$NO2,na.rm = T) # 57,83
sqrt(var(REUTLINGEN76364_2016bis2019$NO2,na.rm = T)) # 24,29
REUTLINGEN76364_2016bis2019$Datum <- ymd_hm(REUTLINGEN76364_2016bis2019$Datum)
#xts objekt erzeugen
REUTLINGEN76364_2016bis2019_hXTS <- xts(REUTLINGEN76364_2016bis2019$NO2,
                            order.by = REUTLINGEN76364_2016bis2019$Datum, tz = "Europe/Berlin") %>% na.locf()
mean(REUTLINGEN76364_2016bis2019_hXTS["2016"]) # 65.87523
mean(REUTLINGEN76364_2016bis2019_hXTS["2017"]) #Stundenmittel  59.35856
mean(REUTLINGEN76364_2016bis2019_hXTS["2018"]) # Stundenmittel 52.25298
mean(REUTLINGEN76364_2016bis2019_hXTS["2018-05-09/2019-05-08"]) # 50.82591
JMW <- c(65.87,59.35,52.25,50.82)
JMW_indx <- as.Date(c("2016-12-31","2017-12-31","2018-12-31","2019-05-08"))
JMW_XTS <- xts(JMW, order.by = JMW_indx)
plot(JMW_XTS)
sqrt(var(REUTLINGEN76364_2016bis2019_hXTS["2018"])) # Standardabweichung #21.31
max(REUTLINGEN76364_2016bis2019_hXTS["2018"]) #151
min(REUTLINGEN76364_2016bis2019_hXTS["2018"]) # 4
#gleitender Mittelwert 2016 bis 2019 jeweils 8760h
x_movingavg <- ma(REUTLINGEN76364_2016bis2019$NO2,order = 8760,centre =T)
X_movingavg <- x_movingavg[!is.na(x_movingavg)]
sqrt(var(X_movingavg)) # 1s = 5.164494
plot(X_movingavg)
last(X_movingavg) #50.84658 letzter Jahresmittelwert
plot(X_movingavg["2016-05"])
length(X_movingavg) #19224+ 8760 = 27984

# Jahreswerte JMW
mean(REUTLINGEN76364_2016bis2019_hXTS["2016"]) #
mean(REUTLINGEN76364_2016bis2019_hXTS["2017"]) #Stundenmittel
mean(REUTLINGEN76364_2016bis2019_hXTS["2018"]) #Stundenmittelmean(REUTLINGEN76364_2016bis2019_hXTS["2018-05-09/2019-05-08"])
mean(REUTLINGEN76364_2016bis2019_hXTS["2018-05-09/2019-05-08"])
#Kontrolle
x <- coredata(REUTLINGEN76364_2016bis2019_hXTS["2018-05-09/2019-05-08"])
mean(x) #50.82591
# Monatsmittelwerte
REUTLINGEN76364_2016bis2019_m <- apply.monthly(REUTLINGEN76364_2016bis2019_hXTS,mean)
plot(REUTLINGEN76364_2016bis2019_m)
last(REUTLINGEN76364_2016bis2019_m) # 44.22807
sqrt(var(REUTLINGEN76364_2016bis2019_m)) #8.3
#Kontrolle
mean(REUTLINGEN76364_2016bis2019_hXTS["2016-9"]) # 75.25417
mean(REUTLINGEN76364_2016bis2019_hXTS["2017-1"]) # 73.95968
mean(REUTLINGEN76364_2016bis2019_hXTS["2018-3"]) # 57.2288
mean(REUTLINGEN76364_2016bis2019_hXTS["2018-7"]) # 63.79704
mean(REUTLINGEN76364_2016bis2019_hXTS["2019-2"]) # 62.17857
MMW <- c(75.25417,73.95968,57.2288,63.79704,62.17857)
MMW_index <- as.Date(c("2016-9-15","2017-1-15","2018-3-15","2018-7-15","2019-2-15"))
MMW_XTS <- xts(MMW, order.by = MMW_index)
plot((MMW_XTS))
