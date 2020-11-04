library(tidyverse)
library(lubridate)
library(knitr)
#=============
load("BW_station.RData")
summary(BW_stations_NO2_tbl)
station_numbers <- BW_stations_NO2_tbl$station
stations_NO2 <- BW_stations_NO2_tbl %>% select(-station) %>% spread(name,value = NO2)
station_names <- colnames(stations_NO2)[-1]
# Trend Staedt. Hintergrund
dat <- stations_NO2 %>% dplyr::select (Brn,Can,Rt_,Lbg_4,Friedri,Heid,Heil,Egg)
dat <- dat %>% mutate(Stationsmittel = rowMeans(dat[,-1],na.rm = TRUE))
dat1 <- bind_cols(stations_NO2[1],dat)

ggplot(dat1, aes( x= datetime, y= Stationsmittel) )+
  geom_smooth(color = "black",linetype = 4,size = 4)+
  geom_smooth(aes(y= Lbg_4),color = "red")+
  geom_smooth (aes(y= Can),color = "green")+
  geom_smooth (aes (y = Brn), color= "blue")+
  geom_smooth(aes (y = Rt_),col = "red",linetype =3)+
  geom_smooth(aes (y = Heid),color = "green",linetype =3)+
  geom_smooth(aes (y = Egg),color = "blue",linetype =3)+
  geom_smooth(aes (y = Heil),color = "green",linetype =5)+
  geom_smooth(aes (y = Friedri),color = "blue",linetype =5)+
  ggtitle("NO2 Staedt. Hintergrund
    Stationen und Mittelwert",
          subtitle = "Mittel(schw - . - .),Lbg(rot),Can(gruen),Brn(blau)
  Rt(rot Pkt),Heidelb(gruen Pkt),Eggenstein(blaue Pkt)
  Heilbr(gruen Striche),Friedrichshafen(blau Striche)")+
  labs ( y = "NO2 [ug/m3] ", x= " Jahr ")
# Trend laendlicher Hintergrund
load("rural_NO2.RData")
BW_rural_NO2_tbl %>% summary()
BW_rural_NO2_tbl$name <- as_factor(BW_rural_NO2_tbl$name)
rural_names <- BW_rural_NO2_tbl %>% select(datetime,name,NO2) %>% spread(key =name,value = NO2) %>% colnames()
rural_names <- rural_names[-1] 
ggplot(BW_rural_NO2_tbl,aes(x =datetime, y = NO2))+
  geom_smooth(method= "lm",aes(color = name))+
  ggtitle ("NO2 Laendl. Hintergrund
           20-Jahrestrend ",
           subtitle = "Stationen: Schwarzwald Süd, Schwaeb. Alb, Odenwald")+
  labs ( y = "NO2 [ug/m3]", x = "")
# Trend Verkehrsnah
load("trafic_NO2.RData")
BW_trafic_NO2_tbl$name <- as_factor(BW_trafic_NO2_tbl$name)
BW_trafic_NO2_tbl %>% summary()
trafic_names <- BW_trafic_NO2_tbl %>% select(datetime,name,NO2) %>% spread(key =name,value = NO2) %>% colnames()
trafic_names <- trafic_names[-1] 

ggplot(BW_trafic_NO2_tbl,aes(x =datetime, y = NO2))+
  geom_smooth(method= "lm",aes(color = name))+
  ggtitle ("NO2 verkehrsnah
           20 Jahrestrend ",
           subtitle = 
           "Stationen: Am Neckartor, 
           Ludwigsburg Friedrichstrasse, 
           Reutlingen Lederstrasse Ost")+
  labs ( y = "NO2 [ug/m3]", x = "")
# staedtischer Hintergrund
 load("staedt_NO2.RData")
BW_staedt_NO2_tbl %>% summary()
staedt_names <- BW_staedt_NO2_tbl %>% select(datetime,name,NO2) %>% spread(key =name,value = NO2) %>% colnames()
staedt_names <- staedt_names[-1] 

ggplot(BW_staedt_NO2_tbl, aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  ggtitle ( "NO2 staedt. Hintergrund
  20 Jahrestrend",
  subtitle = "15 Stationen")+
  labs ( y = "NO2 [ug/m3]", x = "")
# Staedt. Hintergrund mit Verkehrseinfluss
ggplot(BW_staedt_NO2_tbl %>% filter(name == "Heil"| 
                                    name == "Lbg_4"|name == "Heid"|
                                      name == "Brn"), aes ( x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes(color = name))+
  ggtitle ( "NO2 staedt. Hintergrund
  Verkehrseinfluss 20 Jahrestrend",
  subtitle = "4 Stationen:
  Bernhausen, Heidelberg, Heilbronn, Lbg-Weimarstrasse")+
  labs ( y = "NO2 [ug/m3]", x = "")
 # Regressionsparameter
BW_regrpar_NO2 <- BW_stations_NO2_tbl %>% group_by(name) %>% summarise(intcpt = lm(NO2 ~ datetime)$coefficients[1],
                                                     stg = lm ( NO2 ~ datetime)$coefficients[2])
kable(BW_regrpar_NO2)
BW_regr2000_NO2 <- BW_regrpar_NO2 %>% mutate(steigY = stg*365*24*60*60,intcpt2015 = intcpt+steigY*(2015-1970)) #%>%
#  dplyr::select(name,intcpt2015,steigY)
BW_regr2000_NO2 <-BW_regr2000_NO2 %>% 
  arrange ( .$steigY)
kable(BW_regr2000_NO2)
# Veraenderung  in stations_NO2 von 2015 bis 2020
head(stations_NO2)
stations_NO2_15_20 <- stations_NO2 %>% filter (datetime > ymd("2015-01-01"))
summary(stations_NO2_15_20)
stations_NO2_00_15 <- stations_NO2 %>% filter (datetime < ymd("2015-01-01"))
summary(stations_NO2_00_15)
# Mittleren Verlauf aller Stationen zufügen
dat15_20 <- stations_NO2_15_20 %>% mutate(Stationsmittel = rowMeans(.[,-1],na.rm = TRUE))
dat00_15 <- stations_NO2_00_15 %>% mutate(Stationsmittel = rowMeans(.[,-1],na.rm = TRUE))
dat00_15 <- dat00_15 %>% gather(key = name, value = NO2, -datetime)
dat00_15$name <- dat00_15$name %>% as_factor()
dat15_20 <- dat15_20 %>% gather(key = name, value = NO2, -datetime)
dat15_20$name <- dat15_20$name %>% as_factor() 
summary (dat15_20)
NROW(dat00_15) #2103920
dat00_15 <- dat00_15 %>% na.omit()
NROW(dat00_15) #1776469
NROW(dat15_20) #775232
dat15_20 <- dat15_20 %>% na.omit()
NROW(dat15_20) # 679743
ggplot(dat15_20, aes ( x = datetime, y = NO2))+
  geom_smooth(data = dat15_20 %>% filter (name == "Stationsmittel"),method = "auto")+
  geom_smooth(method = "lm",color = "red")+
  ggtitle ( "NO2- Immissionen 15 Stationen
  mittlerer Verlauf  2015 bis 2020 (blau),
  und Regressionsgerade (rot)")+
  labs ( y = "NO2 [ug/m3]", x = "")
ggplot(dat00_15, aes ( x = datetime, y = NO2))+
  geom_smooth(data = dat00_15 %>% filter (name == "Stationsmittel"),method = "auto")+
  geom_smooth(method = "lm",color = "red")+
  ggtitle ( "NO2- Immissionen 15 Stationen
  mittlerer Verlauf  2000 bis 2015 (blau),
  und Regressionsgerade (rot)")+
  labs ( y = "NO2 [ug/m3]", x = "")
ggplot(dat15_20, aes(x = datetime, y = NO2))+
  geom_smooth(aes( color = name))+
  ggtitle ("BW NO2 - Mittelwerte
   2015 bis 2020 15 Stationen")+
  labs(x= "",y = "NO2[ug/m3]")
ggplot(dat00_15, aes(x = datetime, y = NO2))+
  geom_smooth(aes( color = name[16]))+
  ggtitle ("BW NO2 - Mittelwerte
   2000 bis 2015 15 Stationen")+
  labs(x= "",y = "NO2[ug/m3]")

ggplot(dat15_20, aes(x = datetime, y = NO2))+
  geom_smooth(method = "lm",aes( color = name))+
  ggtitle ("BW NO2 - Regressiosgeraden
       2015 bis 2020",
  subtitle = "15 Stationen")+
  labs(x= "",y = "NO2[ug/m3]")
# Regressionsparameter
BW_regrpar_NO2 <- dat3 %>% group_by(name) %>% summarise(intcpt = lm(NO2 ~ datetime)$coefficients[1],
                                                                       stg = lm ( NO2 ~ datetime)$coefficients[2])
BW_regr2000_NO2 <- BW_regrpar_NO2 %>% mutate(steigY = stg*365*24*60*60,
                                             intcpt2000 = intcpt*steigY*(2000-1970),
                                             intcpt2015) %>%
  dplyr::select(name,intcpt2000,steigY)
BW_regr2000_NO2 <-BW_regr2000_NO2 %>% 
  arrange ( .$steigY)
kable(BW_regr2000_NO2)
