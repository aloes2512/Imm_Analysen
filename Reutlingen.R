library(readxl)#
library(dplyr)#
library(xts)#
library(tibble)
library(lubridate)#
library(forecast)#
install.packages("DataCombine")
library(DataCombine)
in_path2 <- "~/Desktop/Fahrverbote/Reutlingen/Daten/ReutlingenNOx2017bis2019.xlsx"
in_path <-  "~/Desktop/Fahrverbote/Reutlingen/Daten/ReulingenLederOst2017bis2019.xlsx"
ReutlingenLederer2017bis2019 <- read_excel(in_path)
View(ReutlingenLederer2017bis2019)
#nur Datum und NOx Werte als "data.frame"
ReutlingenLederer_h <- ReutlingenLederer2017bis2019[,c(4,5)]
dim(ReutlingenLederer_h) #17761h = Zeilen; 2 Spalten
#ReutlingenLederer_h[17761,] # 2019-04-13 24:00     35
# Zahl der Stunden auf105 volle Wochen begrenzen 24h*7Tage
#ReutlingenLederer_h <- ReutlingenLederer_h[1:17641,]   %>% na.locf()

colnames(ReutlingenLederer_h) <- c("Datum","NOx")
head(ReutlingenLederer_h,1) # 2017-04-01 01:00    75
tail(ReutlingenLederer_h,1) # 2019-04-08 23:00    44
# MIttelwert über gesamte Zeit:
mean(ReutlingenLederer_h$NOx,na.rm = T) # 53.9
sqrt(var(ReutlingenLederer_h$NOx,na.rm = T)) # 22.54
ReutlingenLederer_h$Datum <- ymd_hm (ReutlingenLederer_h$Datum)
ReutlingenLederer_hXTS <- xts(ReutlingenLederer_h$NOx, order.by = ReutlingenLederer_h$Datum, tz = "CET") %>% na.locf()
nox_index <- index(ReutlingenLederer_hXTS)
ReutlingenLedere_yXTS <- apply.yearly(Reutlingen_4470hXTS,mean)
ReutlingenLedere_mXTS <-apply.monthly(Reutlingen_4470hXTS,mean)
plot(ReutlingenLedere_mXTS)
avx <- ma(ReutlingenLederer_hXTS, order = 8760, centre = F)
avx0 <- avx[!is.na(avx)]
plot(avx0)
reduz <- avx0[1]-avx0[length(avx0)]
attr(ReutlingenLederer_hXTS,"frequency") <- 24*7*26
x <- decompose(as.ts(ReutlingenLederer_hXTS))
plot(x)
plot(x$trend[15624:17640])^
plot(x$seasonal[15624:17640])
tail(merge(Wind_XTS,ReutlingenLederer_hXTS))
# MITTELWERTE TAGE
ReutlingenLederer_dXTS <- apply.daily(ReutlingenLederer_hXTS, mean)
dim(ReutlingenLederer_dXTS) #738 Tage
attr(ReutlingenLederer_dXTS,"frequency") <- 7*30
xd <- decompose(as.ts(ReutlingenLederer_dXTS))
plot(xd)
# Wochenmittel aus Stundenwerten
ReutlingenLederer_wXTS <- apply.weekly(ReutlingenLederer_hXTS,mean)
dim(ReutlingenLederer_wXTS) #107 Wochen
attr(ReutlingenLederer_wXTS,"frequency") <- 35
xw <- decompose(as.ts(ReutlingenLederer_wXTS) )
plot(xw)
# Umwandlung in Matrix 24Spalten = 24 h und 735 Zeilen = Tage
sum(is.na(ReutlingenLederer_h)) # 0
lederer_NOXmatrix <- t(matrix(ReutlingenLederer_h$NOx,nrow = 24)) #enthält noch NA
head(lederer_NOXmatrix)
stundenWerteNOx_mittel <- colMeans(lederer_NOXmatrix, na.rm = TRUE)
plot(stundenWerteNOx_mittel)
# Die Messstation liefert seit 1.4.17 Stundenwerte
ReutlingenLederer_h_letztesJahr <- ReutlingenLederer_h["2017-04-09/2019-04-08"]
ReutlingenLederer_h_letztesJahr <- na.locf(ReutlingenLederer_h_letztesJahr)
#Tagesmittel
tagesmittel_ReutlingenLederer_letztesJahr <- apply.daily(ReutlingenLedererXTS_letztesJahr[,1],FUN = mean, na.rm = TRUE)
# Tageswerte ohne Aufteilung in Stunden etc.
letzteTage <- as.Date(index(ReutlingenLedererXTS_letztesJahr))
tagesmittel_ReutlingenLederer_letztesJahr <- xts(ReutlingenLedererXTS_letztesJahr, order.by = letzteTage)
tail(tagesmittel_ReutlingenLederer_letztesJahr)
plot(tagesmittel_ReutlingenLederer_letztesJahr)
# Zerlegung in Trend, periodischen Anteil und Störungen
attr (tagesmittel_ReutlingenLederer_letztesJahr,'frequency') <- 180
decompRes <- decompose(tagesmittel_ReutlingenLederer_letztesJahr, type = "additive",frequency(7))
length(tagesmittel_ReutlingenLederer_letztesJahr) #728 ntspricht 104 Wochen
# nur NOx Daten
tagesmittel_letztesJahr_tbl <- tibble(tagesmittel_ReutlingenLederer_letztesJahr)
my.index <- letzteTage
tagesmittel_letztesJahr_tbl <- cbind(my.index,tagesmittel_ReutlingenLederer_letztesJahr)
# Mittelwert letztesJahr
mean(tagesmittel_ReutlingenLederer_letztesJahr) #51.45
#Standardabweichung letztesJahr
qu1_2019 <-tagesmittel_ReutlingenLederer_letztesJahr["201901/03"]
plot(qu1_2019)
class(qu1_2019)


my.datamatrix <- t(matrix(data =all_data, nrow  = 24))
head(my.datamatrix,1)
head(all_data)
select00_06 <-ReutlingenLedererXTS["T00:00/T06:00"]
select06_07 <-ReutlingenLedererXTS["T06:00/T07:00"]
select07_08 <-ReutlingenLedererXTS["T07:00/T08:00"]
select08_10 <-ReutlingenLedererXTS["T08:00/T10:00"]
select10_12 <-ReutlingenLedererXTS["T10:00/T12:00"]
select12_14 <-ReutlingenLedererXTS["T12:00/T14:00"]
select14_16 <-ReutlingenLedererXTS["T14:00/T16:00"]
select16_18 <-ReutlingenLedererXTS["T16:00/T18:00"]
select18_20 <-ReutlingenLedererXTS["T18:00/T20:00"]
select20_22 <-ReutlingenLedererXTS["T20:00/T22:00"]
select22_24 <-ReutlingenLedererXTS["T22:00/T24:00"]
mean(select00_06,na.rm =T)
mean(select06_07,na.rm =T)
mean(select08_10,na.rm =T)
mean(select10_12,na.rm =T)
mean(select12_14,na.rm =T)
mean(select14_16,na.rm =T)
mean(select16_18,na.rm =T)
mean(select18_20,na.rm =T)
mean(select20_22,na.rm =T)
mean(select22_24,na.rm =T)
auswahltag <- function(x,thisday){x %>% mutate(Tag = weekdays(x$Datum))%>%
                 filter (Tag == thisday) }
reutlingen_monday <-  auswahltag(ReutlingenLederer,"Montag")
reutlingen_tuesday <- auswahltag(ReutlingenLederer,"Dienstag")
reutlingen_wednesday <- auswahltag(ReutlingenLederer,"Mittwoch")
reutlingen_thursday <- auswahltag(ReutlingenLederer,"Donnerstag")
reutlingen_friday <- auswahltag(ReutlingenLederer,"Freitag")
reutlingen_saturday <- auswahltag(ReutlingenLederer,"Samstag")
reutlingen_sunday <- auswahltag(ReutlingenLederer,"Sonntag")
tail(reutlingen_monday)
sum(is.na(reutlingen_monday))
monday_hour <- t(matrix(reutlingen_monday$NOx,nrow =24))
monday.means <- colMeans(monday_hour,na.rm = TRUE)
plot(monday.means)
hour_perday <- function(x){colMeans(t(matrix(x$NOx,nrow = 24)),na.rm = T) }
plot(hour_perday(reutlingen_monday))
plot(hour_perday(reutlingen_tuesday))
plot(hour_perday(reutlingen_wednesday))
plot(hour_perday(reutlingen_thursday))
plot(hour_perday(reutlingen_friday))
plot(hour_perday(reutlingen_saturday))
plot(hour_perday(reutlingen_sunday))
is.na(ReutlingenLederer$NOx)
#Auswahl der Zeilen mit Messwerten
no_na <- !is.na(ReutlingenLederer$NOx)
sum(no_na)
ReutlingenLedererXTS <- ReutlingenLedererXTS[no_na,]
sum(is.na(ReutlingenLedererXTS))
class(ReutlingenLedererXTS)
#Gleitender Mittelwert über 365 Tage
my.ma <- ma(ReutlingenLedererXTS, order = 365*24, centre = F)
ReutlingenLedererXTS[((365*12)+1):((365*12)+6)]
my.ma[365*12+1] # erster Wert <> NA
is_val <- !is.na(my.ma)
my.ma[is_val]
plot(my.ma[is_val])
ReutlingenLedererZoo <- as.zoo(ReutlingenLedererXTS)
colnames( ReutlingenLedererZoo) <- "Daten"
index_no_na <-ReutlingenLedererZoo$Daten != NA
ReutlingenLedererZoo <- ReutlingenLedererZoo[index_no_na]
new_ma_Lederer <- ma(ReutlingenLedererZoo)
ggplot(aes(x= index(ReutlingenLedererXTS), y = my.ma) +geom_line())
fit <- stl(ReutlingenLedererXTS_2018,s.window = "periodic",s.degree = 1)
#NA entfernen mit nächstem Wert
ReutlingenLedererZoo1 <- na.locf(ReutlingenLedererZoo)
# NA entfernen mit letztem Wert
ReutlingenLedererZoo2 <- na.locf(ReutlingenLedererZoo,fromLast = TRUE)
dim(ReutlingenLedererZoo2)
last(ReutlingenLedererZoo2)
letztesJahr <-ReutlingenLederer_hXTS["2018-04-09/2019-04-08",1]
sum(is.na(letztesJahr)) #0
letztesJahr %>% mean() # 51.465
letztesJahr_lowWind <- letztesJahr[low_Wind_XTS]
letztesJahr_lowWind %>% mean()
sum(is.na(letztesJahr_lowWind))
dim(letztesJahr_lowWind) #28822 h Werte
