# BW log
library(lubridate)
library(tidyverse)# enthält ggplot
library(magrittr)
library(xts)
library(forecast)
library(readxl)
library(modelr)
#options(na.action = na.warn) # globale Optionen
base_path <- "~/Documents/Luftqualitaet/Daten"
list.files(base_path) # [5]  = BW_dat
BW_path <- file.path(base_path,"BW_dat")# "~/Documents/Luftqualitaet/Daten/BW_dat"
BW_files <- list.files(BW_path) # 18 files "Alb, Odw, Hall, SWS"
# local_path zum Ordner BW_dat  (enthält "SWS", "Odw", "Alb")
SWS_files <- BW_files[str_detect(BW_files,"^SWS")] # 8 files
Alb_files <- BW_files[str_detect(BW_files,"^Alb")] # 5 files
Odw_files <- BW_files[str_detect(BW_files,"^Odw")] #  4 files
# Dateien lesen
read.data <- function(path,names= TRUE,skp= 0) {
  read_delim(path, ";",skip = skp,
             escape_double = FALSE,
             col_names = names,
             locale = locale(#date_format = "%d.%m.%y",
                             #time_format = "%H:%M:%S",
                             decimal_mark = ",",
                             grouping_mark = "",
                             tz = "CET"),
             trim_ws = TRUE)
}
read.locfiles <- function ( loc, comp,y= "_00") {
  loc.list <- list.files(file.path(base_path,"BW_dat"))
  loc.files <-loc.list[str_detect(loc.list,paste0("^",loc))]
  comp.i<- paste0(comp,y)
  comp.select <- loc.files[str_detect(loc.files,comp.i)]
  full_path <- file.path(BW_path,comp.select)
  read.data(full_path)
}
bw_list<- list(locs = c("SWS","Odw","Alb"),
                       comp = c("Temp","NO2","NO","O3"))
SWS_NO <- read.locfiles(loc="SWS",comp = "NO")
SWS_NO2 <- read.locfiles(loc= "SWS", comp = "NO2")
SWS_O3 <- read.locfiles(loc = "SWS", comp = "O3")
SWS_Temp <- read.locfiles(loc ="SWS", comp = "Temp", y = "_01")
ODW_NO2 <- read.locfiles(loc = "Odw", comp = "NO2")
ODW_NO <- read.locfiles(loc = "Odw", comp= "NO")
ODW_O3 <- read.locfiles(loc = "Odw", comp = "O3") #"Alb_47650_O3_00_19.csv"
ODW_temp <- read.locfiles(loc ="Odw", comp = "Temp")
dim(ODW_temp)# 210383  8
head(ODW_temp)
head(ODW_NO)# Stationsnummer `Datum / Uhrzeit`    Wert
ALB_NO <- read.locfiles(loc = "Alb",comp = "NO")
ALB_NO2 <- read.locfiles(loc = "Alb", comp = "NO2")
ALB_O3 <- read.locfiles(loc = "Alb", comp = "O3")
#direktimport
ALB_O3 <- Alb_47650_O3_00_19[,c(1,4,5)]
dim(ALB_O3) #173207      7
head(ALB_O3,2)
names_NO2 <- c ("Station","Datum_Zeit","NO2")
names_O3 <- c("Station","Datum_Zeit","O3")
names_NO <- c("Station","Datum_Zeit","NO")
head(ODW_temp,2)
names(ODW_temp) <- c("Station","Datum_Zeit","temp")
names(SWS_NO) <- names_NO
names(SWS_NO2) <- names_NO2
names(SWS_O3) <- names_O3
names(ODW_NO2) <- names_NO2
names(ODW_NO) <- names_NO
names(ODW_O3) <- names_O3
names(ALB_NO2) <- names_NO2
names(ALB_NO) <- names_NO
names(ALB_O3) <- names_O3
# cleaning data
sum(is.na(SWS_NO2$NO2)) #8501
length(SWS_NO2$NO2) # 173159
# fehlende Messwerte durch Mittelwerte der Zeitreihe ersetzen
repl <- function(x){
  replcmt <- mean(x, na.rm = TRUE)
  replace_na(x,replcmt)

}
SWS_NO$NO <- repl(SWS_NO$NO)
SWS_NO2$NO2 <-repl(SWS_NO2$NO2)
SWS_O3$O3 <- repl(SWS_O3$O3)
ODW_NO2$NO2 <-repl(ODW_NO2$NO2)
ODW_O3$O3 <- repl(ODW_O3$O3)
ODW_NO$NO <- repl(ODW_NO$NO)
ODW_temp$temp <- repl(ODW_temp$temp)
ALB_NO2$NO2 <- repl(ALB_NO2$NO2)
ALB_NO$NO <- repl(ALB_NO$NO)
ALB_O3$O3 <- repl(ALB_O3$O3)
# Zeitreihe bereinigen
length(ODW_NO2$NO2) # 105216 bis 2012-01-01 23:00
head(ODW_NO2,1) #76118  strat = "2000-01-01 01:00:00"   NO2 = 20
make_xts <- function(df,start) {
  df <- as_tibble(df)
  ind <- seq(from = start, by = "1 hour",
             length.out = nrow(df))
  xts(df[,3], order.by = ind)
}
start <- ymd_h("2000-01-01 01")
SWS_NO_XTS <- make_xts(SWS_NO,start)
SWS_NO2_XTS <- make_xts(SWS_NO2,start)
SWS_O3_XTS <- make_xts(SWS_O3,start)
ODW_NO2_XTS <- make_xts(ODW_NO2,start)
ODW_NO_XTS <- make_xts(ODW_NO,start )
ODW_O3_XTS <- make_xts(ODW_O3,start)
SWS_O3_XTS <- make_xts(SWS_O3,start)
#Tagesmittelwerte Odenwald
ODW_NO2.d_XTS <- ODW_NO2_XTS %>% apply.daily(mean)
length(ODW_NO2.d_XTS) #4385
ODW_NO.d_XTS <- ODW_NO_XTS %>% apply.daily(mean)
length(ODW_NO.d_XTS)
ODW_O3.d_XTS <- ODW_O3_XTS %>% apply.daily(mean)
ODW_temp.d_XTS <- make_xts(ODW_temp,start) %>% apply.daily(mean)
ODW.d_XTS <- ODW_temp.d_XTS %>% merge(ODW_NO2.d_XTS) %>%
                merge(ODW_NO.d_XTS) %>%
                merge (ODW_O3.d_XTS)
ALB_O3_XTS <- make_xts(ALB_O3,start)[1:173159,]
ALB_NO2_XTS <-make_xts(ALB_NO2,start)
ALB_NO_XTS <- make_xts(ALB_NO,start)
ALB_XTS <- merge(ALB_NO2_XTS,ALB_NO_XTS) %>%
  merge(ALB_O3_XTS)
ODW_XTS <- merge(ODW_NO2_XTS,ODW_NO_XTS) %>%
           merge(ODW_O3_XTS)
SWS_XTS <- merge(SWS_NO2_XTS,SWS_NO_XTS) %>%
  merge(SWS_O3_XTS)
ind_SWS <- index(SWS_XTS) # length 173183
ind_ODW <- index(ODW_XTS) # length 105216 3 Spalten
ind_ALB <- index(ALB_XTS) # length 173159
head(SWS_XTS) # indx NO2, NO, O3
length(SWS_XTS$NO2) #173183
ODW_log2 <-coredata(ODW_XTS)+0.1 %>% log2
ODW_log2 <- as_data_frame(ODW_log2 )
head(ODW_log,2) #NO2        NO       O3
                #16.67807 -1.321928 33.67807
str(ODW_log2)

names(ODW_log2) <- c("y","x1","x2")
my_ODW_md <- lm(y ~ x1+x2,data=ODW_log2)
coef(my_ODW_md) # (Intercept)          x1          x2
                #17.5035682       0.9958627  -0.1240933

var(ODW_log2)
ODW_mean <- ODW_log2 %>% map_dbl(mean) #NO2        NO        O3
                                      #7.516011   -2.269685  62.269768
ODW_var <- ODW_log2 %>% map_dbl(var)  #76.586855   9.579946 945.232552
cor(ODW_log2$y,ODW_log2$x1) #0.4640086
cor(ODW_log2$y,(ODW_log2$x1-ODW_log2$x2))
