library(lubridate)
library(tidyverse)# enth??lt ggplot
library(magrittr)
library(xts)
library(forecast)
library(readxl)
library(modelr)
#options(na.action = na.warn) # globale Optionen
base_path <- "~/Documents/Luftqualitaet/Daten"
base_path_mac <- "~/Desktop/Luft_macbook19_11_11/Daten"
list.files(base_path) # [5]  = BW_dat
list.files(base_path_mac)
BW_path <- file.path(base_path,"BW_dat")# "~/Documents/Luftqualitaet/Daten/BW_dat"

BW_path_mac <- file.path(base_path_mac,"BW_dat")
BW_files <- list.files(BW_path) # 19 files
BW_files_mac <- list.files(BW_path_mac )
BW_files_mac
SZ_list <-BW_path %>% file.path("SZ_1987_2019") %>% list.files
SZ_list_00_18 <- SZ_list[14:32]
path_00 <- BW_path %>% file.path("SZ_1987_2019") %>%
  file.path(SZ_list[[14]]) #"~/Documents/Luftqualitaet/Daten/BW_dat
                            #/SZ_1987_2019/2000-Tabelle 1.csv"
SZ_00 <- read.data(path_00)

# local_path zum Ordner BW_dat  (enth??lt "SWS", "Odw", "Alb")
SWS_files <- BW_files_mac[BW_files_mac %>% str_detect ("SWS") ]
Alb_files <- BW_files_mac [ BW_files_mac %>% str_detect("Alb")]
Odw_files <- BW_files_mac [ BW_files_mac %>% str_detect("Odw")]
# Dateien lesen
read.data <- function(path,names= TRUE,skp= 0) {
  read_delim(path, ";",skip = skp,
             escape_double = FALSE,
             col_names = names,
             locale = locale(date_format = "%Y-%m-%d",
                             time_format = "%H:%M",
                             decimal_mark = ",",
                             grouping_mark = "",
                             tz = "CET"),
             trim_ws = TRUE)
}
#====NO2 -Daten lesen=======
Odw_NO2_path <- file.path(BW_path_mac,"Odw_76118_NO2_00_19.csv")
Odw_NO2 <- read.data (Odw_NO2_path)
SWS_NO2_path <- file.path(BW_path_mac,"SWS_4467_NO2_00_19.csv" )
SWS_NO2 <-read.data(SWS_NO2_path)
Alb_NO2_path <- file.path(BW_path_mac,"Alb_47650_NO2_00_19.csv")
Alb_NO2 <- read.data(Alb_NO2_path)
Brn_NO2_path <- file.path (BW_path_mac,"Brn_39253_NO2_00_19.csv")
Brn_NO2 <- read.data(Brn_NO2_path)
Rt_NO2_path <- file.path(BW_path_mac,"Rt_4470_NO2_07_19.csv")
Rt_NO2 <- read.data(Rt_NO2_path)
Can_NO2_path <- file.path(BW_path_mac,"Can_4452_NO2_00_19.csv")
Can_NO2 <- read.data(Can_NO2_path)
Lbg_NO2_path <- file.path(BW_path_mac,"Lbg_4463_NO200_19.csv")
Lbg_NO2 <- read.data(Lbg_NO2_path)
Lbg_vk_NO2_path <- file.path(BW_path_mac, "Lbg_76359_NO2_00_19.csv" )
Lbg_vk_NO2 <- read.data(Lbg_vk_NO2_path)
load("~/Documents/Luftqualitaet/Daten/Reutlingen/RT.RData")
RT_NO2 <- RT_data$Rt.no2

Nck_vk_NO2_path <- file.path(BW_path_mac,"Nck_76361_NO2_00_19.csv" )
Nck_vk_NO2 <-  read.data(Nck_vk_NO2_path)
# ==== NO2 Daten bereinigen =====
summary(Odw_NO2)






SWS_NO <- read.locfiles(loc="SWS",comp = "NO")[,c(1,4,5)]
SWS_NO2 <- read.locfiles(loc= "SWS", comp = "NO2")[,c(1,4,5)]
SWS_temp <- read.data(file.path(BW_path,"SWS_4467_Temp_01_19.csv" ))[,c(1,4,5)]
dim(SWS_temp) # 326372      3
head(SWS_temp,2) # 2001-02-21 15:00:00
                 # 2001-02-21 15:30:00
ODW_NO2 <- read.locfiles(loc = "Odw", comp = "NO2")[,c(1,4,5)]
ODW_NO <- read.locfiles(loc = "Odw", comp= "NO")[,c(1,4,5)]
ODW_O3 <- read.locfiles(loc = "Odw", comp = "O3") [,c(1,4,5)]#"Alb_47650_O3_00_19.csv"
ODW_temp <- read.locfiles(loc = "Odw",comp = ("Temp")) [,c(1,4,5)]
dim(ODW_temp) # 210383
ALB_NO <- read.locfiles(loc = "Alb",comp = "NO")[,c(1,4,5)]
ALB_NO2 <- read.locfiles(loc = "Alb", comp = "NO2")[,c(1,4,5)]
ALB_O3 <- read.locfiles(loc = "Alb", comp = "O3")[,c(1,4,5)]
ALB_temp <- read.locfiles(loc ="Alb",comp = "Temp")[,c(1,4,5)]
Ber_path <- file.path(BW_path,)
Ber_temp <- read.data(my_path)[,c(1,4,5)]
names_NO2 <- c ("Station","Datum_Zeit","NO2")
names_O3 <- c("Station","Datum_Zeit","O3")
names_NO <- c("Station","Datum_Zeit","NO")
names_temp <- c("Station","Datum_Zeit","temp")
names(SWS_NO) <- names_NO
names(SWS_NO2) <- names_NO2
names(SWS_O3) <- names_O3
names(SWS_temp) <- names_temp
names(ODW_NO2) <- names_NO2
names(ODW_NO) <- names_NO
names(ODW_O3) <- names_O3
names(ODW_temp) <- names_temp
names(ALB_NO2) <- names_NO2
names(ALB_NO) <- names_NO
names(ALB_O3) <- names_O3
names(ALB_temp) <- names_temp
names(Ber_temp) <- names_temp
# CLEANING DATA
#=============================
sum(is.na(SWS_NO2$NO2)) #8501
length(SWS_NO2$NO2) # 173159
sum(is.na(Ber_temp$Datum_Zeit))
NROW(Ber_temp) #326372
# fehlende Messwerte durch Mittelwerte der Zeitreihe ersetzen
repl <- function(x){
  replcmt <- mean(x, na.rm = TRUE)
  replace_na(x,replcmt)

}
hourly_mean <- function(my_ts) {
  res <- my_ts %>%
    group_by(day_h =format(Datum_Zeit, '%Y-%m-%d %H') )%>%
    summarise(Meanval=mean(temp, na.rm=TRUE))
}
head(SWS_NO)
SWS_NO$NO <- repl(SWS_NO$NO)
SWS_NO2$NO2 <-repl(SWS_NO2$NO2)
SWS_O3$O3 <- repl(SWS_O3$O3)
SWS_temp$temp <- repl(SWS_temp$temp)
mean(SWS_temp$temp) # 8.18
ODW_NO2$NO2 <-repl(ODW_NO2$NO2)
ODW_O3$O3 <- repl(ODW_O3$O3)
ODW_NO$NO <- repl(ODW_NO$NO)
ODW_temp$temp <- repl(ODW_temp$temp)
ALB_NO$NO <- repl(ALB_NO$NO)
ALB_NO2$NO2 <- repl(ALB_NO2$NO2)
ALB_O3$O3  <- repl(ALB_O3$O3)
ALB_temp$temp <- repl(ALB_temp$temp)
# Mache TBL
make_tbl <- function(ODW_temp,comp) {
    ODW_temp_tbl <- as.data.frame(ODW_temp) %>%
  select(Datum_Zeit,comp) %>% as_tibble()
  }
ALB_NO_TBL <- make_tbl(ALB_NO,comp ="NO")
ALB_NO2_TBL <- make_tbl(ALB_NO2, "NO2")
ALB_O3_TBL <- make_tbl(ALB_O3,"O3")
ALB_TEMP_TBL <- make_tbl(ALB_temp,"temp")

str(ODW_temp_tbl)# 210383 obs. 2 var "Datum_Zeit" "temp"


ODW_Temp <- hourly_mean(ODW_temp)[-1,]
SWS_Temp <- hourly_mean(SWS_temp)[-1,]
Alb_Temp <- hourly_mean(ALB_temp)[-1,]
Ber_Temp <- hourly_mean(Ber_temp)[-1,]
# Kontrolle
head(ODW_Temp)
mean(ODW_Temp$Meanval) # 9.05
NROW(ODW_Temp) # 105180
NROW(Ber_Temp) # 163169
Ber_Temp$Meanval <- repl(Ber_Temp$Meanval)
mean(Ber_Temp$Meanval) # 8.2
ind_ODW_Temp <- seq(from = ymd_h(ODW_Temp[1,1]),
                    by = "1 hour", length.out = NROW(ODW_Temp))
ODW_Temp_XTS <- xts(ODW_Temp[,2], order.by = ind_ODW_Temp)


ALB_NO2$NO2 <- repl(ALB_NO2$NO2)
ALB_NO$NO <- repl(ALB_NO$NO)
ALB_O3$O3 <- repl(ALB_O3$O3)
ALB_temp$temp <- repl(ALB_temp$temp)
mean(ALB_temp$temp) # 7.88
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
Odw_NO2_XTS <- ODW_NO2_XTS[ind_Odw_Temp]#verk??rzte Sequenz
ODW_NO_XTS <- make_xts(ODW_NO,start )
Odw_NO_XTS <- ODW_NO_XTS[ind_Odw_Temp]#verk??rzte Sequenz
ODW_O3_XTS <- make_xts(ODW_O3,start)
Odw_O3_XTS <- ODW_O3_XTS[ind_Odw_Temp]#verk??rzte Sequenz
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
# ??bersicht gewinnen
ggplot(SWS_NO2_XTS)+
  geom_smooth(method = "auto",
              aes(x= index(SWS_NO2_XTS),y = NO2))
ggplot(ODW_NO2_XTS)+
  geom_smooth(method = "auto",
              aes(x= index(ODW_NO2_XTS),y = NO2))
ggplot(ALB_NO2_XTS)+
  geom_smooth(method = "auto",
              aes(x= index(ALB_NO2_XTS),y = NO2))
# Darstellungen Schwarzwald - S??d Station: 4476
SWS_plot <- ggplot(SWS_XTS)+
  geom_smooth(method = "auto",aes(x= SWS_XTS$Datum_Zeit,
                                  y = SWS_XTS$O3))
SWS_plot1 <- SWS_plot+
  geom_smooth(method = "auto",aes(x =SWS_XTS$Datum_Zeit,
                                  y = SWS_XTS$NO2, color = "red"))
SWS_plot2 <- SWS_plot1+
  geom_smooth(method = "auto",aes(x =SWS_XTS$Datum_Zeit,
                                  y = SWS_XTS$NO, color = "green"))
SWS_plot3 <-SWS_plot2 + ggtitle ("Schwarzwald 4476 Immissionen",
                     subtitle = " O3 = blau; NO = gr??n; NO2 = rot")+
                     labs(  x = "Jahr",y = "??g/m3 ")
SWS_plot3
head(SWS_XTS)
SWS_tbl <- as_tibble(SWS_XTS)[,-1]
SWS_comp <- SWS_tbl %>%
            gather("NO2","NO","O3",
            key = comp,value = "??g/m3" )
SWS_comp$comp <- SWS_comp$comp %>% as_factor()
head(SWS_comp,2)
ggplot(SWS_comp,aes(x = SWS_comp$Datum_Zeit, y = SWS_comp$`??g/m3`))+
         geom_smooth(method = "auto",aes(color = comp))+
         ggtitle ("Immi. Schwarzwald 4476 ",
           subtitle = " gegl??ttete Mittelwerte (GAM)")+
           labs(  x = "Jahr",y = "??g/m3 ")
str(SWS_comp)
head(SWS_comp)
SWS_tbl <-as_tibble( SWS_XTS) %>% bind_cols(date =ind_SWS)
length(SWS_tbl$NO2)
SWS_tbl_comp <- SWS_tbl %>% gather(key = "comp", value = Wert,-date)
SWS_tbl_comp$comp <- SWS_tbl_comp$comp %>% as_factor
SWS_tbl_comp$Wert <- SWS_tbl_comp$Wert %>% na.locf()
str(SWS_tbl_comp)
dim(SWS_tbl_comp) # 519549  3  date ; comp ; "??g/m3"

ggplot(SWS_tbl_comp,aes(x= date,y = Wert, color = comp))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "auto")+
  ggtitle ("Immi. Schwarzwald 4476 ",
           subtitle = " Regress % Mittelwerte (GAM)")+
            labs(  x = "Jahr",y = "Wert ??g/m3 ")
ODW_tbl <- as_tibble(ODW_XTS)
ODW_tbl <- ODW_tbl %>% bind_cols(date = index(ODW_XTS))
ODW_comp <- ODW_tbl %>%
  gather("NO2","NO","O3",
         key = comp,value = Wert,-date )
ODW_comp$comp <- ODW_comp$comp %>% as_factor()
head(ODW_comp,2)
ggplot(ODW_comp,aes(x= date,y = Wert, color = comp))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "auto")+
  ggtitle ("Immi Odenwald 76118 ",
           subtitle = " Regress & Mittelwerte (GAM)")+
  labs(  x = "Jahr",y = "Wert ??g/m3 ")
ALB_tbl <- as_tibble(ALB_XTS) %>% bind_cols(date = index(ALB_XTS))
ALB_comp <- ALB_tbl %>%
                    gather("NO2","NO","O3", key = comp,
                           value = "Wert", -date)
ALB_comp$comp <- ALB_comp$comp %>% as_factor()
str(ALB_comp)
ggplot(ALB_comp,aes(x= date,y = Wert, color = comp))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "auto")+
  ggtitle ("Immi SW-Alb 47650 ",
           subtitle = " Regress & Mittelwerte (GAM)")+
           labs(  x = "Jahr",y = "Wert ??g/m3 ")
Odw_plot <-ggplot(Odw_NO2_XTS, aes( x= ind_Odw_Temp, y = NO2))+
                           geom_smooth(method = "auto", aes(color = "red"))
Odw_plot2 <-Odw_plot+ geom_smooth(Odw_Temp_XTS,method ="auto",mapping = aes(x= ind_Odw_Temp,
                                                 y =coredata(Odw_Temp_XTS,
                                                             color ="blue")))
Odw_plot3 <- Odw_plot2+ geom_smooth(Odw_NO_XTS,method = "auto",
                                    mapping = aes(x = ind_Odw_Temp,
                                        y = coredata(Odw_NO_XTS),color ="green"))
Odw_plot4 <- Odw_plot3 +geom_smooth(Odw_O3_XTS, method = "auto",
                                    mapping = aes(x= ind_Odw_Temp,
                                                  y = coredata(Odw_O3_XTS, color = "black")))
Odw_plot4
Odw_XTS <- Odw_Temp_XTS %>%
           merge(Odw_NO2_XTS) %>%
           merge(Odw_NO_XTS) %>%
           merge(Odw_O3_XTS)
names(Odw_XTS) <- c("temp","NO2","NO","O3")
# als tibble formatieren
Odw_all_com <- as_tibble(coredata(Odw_XTS))
datetime <- index(Odw_XTS) %>% as_tibble()
Odw_all_comp <- datetime %>%
         merge(Odw_all_com)

ggplot(Odw_XTS,aes(x= date,y = Wert, color = comp))+
  geom_smooth(method = "lm")+
  geom_smooth(method = "auto")+
  ggtitle ("Immi Odenwald 76118 ",
           subtitle = " Regress & Mittelwerte (GAM)")+
  labs(  x = "Jahr",y = "Wert ??g/m3 ")
# Bernhausen
load("~/Documents/Luftqualitaet/Daten/BW_dat/BRN.RData")
Brn_NO2 <- BRN_data$Brn.NO2
Brn_WG <- BRN_data$Brn.Wg %>% na.omit()
summary(Brn_WG)
Brn_WG$datetime
# Nur Daten mit vorhandnen WG Werten
 BRN_NO2 <- inner_join(Brn_WG,Brn_NO2, by = c("datetime","station"))
summary(BRN_NO2)
wg.breaks <- c(0,2,4,6,8,10,13)
BRN_NO2$ klasse <- cut (BRN_NO2$WG,wg.breaks, labels = c(2,4,6,8,10,13))
ggplot(BRN_NO2)+
  geom_boxplot(aes(x = klasse,y = NO2))+
  ggtitle("Bernhausen Streuung NO2 1-h Werte",
          subtitle =" 6 WG Klassen, Station 39253" )+
  labs ( x = "Wingeschwindigkeit m/s", y = "NO2")
