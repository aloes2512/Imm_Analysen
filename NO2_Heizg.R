#' gam'
library(gam)
library(mgcv)
library(tidyverse)
library(lubridate)
library(modelr)
library(xts)
library(forecast)
# lade Daten
load("/Users/alfloeffler/Documents/Luftqualitaet/Daten/BW/BW.RData")
summary(BW.all_data$Brn_data$Brn.NO2)
Brn_NO2 <- BW.all_data$Brn_data$Brn.NO2
Brn_Temp <- BW.all_data$Brn_data$Brn.Temp
HeizDaten_Brn <-Brn_Temp %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Brn_Temp) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Brn_Temp),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg)
write.csv2(HeizDaten_Brn, file = "HeizDatenBrn.csv",row.names = F)
Brn_data <- left_join(Brn_NO2,Brn_Temp)
Brn_dat_15_19 <- Brn_data %>% filter (datetime > ymd("2015-01-01")&datetime < ymd("2019-12-31"))
Brn_dat_15_19_Heizg <- Brn_dat_15_19 %>% mutate(Grdz = ifelse(Temp <15,20-Temp,0))
Brn_dat_15_19_Heizg$NO2 <- Brn_dat_15_19_Heizg$NO2 %>% na.locf()
Brn_dat_15_19_Heizg$Temp <- Brn_dat_15_19_Heizg$Temp %>% na.locf()

Brn_dat_15_19_Heizg %>% head(2)
names(Brn_dat_15_19_Heizg)
Brn_15_19_Grdz <- Brn_dat_15_19_Heizg$Grdz
Brn_Heiz_data <- tibble(x =Brn_dat_15_19_Heizg$datetime,
                       y =Brn_15_19_Grdz)
Brn_Heiz_data %>% ggplot(aes(x=Brn_dat_15_19_Heizg$datetime,
                             y =Brn_15_19_Grdz))+
  geom_point(aes(x,y),size = 0.0001,alpha= 0.5)+
  geom_smooth(method = mgcv::gam,formula= y ~s(x,k= 12),col = "red")+
  ggtitle("Heizbedarf aus 1-h Werten
  Bernhausen 2015 bis 2019",
          subtitle = "Gradzahl Einzelwerte für Temp < 15 °C 
     GAM Mittelwerte (k =12 ) (rote Linie)")+
  labs (x = "", y = "Gradzahl [°C]")+
  coord_cartesian(xlim = ymd(c("2015-05-31","2019-05-31"))%>% as.POSIXct())


# Example from "http://environmentalcomputing.net/intro-to-gams/"
x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)
y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))
#================================================================
Sample_data <- data.frame(y,x)
#=============================================
ggplot(Sample_data,aes(x,y))+
  geom_point(shape =1)
gam_y <- mgcv:: gam(y ~ s(x), method = "REML")
#extract fitted values
#fitting with linear model
lm_y <- lm(y ~ x, data = Sample_data)
x_new <- seq(0, max(x), length.out = 100)
y_pred <- predict(gam_y, data.frame(x = x_new))
ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = "gam", formula = y ~s(x))
ggplot(Sample_data, aes(x, y)) + geom_point() + geom_smooth(method = "gam")
# check diagnostics
par(mfrow = c(2,2))
gam.check(gam_y)
plot(gam_y,residuals = TRUE,pch =1)
# Application to NO2 Analysis
Can_dat_15_19_Heizg %>% head(2)
Can_dat_15_19_Heizg %>% summary()
Can_15_19_Heizg <- Can_dat_15_19_Heizg$datetime%>% as.numeric()
Can_15_19_Grdz <- Can_dat_15_19_Heizg$Grdz%>% as_vector()
Can_Heiz_data <- tibble(x =Can_dat_15_19_Heizg$datetime,
                    y =Can_15_19_Grdz)
xlim <- c(Can_dat_15_19_Heizg$datetime[4343],
          Can_dat_15_19_Heizg$datetime[NROW(Can_Heiz_data)-24*15*7])
Can_Heiz_data %>% ggplot(aes(x,y))+
  geom_point(size = 0.0001,alpha = 0.5)+
  geom_smooth( mapping = aes(x,y),method = mgcv::gam,formula= y ~s(x,k= 12),col = "red")+
  ggtitle("Heizbedarf aus 1-h Werten
  Bad Cannstatt 2015 bis 2019",
          subtitle = "Gradzahl Einzelwerte für Temp < 15 °C 
      GAM Mittelwerte (k = 12) (rote Linie)")+
  labs (x = "", y = "Gradzahl [°C]")+
  coord_cartesian(xlim = xlim)
gam_yy <- mgcv:: gam(yy ~ s(xx), method = "REML")
coefficients(gam_yy)
par(mfrow = c(1,1))
termplot(lm(yy ~xx),partial.resid = TRUE, se = TRUE)
termplot(lm(y ~x,data = Heiz_data),partial.resid = TRUE, se = TRUE)
#extract fitted values
yy_pred <- predict(gam_yy,data.frame(xx))
summary(yy_pred)
ggplot(Heiz_data,aes(x =xx,y=yy))+geom_point(shape = 1,size = 0.1)+
  geom_smooth(method = "gam",formula = y ~ s(x))
# setting komplexity
gam_yy_20 <- mgcv:: gam(yy ~ s(xx,k= 20), method = "REML")
plot(gam_yy_20,residuals = F,pch =1)
summary(gam_yy)
 my_tbl <- tibble(tm =Can_dat_15_19_Heizg$datetime[1:9996],
                  res =gam_yy_20$residuals,
                  fitted = gam_yy_20$fitted.values)
 my_tbl %>% ggplot(aes(x= tm, y =fitted))+
   geom_line()+
   geom_point(aes(y =res),shape = 1)
ggplot(my_tbl,aes(x = tm))+
  geom_line(aes(y =fitted))
# smoothing parameters
gam_yy_sm1 <- mgcv:: gam(yy ~ s(xx),sp = 2, method = "REML")# stark geglättet
gam_yy_sm2 <- mgcv:: gam(yy ~ s(xx),sp = 0.001, method = "REML")# stark angepasst
par(mfrow= c(1,1))
plot(gam_yy_sm1,residuals = F,pch =1)
plot(gam_yy_sm2,residuals = F,pch =1)

my_sm_tbl <- tibble(tm =Can_dat_15_19_Heizg$datetime[1:9996],
                    resid = gam_yy_sm1$residuals,
                    fit1= gam_yy_sm1$fitted.values,# stark geglättet
                    fit2 = gam_yy_sm2$fitted.values)# stark angepasst
my_sm_tbl %>% ggplot(aes(x=tm))+
  geom_line(aes(y= fit1),col = "red")+
  geom_line(aes(y = fit2),col = "green")

no2 <- Can_dat_15_19_Heizg$NO2
length(no2) #41951
zeitind <- Can_dat_15_19_Heizg$datetime
length(zeitind) #41951
Can_NO2.gam <-gam(yy ~s(xx),method = "REML")
Can_NO2.gam$fitted.values %>% summary()
Can_dat_15_19_Heizg$NO2 %>% summary()
length(Can_NO2.gam$fitted.values) #41951
NROW(Can_dat_15_19_Heizg)
Can_dat_15_19_Heizg <- Can_dat_15_19_Heizg %>% mutate(no2.gam = Can_NO2.gam$fitted.values )
Can_dat_15_19_Heizg %>% ggplot(aes(datetime, NO2))+
  geom_line(aes(y= no2.gam))
Can_NO2.gam$deviance
Can_NO2.gam$null.deviance
Can_NO2.gam$call
# Reutlingen
Rt_NO2 <- BW.all_data$Rt_data$Rt.no2
Rt_Temp <- BW.all_data$Rt_data$Rt.temp
HeizDaten_Rt <-Rt_Temp %>% filter (Temp < 15) %>%
  summarise (Gesamt_stunden =NROW(Rt_Temp) ,Hz_stunden = n(),Anteil_Hzg = Hz_stunden / NROW(Rt_Temp),
             Temp_mittel_Hzg =mean(Temp, na.rm = TRUE),
             Gradzahl_Hzg = 20-Temp_mittel_Hzg)
write.csv2(HeizDaten_Brn, file = "HeizDatenBrn.csv",row.names = F)
Rt_data <- left_join(Rt_NO2,Rt_Temp)
Rt_dat_15_19 <- Rt_data %>% filter (datetime > ymd("2015-01-01")&datetime < ymd("2019-12-31"))
Rt_dat_15_19_Heizg <- Rt_dat_15_19 %>% mutate(Grdz = ifelse(Temp <15,20-Temp,0))
Rt_dat_15_19_Heizg$NO2 <- Rt_dat_15_19_Heizg$NO2 %>% na.locf()
Rt_dat_15_19_Heizg$Temp <- Rt_dat_15_19_Heizg$Temp %>% na.locf()

Rt_dat_15_19_Heizg %>% head(2)
names(Rt_dat_15_19_Heizg)
Rt_15_19_Grdz <- Rt_dat_15_19_Heizg$Grdz
Rt_Heiz_data <- tibble(x =Rt_dat_15_19_Heizg$datetime,
                        y =Rt_15_19_Grdz)
Rt_Heiz_data %>% ggplot(aes(x,y))+
  geom_point(size = 0.0001,alpha = 0.5)+
  geom_smooth( mapping = aes(x,y),method = mgcv::gam,formula= y ~s(x,k= 12),col = "red")+
  ggtitle("Heizbedarf aus 1-h Werten
  Reutlingen 2015 bis 2019",
          subtitle = "Gradzahl Einzelwerte für Temp < 15 °C 
      GAM Mittelwerte (k = 12) (rote Linie)")+
  labs (x = "", y = "Gradzahl [°C]")+
  coord_cartesian(xlim = xlim)




y_hat <- function(t,day){ 
  sin (t+day*24)
  }
y_hat(pi/2,10)
time <- Can_dat_15_19_Heizg$datetime %>% as.numeric()
time[1] 
time %>% head()
Can_dat_15_19_Heizg$datetime %>% format("%m-%d %H") %>% head()
ymd_hms("2015-01-01 01:00:00 UTC") %>% as.numeric()
ti <- 1:8760/8760*2*pi # 0 bis 2 pi
sin_y <- y_hat(ti,day = 365)
plot(x= ti+ pi,y= sin_y)
sin_y %>% head()
