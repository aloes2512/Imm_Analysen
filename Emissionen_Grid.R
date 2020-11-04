#'Grid Emissionen'
library(tidyverse)
library(scales)
install.packages("rmarkdown")
library(rmarkdown)
library(readxl)
DE_GRID_2017 <- read_delim("~/Documents/Luftqualitaet/Daten/BRD/DE_GRID_2017.csv",
                           ";", escape_double = FALSE, trim_ws = TRUE)
head(DE_GRID_2017)
names(DE_GRID_2017) # c("Land",  "Jahr", "N14 F_RoadTransport" ,"NOx" ,"Laenge"  , "Breite", "kT" , "3.21868613317348"  ,  "X9" ) 
DE_GRID_2017 <- DE_GRID_2017[,-9]
names(DE_GRID_2017) <- c("Land",  "Jahr", "N14 F_RoadTransport" ,"NOx" ,"Laenge"  , "Breite", "kT" , "kiloTonnen"  ) 
DE_GRID_2017 %>% mutate ( Land = as_factor(Land))

DE_GRID_2017 %>% tail()
DE_GRID_2017 %>% summary()
DE_GRID_2017 %>% filter ( Breite > 48.6 & Breite < 48.9) %>% filter ( Laenge > 9.0 & Laenge < 9.3)
DE_GRID_2017 %>% filter ( Laenge >= 9.0 & Laenge < 9.3) %>% filter ( Breite >= 48.6 & Breite <= 48.95)
NOx_F_RoadTransport_2019_GRID_2017 <- read_excel("~/Documents/Luftqualitaet/Daten/EU/NOx_F_RoadTransport_2019_GRID_2017.xlsx",
                                                 col_types = c("text", "numeric", "text",
                                                               "text", "numeric", "numeric", "text", 
                                                               "text"), skip = 4)
NOx_F_RoadTransport_2019_GRID_2017$EMISSION <- NOx_F_RoadTransport_2019_GRID_2017$EMISSION %>% 
  str_replace("E","e") %>% str_replace(",",".")%>% as.numeric()
names(NOx_F_RoadTransport_2019_GRID_2017) <-names(NOx_F_RoadTransport_2019_GRID_2017) %>% str_replace("# Format: ISO2","Land")
NOx_F_RoadTransport_2019_GRID_2017 <- NOx_F_RoadTransport_2019_GRID_2017 %>% mutate( Land = as_factor(Land))
# Alle erfassten Länder
summary(NOx_F_RoadTransport_2019_GRID_2017)
# Auswahl DE
NOx_DE_Road_Transport_2017 <- NOx_F_RoadTransport_2019_GRID_2017 %>% filter( Land == "DE")
NOx_Stgt_Road_Transport_2017 <-NOx_DE_Road_Transport_2017 %>% 
  filter(LONGITUDE > 9.0 & LONGITUDE < 9.3)%>% filter (LATITUDE  > 48.6 & LATITUDE  < 48.90)
summary(NOx_Stgt_Road_Transport_2017)
NOx_Stgt_Road_Transport_2017$EMISSION %>% sum()	# 4897.331


Gesamt_Stgt_NOx_Transport_2017 <- NOx_Stgt_Road_Transport_2017$EMISSION %>% sum (na.rm = T) # 5704.706 t
## Test auf NA wegen E-2
summary(NOx_Stgt_Road_Transport_2017)
## EU Grid Daten 2016
NOx_F_RoadTransport_2019_GRID_2016 <- read.table("~/Documents/Luftqualitaet/Daten/EU/NOx_2019_GRID_2016/NOx_F_RoadTransport_2019_GRID_2016.txt",
                                                 sep=";", quote="\"")
names(NOx_F_RoadTransport_2019_GRID_2016) <- names(NOx_F_RoadTransport_2019_GRID_2017)
NOx_DE_Road_Transport_2016 <- NOx_F_RoadTransport_2019_GRID_2016 %>% filter( Land == "DE")
NOx_Stgt_Road_Transport_2016 <-NOx_DE_Road_Transport_2016 %>% 
  filter(LONGITUDE > 9.0 & LONGITUDE < 9.3)%>% filter (LATITUDE  > 48.6 & LATITUDE  < 48.9)
summary(NOx_Stgt_Road_Transport_2016)
NOx_Stgt_Road_Transport_2016$EMISSION %>% sum()
Gesamt_Stgt_NOx_Transport_2016 <- NOx_Stgt_Road_Transport_2016$EMISSION %>% sum (na.rm = T) # 6024.965
## EU Grid 2015
NOx_F_RoadTransport_2019_GRID_2015 <- read.table("~/Documents/Luftqualitaet/Daten/EU/NOx_2019_GRID_2015/NOx_F_RoadTransport_2019_GRID_2015.txt", 
                                                 sep=";", quote="\"")
names(NOx_F_RoadTransport_2019_GRID_2015) <- names(NOx_F_RoadTransport_2019_GRID_2017)
NOx_DE_Road_Transport_2015 <- NOx_F_RoadTransport_2019_GRID_2015 %>% filter( Land == "DE")
NOx_Stgt_Road_Transport_2015 <-NOx_DE_Road_Transport_2015 %>% 
  filter(LONGITUDE > 9.0 & LONGITUDE < 9.3)%>% filter (LATITUDE  > 48.6 & LATITUDE  <= 48.9)
summary(NOx_Stgt_Road_Transport_2015)
NOx_Stgt_Road_Transport_2015$EMISSION %>% sum()

Gesamt_Stgt_NOx_Transport_2015 <- NOx_Stgt_Road_Transport_2015$EMISSION %>% sum (na.rm = T) # 6345.561
Stg_Emissions_Trend <- tibble ( Jahr = 2015:2017,
                               Mittel = c( mean(NOx_Stgt_Road_Transport_2015$EMISSION),
                                           mean(NOx_Stgt_Road_Transport_2016$EMISSION),
                                          mean(NOx_Stgt_Road_Transport_2017$EMISSION)),
                               Mingrid = c( min(NOx_Stgt_Road_Transport_2015$EMISSION),
                                            min(NOx_Stgt_Road_Transport_2016$EMISSION),
                                           min(NOx_Stgt_Road_Transport_2017$EMISSION)),
                               Maxgrid = c( max(NOx_Stgt_Road_Transport_2015$EMISSION),
                                            max(NOx_Stgt_Road_Transport_2016$EMISSION),
                                            max(NOx_Stgt_Road_Transport_2017$EMISSION)),
                               Median= c( median(NOx_Stgt_Road_Transport_2015$EMISSION),
                                          median(NOx_Stgt_Road_Transport_2016$EMISSION),
                                           median(NOx_Stgt_Road_Transport_2017$EMISSION)))
Stg_Emissions_Trend 
ggplot(Stg_Emissions_Trend,aes(x = Jahr))+
  geom_line(aes( y = Mittel),col = "red")+
  geom_line(aes( y= Median), col = "purple")+
  geom_line(aes( y = Mingrid), col = "green")+
  geom_line(aes ( y = Maxgrid), col = "blue")+
  ggtitle("NOx Emissionen Stgt & Umgebung 
  Strassenverkehr  2015 bis 2017
        0.1° Grid 
Min (gruen),Median(violet),Mittel(rot), Max (blau)",
          subtitle = "Jahrestonnen pro 0.1-Grad Gridelement")+
  labs (y = "Jahrestonnen NOx ")
save(Stg_Emissions_Trend, file = "Stg_Emissions_Trend.RData")
NOx_RoadTansport <- list(NOx_2015= NOx_F_RoadTransport_2019_GRID_2015,
                         NOx_2016 = NOx_F_RoadTransport_2019_GRID_2016,
                         NOx_2017 = NOx_F_RoadTransport_2019_GRID_2017)
save(NOx_RoadTansport, file = "NOx_RoadTransport.RData")
# Daten des UBA
 BRD_NOx <-read_xlsx("~/Documents/Luftqualitaet/Daten/BRD/2019-02-15_em_entwicklung_in_d_trendtabelle_luft_v1.3_final.xlsx",
                     sheet = 4,skip = 1) %>% as_tibble()

 names(BRD_NOx) <- c("categories", "A", 1990:2017 %>% as.character())
 BRD_NOx <- BRD_NOx %>% dplyr::select (-"A")

 BRD_NOx <- BRD_NOx %>% gather(-"categories",key = "Jahr",value = "ktonnen",factor_key = TRUE)
 BRD_NOx$categories <- BRD_NOx$categories %>% as_factor()
 BRD_NOx$Jahr <- BRD_NOx$Jahr %>% as.numeric() 
 length(BRD_NOx$categories)/28
 
 x <-rep(1990:2017,39)
 x <- x[order(x)]
 BRD_NOx$Jahr <- x
 summary(BRD_NOx)
BRD_NOx$categories 
# Beispiel  Auswahl "Total Emissions"
 BRD_NOx %>% filter (categories == "Total Emissions")
BRD_NOx_transport <- BRD_NOx %>% filter(categories == "Total Emissions"|categories== "3. Transport"| categories == "thereof Road transportation") 
Emission_Trend.BRD <- ggplot(BRD_NOx_transport,aes(x = Jahr , y = ktonnen, col= categories) )+
  geom_point()+
  geom_smooth(method = "lm",mapping = aes(x = Jahr , y = ktonnen, col= categories))+
  ggtitle(" NOx Emissionen in BRD 1990- 2017 ",
         subtitle = "Quelle UBA; Regressionsgeraden")
ggsave(Emission_Trend.BRD, file = "Abbldg/Emissionen_Trend_BRD.png")
# Abnahme

Abnahme <- BRD_NOx_transport %>% group_by(categories) %>% 
  summarise(p =1- last(ktonnen)/first(ktonnen), Abnahme  = scales::percent(-p), A_p_J = p/28, Abnahme_pro_Jahr = scales::percent(-A_p_J))
save (Abnahme, file = "Emissionsabn90-17.RData")
