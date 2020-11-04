library(xts)
library(dplyr)
library(lubridate)
# Import Excel von ido.lubw
library(readxl)
OzonStuttgart2019 <- read_excel("~/Documents/OzonStuttgart2019.xls")
Ozon <- OzonStuttgart2019[,c(4,5)]
names(Ozon) <- c("Datum","Werte")
zeit <- as.POSIXct(Ozon$Datum)
ozonXTS <- xts(Ozon$Werte, zeit) %>% na.locf()
head(ozonXTS)
plot(ozonXTS)
ti <- index(ozonXTS)
o_data <- coredata(ozonXTS)
