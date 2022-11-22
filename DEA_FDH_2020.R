########DATA ENVOLVENT ANALYSIS#####

#limpiamos el espacio de trabajo
rm(list = ls())
getwd()

#install.packages("googledrive")
l#ibrary(googledrive)

setwd("C:/Users/ADMN/Downloads/TIPOS_2020")

##instalamos paquetes
install.packages("Benchmarking")
install.packages("psych")
install.packages("foreign")
install.packages("haven")

#cargamos libreria 
library(Benchmarking) #for DEA
library(psych)  #for basic statictics
library(foreign) #read stata, spss files, and save stata files
library(haven) #also read stata files ,with labels 

##################### EFICIENCIA 2020 ###################

data <-read_dta("Tipo_B3_2020.dta")
summary(data)

describe((data$gasto_percapita_2020))
describe((data$IDEM_2020_normal))

#some basics
class(data)
str(data)

#input and ouput selection
x <- with(data, cbind(gasto_percapita_2020))
y <- matrix(data$IDEM_2020_normal)

##############Calculating Efficiency DEA-VRS#########

#variable returns the scale
bcc <- dea(x,y, RTS="vrs", ORIENTATION = "in")
bcc
shapiro.test((bcc$eff)) #to check normality
#ad.test(bcc$eff)


eff(bcc) #score de eficiencia
data.frame(bcc$eff)
summary(bcc)
  
#guardamos puntaje de eficiencia
data$dea_vrs_eff=bcc$eff

##############Calculating Efficiency DEA-FDH#########

fdh <- dea(x,y,RTS = "fdh", ORIENTATION = "in" )
fdh
shapiro.test(fdh$eff) #to check normality
#ad.test(fdh$eff)

#eficiencia fdh
eff(fdh) #score de eficiencia
data.frame(fdh$eff)
summary(fdh)

#guardamos puntaje de eficiencia
data$dea_fdh_eff=fdh$eff


############  PLOTS #############
# Two rows, two columns
par(mfrow = c(1, 2))
title(main = "TIPOLOGIA B3")
#plot dea vrs
dea.plot(x,y, RTS = "vrs", ORIENTATION = "in", ann = FALSE, col = "springgreen4",  pch = 16) + 
  title(main = "DEA-VRS",  xlab = "GASTO PERCAPITA",
        ylab = "IDEM")

#plot fdh
dea.plot(x,y, RTS = "fdh", ORIENTATION = "in-out", ann = FALSE,  col = "darkred",  pch = 16) +
title(main = "FDH ",  xlab = "GASTO PERCAPITA",
      ylab = "IDEM")


##########exportar a stata
write.dta(data, "Tipo_B3_2020.dta")

