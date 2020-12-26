#En primer lugar se debe establecer el Working Directory a la carpeta de la práctica

fileDirectory <- getwd()
csv_usa <- file.path(fileDirectory, '2020 November General Election - Turnout Rates.csv')
usa_elections <- read.csv(csv_usa)

# 1. Exploración de los datos
summary(usa_elections[, 4:13])

library(Hmisc)
Hmisc::describe(usa_elections)

# 2. Selección de datos de interés
usa_elections <- usa_elections[, -ncol(usa_elections)]

usa_total <- usa_elections[1,]
usa_elections <- usa_elections[2:nrow(usa_elections),-ncol(usa_elections)]

usa_elections <- usa_elections[,-c(grep("Source", colnames(usa_elections)), grep("Official.Unofficial", colnames(usa_elections)))]