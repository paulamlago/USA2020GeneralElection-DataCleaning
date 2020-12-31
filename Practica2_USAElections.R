## -------------------------------------------------------------------------------------------------------------------------------------------------------------
fileDirectory <- getwd()
csv_usa <- file.path(fileDirectory, '2020 November General Election - Turnout Rates.csv')
usa_elections <- read.csv(csv_usa)
attach(usa_elections)


## ----warning = FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------
head(usa_elections[,-2])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
csv_dem_rep<- file.path(fileDirectory, 'democratic_vs_republican_votes_by_usa_state_2020.csv')
usa_winner <- read.csv(csv_dem_rep)
attach(usa_winner)


## ----warning = FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------
head(usa_winner)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
usa_elections <- usa_elections[, -ncol(usa_elections)]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
usa_total <- usa_elections[1,]
usa_elections <- usa_elections[2:nrow(usa_elections),-ncol(usa_elections)]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
usa_elections <- usa_elections[,-c(grep("Source", colnames(usa_elections)), grep("Official.Unofficial", colnames(usa_elections)))]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
party_winner<- ifelse(DEM>REP, "DEM","REP")
usa_elections$party_winner<-party_winner


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
str(usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Definición de las funciones
remove_comma <- function(x) gsub(',', '', x)
remove_percent <- function(x) gsub('%', '', x)

# Aplicación de las mismas sobre las columnas apropiadas
usa_elections[,2] <- sapply(usa_elections[,2], remove_comma)
usa_elections[,3] <- sapply(usa_elections[,3], remove_comma)
usa_elections[,4] <- sapply(usa_elections[,4], remove_percent)
usa_elections[,5] <- sapply(usa_elections[,5], remove_comma)
usa_elections[,6] <- sapply(usa_elections[,6], remove_comma)
usa_elections[,7] <- sapply(usa_elections[,7], remove_percent)
usa_elections[,8] <- sapply(usa_elections[,8], remove_comma)
usa_elections[,9] <- sapply(usa_elections[,9], remove_comma)
usa_elections[,10] <- sapply(usa_elections[,10], remove_comma)
usa_elections[,11] <- sapply(usa_elections[,11], remove_comma)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
usa_elections[,2] <- as.numeric(usa_elections[,2])
usa_elections[,3] <- as.numeric(usa_elections[,3])
usa_elections[,4] <- as.numeric(usa_elections[,4])
usa_elections[,5] <- as.numeric(usa_elections[,5])
usa_elections[,6] <- as.numeric(usa_elections[,6])
usa_elections[,7] <- as.numeric(usa_elections[,7])
usa_elections[,8] <- as.numeric(usa_elections[,8])
usa_elections[,9] <- as.numeric(usa_elections[,9])
usa_elections[,10] <- as.numeric(usa_elections[,10])
usa_elections[,11] <- as.numeric(usa_elections[,11])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
usa_elections[,12] <- as.factor(usa_elections[,12])


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
str(usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(usa_elections))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Calculamos la media de los votos válidos para la presidencia
mean_president=mean(usa_elections$Vote.for.Highest.Office..President.,na.rm=TRUE)
# Calculamos la media de votos totales que tienen información sobre los votos a la presidencia
mean_total=mean(usa_elections$Total.Ballots.Counted..Estimate.[!is.na(usa_elections$Vote.for.Highest.Office..President.)],na.rm=TRUE)
# Sacamos el procentaje de la media de votos válidos
percentage_votes= mean_president/mean_total
# Aplicamos dicho porcentage a los votos totales que no disponen dicha información y guardamos los votos válidos en su correspondiente estado
# Como los votos deben de ser un numero entero se va a redondear el resultado de multiplicar los votos totales por el porcentage de votos válidos
usa_elections$Vote.for.Highest.Office..President.<-ifelse(is.na(usa_elections$Vote.for.Highest.Office..President.),trunc(usa_elections$Total.Ballots.Counted..Estimate. * percentage_votes), usa_elections$Vote.for.Highest.Office..President.  )



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(usa_elections))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(usa_elections, function(r) any(c(0) %in% r))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Dado que vamos a ejecutar estas líneas de código en diferentes ocasiones, crearemos una función.
replace_0 <- function(column_index) {
  mean_column <- mean(usa_elections[,column_index])
  mean_total = mean(usa_elections$Total.Ballots.Counted..Estimate.[!is.na(usa_elections[,column_index])])
  percentage_votes = mean_column/mean_total
  final_column <- ifelse(usa_elections[,column_index] == 0,trunc(usa_elections$Total.Ballots.Counted..Estimate. * percentage_votes), usa_elections[,column_index])
  return(final_column)
}

usa_elections$Prison <- replace_0(which(colnames(usa_elections) == "Prison"))
usa_elections$Probation <- replace_0(which(colnames(usa_elections) == "Probation"))
usa_elections$Parole <- replace_0(which(colnames(usa_elections) == "Parole"))
usa_elections$Total.Ineligible.Felon <- replace_0(which(colnames(usa_elections) == "Total.Ineligible.Felon"))



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Total.Ballots.Counted..Estimate.)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Vote.for.Highest.Office..President.)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$VEP.Turnout.Rate)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Voting.Eligible.Population..VEP.)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Voting.Age.Population..VAP.)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$X..Non.citizen)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Prison)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Probation)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Parole)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot.stats(usa_elections$Total.Ineligible.Felon)$out


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
my_scale <- function(column_index) {
  min_col <- min(as.numeric(usa_elections[,column_index]))
  max_col <- max(as.numeric(usa_elections[,column_index]))
  column_scaled <- (usa_elections[,column_index] - min_col) / (max_col - min_col)
  return(column_scaled)
}

indexes_to_scale = c(2:(ncol(usa_elections) - 1))
for (col in indexes_to_scale){
  usa_elections[,col] <- my_scale(col)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
head(usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(usa_elections, "2020 November General Election - Turnout Rates_data_clean.csv",  row.names = FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Agrupación por ganadores
usa_elections.dem=usa_elections[usa_elections$party_winner=="DEM",]
usa_elections.rep=usa_elections[usa_elections$party_winner=="REP",]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
library(nortest)
alpha = 0.05
col.names = colnames(usa_elections)
for (i in 1:ncol(usa_elections)) {
  if (i == 1) cat("Variables que no siguen una distribución normal segun el test de Anderson-Darling:\n")
  if (is.integer(usa_elections[,i]) | is.numeric(usa_elections[,i])) {
    p_val = ad.test(usa_elections[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(usa_elections) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
library(nortest)
alpha = 0.05
col.names = colnames(usa_elections)
for (i in 1:ncol(usa_elections)) {
  if (i == 1) cat("Variables que no siguen una distribución normal segun el test de Shapiro-Wilk:\n")
  if (is.integer(usa_elections[,i]) | is.numeric(usa_elections[,i])) {
    p_val = shapiro.test(usa_elections[,i])$p.value
    if (p_val < alpha) {
      cat(col.names[i])
      # Format output
      if (i < ncol(usa_elections) - 1) cat(", ")
      if (i %% 3 == 0) cat("\n")
    }
  }
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Cremaos las variables turnout en función del partido ganador
turnout_dem <- usa_elections.dem$VEP.Turnout.Rate
turnout_rep <- usa_elections.rep$VEP.Turnout.Rate


## ----Fig1, echo=TRUE, fig.height=8, fig.width=15--------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(4,4,4,1), oma=c(0.5,0.5,0.5,0)) 

# histograma, densidad de probabilidad y normal calculada para turnout
hist(turnout_dem, col = 'lightcyan',
     main = 'Democratas',
     freq = FALSE,
     xlab = 'Turnout Democratas ',
     pch=16)
lines(density(turnout_dem), 
      col = 'blue', 
      lwd='4')
curve(dnorm(x,mean(turnout_dem), sd(turnout_dem)),col='green', lwd=4, add=T)


hist(turnout_rep, col = 'lightcyan',
     main = 'Republicanos',
     freq = FALSE,
     xlab = 'Turnout Republicanos',
     pch=16)
lines(density(turnout_rep), 
      col = 'blue', 
      lwd='4')
curve(dnorm(x,mean(turnout_rep), sd(turnout_rep)),col='green', lwd=4, add=T)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(turnout_dem)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
shapiro.test(turnout_rep)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
fligner.test(VEP.Turnout.Rate ~ party_winner, data = usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
var.test(turnout_dem,turnout_rep)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa
# con respecto al campo "party_winner"
for (i in 2:(ncol(usa_elections) - 1)) {
  if (i!=4){
    if (is.integer(usa_elections[,i]) | is.numeric(usa_elections[,i])) {
      spearman_test = cor.test(usa_elections[,i], usa_elections[,4], method = "spearman", exact=FALSE)
      corr_coef = spearman_test$estimate
      p_val = spearman_test$p.value
      # Add row to matrix
      pair = matrix(ncol = 2, nrow = 1)
      pair[1][1] = corr_coef
      pair[2][1] = p_val
      corr_matrix <- rbind(corr_matrix, pair)
      rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(usa_elections)[i]
    }
  }
  else{
    next()
  }
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
print(corr_matrix)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
t.test(turnout_dem,turnout_rep,alternative="greater", var.equal=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO: revisar por qué con todas las variables da mejor resultado que solo con las que tienen más correlación
votos = usa_elections$Total.Ballots.Counted..Estimate.
votos_validos = usa_elections$Vote.for.Highest.Office..President.
vep= usa_elections$Voting.Eligible.Population..VEP.
vap = usa_elections$Voting.Age.Population..VAP.
no_ciudadanos = usa_elections$X..Non.citizen
prision = usa_elections$Prison
probation=usa_elections$Probation
parole=usa_elections$Parole
felon = usa_elections$Total.Ineligible.Felon

# Regresores cualitativos
winner=usa_elections$party_winner

# Variable a predecir
turnout = usa_elections$VEP.Turnout.Rate

# Modelo usando todas las variables
model <- lm(turnout ~ votos  + votos_validos + vep + vap + no_ciudadanos + prision + probation + parole + felon + winner, data = usa_elections)
summary(model)$r.squared
# Modelo usando únicamente las variables cuantitativas que tienen una correlación positiva
model_positive <- lm(turnout ~ votos  + votos_validos + vep + vap + no_ciudadanos + probation + winner, data = usa_elections)
summary(model_positive)$r.squared

# Generación de varios modelos
# modelo1 <- lm(turnout ~   votos_validos + parole + vep, data = usa_elections)
# 
# modelo2 <- lm(turnout ~ winner  + votos +  + parole +probation  , data = usa_elections)
# 
# modelo3 <- lm(turnout ~ winner  + votos + vep + parole +probation , data = usa_elections)
# 
# modelo4 <- lm(turnout ~  winner + votos + votos_validos + parole + probation , data = usa_elections)
# 
# modelo5 <- lm(turnout ~ winner  + votos + parole +probation , data = usa_elections)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Tabla con los coeficientes de determinación de cada modelo
# tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared,
# 2, summary(modelo2)$r.squared,
# 3, summary(modelo3)$r.squared,
# 4, summary(modelo4)$r.squared,
# 5, summary(modelo5)$r.squared),
# ncol = 2, byrow = TRUE)
# colnames(tabla.coeficientes) <- c("Modelo", "R^2")
# tabla.coeficientes


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO: escalar
newdata <- data.frame(
winner= "REP",
votos = 800000,
vep = 1400000,
parole = 4000,
probation = 7000
)

# Predecir el turnout
#predict(modelo3, newdata)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generación de varios modelos

modelo <- glm(as.factor(winner) ~ votos  + votos_validos + vep + vap + no_ciudadanos + prision + probation + parole + felon, family = binomial(link=logit))

modelo_possitive <- glm (as.factor(winner) ~ votos  + votos_validos + vep + vap + no_ciudadanos + probation + turnout, family = binomial(link=logit))

# modelo1 <- glm(as.factor(winner) ~   votos + parole + vep +turnout, data = usa_elections,family=binomial(link=logit))
# 
# modelo2 <- glm(as.factor(winner) ~  turnout  + parole +probation  , data = usa_elections, family=binomial(link=logit))
# 
# modelo3 <- glm(as.factor(winner) ~  votos_validos + turnout  +probation + parole , data = usa_elections, family=binomial(link=logit))



## -------------------------------------------------------------------------------------------------------------------------------------------------------------
get_precision <- function(table) {
  df <- as.data.frame(table)
  true_DEM <- df[1, "Freq"]
  true_REP <- df[4, "Freq"]
  return((true_DEM + true_REP) / 51)
}


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generamos la predicción del modelo
pdata<-predict(modelo,type="response")
# Generamos un vector en el que si la predicción es superior a 0.5, se clasifica como REP y en caso contrario como DEM
estimatedResponses=ifelse(pdata>0.5,"REP","DEM")
# Gurdamos en trueResponse, los resultados que se esperan de la variable winner
trueResponse=winner
# Generamos la matriz de confusión
table_results <- table(estimatedResponses,trueResponse)
get_precision(table_results)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generamos la predicción del modelo
pdata<-predict(modelo_possitive,type="response")
# Generamos un vector en el que si la predicción es superior a 0.5, se clasifica como REP y en caso contrario como DEM
estimatedResponses=ifelse(pdata>0.5,"REP","DEM")
# Gurdamos en trueResponse, los resultados que se esperan de la variable winner
trueResponse=winner
# Generamos la matriz de confusión
table_results <- table(estimatedResponses,trueResponse)
get_precision(table_results)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Generamos la predicción del modelo
# pdata<-predict(modelo1,type="response")
# # Generamos un vector en el que si la predicción es superior a 0.5, se clasifica como REP y en caso contrario como DEM
# estimatedResponses=ifelse(pdata>0.5,"REP","DEM")
# # Gurdamos en trueResponse, los resultados que se esperan de la variable winner
# trueResponse=winner
# # Generamos la matriz de confusión
# table_results <- table(estimatedResponses,trueResponse)
# get_precision(table_results)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Generamos la predicción del modelo
# pdata<-predict(modelo2,type="response")
# # Generamos un vector en el que si la predicción es superior a 0.5, se clasifica como REP y en caso contrario como DEM
# estimatedResponses=ifelse(pdata>0.5,"REP","DEM")
# # Gurdamos en trueResponse, los resultados que se esperan de la variable winner
# trueResponse=winner
# # Generamos la matriz de confusión
# table(estimatedResponses,trueResponse)
# table_results <- table(estimatedResponses,trueResponse)
# get_precision(table_results)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Generamos la predicción del modelo
# pdata<-predict(modelo3,type="response")
# # Generamos un vector en el que si la predicción es superior a 0.5, se clasifica como REP y en caso contrario como DEM
# estimatedResponses=ifelse(pdata>0.5,"REP","DEM")
# # Gurdamos en trueResponse, los resultados que se esperan de la variable winner
# trueResponse=winner
# # Generamos la matriz de confusión
# table(estimatedResponses,trueResponse)
# table_results <- table(estimatedResponses,trueResponse)
# get_precision(table_results)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
# TODO: scale
newdata <- data.frame(
votos = 800000,
parole = 4000,
vep = 1400000,
turnout=70.2)

# Predecir el partido ganador
# predicted_winner=ifelse(predict(modelo1, newdata, type="response")>0.5,"REP","DEM")
# predicted_winner


