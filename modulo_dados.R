library(readr)
library(zoo)
#Bases
dados <- read_csv("bases/lista_paciente.csv", 
    locale = locale(encoding = "UTF-8"))
dados$DATA_CONSULTA <- gsub("-","/",dados$DATA_CONSULTA)

#Arrumando dados da última consulta
dados$DATA_CONSULTA <- as.Date(dados$DATA_CONSULTA, format =  "%d/%m/%Y") 
dados$DATA_CONSULTA <- as.yearmon(dados$DATA_CONSULTA, format =  "%d/%m/%Y") 


#Cálculo da populacão ativa = população com última consulta há menos de 2 anos
dados$POPULACAO_ATIVA <-ifelse(as.Date(dados$DATA_CONSULTA,frac = 0) >= as.Date((as.yearmon(Sys.Date())-2),frac = 0), 1,0)

#Cálculo da populacão reserva = população com última consulta há menos de 5 anos
dados$POPULACAO_RESERVA <-ifelse(
   ((as.Date(dados$DATA_CONSULTA,frac = 0) < as.Date(as.yearmon(Sys.Date())-2,frac = 0)) &
    (as.Date(dados$DATA_CONSULTA,frac = 0) >= as.Date(as.yearmon(Sys.Date())-5,frac = 0))), 1 , 0)

write.csv(dados, "bases/dados.csv", fileEncoding = "UTF-8", row.names = F)
