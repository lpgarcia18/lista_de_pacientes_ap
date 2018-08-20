library(readr)
library(zoo)
#Bases
dados <- read_csv("bases/teste_lista_paciente.csv", 
    locale = locale(encoding = "cp1252"))
dados <- dados[,-c(5,7,8,9)]
dados_cs <- read_csv("bases/unidades_areas.csv", 
    locale = locale(encoding = "UTF-8"))

#Merge para agregar informação dos distritos e áreas
dados_cs <- unique(dados_cs)
dados_cs$AREA <- dados_cs$EQUIPE
dados_cs$EQUIPE <- substr(dados_cs$EQUIPE,3,3)
dados <- merge(dados, dados_cs, by = c("UNIDADE", "EQUIPE"), all = T)
dados <- dados[,-c(2)]

#Arrumando dados da última consulta
dados$DATA_CONSULTA <- as.Date(dados$DATA_CONSULTA, format =  "%d/%m/%y") 
dados$DATA_CONSULTA <- as.yearmon(dados$DATA_CONSULTA, format =  "%d/%m/%y") 


#Cálculo da populacão ativa = população com última consulta há menos de 2 anos
dados$POPULACAO_ATIVA <-ifelse(as.Date(dados$DATA_CONSULTA,frac = 0) >= as.Date((as.yearmon(Sys.Date())-2),frac = 0), 1,0)

#Cálculo da populacão reserva = população com última consulta há menos de 5 anos
dados$POPULACAO_RESERVA <-ifelse(
   ((as.Date(dados$DATA_CONSULTA,frac = 0) < as.Date(as.yearmon(Sys.Date())-2,frac = 0)) &
    (as.Date(dados$DATA_CONSULTA,frac = 0) >= as.Date(as.yearmon(Sys.Date())-5,frac = 0))), 1 , 0)

write.csv(dados, "bases/dados.csv", fileEncoding = "UTF-8", row.names = F)


    esf_lista <- dados[,c(1,6,7,8)]
    esf_lista <- subset(esf_lista, esf_lista$UNIDADE == "CS ABRAÃO")
    esf_lista <- esf_lista[,-1]
    esf_lista$AREA <- as.character(esf_lista$AREA)
    esf_lista <- melt(esf_lista) 
    names(esf_lista) <- c("AREA", "LISTA", "VALOR")
    esf_lista <- aggregate(esf_lista$VALOR, by = list(esf_lista$AREA, esf_lista$LISTA), FUN = sum, na.rm=TRUE, na.action=NULL)
    names(esf_lista) <- c("AREA", "LISTA", "VALOR")
    
    a <- ggplot(esf_lista, aes(x = AREA, y = VALOR, fill = LISTA)) + 
            geom_col(position = "dodge")+ 
            ylab("  ")+
            xlab("  ")+
            theme_classic()+
            theme(axis.text.x = element_text(hjust = 1))
          
    ggplotly(a)
    
    


