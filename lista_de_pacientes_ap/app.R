Sys.setlocale("LC_TIME","English_United States.1252")
options(encoding = "UTF-8")

########################################################################################### 
#pacotes 
###########################################################################################
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(readr)
library(stringr)
library(DT)
library(plotly)
library(reshape2)


########################################################################################### 
#Banco de dados 
###########################################################################################
dados <- read_csv("bases/dados.csv")
senhas <- read_csv("bases/senhas.csv")
###########################################################################################
#Login
###########################################################################################
Logged = FALSE
my_username <- "teste"
my_password <- "teste"
#my_username <- ifelse(input$username %in% senhas$nome, input$username, NA)
#my_password <- ifelse(input$password %in% senhas$senha, input$password, NA)
########################################################################################### 
#UI
###########################################################################################
########################################################################################### 
ui <- dashboardPage(skin = "blue",
########################################################################################### 
        dashboardHeader(title = "Pacientes em Lista na Atenção Primária", titleWidth = 550),
        ########################################################################################### 
        dashboardSidebar(
          ########################################################################################### 
          sidebarMenu(
            menuItem("Florianóplis",tabName = "florianopolis", icon = icon("dashboard")),
            menuItem("Distritos",tabName = "distritos", icon = icon("dashboard")),  
            menuItem("Centros de Saúde", tabName = "centros_de_saude",icon = icon("dashboard")), 
            menuItem("Equipes", tabName = "esf", icon = icon("dashboard")), 
            menuItem("Instruções", icon = icon("question-circle"),
                     href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/wiki/Instru%C3%A7%C3%B5es-para-Utiliza%C3%A7%C3%A3o-das-Salas-de-Situa%C3%A7%C3%A3o-em-Sa%C3%BAde"),
            #menuItem("Dados", icon = icon("database"),
                     #href = "http://floripadadosabertos.univille.br/"),
            menuItem("Código-fonte", icon = icon("code"), 
                     href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/master/aps"),
            menuItem("Licença de Uso", icon = icon("cc"), 
                     href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/LICENSE")
          )
        ),
        ########################################################################################### 
        dashboardBody(
          tabItems(
            ###########################################################################################
            #Florianópolis
            ###########################################################################################                         
            tabItem(tabName = "florianopolis", h2("Pacientes em Lista na Atenção Primária - Florianópolis"),
                    
                    fluidRow(
                      tabBox(title = "Pacientes em Lista", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "florianopolis_plot")),
                             tabPanel("Lista Ativa", dataTableOutput("florianopolis_ativa_tab")),
                             tabPanel("Lista Reserva", dataTableOutput("florianopolis_reserva_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("florianopolis_info"))
                      )
                    )
            ),
            ########################################################################################### 
            #Distritos
            ###########################################################################################
            tabItem(tabName = "distritos", h2("Pacientes em Lista na Atenção Primária - Distritos"),
                    
                    fluidRow(
                      tabBox(title = "Pacientes em Lista", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "distritos_plot")),
                             tabPanel("Lista Ativa", dataTableOutput("distritos_ativa_tab")),
                             tabPanel("Lista Reserva", dataTableOutput("distritos_reserva_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("distritos_info"))
                      )
                    )
            ),
            ########################################################################################### 
            #Centros de Saúde
            ###########################################################################################
            tabItem(tabName = "centros_de_saude", h2("Pacientes em Lista na Atenção Primária - Centros de Saúde"),
                    
                    fluidRow(
                       box(selectInput(
                        inputId="lista_cspordistrito",
                        label="Selecione um Distrito:",
                        choices=list("Centro" = "Centro", "Continente" = "Continente", "Norte" = "Norte",
                                     "Sul" = "Sul"),
                        selected="Centro"),
                        width = 12, status = "primary")
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Pacientes em Lista", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "centros_de_saude_plot")),
                             tabPanel("Lista Ativa", dataTableOutput("centros_de_saude_ativa_tab")),
                             tabPanel("Lista Reserva", dataTableOutput("centros_de_saude_reserva_tab")),
                             tabPanel("Sobre a Lista", htmlOutput("centros_de_saude_info"))
                      )
                    )
            ),
            ###########################################################################################
            #Equipes
            tabItem(tabName = "esf", h2("Pacientes em Lista na Atenção Primária - Equipes de Saúde da Família"),
                    
                    fluidRow(
                      box(selectInput(
                        inputId="lista_esfporcs",
                        label="Selecione um Centro de Saúde:",
                        choices=list("CS ABRAÃO" = "CS ABRAÃO" ,  "CS AGRONÔMICA"  = "CS AGRONÔMICA",  "CS ALTO RIBEIRÃO" = "CS ALTO RIBEIRÃO",  
                                     "CS ARMAÇÃO"  =  "CS ARMAÇÃO",  "CS BALNEÁRIO" =  "CS BALNEÁRIO",  "CS BARRA DA LAGOA" = "CS BARRA DA LAGOA",
                                     "CS CACHOEIRA BOM JESUS" = "CS CACHOEIRA BOM JESUS", "CS CAIEIRA BARRA DO SUL" = "CS CAIEIRA BARRA DO SUL", 
                                     "CS CAMPECHE" =  "CS CAMPECHE",  "CS CANASVIEIRAS" = "CS CANASVIEIRAS",  "CS CANTO DA LAGOA" = "CS CANTO DA LAGOA",  
                                     "CS CAPOEIRAS"  =  "CS CAPOEIRAS",  "CS CARIANOS"  =  "CS CARIANOS",  "CS CENTRO" = "CS CENTRO",  
                                     "CS COLONINHA" = "CS COLONINHA",  "CS COQUEIROS"  = "CS COQUEIROS",  "CS CÓRREGO GRANDE" = "CS CÓRREGO GRANDE",  
                                     "CS COSTA DA LAGOA" =  "CS COSTA DA LAGOA",  "CS COSTEIRA DO PIRAJUBAÉ"="CS COSTEIRA DO PIRAJUBAÉ",  
                                     "CS ESTREITO" = "CS ESTREITO",  "CS FAZENDA DO RIO TAVARES" = "CS FAZENDA DO RIO TAVARES", "CS INGLESES"= "CS INGLESES",  
                                     "CS ITACORUBI"= "CS ITACORUBI",  "CS JARDIM ATLÂNTICO"= "CS JARDIM ATLÂNTICO",  "CS JOÃO PAULO"= "CS JOÃO PAULO",  "CS JURERE" ="CS JURERE" ,
                                     "CS LAGOA DA CONCEIÇÃO"= "CS LAGOA DA CONCEIÇÃO", "CS MONTE CRISTO"= "CS MONTE CRISTO",  "CS MONTE SERRAT"= "CS MONTE SERRAT",  
                                     "CS MORRO DAS PEDRAS"= "CS MORRO DAS PEDRAS",  "CS NOVO CONTINENTE"= "CS NOVO CONTINENTE",  "CS PANTANAL" =  "CS PANTANAL",  
                                     "CS PANTANO DO SUL" = "CS PANTANO DO SUL", "CS PONTA DAS CANAS"=  "CS PONTA DAS CANAS",  "CS PRAINHA"=  "CS PRAINHA",  "CS RATONES"=  "CS RATONES",  
                                     "CS RIBEIRÃO DA ILHA"= "CS RIBEIRÃO DA ILHA",  "CS RIO TAVARES"=  "CS RIO TAVARES",  "CS RIO VERMELHO"= "CS RIO VERMELHO",  
                                     "CS SACO DOS LIMÕES"= "CS SACO DOS LIMÕES",  "CS SACO GRANDE"= "CS SACO GRANDE",  "CS SANTINHO"=  "CS SANTINHO",  
                                     "CS SANTO ANTONIO LISBOA" = "CS SANTO ANTONIO LISBOA", "CS SAPE"  = "CS SAPE" , "CS TAPERA"=  "CS TAPERA",  "CS TRINDADE"= "CS TRINDADE",  
                                     "CS VARGEM GRANDE"= "CS VARGEM GRANDE", "CS VARGEM PEQUENA"=  "CS VARGEM PEQUENA",  "CS VILA APARECIDA" = "CS VILA APARECIDA"),
                        selected="CS ABRAÃO"),
                        width = 12, status = "primary")
                    ),
                    
                    
                    fluidRow(
                      tabBox(title = "Pacientes em Lista", width=12,
                             tabPanel("Gráfico", plotlyOutput(outputId = "esf_plot")),
                             tabPanel("Lista Ativa", dataTableOutput("esf_ativa_tab")),
                             tabPanel("Lista Reserva", dataTableOutput("esf_reserva_tab")),
                                tabPanel("Sobre a Lista", htmlOutput("esf_info"))
             )
           )
         ) 
      )
   )
)
########################################################################################### 
server <- function(input, output, session) {
###########################################################################################
###########################################################################################
#Login
###########################################################################################
values <- reactiveValues(authenticated = FALSE)

# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("username", "Nome:"),
    passwordInput("password", "Senha:"),
    footer = tagList(
      #modalButton("Cancel"),
      actionButton("entrar", "Entrar")
    )
  )
}

# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential

obs1 <- observe({
  showModal(dataModal())
})

# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 

obs2 <- observe({
  req(input$entrar)
  isolate({
    Username <- input$username
    Password <- input$password
  })
  Id.username <- which(my_username == Username)
  Id.password <- which(my_password == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()

    } else {
      values$authenticated <- FALSE
    }     
  }
})

###########################################################################################
#Florianópolis
###########################################################################################

#gráfico 
output$florianopolis_plot <- renderPlotly({
 
 florianopolis_lista <- dados[,c(7,8)] 
 florianopolis_lista <- melt(florianopolis_lista) 
 names(florianopolis_lista) <- c("LISTA", "VALOR")
 florianopolis_lista <- aggregate(florianopolis_lista$VALOR, by = list(florianopolis_lista$LISTA), FUN = sum, na.rm=TRUE, na.action=NULL)
 names(florianopolis_lista) <- c("LISTA", "VALOR")
 
 a <- ggplot(florianopolis_lista, aes(x = LISTA, y = VALOR, fill = LISTA)) + 
         geom_col()+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         theme(axis.text.x = element_text(hjust = 1))
       
 ggplotly(a)
 
})

#tabela - ativos
output$florianopolis_ativa_tab <- renderDataTable({
 
 florianopolis_lista <- dados 
 florianopolis_lista <- subset(florianopolis_lista, florianopolis_lista$POPULACAO_ATIVA == 1)
 florianopolis_lista <- florianopolis_lista[,-c(5,7,8)]
 as.data.frame(florianopolis_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


 #tabela - reserva
output$florianopolis_reserva_tab <- renderDataTable({
 
 florianopolis_lista <- dados 
 florianopolis_lista <- subset(florianopolis_lista, florianopolis_lista$POPULACAO_RESERVA == 1)
 florianopolis_lista <- florianopolis_lista[,-c(5,7,8)]
 as.data.frame(florianopolis_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$florianopolis_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
###########################################################################################       
#Distritos
###########################################################################################
#gráfico 
output$distritos_plot <- renderPlotly({
 
 distritos_lista <- dados[,c(1,7,8)] 
 distritos_lista <- melt(distritos_lista) 
 names(distritos_lista) <- c("distritos", "LISTA", "VALOR")
 distritos_lista <- aggregate(distritos_lista$VALOR, by = list(distritos_lista$distritos, distritos_lista$LISTA), FUN = sum, na.rm=TRUE, na.action=NULL)
 names(distritos_lista) <- c("distritos", "LISTA", "VALOR")
 
 a <- ggplot(distritos_lista, aes(x = distritos, y = VALOR, fill = LISTA)) + 
         geom_col(position = "dodge")+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         theme(axis.text.x = element_text(hjust = 1))
       
 ggplotly(a)

 
})

#tabela - ativos
output$distritos_ativa_tab <- renderDataTable({
 
 distritos_lista <- dados 
 distritos_lista <- subset(distritos_lista, distritos_lista$POPULACAO_ATIVA == 1)
 distritos_lista <- distritos_lista[,-c(6,7,8)]
 as.data.frame(distritos_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


 #tabela - reserva
output$distritos_reserva_tab <- renderDataTable({
 
 distritos_lista <- dados 
 distritos_lista <- subset(distritos_lista, distritos_lista$POPULACAO_RESERVA == 1)
 distritos_lista <- distritos_lista[,-c(6,7,8)]
 as.data.frame(distritos_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$distritos_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

###########################################################################################       
#Centros de Saúde
########################################################################################### 
#gráfico 
output$centros_de_saude_plot <- renderPlotly({
 
 centros_de_saude_lista <- dados[,c(1,2,7,8)]
 centros_de_saude_lista <- subset(centros_de_saude_lista, centros_de_saude_lista$DISTRITO == input$lista_cspordistrito)
 centros_de_saude_lista <- centros_de_saude_lista[,-1]
 centros_de_saude_lista <- melt(centros_de_saude_lista) 
 names(centros_de_saude_lista) <- c("UNIDADE", "LISTA", "VALOR")
 centros_de_saude_lista <- aggregate(centros_de_saude_lista$VALOR, by = list(centros_de_saude_lista$UNIDADE, centros_de_saude_lista$LISTA), FUN = sum, na.rm=TRUE, na.action=NULL)
 names(centros_de_saude_lista) <- c("UNIDADE", "LISTA", "VALOR")
 
 a <- ggplot(centros_de_saude_lista, aes(x = UNIDADE, y = VALOR, fill = LISTA)) + 
         geom_col(position = "dodge")+ 
         ylab("  ")+
         xlab("  ")+
         theme_classic()+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
 ggplotly(a)

 
})

#tabela - ativos
output$centros_de_saude_ativa_tab <- renderDataTable({
 
 centros_de_saude_lista <- dados
 centros_de_saude_lista <- subset(centros_de_saude_lista, centros_de_saude_lista$DISTRITO == input$lista_cspordistrito)
 centros_de_saude_lista <- subset(centros_de_saude_lista, centros_de_saude_lista$POPULACAO_ATIVA == 1)
 centros_de_saude_lista <- centros_de_saude_lista[,-c(6,7,8)]
 as.data.frame(centros_de_saude_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


 #tabela - reserva
output$centros_de_saude_reserva_tab <- renderDataTable({
 
 centros_de_saude_lista <- dados
 centros_de_saude_lista <- subset(centros_de_saude_lista, centros_de_saude_lista$DISTRITO == input$lista_cspordistrito)
 centros_de_saude_lista <- subset(centros_de_saude_lista, centros_de_saude_lista$POPULACAO_RESERVA == 1)
 centros_de_saude_lista <- centros_de_saude_lista[,-c(6,7,8)]
 as.data.frame(centros_de_saude_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$centros_de_saude_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})
###########################################################################################
#Equipes
###########################################################################################
#gráfico 
output$esf_plot <- renderPlotly({
 
 esf_lista <- dados[,c(2,3,7,8)]
 esf_lista <- subset(esf_lista, esf_lista$UNIDADE == input$lista_esfporcs)
 esf_lista <- esf_lista[,-1]
 esf_lista$AREA <- esf_lista$EQUIPE
 esf_lista$EQUIPE <- NULL
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

 
})

#tabela - ativos
output$esf_ativa_tab <- renderDataTable({
 
 esf_lista <- dados
 esf_lista <- subset(esf_lista, esf_lista$UNIDADE == input$lista_esfporcs)
 esf_lista <- subset(esf_lista, esf_lista$POPULACAO_ATIVA == 1)
 esf_lista <- esf_lista[,-c(6,7,8)]
 as.data.frame(esf_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


 #tabela - reserva
output$esf_reserva_tab <- renderDataTable({
 
 esf_lista <- dados
 esf_lista <- subset(esf_lista, esf_lista$UNIDADE == input$lista_esfporcs)
 esf_lista <- subset(esf_lista, esf_lista$POPULACAO_RESERVA == 1)
 esf_lista <- esf_lista[,-c(6,7,8)]
 as.data.frame(esf_lista, row.names = F)
 
}, extensions = 'Buttons',
options = list(
 "dom" = 'T<"clear">lBfrtip',
 buttons = list('copy', 'csv', 'pdf', 'print')))


#informações 
output$esf_info <- renderText({
 
 paste("<b>teste </b>", "<br>",
       "<b>teste: </b> teste")
 
})

}    

###########################################################################################
shinyApp(ui, server)


