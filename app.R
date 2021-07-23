
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(data.table)
library(plotly)
library(readxl)
library(shinyWidgets)
library(shinydashboard)
library(tidyr)
library(leaflet)
library(tidyverse)
library(brazilmaps)
library(leaflet)
library(installr)
library(geobr)
dados_Final2 <-read_excel("dados_Final3.xlsx")
media_multas <- dados_Final2 %>% group_by(anopago) %>%
    summarise(qnt_multas = n()) %>%
    summarise(media_multa = mean(qnt_multas)) %>%
    as.numeric() %>% formatC()
options(scipen=20)
cabecalho <- dashboardHeader(title = "DASHBOARD MULTAS IBAMA")

barra_lateral <- dashboardSidebar(width="250px",
                                  sidebarMenu(
                                      menuItem("DASHBOARD",
                                                tabName = "Dashboard",
                                               icon=icon("dashboard")),
                                      menuItem("Informações",
                                                tabName = "infos",
                                                icon=icon("info-circle"))
                                  ))

painel_principal<-dashboardBody(
    tabItems(
        tabItem(tabName = "infos",
                h1="Informações",
                infoBox(title="Informações",icon=icon("envelope-square"),
                        subtitle= "Dashboard criado para a disciplina de visualização e exploração de dados (2021) Criado por: Isaac Werner Sales Fritsch - 12674846")),
        tabItem(tabName ="Dashboard",
                fluidRow(
                    valueBox(subtitle="Registros de multas (1995-2021)",value=nrow(dados_Final2),
                             icon=icon("database")),
                    infoBox(title="",subtitle="Média de multas por ano (1995-2021)",
                            value=media_multas, icon=icon("list")),
                    valueBoxOutput(outputId = "municípios_selecionados")
                ),
                fluidRow(
                    column(width=12,
                           box(title="Filtros",width='100%',
                               column(width=12,
                                      box(width='100%',
                                          awesomeCheckboxGroup(inline = TRUE,inputId = "select_município",
                                                               label="Municípios:", 
                                                               choices=c('TODOS',unique(dados_Final2$Município)),
                                                               selected = 'TODOS')
                                      )
                               ),
                               column(width=6,
                                      box(width="100%",
                                          dateRangeInput(inputId= 'data_abertura',
                                                         label = 'Data abertura:',format= "dd-mm-yyyy",
                                                         start = min(as.Date(dados_Final2$Data.Auto)),
                                                         end = max(as.Date(dados_Final2$Data.Auto)))
                                          
                                      )
                                      
                                      
                               ),
                               column(width=6,
                                      box(width="100%",
                                          selectizeInput(inputId="assunto",
                                                         label="Tipos de infrações:",
                                                         choices = c("TODOS",unique(dados_Final2$Tipo.Infração)),
                                                         multiple=TRUE,
                                                         options=list(maxItems=9),
                                                         selected='TODOS') 
                                          
                                      )
                                      
                               )
                           )##FINAL BOX
                    )
                ),##FINAL FLUID1
                fluidRow(
                    column(width=12,
                           box(width="100%",
                               leafletOutput(outputId = 'mapa',width="100%"),
                               verbatimTextOutput(outputId = "descmapa")
                               
                               
                           )
                           
                    )
                    
                ),
                fluidRow(
                    column(width=12,
                           box(width="100%",
                               plotlyOutput(outputId = 'data',width="100%"),
                               verbatimTextOutput(outputId = "descData")
                               
                               
                           )
                           
                    )
                    
                ),
                fluidRow(
                    column(width=12,
                           box(width="100%",
                               plotlyOutput(outputId = 'numeroauto',width="100%"),
                               textOutput(outputId = "descAuto")
                           )
                    )
                ),
                fluidRow(
                    column(width=6,
                           box(width="100%",
                               plotlyOutput(outputId = 'Quantidade_infracao')
                           )
                    ),
                    column(width=6,
                           box(width="100%",
                               plotlyOutput(outputId = 'autopago'))
                           )
                ),
                fluidRow(
                    column(width=12,
                           box(width="100%",
                               plotlyOutput(outputId = 'anoano')
                           )
                    )
                )
                    )
            )
    
)

ui<- dashboardPage(header=cabecalho,
                   sidebar=barra_lateral,
                   body = painel_principal)

# Define UI for application that draws a histogram
ui2 <- fluidPage(

    # Application title
    titlePanel('Título'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "select_município",
                               label="Municípios:", choices=c('TODOS',unique(dados_Final2$Município)),
                               selected = 'TODOS'),
            dateRangeInput(inputId= 'data_abertura',
                           label = 'Data abertura:',format= "dd-mm-yyyy",
                           start = min(as.Date(dados_Final2$Data.Auto)),
                           end = max(as.Date(dados_Final2$Data.Auto))),
            selectizeInput(inputId="assunto",
                        label="Tipos de infrações:",
                        choices = c("TODOS",unique(dados_Final2$Tipo.Infração)),
                        multiple=TRUE,
                        options=list(maxItems=9),
                        selected='TODOS') 
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput(outputId="mapa"),
            verbatimTextOutput(outputId = "descmapa"),
           plotlyOutput(outputId = 'data'),
           verbatimTextOutput(outputId = "descData"),
           plotlyOutput(outputId = 'numeroauto'),
           textOutput(outputId = "descAuto"),
           plotlyOutput(outputId = 'Quantidade_infracao'),
           plotlyOutput(outputId = 'anoano'),
           plotlyOutput(outputId = 'autopago')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dados_selecionados <- reactive({
        if(!'TODOS' %in% input$select_município){
            dados_Final2 <- dados_Final2 %>%
                filter(Município %in% input$select_município)
        }
        if(!'TODOS' %in% input$assunto){
            dados_Final2 <- dados_Final2 %>%
                filter(Tipo.Infração %in% input$assunto)
        }
        dados_Final2 <- dados_Final2 %>% filter(as.Date(Data.Auto) >= input$data_abertura[1] &
                        as.Date(Data.Auto) <= input$data_abertura[2])
        
        dados_Final2
        
    })
     
   output$data <- renderPlotly({
       
        ggplotly(data.frame(table(as.Date(dados_selecionados()$Data.Auto))) %>% 
            rename(Data=Var1,Quantidade=Freq) %>%
            ggplot(aes(as.Date(Data),Quantidade))+
            geom_line(group=1)+
            theme_bw()+
            ggtitle("Quantidade de multas por ano-mês")+
            theme(axis.text.x = element_text(angle = 45, hjust=1))+
            scale_x_date(date_labels = '%b-%Y', breaks='8 months')+
            xlab('Data'))
    })
    
    output$numeroauto <- renderPlotly({
        ggplotly(data.frame(table(dados_selecionados()$Município)) %>%
                     rename(Município=Var1,Quantidade_Multas=Freq) %>% ggplot(aes(x= reorder(Município,Quantidade_Multas),
                    y=Quantidade_Multas,text=paste("Município:",Município,"<br>", "Quantidade Multas:",Quantidade_Multas)))+
                     geom_bar(fill="blue", stat='identity')+
                     coord_flip()+xlab('Município')+theme_bw()+
                     ggtitle("Quantidade de multas por Município"),tooltip="text"
                 
                 
        )
    })
    
    output$descData <- renderText({
        paste("Gráfico com a quantidade de multas aplicadas pelo IBAMA entre:",
              min(dados_selecionados()$Data.Auto),'-',
              max(dados_selecionados()$Data.Auto))
    })
    output$descAuto <- renderText({
        cidades <- paste(unique(dados_selecionados()$Município), collapse=',')
        paste("Gráfico com a quantidade de multas por município(s):", cidades)
    })
    
    output$Quantidade_infracao <- renderPlotly({
        ggplotly(
            data.frame(table(dados_selecionados()$Tipo.Infração)) %>%
                rename(Infração=Var1,Quantidade=Freq) %>% ggplot(aes(x= reorder(Infração,Quantidade),y=Quantidade))+
                geom_bar(stat='identity')+
                coord_flip()+xlab("")+theme_bw()+
                ggtitle("Tipos de infrações")
        )
    })

    
    output$autopago <- renderPlotly({
        sumdata <- data.frame(value=apply(dados_selecionados()[,c(6,7)],2,sum))
        sumdata$Valores <- rownames(sumdata)
        
        ggplotly(
            ggplot(data=sumdata, aes(x=Valores, y=value,fill=Valores)) +
                       geom_bar(colour="black", stat="identity")+
                       theme_bw()+
                       ggtitle("Valor total das multas e valor total recebido")+
                       ylab('Valor em reais')+
                       xlab('')
        )
    })
    output$municípios_selecionados <- renderValueBox({
        valueBox(value = length(unique(dados_selecionados()$Município)),
                 subtitle="Municípos selecionados", icon=icon("map-maker"))
    })
    output$anoano <- renderPlotly({
        
    ggplotly( 
        ggplot(dados_Final2 %>% drop_na())+
           geom_bar(aes(x = Ano  , y= scores, 
                        fill = Categorias),
                    stat="identity",
                    position=position_dodge2())+
           theme_bw()+
           ggtitle("Valores totais de multas e pagos ao IBAMA por ano (sem filtragem)")+
           ylab("Valor em reais")
            
         )
    })
    output$mapa <- renderLeaflet({
        
        sf <- get_brmap(geo = "City",
                        geo.filter = list(State = 27))
        sf$nome <- as.character(sf$nome)
        
        rm_accent <- function(str,pattern="all") {
            # Rotinas e funções úteis V 1.0
            # rm.accent - REMOVE ACENTOS DE PALAVRAS
            # Função que tira todos os acentos e pontuações de um vetor de strings.
            # Parâmetros:
            # str - vetor de strings que terão seus acentos retirados.
            # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
            #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
            #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
            #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
            if(!is.character(str))
                str <- as.character(str)
            
            pattern <- unique(pattern)
            
            if(any(pattern=="Ç"))
                pattern[pattern=="Ç"] <- "ç"
            
            symbols <- c(
                acute = "áéíóúÁÉÍÓÚýÝ",
                grave = "àèìòùÀÈÌÒÙ",
                circunflex = "âêîôûÂÊÎÔÛ",
                tilde = "ãõÃÕñÑ",
                umlaut = "äëïöüÄËÏÖÜÿ",
                cedil = "çÇ"
            )
            
            nudeSymbols <- c(
                acute = "aeiouAEIOUyY",
                grave = "aeiouAEIOU",
                circunflex = "aeiouAEIOU",
                tilde = "aoAOnN",
                umlaut = "aeiouAEIOUy",
                cedil = "cC"
            )
            
            accentTypes <- c("´","`","^","~","¨","ç")
            
            if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
                return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
            
            for(i in which(accentTypes%in%pattern))
                str <- chartr(symbols[i],nudeSymbols[i], str)
            
            return(str)
        }
        
        sf$nome <- rm_accent(sf$nome)
        
        sf$nome <- as.character(sf$nome)
        
        dados_mapa <- left_join(sf, dados_selecionados(), by = c("nome" = "Município"),
                                keep = FALSE, copy=TRUE)
        
        dados_mapa %>% leaflet() %>% addTiles() %>%  addPolygons(label = ~nome)
           
    })
    output$descmapa <- renderText({
        cidades2 <- paste(unique(dados_selecionados()$Município), collapse=',')
        paste("Mapa com os municípios selecionados:", cidades2)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
