# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##########################
## Librerias necesarias ##
##########################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(rmarkdown)
library(RColorBrewer)
library(kableExtra)
library(knitr)
library(stringi)
library(DT)


########################################################
## Lectura de las bases y desegregacion por categoria ##
########################################################

base=read.csv("data1.csv")
base1=read.csv("data_inv.csv")
base_banca=base[base$Categoria%in%c("CAPTACION DINERO","SEGUROS","TARJETAS CREDITO","BANCA ELECTR CNCPT",
                                   "BANCA IMAGEN","BANCA PAGOS SERVIC","CREDITO OTROS","CREDITO VIVIENDA",
                                   "SERVICIOS PYMES",	"CUENTA DE AHORRO",	"SERVICIOS BANCARIO",
                                   "BANCA ELECTR PERSO",	"PENSIONES",	"PUESTOS DE BOLSA",
                                   "TARJETAS CRED COME",	"TARJETAS DEB AFILI",	"TARJETAS DEB EMISO",
                                   "CREDITO PERSONAL",	"SEGUROS DE VIDA"	,"BANCA INTERNACIONA"	,
                                   "TARJETAS CRED AFIL",	"CREDITO EMPRESARIA",	"FACTOREO",
                                   "LEASING",	"SEGUROS AUTOMOVILE",	"ENVIOS DE DINERO",	"CREDITO PRENDARIO",
                                   "BANCA CAJEROS AUTO"	,"BANCA EMPRESARIAL",	"SEGUROS GASTOS MED",	
                                   "PUBLICIDAD COOPERA",	"BANCA DE INVERSION",	"TARJETAS CRED MARC",
                                   "BANCA ACTUAL.DATOS",	"TARJETAS DEBITO",	"TARJETAS DEB COMER",
                                   "CERTIFICADOS INVER"),c(1:14,18)]
base_frijoles=base[base$Categoria%in%c("FRIJOLES EN GRANO"),c(1:15)]
base_arroz=base[base$Categoria%in%c("ARROZ"),c(1:15)]
base_restaurantes=base[base$Categoria%in%c("FAST FOOD HAMBURGU",	"FAST FOOD TACO",	"FAST FOOD CASUAL",
                                          "FAST FOOD SANDWICH",	"FAST FOOD POLLO",	"FAST FOOD PIZZA"	,
                                          "REST TIPO AMERICAN",	"FAST FOOD ORIENTAL"),c(1:14,18)]
base_salsa=base[base$Categoria%in%c("SALSA INGLESA"),c(1:15)]
base_celulares=base[base$Categoria%in%c("CELULARES"),c(1:14,19)]
base_azar=base[base$Categoria%in%c("AZAR"),c(1:14,19)]
base_pintura=base[base$Categoria%in%c("PINTURAS"),c(1:14,17)]
base_inmuebles=base[base$Categoria%in%c("VIVIENDA","BIENES RAICES PLAY","BIENES RAICES","INMOBILIARIA VIVIE"),c(1:14,22)]
base_cerveza=base[base$Categoria%in%"CERVEZA",c(1:14,18)]
base_Movil=base[base$Categoria%in%"OPERADORES MOV FUL",c(1:14,19)]
base_TV=base[base$Categoria%in%"TELEVISION CABLE",c(1:14,19)]
base_ins=base[base$Categoria%in%c("ADMINISTRATIVOS","ESTADO" , "FONDOS DE INVERSION",
                                  "MUSEOS","REPUESTOS","SEGUROS","SEGUROS AUTOMOVILE","SEGUROS DE VIDA",
                                  "SEGUROS GASTOS MED","SITIOS"),c(1:14,22)]
base_universidad=base[base$Categoria%in%"UNIVERSIDAD PRIVAD",c(1:14,22)]
base_fifcoAgua=base[base$Categoria%in%"AGUA PURA",c(1:14,21)]
base_fifcoJugos=base[base$Categoria%in%c("JUGOS","REFRESCOS","GASEOSOS CRISTALIN","GASEOSOS COLAS","GASEOSOS SABORES",
                                         "REFRESCOS DE TE","BEBIDAS NATURALES"),c(1:14,22)]
base_fifcoEnergizantes=base[base$Categoria%in%"BEBIDAS ENERGIZANT",c(1:14,16)]
rm(base)

#######################################
## Definicion de Usuario y contrsena ##
#######################################


my_username="Camedia planners"
my_password="CamediaCentral"
Logged=F


#################################################################################
## Se da el formato del dashboard y todas las variables que se van a solicitar ##
#################################################################################

ui <- dashboardPage(
  dashboardHeader(title = ":CAMedia"),
  
  
  dashboardSidebar(
    
    selectInput("cliente",label = "Cliente",choices=c("BANCO POPULAR","BCR","Claro TV","Claro Movil","Cuestamoras",
                                                      "Fidelitas","FIFCO Agua","FIFCO Cerveza","FIFCO Energizantes",
                                                      "FIFCO Jugos","HUAWEI","IGT","INS","LANCO","Pizza Hut",
                                                      "Rostipollos","SKY","Tio Pelon Arroz","Tio Pelon Frijoles",
                                                      "Tio Pelon Salsa Inglesa","UCIMED")),
    
    
    
    selectInput("ano",label = "Año", choices = c(2016,2017,2018),2018),
    
    
    selectInput("date_in",label="Mes inicial",choices = c("Enero","Febrero","Marzo","Abril",
                                                          "Mayo","Junio","Julio","Agosto","Septiembre",
                                                          "Octubre","Noviembre","Diciembre")),
    
    
    
    selectInput("date_fin",label="Mes final",choices =c("Enero","Febrero","Marzo","Abril",
                                                        "Mayo","Junio","Julio","Agosto","Septiembre",
                                                        "Octubre","Noviembre","Diciembre")),
    
    
    
    selectInput("moneda",label="Moneda",choices =c("Colones","Dólares"),"Dólares"),
    
    
    
    selectInput("unidad",label="Inversión",choices =c("Miles","Unidades"),"Unidades"),
    
    
    
    div(style="display:inline-block;width:47%;text-align:right",downloadButton("report", label= "Reporte")),
    
    
    
    div(style="display:inline-block;width:43%;text-align:right",downloadButton("base", label= "Base"))
    
  ),
  
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: red;
                      font-size: 40px;
                      }
                      "))
      ),
    
    div(style = 'overflow-x: scroll', tabItem(tabName = "Data set",dataTableOutput(outputId = "dataset")))
    
      )
  
  
      )

#################################################################################################
## Se Programan ciertas funciones como el ingreso al sistema y cual reporte usar segun cliente ##
#################################################################################################


server <- function(input, output) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }
  
  
  
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  
  
  obs2 <- observe({
    req(input$ok)
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
  
  
  
  mes_f=reactive({
    switch(input$date_fin,
           "Enero"=1,
           "Febrero"=2,
           "Marzo"=3,
           "Abril"=4,
           "Mayo"=5,
           "Junio"=6,
           "Julio"=7,
           "Agosto"=8,
           "Septiembre"=9,
           "Octubre"=10,
           "Noviembre"=11,
           "Diciembre"=12
    )        
  })
  
  
  
  mes_i=reactive({
    switch(input$date_in,
           "Enero"=1,
           "Febrero"=2,
           "Marzo"=3,
           "Abril"=4,
           "Mayo"=5,
           "Junio"=6,
           "Julio"=7,
           "Agosto"=8, 
           "Septiembre"=9,
           "Octubre"=10,
           "Noviembre"=11,
           "Diciembre"=12
    )        
  })
  
  
  
  datasetInput = reactive({
    
    validate(
      if(mes_f()<mes_i()){
        "ERROR: El mes final debe ser mayor o igual al mes inicial"
      } else {
        NULL
      }
    )
    switch(input$cliente,
           "BANCO POPULAR" = base_banca,
           "BCR" = base_banca,
           "Claro TV" = base_TV,
           "Claro Movil" = base_Movil,
           "Cuestamoras" = base_inmuebles,
           "Fidelitas" = base_universidad,
           "FIFCO Agua" = base_fifcoAgua,
           "FIFCO Cerveza" = base_cerveza,
           "FIFCO Energizantes" = base_fifcoEnergizantes,
           "FIFCO Jugos" = base_fifcoJugos,
           "HUAWEI" = base_celulares,
           "IGT" = base_azar,
           "INS" = base_ins,
           "LANCO" = base_pintura,
           "Pizza Hut" = base_restaurantes,
           "Rostipollos" = base_restaurantes,
           "SKY" = base_TV,
           "Tio Pelon Arroz" = base_arroz,
           "Tio Pelon Frijoles" = base_frijoles,
           "Tio Pelon Salsa Inglesa" =base_salsa,
           "UCIMED" = base_universidad
    )
  })
  
  datasetInput1=reactive({
    base = datasetInput()
    names(base)=c("Año","Mes","Categoría","Marca","Anunciante","Tipo.de.Medio","Medio","Producto",
                  "Versión","Franja","Duración","Msj","INV.Dólar","INV","TRP")
    base
  })
  
  output$dataset <- renderDataTable(datasetInput1(),rownames=F, filter="top",options=list(autoWidth=T))
                                    
  
  
  ##############################################################
  ##  Dos machotes, dependiendo de la cantidad de categorias  ##
  ##############################################################
  
  output$report <- downloadHandler(
    # Para la salida en PDF, usa "report.html"
    filename = "report.html",
    content = function(file) {
      # Copia el reporte a un directorio temporal antes de porcesarlo, en 
      #caso de que no tengamos permiso de escritura en el directorio actual
      #puede ocurrir un error
      tempReport <- file.path(tempdir(),"report.Rmd")
      
      ########################################################
      ## Indicar cual reporte le corresponde a cual cliente ##
      ########################################################
      
      file.copy(switch(input$cliente,"BANCO POPULAR"= "report1.Rmd","BCR"= "report1.Rmd","Claro TV"= "report.Rmd",
                       "Claro Movil"= "report.Rmd","Fidelitas"= "report.Rmd","FIFCO Agua"= "report.Rmd",
                       "FIFCO Cerveza"= "report.Rmd","Cuestamoras"= "report2.Rmd","FIFCO Energizantes"= "report.Rmd",
                       "FIFCO Jugos"= "report1.Rmd","HUAWEI"= "report.Rmd","IGT"= "report.Rmd","INS"= "report1.Rmd",
                       "LANCO"= "report.Rmd","Pizza Hut"= "report1.Rmd","Rostipollos"= "report1.Rmd",
                       "SKY"= "report.Rmd","Tio Pelon Arroz"= "report.Rmd","Tio Pelon Frijoles"= "report.Rmd",
                       "Tio Pelon Salsa Inglesa"= "report.Rmd","UCIMED"= "report.Rmd"), 
                tempReport, overwrite = TRUE)
      
      # configurar los parametros para pasar al documento .Rmd
      params <- list(ano = input$ano, cliente = input$cliente, mes_in = input$date_in, 
                     mes_fin = input$date_fin,unidad=input$unidad,
                     base=datasetInput1(), mes_i = mes_i(), mes_f = mes_f(), base1 = base1,moneda = input$moneda) 
      
      #Copilar el documento con la lista de parametros, de tal manera que se 
      #evalue de la misma manera que el entorno de la aplicacion.
      rmarkdown::render(tempReport, output_file = file,
             params = params,
             envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  output$base <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = "base.csv",
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      s=input$dataset_rows_all
      write.csv(datasetInput1()[s,,drop=FALSE],file,row.names = F,
                col.names = c("Anno","Mes","Categoria","Marca","Anunciante",
                              "Tipo.de.Medio","Medio","Producto",
                              "Version","Franja","Duracion","Msj","INV.Dolar","INV","TRP"))
      
    }
  )
}


############################
## Se corre la aplicacion ##
############################


shinyApp(ui = ui, server = server)

