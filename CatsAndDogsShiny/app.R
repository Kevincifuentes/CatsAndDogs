library(shiny)

server <- shinyServer(function(input, output) {
  #Preparar red neuronal
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  setwd("..")
  datos_train=read.csv("DatasetTrain.csv")
  datos_train[1]<-NULL
  datos_test=read.csv("DatasetTest.csv")
  datos_test[1:2]<-NULL
  datos_test$animaltype=0
  
  library(neuralnet)
  # RED NEURONAL
  # -----------------------------------------------------
  nms  <- names(datos_train[3:15])
  frml <- as.formula(paste("animaltype ~", paste(nms, collapse = " + ")))
  
  # VARIABLES DE CONFIGURACION
  # -----------------------------------------------------
  numeroCapasOcultas=c(20,10,5)
  thres=0.05
  
  # MODELO
  # -----------------------------------------------------
  modelo <- neuralnet(frml,
                      data = datos_train,
                      hidden = numeroCapasOcultas,
                      rep = 1, #numero de iteraciones
                      lifesign = "full",
                      linear.output = FALSE,
                      threshold     = thres,
                      algorithm     = "rprop+")
  
  
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  
  #Añado la función para obtener el csv
  source("../funcionCSV.R")
  
  output$images <- renderUI({
    if(is.null(input$files)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(files()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    if(is.null(input$files)) return(NULL)
    for (i in 1:nrow(files()))
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        print(imagename)
        output[[imagename]] <- 
          renderImage({
            list(src = files()$datapath[my_i],
                 alt = "Error al renderizar la imagen")
          }, deleteFile = FALSE)
        #Convertir a CSV la imagen
        print(input$files$datapath)
        datagram <- crearCSVImagen(input$files)
        write.csv(datagram,file=paste(input$files$name, ".csv"))
        filename = paste(input$files$name, ".csv")
      })
    }
  })
  
  observeEvent(input$predecir, {
    #Aquí habría que poner la prediccion con la red neuronal
    datos_test <- read.csv(paste(filename, ".csv"))
    prediccion  <- compute(modelo,within(datos_test,rm(animaltype)))
  })
  
})

ui <- shinyUI(fluidPage(
  titlePanel("Cats and Dogs"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'files', 
                label = 'Selecciona un perro o un gato',
                multiple = TRUE,
                accept=c('image/png', 'image/jpeg')),
      actionButton("predecir","Predecir Imagen")
    ),
    mainPanel(
      #tableOutput('files'),
      uiOutput('images')
    )
  )
))

shinyApp(ui=ui,server=server)
