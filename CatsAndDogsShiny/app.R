library(shiny)

server <- shinyServer(function(input, output) {
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
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
      })
    }
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
      submitButton("Predecir Imagen")
    ),
    mainPanel(
      #tableOutput('files'),
      uiOutput('images')
    )
  )
))

shinyApp(ui=ui,server=server)
