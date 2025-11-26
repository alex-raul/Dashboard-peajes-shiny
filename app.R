library(shiny)

# Cargar UI y Server
source("ui.R")
source("server.R")

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)