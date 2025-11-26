library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

server <- function(input, output, session) {
  
  # ================================##
  # CARGA Y PREPARACIÓN DE DATOS
  # ============================================================================
  
  # Cargar datos 
  df <- reactive({
    tryCatch({
      data <- read.csv("datos_limpios.csv")
      
      # Asegurar que las columnas numéricas sean numéricas
      numeric_cols <- c("VEH_TOTAL", "VEH_LIGEROS_TOTAL", "VEH_PESADOS_TOTAL",
                        "VEH_2_EJES", "VEH_3_EJES", "VEH_4_EJES", "VEH_5_EJES", 
                        "VEH_6_EJES", "VEH_7_EJES", "VEH_8_EJES", "VEH_9_EJES")
      
      for(col in numeric_cols) {
        if(col %in% names(data)) {
          data[[col]] <- as.numeric(as.character(data[[col]]))
        }
      }
      
      data
      
    }, error = function(e) {
      # Datos de ejemplo si no se encuentra el archivo
      set.seed(123)
      n <- 250
      data.frame(
        DEPARTAMENTO = rep(c("Lima", "Arequipa", "Cusco", "Piura", "La Libertad"), n/5),
        ANIO = rep(2014, n),
        MES = rep(1:12, length.out = n),
        ADMINIST = sample(c("Concesionado", "No concesionado"), n, replace = TRUE),
        NOMBRE_PEAJE = paste("Peaje", LETTERS[1:5])[rep(1:5, n/5)],
        VEH_TOTAL = sample(5000:25000, n),
        VEH_LIGEROS_TOTAL = sample(3000:15000, n),
        VEH_PESADOS_TOTAL = sample(1000:8000, n),
        VEH_2_EJES = sample(2000:10000, n),
        VEH_3_EJES = sample(500:3000, n),
        VEH_4_EJES = sample(300:2000, n),
        VEH_5_EJES = sample(200:1500, n),
        VEH_6_EJES = sample(100:1000, n),
        VEH_7_EJES = sample(50:500, n),
        VEH_8_EJES = sample(20:300, n),
        VEH_9_EJES = sample(10:200, n)
      )
    })
  })
  
  # Actualizar opciones de selectInput cuando carguen los datos
  observe({
    data <- df()
    updateSelectInput(session, "departamento", 
                      choices = c("Todos", sort(unique(data$DEPARTAMENTO))))
    updateSelectInput(session, "anio", 
                      choices = sort(unique(data$ANIO)))
  })
  
  # ============================================================================
  # FILTRADO DE DATOS
  # ============================================================================
  
  # Datos filtrados (reactivo a los filtros)
  filtered_data <- eventReactive(input$apply_filters, {
    data <- df()
    
    # Filtro por departamento
    if(input$departamento != "Todos") {
      data <- data %>% filter(DEPARTAMENTO == input$departamento)
    }
    
    # Filtro por administración
    if(input$administracion != "Todos") {
      data <- data %>% filter(ADMINIST == input$administracion)
    }
    
    # Filtro por año
    data <- data %>% filter(ANIO == input$anio)
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # ============================================================================
  # INDICADORES CLAVE (KPIs)
  # ============================================================================
  
  # Total de vehículos
  output$kpi_total <- renderText({
    data <- filtered_data()
    format(sum(data$VEH_TOTAL, na.rm = TRUE), big.mark = ",")
  })
  
  # Promedio de vehículos pesados
  output$kpi_pesados <- renderText({
    data <- filtered_data()
    format(round(mean(data$VEH_PESADOS_TOTAL, na.rm = TRUE)), big.mark = ",")
  })
  # Promedio de vehículos ligeros
  output$kpi_ligeros <- renderText({
    data <- filtered_data()
    format(round(mean(data$VEH_LIGEROS_TOTAL, na.rm = TRUE)), big.mark = ",")
  })
  
  # Número de departamentos
  output$kpi_departamentos <- renderText({
    data <- filtered_data()
    length(unique(data$DEPARTAMENTO))
  })
  
  # Administración dominante
  output$kpi_admin <- renderText({
    data <- filtered_data()
    admin_counts <- table(data$ADMINIST)
    names(admin_counts)[which.max(admin_counts)]
  })
  # Número de peajes
  output$kpi_peajes <- renderText({
    data <- filtered_data()
    n_peajes <- length(unique(data$NOMBRE_PEAJE))
    format(n_peajes, big.mark = ",")
  })
  
  # ==============MINIMAPA ==============
  # Mini mapa en el sidebar
  output$mini_map <- renderPlotly({
    data <- df()
    
    # Coordenadas aproximadas de departamentos del Perú (centroides)
    dept_coords <- data.frame(
      DEPARTAMENTO = c("Lima", "Arequipa", "Cusco", "Piura", "La Libertad", "Puno",
                       "Junín", "Cajamarca", "Loreto", "Lambayeque", "Ancash", "Ica",
                       "Ayacucho", "Huancavelica", "San Martín", "Ucayali", "Amazonas",
                       "Apurímac", "Huánuco", "Madre de Dios", "Moquegua", "Pasco",
                       "Tacna", "Tumbes"),
      lat = c(-12.04, -16.39, -13.53, -5.19, -8.11, -15.84,
              -11.85, -7.16, -3.75, -6.70, -9.53, -13.71,
              -13.16, -12.78, -6.48, -8.38, -5.99,
              -13.64, -9.93, -12.59, -17.19, -10.68,
              -18.01, -3.57),
      lon = c(-77.03, -71.54, -71.97, -80.63, -78.98, -70.02,
              -75.27, -78.51, -73.25, -79.91, -77.53, -75.73,
              -74.22, -75.01, -76.36, -74.54, -77.86,
              -73.36, -76.24, -69.19, -70.93, -76.26,
              -70.25, -80.45)
    )
    
    # Agregar tráfico del período filtrado
    traffic_by_dept <- filtered_data() %>%
      group_by(DEPARTAMENTO) %>%
      summarise(Total = sum(VEH_TOTAL, na.rm = TRUE), .groups = 'drop')
    
    map_data <- dept_coords %>%
      left_join(traffic_by_dept, by = "DEPARTAMENTO") %>%
      mutate(Total = ifelse(is.na(Total), 0, Total))
    
    # Resaltar departamento seleccionado
    if(input$departamento != "Todos") {
      map_data$selected <- ifelse(map_data$DEPARTAMENTO == input$departamento, "Seleccionado", "Otros")
      map_data$size <- ifelse(map_data$DEPARTAMENTO == input$departamento, 15, 8)
      map_data$opacity <- ifelse(map_data$DEPARTAMENTO == input$departamento, 1, 0.4)
    } else {
      map_data$selected <- "Todos"
      map_data$size <- 8
      map_data$opacity <- 0.7
    }
    
    # Crear mapa
    plot_ly(map_data, lat = ~lat, lon = ~lon, 
            type = 'scattergeo',
            mode = 'markers',
            marker = list(
              size = ~size,
              color = ~Total,
              colorscale = list(c(0, '#e3f2fd'), c(1, '#007bff')),
              showscale = FALSE,
              line = list(color = '#fff', width = 1),
              opacity = ~opacity
            ),
            text = ~paste0("<b>", DEPARTAMENTO, "</b><br>",
                           "Tráfico: ", format(Total, big.mark = ",")),
            hoverinfo = 'text') %>%
      layout(
        geo = list(
          scope = 'south america',
          center = list(lon = -75, lat = -10),
          projection = list(type = 'mercator'),
          showland = TRUE,
          landcolor = toRGB("gray95"),
          coastlinecolor = toRGB("gray80"),
          showcountries = TRUE,
          countrycolor = toRGB("gray80"),
          showframe = FALSE,
          lonaxis = list(range = c(-82, -68)),
          lataxis = list(range = c(-19, -0))
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        paper_bgcolor = '#f8f9fa'
      )
  })
  # ============================================================================
  # VISUALIZACIONES PRINCIPALES
  # ============================================================================
  
  # Gráfico de tráfico por mes
  output$traffic_plot <- renderPlotly({
    data <- filtered_data()
    
    monthly_data <- data %>%
      group_by(MES) %>%
      summarise(Total = sum(VEH_TOTAL, na.rm = TRUE))
    
    plot_ly(monthly_data, x = ~MES, y = ~Total, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#007bff', width = 3),
            marker = list(size = 8, color = '#007bff')) %>%
      layout(
        xaxis = list(title = "Mes", tickmode = "linear", tick0 = 1, dtick = 1),
        yaxis = list(title = "Total de Vehículos"),
        hovermode = "x unified",
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = 'white'
      )
  })
  
  # Top 10 Peajes
  output$top_peajes <- renderUI({
    data <- filtered_data()
    
    top_peajes <- data %>%
      group_by(NOMBRE_PEAJE) %>%
      summarise(Total = sum(VEH_TOTAL, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      head(10)
    
    items <- lapply(1:nrow(top_peajes), function(i) {
      tags$li(paste0(i, ". ", top_peajes$NOMBRE_PEAJE[i], " - ", 
                     format(top_peajes$Total[i], big.mark = ",")))
    })
    
    tags$ol(class = "top-peajes-list", style = "margin: 0; padding-left: 20px;", items)
  })
  
  # Gráfico de distribución (pie chart)
  output$distribution_plot <- renderPlotly({
    data <- filtered_data()
    
    tipo_data <- data.frame(
      Tipo = c("Vehículos Ligeros", "Vehículos Pesados"),
      Total = c(sum(data$VEH_LIGEROS_TOTAL, na.rm = TRUE),
                sum(data$VEH_PESADOS_TOTAL, na.rm = TRUE))
    )
    
    plot_ly(tipo_data, labels = ~Tipo, values = ~Total, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = c('#007bff', '#6c757d'))) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = 'white'
      )
  })
  # Gráfico de composición de vehículos pesados
  output$heavy_vehicles_plot <- renderPlotly({
    data <- filtered_data()
    
    heavy_vars <- c("VEH_PESADOS_TAR_DIF", "VEH_PESADOS_2E", "VEH_PESADOS_3E", 
                    "VEH_PESADOS_4E", "VEH_PESADOS_5E", "VEH_PESADOS_6E", "VEH_PESADOS_7E")
    
    # Filtrar solo las variables que existen en los datos
    available_vars <- heavy_vars[heavy_vars %in% names(data)]
    
    if(length(available_vars) > 0) {
      heavy_data <- data.frame(
        Tipo = c("Tar_Diferen", "2 Ejes", "3 Ejes", "4 Ejes", "5 Ejes", "6 Ejes", "7 Ejes"),
        Variable = heavy_vars,
        stringsAsFactors = FALSE
      )
      
      heavy_data <- heavy_data[heavy_data$Variable %in% available_vars, ]
      
      heavy_data$Total <- sapply(heavy_data$Variable, function(var) {
        sum(data[[var]], na.rm = TRUE)
      })
      
      heavy_data <- heavy_data %>% filter(Total > 0) %>% arrange(desc(Total))
      
      plot_ly(heavy_data, x = ~Total, y = ~reorder(Tipo, Total), 
              type = 'bar', orientation = 'h',
              marker = list(color = '#dc3545'),
              text = ~paste(format(Total, big.mark = ","), "veh."),
              textposition = 'outside',
              hovertemplate = '%{y}<br>Total: %{x:,}<extra></extra>') %>%
        layout(
          xaxis = list(title = "Total de Vehículos"),
          yaxis = list(title = ""),
          margin = list(l = 130, r = 50),
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa'
        )
    } else {
      plot_ly() %>%
        layout(annotations = list(text = "Variables de vehículos pesados no disponibles",
                                  x = 0.5, y = 0.5, showarrow = FALSE))
    }
  })
  
  # Gráfico de composición de vehículos ligeros
  output$light_vehicles_plot <- renderPlotly({
    data <- filtered_data()
    
    light_vars <- c("VEH_LIGEROS_TAR_DIF", "VEH_LIGEROS_AUTOMOVILES")
    available_vars <- light_vars[light_vars %in% names(data)]
    
    if(length(available_vars) > 0) {
      light_totals <- data.frame(
        Tipo = c("Tarifa Diferencial", "Automóviles"),
        Total = c(
          sum(data$VEH_LIGEROS_TAR_DIF, na.rm = TRUE),
          sum(data$VEH_LIGEROS_AUTOMOVILES, na.rm = TRUE)
        )
      ) %>% filter(Total > 0)
      
      plot_ly(light_totals, labels = ~Tipo, values = ~Total, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              marker = list(colors = c('#17a2b8', '#ffc107'))) %>%
        layout(
          showlegend = TRUE,
          paper_bgcolor = 'white'
        )
    } else {
      plot_ly() %>%
        layout(annotations = list(text = "Variables de vehículos ligeros no disponibles",
                                  x = 0.5, y = 0.5, showarrow = FALSE))
    }
  })
  
  # Mapa de peajes (gráfico de barras por departamento o por peajes)
  output$map_plot <- renderPlotly({
    data <- filtered_data()
    
    # Si se selecciona "Todos", mostrar por departamento
    if(input$departamento == "Todos") {
      dept_data <- data %>%
        group_by(DEPARTAMENTO) %>%
        summarise(Total = sum(VEH_TOTAL, na.rm = TRUE)) %>%
        arrange(desc(Total))
      
      plot_ly(dept_data, x = ~Total, y = ~reorder(DEPARTAMENTO, Total), 
              type = 'bar', orientation = 'h',
              marker = list(color = '#007bff'),
              text = ~paste(format(Total, big.mark = ","), "vehículos"),
              textposition = 'outside',
              hovertemplate = '%{y}<br>Total: %{x:,}<extra></extra>') %>%
        layout(
          xaxis = list(title = "Total de Vehículos"),
          yaxis = list(title = ""),
          margin = list(l = 120),
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa'
        )
    } else {
      # Si se selecciona un departamento específico, mostrar por peajes
      peaje_data <- data %>%
        group_by(NOMBRE_PEAJE) %>%
        summarise(Total = sum(VEH_TOTAL, na.rm = TRUE)) %>%
        arrange(desc(Total))
      
      plot_ly(peaje_data, x = ~Total, y = ~reorder(NOMBRE_PEAJE, Total), 
              type = 'bar', orientation = 'h',
              marker = list(color = '#28a745'),
              text = ~paste(format(Total, big.mark = ","), "vehículos"),
              textposition = 'outside',
              hovertemplate = '%{y}<br>Total: %{x:,}<extra></extra>') %>%
        layout(
          xaxis = list(title = "Total de Vehículos"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa'
        )
    }
  })
  
  # Título dinámico para composición por peaje
  output$composition_title <- renderText({
    if(input$departamento == "Todos") {
      "Composición vehicular por departamento (Top 10)"
    } else {
      paste("Composición vehicular detallada -", input$departamento)
    }
  })
  
  # Gráfico de composición detallada por peaje
  output$composition_by_peaje_plot <- renderPlotly({
    data <- filtered_data()
    
    # Variables a analizar
    heavy_vars <- c("VEH_PESADOS_TAR_DIF", "VEH_PESADOS_2E", "VEH_PESADOS_3E", 
                    "VEH_PESADOS_4E", "VEH_PESADOS_5E", "VEH_PESADOS_6E", "VEH_PESADOS_7E")
    light_vars <- c("VEH_LIGEROS_TAR_DIF", "VEH_LIGEROS_AUTOMOVILES")
    
    all_vars <- c(light_vars, heavy_vars)
    available_vars <- all_vars[all_vars %in% names(data)]
    
    if(length(available_vars) == 0) {
      return(
        plot_ly() %>%
          layout(annotations = list(
            text = "Variables de composición vehicular no disponibles en los datos",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 14, color = '#6b7280')
          ))
      )
    }
    
    # Decidir agrupación según selección
    if(input$departamento == "Todos") {
      # Agrupar por departamento y tomar top 10
      composition_data <- data %>%
        group_by(DEPARTAMENTO) %>%
        summarise(across(all_of(available_vars), ~sum(., na.rm = TRUE)), .groups = 'drop') %>%
        mutate(Total = rowSums(select(., all_of(available_vars)), na.rm = TRUE)) %>%
        arrange(desc(Total)) %>%
        head(10) %>%
        select(-Total)
      
      group_var <- "DEPARTAMENTO"
    } else {
      # Agrupar por peaje
      composition_data <- data %>%
        group_by(NOMBRE_PEAJE) %>%
        summarise(across(all_of(available_vars), ~sum(., na.rm = TRUE)), .groups = 'drop') %>%
        mutate(Total = rowSums(select(., all_of(available_vars)), na.rm = TRUE)) %>%
        arrange(desc(Total)) %>%
        head(15) %>%  # Top 15 peajes
        select(-Total)
      
      group_var <- "NOMBRE_PEAJE"
    }
    
    # Crear el gráfico de barras apiladas
    p <- plot_ly()
    
    # Colores diferenciados: tonos azules/cian para ligeros, rojos/naranjas para pesados
    colors_map <- list(
      "VEH_LIGEROS_AUTOMOVILES" = "#17a2b8",
      "VEH_LIGEROS_TAR_DIF" = "#20c997",
      "VEH_PESADOS_TAR_DIF" = "#6c757d",
      "VEH_PESADOS_2E" = "#fd7e14",
      "VEH_PESADOS_3E" = "#dc3545",
      "VEH_PESADOS_4E" = "#e83e8c",
      "VEH_PESADOS_5E" = "#6f42c1",
      "VEH_PESADOS_6E" = "#007bff",
      "VEH_PESADOS_7E" = "#343a40"
    )
    
    # Nombres legibles
    names_map <- list(
      "VEH_LIGEROS_AUTOMOVILES" = "Ligeros: Automóviles",
      "VEH_LIGEROS_TAR_DIF" = "Ligeros: Tarifa Dif.",
      "VEH_PESADOS_TAR_DIF" = "Pesados: Tarifa Dif.",
      "VEH_PESADOS_2E" = "Pesados: 2 Ejes",
      "VEH_PESADOS_3E" = "Pesados: 3 Ejes",
      "VEH_PESADOS_4E" = "Pesados: 4 Ejes",
      "VEH_PESADOS_5E" = "Pesados: 5 Ejes",
      "VEH_PESADOS_6E" = "Pesados: 6 Ejes",
      "VEH_PESADOS_7E" = "Pesados: 7 Ejes"
    )
    
    # Agregar cada categoría como una traza
    for(var in available_vars) {
      if(sum(composition_data[[var]], na.rm = TRUE) > 0) {
        p <- p %>%
          add_trace(
            y = composition_data[[group_var]],
            x = composition_data[[var]],
            name = names_map[[var]],
            type = 'bar',
            orientation = 'h',
            marker = list(color = colors_map[[var]]),
            hovertemplate = paste0(
              '<b>%{y}</b><br>',
              names_map[[var]], ': %{x:,}<br>',
              '<extra></extra>'
            )
          )
      }
    }
    
    p %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = "Total de Vehículos"),
        yaxis = list(title = "", categoryorder = "total ascending"),
        legend = list(
          orientation = 'v',
          x = 1.02,
          y = 1,
          bgcolor = 'rgba(255,255,255,0.9)',
          bordercolor = '#e0e0e0',
          borderwidth = 1
        ),
        margin = list(l = 150, r = 200),
        paper_bgcolor = 'white',
        plot_bgcolor = '#f8f9fa',
        hovermode = 'closest'
      )
  })
  
  # Boxplot por departamento o por peajes
  output$boxplot_plot <- renderPlotly({
    data <- filtered_data()
    
    # Si se selecciona "Todos", mostrar por departamento
    if(input$departamento == "Todos") {
      plot_ly(data, y = ~VEH_TOTAL, x = ~DEPARTAMENTO, type = 'box',
              marker = list(color = '#007bff'),
              hovertemplate = '<b>%{x}</b><br>Valor: %{y:,}<extra></extra>') %>%
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Total de Vehículos"),
          showlegend = FALSE,
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa',
          margin = list(b = 100)
        )
    } else {
      # Si se selecciona un departamento específico, mostrar por peajes
      plot_ly(data, y = ~VEH_TOTAL, x = ~NOMBRE_PEAJE, type = 'box',
              marker = list(color = '#28a745'),
              hovertemplate = '<b>%{x}</b><br>Valor: %{y:,}<extra></extra>') %>%
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Total de Vehículos"),
          showlegend = FALSE,
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa',
          margin = list(b = 120)
        )
    }
  })
  
  # ============================================================================
  # EXPORTACIÓN DE DATOS
  # ============================================================================
  
  # Exportar CSV
  observeEvent(input$export_csv, {
    data <- filtered_data()
    write.csv(data, "datos_exportados.csv", row.names = FALSE)
    showNotification("Datos exportados a CSV exitosamente", type = "message")
  })
  
  # Exportar Excel
  observeEvent(input$export_excel, {
    tryCatch({
      library(writexl)
      data <- filtered_data()
      write_xlsx(data, "datos_exportados.xlsx")
      showNotification("Datos exportados a Excel exitosamente", type = "message")
    }, error = function(e) {
      showNotification("Instala 'writexl' para exportar a Excel: install.packages('writexl')", 
                       type = "error")
    })
  })
  
  # ============================================================================
  # ANÁLISIS AVANZADO - CLUSTERING
  # ============================================================================
  
  # Datos para clustering (reactivo)
  cluster_data <- eventReactive(input$run_cluster, {
    data <- filtered_data()
    
    # Preparar datos agregados por peaje
    cluster_df <- data %>%
      group_by(NOMBRE_PEAJE, DEPARTAMENTO) %>%
      summarise(
        Total_Vehiculos = sum(VEH_TOTAL, na.rm = TRUE),
        Promedio_Ligeros = mean(VEH_LIGEROS_TOTAL, na.rm = TRUE),
        Promedio_Pesados = mean(VEH_PESADOS_TOTAL, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Total_Vehiculos > 0)
    
    # Seleccionar variables para clustering
    cluster_vars <- cluster_df %>%
      select(Total_Vehiculos, Promedio_Ligeros, Promedio_Pesados) %>%
      scale()
    
    # K-means clustering
    set.seed(42)
    kmeans_result <- kmeans(cluster_vars, centers = input$n_clusters, nstart = 25)
    
    cluster_df$Cluster <- as.factor(kmeans_result$cluster)
    
    list(data = cluster_df, centers = kmeans_result$centers, model = kmeans_result)
  }, ignoreNULL = FALSE)
  
  # Gráfico de clustering
  output$cluster_plot <- renderPlotly({
    tryCatch({
      cluster_result <- cluster_data()
      data <- cluster_result$data
      
      plot_ly(data, x = ~Promedio_Ligeros, y = ~Promedio_Pesados, 
              color = ~Cluster, colors = c('#007bff', '#28a745', '#ffc107', '#dc3545', '#6f42c1'),
              type = 'scatter', mode = 'markers',
              marker = list(size = 10, opacity = 0.7),
              text = ~paste("Peaje:", NOMBRE_PEAJE, "<br>",
                            "Departamento:", DEPARTAMENTO, "<br>",
                            "Total:", format(Total_Vehiculos, big.mark = ","))) %>%
        layout(
          xaxis = list(title = "Promedio Vehículos Ligeros"),
          yaxis = list(title = "Promedio Vehículos Pesados"),
          hovermode = "closest",
          legend = list(title = list(text = "Cluster")),
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa'
        )
    }, error = function(e) {
      plot_ly() %>%
        layout(
          annotations = list(
            text = "Presiona 'Ejecutar Clustering' para generar el análisis",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 14, color = '#6b7280')
          )
        )
    })
  })
  
  # Gráfico de correlación
  output$correlation_plot <- renderPlotly({
    data <- filtered_data()
    
    # Seleccionar variables numéricas relevantes
    cor_vars <- c("VEH_TOTAL", "VEH_LIGEROS_TOTAL", "VEH_PESADOS_TOTAL")
    
    # Agregar más variables si existen
    additional_vars <- c("VEH_2_EJES", "VEH_3_EJES", "VEH_4_EJES", "VEH_5_EJES")
    for(var in additional_vars) {
      if(var %in% names(data) && sum(!is.na(data[[var]])) > 0) {
        cor_vars <- c(cor_vars, var)
      }
    }
    
    # Calcular matriz de correlación
    cor_data <- data %>%
      select(all_of(cor_vars)) %>%
      na.omit()
    
    if(nrow(cor_data) > 0) {
      cor_matrix <- cor(cor_data)
      
      # Crear nombres más cortos para visualización
      short_names <- c(
        "VEH_TOTAL" = "Total",
        "VEH_LIGEROS_TOTAL" = "Ligeros",
        "VEH_PESADOS_TOTAL" = "Pesados",
        "VEH_2_EJES" = "2 Ejes",
        "VEH_3_EJES" = "3 Ejes",
        "VEH_4_EJES" = "4 Ejes",
        "VEH_5_EJES" = "5 Ejes"
      )
      
      rownames(cor_matrix) <- short_names[rownames(cor_matrix)]
      colnames(cor_matrix) <- short_names[colnames(cor_matrix)]
      
      plot_ly(z = cor_matrix, x = colnames(cor_matrix), y = rownames(cor_matrix),
              type = "heatmap", colors = colorRamp(c("#dc3545", "white", "#007bff")),
              zmin = -1, zmax = 1,
              text = round(cor_matrix, 2),
              hovertemplate = '%{y} vs %{x}<br>Correlación: %{z:.2f}<extra></extra>') %>%
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = ""),
          paper_bgcolor = 'white'
        )
    } else {
      plot_ly() %>%
        layout(
          annotations = list(
            text = "No hay datos suficientes para calcular correlaciones",
            x = 0.5, y = 0.5, showarrow = FALSE
          )
        )
    }
  })
  
  # ============================================================================
  # PREDICCIONES Y MODELADO
  # ============================================================================
  
  # Modelo y predicciones (reactivo)
  prediction_results <- eventReactive(input$run_prediction, {
    data <- filtered_data()
    
    # Preparar datos de serie temporal
    ts_data <- data %>%
      group_by(ANIO, MES) %>%
      summarise(Total = sum(VEH_TOTAL, na.rm = TRUE), .groups = 'drop') %>%
      arrange(ANIO, MES)
    
    if(nrow(ts_data) < 12) {
      return(list(error = "Datos insuficientes para predicción (mínimo 12 meses)"))
    }
    
    # Dividir en train/test (80/20)
    n <- nrow(ts_data)
    train_size <- floor(0.8 * n)
    train_data <- ts_data[1:train_size, ]
    test_data <- ts_data[(train_size + 1):n, ]
    
    # MÉTODO ARIMA
    if(input$pred_method == "ARIMA") {
      tryCatch({
        library(forecast)
        ts_train <- ts(train_data$Total, frequency = 12)
        model <- auto.arima(ts_train)
        
        # Predicción
        forecast_result <- forecast(model, h = input$horizon)
        
        # Métricas en test set
        if(nrow(test_data) > 0) {
          test_pred <- forecast(model, h = nrow(test_data))
          rmse <- sqrt(mean((test_data$Total - test_pred$mean)^2))
          mae <- mean(abs(test_data$Total - test_pred$mean))
          r2 <- 1 - sum((test_data$Total - test_pred$mean)^2) / 
            sum((test_data$Total - mean(train_data$Total))^2)
        } else {
          rmse <- mae <- r2 <- NA
        }
        
        # Crear dataframe de predicción
        last_date <- max(ts_data$MES + (ts_data$ANIO - min(ts_data$ANIO)) * 12)
        pred_df <- data.frame(
          Periodo = (last_date + 1):(last_date + input$horizon),
          Prediccion = as.numeric(forecast_result$mean),
          Lower = as.numeric(forecast_result$lower[, 2]),
          Upper = as.numeric(forecast_result$upper[, 2])
        )
        
        list(
          historical = ts_data,
          predictions = pred_df,
          rmse = rmse,
          mae = mae,
          r2 = r2,
          method = "ARIMA"
        )
      }, error = function(e) {
        list(error = paste("Error en ARIMA:", e$message))
      })
      
      # MÉTODO RANDOM FOREST
    } else if(input$pred_method == "Random Forest") {
      tryCatch({
        library(randomForest)
        
        # Preparar features
        train_data$lag1 <- lag(train_data$Total, 1)
        train_data$lag2 <- lag(train_data$Total, 2)
        train_data$trend <- 1:nrow(train_data)
        train_data <- na.omit(train_data)
        
        model <- randomForest(Total ~ lag1 + lag2 + trend + MES, 
                              data = train_data, ntree = 100)
        
        # Predicciones iterativas
        last_values <- tail(train_data$Total, 2)
        predictions <- numeric(input$horizon)
        
        for(i in 1:input$horizon) {
          new_data <- data.frame(
            lag1 = last_values[2],
            lag2 = last_values[1],
            trend = nrow(train_data) + i,
            MES = ((max(train_data$MES) + i - 1) %% 12) + 1
          )
          pred <- predict(model, new_data)
          predictions[i] <- pred
          last_values <- c(last_values[2], pred)
        }
        
        # Métricas
        if(nrow(test_data) > 0) {
          test_pred <- numeric(nrow(test_data))
          test_lags <- tail(train_data$Total, 2)
          for(i in 1:nrow(test_data)) {
            new_data <- data.frame(
              lag1 = test_lags[2],
              lag2 = test_lags[1],
              trend = nrow(train_data) + i,
              MES = test_data$MES[i]
            )
            test_pred[i] <- predict(model, new_data)
            test_lags <- c(test_lags[2], test_data$Total[i])
          }
          rmse <- sqrt(mean((test_data$Total - test_pred)^2))
          mae <- mean(abs(test_data$Total - test_pred))
          r2 <- 1 - sum((test_data$Total - test_pred)^2) / 
            sum((test_data$Total - mean(train_data$Total))^2)
        } else {
          rmse <- mae <- r2 <- NA
        }
        
        last_date <- max(ts_data$MES + (ts_data$ANIO - min(ts_data$ANIO)) * 12)
        pred_df <- data.frame(
          Periodo = (last_date + 1):(last_date + input$horizon),
          Prediccion = predictions,
          Lower = predictions * 0.9,
          Upper = predictions * 1.1
        )
        
        list(
          historical = ts_data,
          predictions = pred_df,
          rmse = rmse,
          mae = mae,
          r2 = r2,
          method = "Random Forest"
        )
      }, error = function(e) {
        list(error = "Error en Random Forest. Instala el paquete: install.packages('randomForest')")
      })
      
      # MÉTODO PROPHET
    } else {
      tryCatch({
        library(prophet)
        
        # Preparar datos para Prophet
        prophet_df <- train_data %>%
          mutate(ds = as.Date(paste(ANIO, MES, "01", sep = "-")),
                 y = Total) %>%
          select(ds, y)
        
        model <- prophet(prophet_df, yearly.seasonality = TRUE, 
                         weekly.seasonality = FALSE)
        
        # Crear futuro
        future <- make_future_dataframe(model, periods = input$horizon, freq = "month")
        forecast_result <- predict(model, future)
        
        # Métricas en test set
        if(nrow(test_data) > 0) {
          test_dates <- as.Date(paste(test_data$ANIO, test_data$MES, "01", sep = "-"))
          test_pred <- forecast_result %>%
            filter(ds %in% test_dates) %>%
            pull(yhat)
          
          if(length(test_pred) == nrow(test_data)) {
            rmse <- sqrt(mean((test_data$Total - test_pred)^2))
            mae <- mean(abs(test_data$Total - test_pred))
            r2 <- 1 - sum((test_data$Total - test_pred)^2) / 
              sum((test_data$Total - mean(train_data$Total))^2)
          } else {
            rmse <- mae <- r2 <- NA
          }
        } else {
          rmse <- mae <- r2 <- NA
        }
        
        # Extraer predicciones futuras
        last_date <- max(ts_data$MES + (ts_data$ANIO - min(ts_data$ANIO)) * 12)
        future_forecast <- tail(forecast_result, input$horizon)
        
        pred_df <- data.frame(
          Periodo = (last_date + 1):(last_date + input$horizon),
          Prediccion = future_forecast$yhat,
          Lower = future_forecast$yhat_lower,
          Upper = future_forecast$yhat_upper
        )
        
        list(
          historical = ts_data,
          predictions = pred_df,
          rmse = rmse,
          mae = mae,
          r2 = r2,
          method = "Prophet"
        )
      }, error = function(e) {
        list(error = "Error en Prophet. Instala el paquete: install.packages('prophet')")
      })
    }
  }, ignoreNULL = FALSE)
  
  # Gráfico de predicción
  output$prediction_plot <- renderPlotly({
    tryCatch({
      results <- prediction_results()
      
      if(!is.null(results$error)) {
        return(
          plot_ly() %>%
            layout(
              annotations = list(
                text = results$error,
                x = 0.5, y = 0.5, showarrow = FALSE,
                font = list(size = 12, color = '#dc3545')
              )
            )
        )
      }
      
      # Datos históricos
      hist_df <- results$historical
      hist_df$Periodo <- 1:nrow(hist_df)
      
      # Crear gráfico
      p <- plot_ly()
      
      # Línea histórica
      p <- p %>% add_lines(data = hist_df, x = ~Periodo, y = ~Total,
                           name = "Histórico", line = list(color = '#007bff', width = 2))
      
      # Predicciones
      pred_df <- results$predictions
      p <- p %>% add_lines(data = pred_df, x = ~Periodo, y = ~Prediccion,
                           name = "Predicción", 
                           line = list(color = '#28a745', width = 2, dash = 'dash'))
      
      # Intervalo de confianza
      p <- p %>% add_ribbons(data = pred_df, x = ~Periodo, 
                             ymin = ~Lower, ymax = ~Upper,
                             name = "Intervalo 95%", fillcolor = 'rgba(40, 167, 69, 0.2)',
                             line = list(color = 'transparent'))
      
      p %>% layout(
        xaxis = list(title = "Periodo"),
        yaxis = list(title = "Vehículos Totales"),
        hovermode = "x unified",
        legend = list(orientation = 'h', y = -0.2),
        paper_bgcolor = 'white',
        plot_bgcolor = '#f8f9fa'
      )
    }, error = function(e) {
      plot_ly() %>%
        layout(
          annotations = list(
            text = "Presiona 'Generar predicción' para ver el análisis",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 14, color = '#6b7280')
          )
        )
    })
  })
  
  # ============================================================================
  # MÉTRICAS DEL MODELO
  # ============================================================================
  
  # RMSE
  output$model_rmse <- renderText({
    tryCatch({
      results <- prediction_results()
      if(!is.null(results$error) || is.na(results$rmse)) return("N/A")
      format(round(results$rmse, 2), big.mark = ",")
    }, error = function(e) "N/A")
  })
  
  # MAE
  output$model_mae <- renderText({
    tryCatch({
      results <- prediction_results()
      if(!is.null(results$error) || is.na(results$mae)) return("N/A")
      format(round(results$mae, 2), big.mark = ",")
    }, error = function(e) "N/A")
  })
  
  # R²
  output$model_r2 <- renderText({
    tryCatch({
      results <- prediction_results()
      if(!is.null(results$error) || is.na(results$r2)) return("N/A")
      paste0(round(results$r2 * 100, 1), "%")
    }, error = function(e) "N/A")
  })
  
  # Gráfico de métricas
  output$metrics_plot <- renderPlotly({
    tryCatch({
      results <- prediction_results()
      if(!is.null(results$error) || is.na(results$rmse)) {
        return(plot_ly())
      }
      
      metrics_df <- data.frame(
        Metrica = c("RMSE", "MAE"),
        Valor = c(results$rmse, results$mae)
      )
      
      plot_ly(metrics_df, x = ~Metrica, y = ~Valor, type = 'bar',
              marker = list(color = c('#007bff', '#28a745'))) %>%
        layout(
          yaxis = list(title = "Valor"),
          xaxis = list(title = ""),
          showlegend = FALSE,
          paper_bgcolor = 'white',
          plot_bgcolor = '#f8f9fa'
        )
    }, error = function(e) {
      plot_ly()
    })
  })
  
  # ============================================================================
  # DESCARGAS AVANZADAS
  # ============================================================================
  
  # Descargar predicciones
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste0("predicciones_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        results <- prediction_results()
        if(!is.null(results$error)) {
          write.csv(data.frame(Error = results$error), file, row.names = FALSE)
        } else {
          write.csv(results$predictions, file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = "No hay predicciones generadas"), 
                  file, row.names = FALSE)
      })
    }
  )
  
  # Descargar clusters
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste0("clusters_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        cluster_result <- cluster_data()
        write.csv(cluster_result$data, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = "No hay análisis de clusters generado"), 
                  file, row.names = FALSE)
      })
    }
  )
  
  # Descargar reporte completo
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("reporte_completo_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tryCatch({
        library(writexl)
        
        # Preparar todas las hojas del reporte
        sheets <- list()
        
        # Hoja 1: Datos filtrados
        sheets$Datos_Filtrados <- filtered_data()
        
        # Hoja 2: Resumen estadístico
        data <- filtered_data()
        sheets$Resumen <- data %>%
          summarise(
            Total_Vehiculos = sum(VEH_TOTAL, na.rm = TRUE),
            Promedio_Ligeros = mean(VEH_LIGEROS_TOTAL, na.rm = TRUE),
            Promedio_Pesados = mean(VEH_PESADOS_TOTAL, na.rm = TRUE),
            Max_Trafico = max(VEH_TOTAL, na.rm = TRUE),
            Min_Trafico = min(VEH_TOTAL, na.rm = TRUE),
            Departamentos = n_distinct(DEPARTAMENTO),
            Peajes = n_distinct(NOMBRE_PEAJE)
          )
        
        # Hoja 3: Top peajes
        sheets$Top_Peajes <- data %>%
          group_by(NOMBRE_PEAJE, DEPARTAMENTO) %>%
          summarise(
            Total_Vehiculos = sum(VEH_TOTAL, na.rm = TRUE),
            Promedio_Mensual = mean(VEH_TOTAL, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Total_Vehiculos)) %>%
          head(20)
        
        # Hoja 4: Clusters (si existen)
        tryCatch({
          cluster_result <- cluster_data()
          sheets$Clusters <- cluster_result$data
        }, error = function(e) {
          sheets$Clusters <- data.frame(Mensaje = "No hay análisis de clusters disponible")
        })
        
        # Hoja 5: Predicciones (si existen)
        tryCatch({
          pred_result <- prediction_results()
          if(is.null(pred_result$error)) {
            sheets$Predicciones <- pred_result$predictions
            sheets$Metricas_Modelo <- data.frame(
              Metrica = c("RMSE", "MAE", "R2", "Metodo"),
              Valor = c(
                as.character(round(pred_result$rmse, 2)),
                as.character(round(pred_result$mae, 2)),
                as.character(round(pred_result$r2, 4)),
                pred_result$method
              )
            )
          }
        }, error = function(e) {
          sheets$Predicciones <- data.frame(Mensaje = "No hay predicciones disponibles")
        })
        
        # Escribir el archivo Excel
        write_xlsx(sheets, file)
        
      }, error = function(e) {
        # Si falla, crear un CSV simple
        write.csv(filtered_data(), file, row.names = FALSE)
        showNotification(
          paste("Error al crear Excel. Se creó CSV en su lugar.", 
                "Instala 'writexl': install.packages('writexl')"),
          type = "warning",
          duration = 10
        )
      })
    }
  )
  
}