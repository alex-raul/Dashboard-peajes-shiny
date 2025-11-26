library(shiny)
library(plotly)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$title("Red Vial Nacional"),
    tags$style(HTML("
      /* Estilos globales */
      body { 
        background-color: #f6f9fc !important; 
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; 
      }
      
      /* Cards (paneles blancos) */
      .card { 
        border-radius: 12px; 
        box-shadow: 0 2px 8px rgba(0,0,0,0.1); 
        background: white;
        margin-bottom: 20px;
      }
      
      /* KPIs grandes */
      .kpi { 
        font-size: 2.2rem;             
        font-weight: 700;             
        font-family: 'Poppins', sans-serif;  
        color: #2563eb;                
        text-shadow: 1px 1px 3px rgba(0,0,0,0.1); 
        margin: 0;
        letter-spacing: 0.5px;         
        transition: transform 0.2s ease-in-out;
      }
      
      .kpi:hover {
        transform: scale(1.05);        
      }
      
      .kpi-sub { 
        font-size: 0.9rem; 
        font-weight: 500;
        font-family: 'Poppins', sans-serif;
        color: #6b7280;               
        margin-top: 6px;
        letter-spacing: 0.3px;
      }
      
      /* Sidebar - MEJORADO */
      .well { 
        background-color: #fff !important; 
        border: none !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08) !important;
        border-radius: 12px !important;
      }
      
      /* Botones - MEJORADO */
      .btn-primary, 
      #apply_filters { 
        background-color: #007bff !important; 
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 10px 20px !important;
        width: 100% !important;
        font-weight: 600 !important;
      }
      .btn-primary:hover,
      #apply_filters:hover {
        background-color: #0056b3 !important;
      }
      
      /* Botones secundarios de exportación */
      #export_csv, #export_excel,
      #run_cluster, #run_prediction {
        background-color: #6c757d !important;
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 8px 16px !important;
      }
      #export_csv:hover, #export_excel:hover,
      #run_cluster:hover, #run_prediction:hover {
        background-color: #10940c !important;
      }
      
      /* Botones de descarga */
      .btn-default,
      .shiny-download-link {
        background-color: #6c757d !important;
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 8px 16px !important;
        text-decoration: none !important;
      }
      .btn-default:hover,
      .shiny-download-link:hover {
        background-color: #545b62 !important;
      }
      
      /* Header principal */
      .main-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 20px;
      }
      .export-buttons {
        display: flex;
        gap: 10px;
      }
      
      /* Títulos de gráficos */
      h6 {
        font-weight: 600;
        color: #374151;
        margin-bottom: 15px;
        font-size: 0.95rem;
      }
      h4 {
        font-weight: 600;
        color: #1f2937;
      }
      
      /* Lista de top peajes */
      .top-peajes-list {
        list-style: none;
        padding: 0;
        margin: 0;
      }
      .top-peajes-list li {
        padding: 8px 0;
        border-bottom: 1px solid #f0f0f0;
        font-size: 0.875rem;
      }
      .top-peajes-list li:last-child {
        border-bottom: none;
      }
      
      /* Selectores mejorados */
      .selectize-input {
        border-radius: 6px !important;
        border: 1px solid #d1d5db !important;
      }
      .selectize-input:focus {
        border-color: #007bff !important;
        box-shadow: 0 0 0 3px rgba(0,123,255,0.1) !important;
      }
      
      /* Inputs numéricos */
      input[type='number'] {
        border-radius: 6px !important;
        border: 1px solid #d1d5db !important;
        padding: 6px 12px !important;
      }
      
      /* Filas con mejor espaciado */
      .row {
        margin-bottom: 15px;
      }
    "))
  ),
  
  div(class = "main-header",
      h3("Visualización de Flujo vehicular registrado en las unidades de peaje de la Red Vial Nacional 2014-2025 II Trimestre", 
         style = "
          background-color: #2563eb;
          color: white;
          padding: 15px 25px;
          border-radius: 10px;
          box-shadow: 0 3px 6px rgba(0,0,0,0.15);
          text-align: center;
          font-family: 'Poppins', sans-serif;
          font-weight: 700;
          font-size: 3rem;
          letter-spacing: 0.5px;
          margin-bottom: 25px;
         ")
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "card sidebar", 
          h5("Dashboard Peajes", style = "margin-top: 0;"),
          hr(),
          selectInput("departamento", "Departamento", choices = c("Todos")),
          selectInput("administracion", "Administración", 
                      choices = c("Todos", "Concesionado", "No concesionado"), 
                      selected = "Todos"),
          selectInput("anio", "Año", choices = c(2014)),
          br(),
          actionButton("apply_filters", "Aplicar filtros", class = "btn-primary"),
          # :
          hr(),
          div(style = "margin-top: 40px;",
              h6("Vista geográfica", style = "margin-bottom: 15px; color: #1f92cc; font-size: 20px;"),
              plotlyOutput("mini_map", height = "700px")
          )
      ),
      width = 3
    ),
    
    mainPanel(
      fluidRow(
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_total")), 
                      div(class = "kpi-sub",style = "font-weight: bold; font-size: 2rem;", "Vehículos totales (periodo)")
        )),
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_pesados")), 
                      div(class = "kpi-sub",style = "font-weight: bold; font-size: 2rem;", "Vehículos pesados (promedio)")
        )),
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_ligeros")), 
                      div(class = "kpi-sub",style = "font-weight: bold; font-size: 2rem;", "Vehículos ligeros (promedio)")
        )),
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_departamentos")), 
                      div(class = "kpi-sub", style = "font-weight: bold; font-size: 2rem;","Departamentos")
        )),
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_admin")), 
                      div(class = "kpi-sub",style = "font-weight: bold; font-size: 2rem;", "Administración más común")
        )),
        column(3, div(class = "card", style = "padding: 20px;",
                      div(class = "kpi", textOutput("kpi_peajes")), 
                      div(class = "kpi-sub",style = "font-weight: bold; font-size: 2rem;", "Número de Peajes")
        ))
      ),
      
      fluidRow(
        column(8,
          div(class = "card", style = "padding: 20px;",
              h6("Tráfico total por mes", style = "font-size: 20px;"),
              plotlyOutput("traffic_plot", height = "320px")
          ),
          fluidRow(
            column(6,
              div(class = "card", style = "padding: 20px; margin-top: 20px;",
                  h6("Composición: Vehículos Pesados", style = "font-size: 20px;"),
                  plotlyOutput("heavy_vehicles_plot", height = "250px")
              )
            ),
            column(6,
              div(class = "card", style = "padding: 20px; margin-top: 20px;",
                  h6("Composición: Vehículos Ligeros", style = "font-size: 20px;"),
                  plotlyOutput("light_vehicles_plot", height = "200px")
              )
            )
          )
        ),
      
        column(4, 
               div(class = "card", style = "padding: 20px; margin-bottom: 20px;",
                   h6("Top 10 peajes (VEH_TOTAL)", style = "font-size: 20px;"),
                   uiOutput("top_peajes", style = "font-size: 20px;")
               ),
               div(class = "card", style = "padding: 20px;",
                   h6("Distribución por tipo", style = "font-size: 20px;"),
                   plotlyOutput("distribution_plot", height = "200px")
               )
        )
      ),
      
      fluidRow(
        column(6, div(class = "card", style = "padding: 20px;",
                      h6("Mapa de peajes (por departamento)", style = "font-size: 20px;"),
                      plotlyOutput("map_plot", height = "360px")
        )),
        column(6, div(class = "card", style = "padding: 20px;",
                      h6("Boxplot: Vehículos por departamento", style = "font-size: 20px;"),
                      plotlyOutput("boxplot_plot", height = "360px")
        ))
      ),
      #### barras resumen:
      fluidRow(
        column(12, div(class = "card", style = "padding: 20px;",
                       h6(textOutput("composition_title")),
                       plotlyOutput("composition_by_peaje_plot", height = "500px")
        ))
      ),
      
      # Sección de Análisis Avanzado
      h4("Sección de Análisis Avanzado", style = "margin-top: 30px; margin-bottom: 20px; font-weight: 600;"),
      
      fluidRow(
        column(7, div(class = "card", style = "padding: 20px;",
                      h6("Clustering: Agrupación de peajes", style = "font-size: 20px;"),
                      plotlyOutput("cluster_plot", height = "400px"),
                      br(),
                      div(style = "display: flex; align-items: center; gap: 10px;",
                          tags$label("Número de clusters:", style = "margin: 0; font-weight: 500;"),
                          numericInput("n_clusters", NULL, value = 3, min = 2, max = 10, width = "80px"),
                          actionButton("run_cluster", "Ejecutar Clustering", class = "btn-primary", 
                                       style = "padding: 6px 20px; width: auto;")
                      )
        )),
        column(5, div(class = "card", style = "padding: 20px;",
                      h6("Correlación entre variables", style = "font-size: 20px;"),
                      plotlyOutput("correlation_plot", height = "400px")
        ))
      ),
      
      # Sección de Predicciones
      h4("Sección de Predicciones", style = "margin-top: 30px; margin-bottom: 20px; font-weight: 600;"),
      
      fluidRow(
        column(8, div(class = "card", style = "padding: 20px;",
                      h6("Predicción de VEH_TOTAL (Prophet / ARIMA)", style = "font-size: 20px;"),
                      plotlyOutput("prediction_plot", height = "400px"),
                      br(),
                      div(style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
                          div(style = "display: flex; align-items: center; gap: 10px;",
                              tags$label("Ajuste:", style = "margin: 0; font-weight: 500;"),
                              selectInput("pred_method", NULL, 
                                          choices = c("Prophet", "ARIMA", "Random Forest"), 
                                          selected = "Prophet", width = "150px")
                          ),
                          div(style = "display: flex; align-items: center; gap: 10px;",
                              tags$label("Horizonte (meses):", style = "margin: 0; font-weight: 500;"),
                              numericInput("horizon", NULL, value = 12, min = 1, max = 36, width = "80px")
                          ),
                          actionButton("run_prediction", "Generar predicción", class = "btn-primary",
                                       style = "padding: 6px 20px; width: auto;")
                      )
        )),
        column(4, div(class = "card", style = "padding: 20px;",
                      h6("Resumen del modelo", style = "font-size: 20px;"),
                      div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 10px;",
                          div(style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
                              tags$strong("RMSE:"),
                              tags$span(textOutput("model_rmse", inline = TRUE))
                          ),
                          div(style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
                              tags$strong("MAE:"),
                              tags$span(textOutput("model_mae", inline = TRUE))
                          ),
                          div(style = "display: flex; justify-content: space-between;",
                              tags$strong("Explained Var:"),
                              tags$span(textOutput("model_r2", inline = TRUE))
                          )
                      ),
                      hr(),
                      h6("Métricas de validación", style = "margin-top: 20px;"),
                      plotlyOutput("metrics_plot", height = "180px")
        ))
      ),
      
      # Sección de Descarga de Resultados
      h4("Descarga de Resultados", style = "margin-top: 30px; margin-bottom: 20px; font-weight: 600;"),
      
      fluidRow(
        column(12, div(class = "card", style = "padding: 20px;",
                       div(style = "display: flex; gap: 15px; flex-wrap: wrap;",
                           downloadButton("download_predictions", "Descargar Predicciones (CSV)", 
                                          class = "btn-secondary"),
                           downloadButton("download_clusters", "Descargar Clusters (CSV)", 
                                          class = "btn-secondary"),
                           downloadButton("download_report", "Descargar Reporte Completo (Excel)", 
                                          class = "btn-secondary")
                       )
        ))
      ),
      
      # PIE DE PÁGINA 
      hr(style = "margin-top: 40px; margin-bottom: 20px;"),
      
      div(style = "text-align: center; padding: 20px 0; color: #6b7280; font-size: 1.5rem;",
          p(style = "margin: 5px 0;",
            "Visualizacion de Tráfico Vehicular en Peajes del Perú"
          ),
          p(style = "margin: 5px 0;",
            "© 2025 | Desarrollado por Alex Cruz - UNA PUNO"
          ),
          p(style = "margin: 5px 0;",
            "Fuente de datos: Plataforma Nacional de Datos Abiertos - Ministerios de Transportes y Comunicaciones"
          )
      ),
      
      width = 9
    )
  )
)