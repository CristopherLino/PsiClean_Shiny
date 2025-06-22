



library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(dplyr)
library(ThesiStats)
library(openxlsx)
library(DT)
library(PsyMetricTools)



ui <- navbarPage(
  title = "PsiClean",
  theme = shinytheme("sandstone"),
  
  tabPanel("Introducción",
           fluidPage(
             h2("📊 ¡Bienvenido a la app PsiClean!"),
             p("Esta aplicación te permite limpiar fácilmente los datos descargados de formularios de Google."),
             tags$ul(
               tags$li("📥 Subir y visualizar tu archivo Excel (.xlsx)"),
               tags$li("🛠️ Renombrar variables sociodemográficas"),
               tags$li("📋 Filtrar participantes, transformar edad, y eliminar columnas"),
               tags$li("🔤 Renombrar ítems de escalas"),
               tags$li("🔢 Codificar respuestas tipo Likert"),
               tags$li("⬇️ Descargar tu base de datos limpia")
             )
           )
  ),
  
  tabPanel("Subir archivo",
           sidebarLayout(
             sidebarPanel(
               fileInput("archivo", "Selecciona archivo Excel (.xlsx)", accept = ".xlsx")
             ),
             mainPanel(
               DT::DTOutput("vista_previa_dt")
             )
           )
  ),
  
  tabPanel("Datos sociodemográficos",
           tabsetPanel(
             tabPanel("Renombrar columnas",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("nuevos_nombres", "Nuevos nombres (separados por coma)",
                                    placeholder = "Edad, Sexo, Estado_civil, ..."),
                          numericInput("col_inicio", "Columna inicial", value = 1),
                          numericInput("col_fin", "Columna final", value = 12),
                          actionButton("renombrar_cols", "Aplicar renombrado")
                        ),
                        mainPanel(
                          div(style = "overflow-x: auto; white-space: nowrap;",
                              tableOutput("preview_renombrado"),
                              mainPanel(
                                DT::DTOutput("preview_renombrado_dt")
                              )
                          )
                        )
                      )
             ),
             tabPanel("Filtrar y transformar",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("columna_acepto_ui"),
                          uiOutput("columna_edad_ui"),
                          numericInput("edad_min", "Edad mínima", value = 18, min = 0),
                          numericInput("edad_max", "Edad máxima", value = 40, min = 0),
                          checkboxGroupInput("columnas_a_remover", "Columnas a eliminar", choices = NULL),
                          actionButton("filtrar_transformar", "Aplicar limpieza")
                        ),
                        mainPanel(
                          DT::DTOutput("preview_filtrado_dt")
                        )
                      )
             )
           )
  ),
  
  tabPanel("Ítems",
           tabsetPanel(
             tabPanel("Renombrar ítems",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("n_escalas", "Número de escalas", choices = 1:3),
                          actionButton("aplicar_escalas", "Aplicar número de escalas"),
                          uiOutput("opciones_escalas"),
                          actionButton("renombrar_items", "Renombrar ítems")
                        ),
                        mainPanel(
                          DT::DTOutput("tabla_items_renombrados")
                        )
                      )
             ),
             tabPanel("Codificar respuestas",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("alternativa1", 
                                    "Alternativas escala 1", 
                                    placeholder = "\"Nunca\" = 1, \"Muy pocas veces\" = 2, \"Algunas veces\" = 3, \"Muchas veces\" = 4, \"Siempre\" = 5"),
                          conditionalPanel(
                            condition = "input.n_escalas >= 2",
                            textInput("alternativa2", 
                                      "Alternativas escala 2",
                                      placeholder = "\"Nunca\" = 1, \"Muy pocas veces\" = 2, \"Algunas veces\" = 3, \"Muchas veces\" = 4, \"Siempre\" = 5")
                          ),
                          conditionalPanel(
                            condition = "input.n_escalas == 3",
                            textInput("alternativa3", 
                                      "Alternativas escala 3",
                                      placeholder = "\"Nunca\" = 1, \"Muy pocas veces\" = 2, \"Algunas veces\" = 3, \"Muchas veces\" = 4, \"Siempre\" = 5")
                          ),
                          actionButton("codificar_respuestas", "Aplicar codificación"),
                          shinyWidgets::downloadBttn(
                            outputId = "descargar_excel",
                            label = "Descargar data limpia",
                            style = "jelly",
                            color = "success"
                          )
                        ),
                        mainPanel(
                          div(style = "overflow-x: auto; white-space: nowrap;",
                              DT::DTOutput("preview_codificado_dt"))
                        )
                      )
             )
           )
  ),
  
  tabPanel("Análisis de participantes",
           sidebarLayout(
             sidebarPanel(
               uiOutput("seleccion_vars_participantes"),
               actionButton("generar_analisis_participantes", "Generar tabla APA"),
               shinyWidgets::downloadBttn(
                 outputId = "descargar_tabla_apa",
                 label = "Descargar tabla APA",
                 style = "jelly",
                 color = "success"
               )
             ),
             mainPanel(
               DT::DTOutput("tabla_participantes_APA")
             )
           )
  )
  
)





server <- function(input, output, session) {
  # Reactivo base
  datos <- reactiveVal()
  datos_socio <- reactiveVal()  # solo variables sociodemográficas
  items_seleccionados <- reactiveVal() 
  datos_filtrados <- reactiveVal()
  items_codificados <- reactiveVal() 
  cols_socio <- reactiveVal()
  
  # Paso 1: subir archivo
  observeEvent(input$archivo, {
    req(input$archivo)
    df <- read_excel(input$archivo$datapath)
    datos(df)
  })
  
  output$vista_previa_dt <- DT::renderDT({
    req(datos())
    DT::datatable(head(datos(), 20), options = list(scrollX = TRUE))
  })
  
  # Paso 2: renombrar columnas sociodemográficas y aislarlas
  observeEvent(input$renombrar_cols, {
    req(datos())
    nuevos <- strsplit(input$nuevos_nombres, ",")[[1]] |> trimws()
    cols <- input$col_inicio:input$col_fin
    df <- ThesiStats::rename_columns(datos(), new_names = nuevos, columns = cols)
    datos(df)
    datos_socio(df[, cols])  # guardar solo las columnas sociodemográficas
    cols_socio(names(df[, cols]))
    updateCheckboxGroupInput(session, "columnas_a_remover", choices = names(df[, cols]))
  })
  
  output$preview_renombrado_dt <- DT::renderDT({
    req(datos_socio())
    DT::datatable(head(datos_socio(), 20), options = list(scrollX = TRUE))
  })
  
  # Paso 3: UI dinámica para columna de consentimiento y su valor
  output$columna_acepto_ui <- renderUI({
    req(datos_socio())
    tagList(
      selectInput("columna_acepto", "Columna de consentimiento", choices = names(datos_socio())),
      uiOutput("valor_acepto_ui")
    )
  })
  
  output$valor_acepto_ui <- renderUI({
    req(input$columna_acepto, datos_socio())
    valores <- unique(datos_socio()[[input$columna_acepto]]) |> na.omit()
    selectInput("valor_acepto", "Valor que indica consentimiento", choices = valores)
  })
  
  # UI para columna de edad
  output$columna_edad_ui <- renderUI({
    req(datos_socio())
    selectInput("columna_edad", "Columna de edad", choices = names(datos_socio()))
  })
  
  #Lógica de filtrado
  observeEvent(input$filtrar_transformar, {
    req(datos(), input$columna_acepto, input$valor_acepto, input$columna_edad,
        input$edad_min, input$edad_max)
    
    df <- datos()
    
    df_filtrado <- df |>
      filter(
        !!sym(input$columna_acepto) == input$valor_acepto,
        !!sym(input$columna_edad) >= input$edad_min,
        !!sym(input$columna_edad) <= input$edad_max
      ) |>
      mutate(!!sym(input$columna_edad) := as.integer(readr::parse_number(!!sym(input$columna_edad)))) |>
      select(-all_of(input$columnas_a_remover))
    
    # Guardar resultados
    datos_filtrados(df_filtrado)
    cols_validas <- intersect(cols_socio(), names(df_filtrado))
    datos_socio(df_filtrado[, cols_validas])
  })
  
  # Vista previa del filtrado (solo sociodemográficos)
  output$preview_filtrado_dt <- DT::renderDT({
    req(datos_socio())
    DT::datatable(head(datos_socio(), 20), options = list(scrollX = TRUE))
  })
  
  
  # Paso 4: Mostrar campos según número de escalas
  observeEvent(input$aplicar_escalas, {
    output$opciones_escalas <- renderUI({
      campos <- list(
        textInput("inicio1", "Texto del primer ítem (de todo el bloque)"),
        textInput("final", "Texto del último ítem (de todo el bloque)"),
        textInput("prefijo1", "Prefijo escala 1"),
        numericInput("n_items1", "N° ítems escala 1", value = 10)
      )
      
      if (input$n_escalas >= 2) {
        campos <- append(campos, list(
          textInput("prefijo2", "Prefijo escala 2"),
          numericInput("n_items2", "N° ítems escala 2", value = 10)
        ))
      }
      
      if (input$n_escalas == 3) {
        campos <- append(campos, list(
          textInput("prefijo3", "Prefijo escala 3"),
          numericInput("n_items3", "N° ítems escala 3", value = 10)
        ))
      }
      
      # IMPORTANTE: envolver en tagList para evitar errores de renderizado
      do.call(tagList, campos)
    })
  })
  
  # Paso 5: Mostrar campos de codificación por escala
  observeEvent(input$aplicar_escalas, {
    output$alternativas_inputs <- renderUI({
      inputs <- list(
        textAreaInput("alternativa1", "Diccionario alternativas escala 1 (formato: Nunca = 1, ...)",
                      placeholder = "Nunca = 1, Muy pocas veces = 2, ...")
      )
      
      if (input$n_escalas >= 2) {
        inputs <- c(inputs,
                    textAreaInput("alternativa2", "Diccionario alternativas escala 2",
                                  placeholder = "Nunca = 1, Muy pocas veces = 2, ..."))
      }
      
      if (input$n_escalas == 3) {
        inputs <- c(inputs,
                    textAreaInput("alternativa3", "Diccionario alternativas escala 3",
                                  placeholder = "Nunca = 1, Muy pocas veces = 2, ..."))
      }
      
      do.call(tagList, inputs)
    })
  })
  
  # Paso 4: Renombrar ítems
  observeEvent(input$renombrar_items, {
    req(datos_filtrados(), input$n_escalas, input$inicio1, input$final)
    
    if (input$n_escalas == 1) {
      req(input$prefijo1, input$n_items1)
      df <- rename_items(
        datos_filtrados(),
        prefix1 = input$prefijo1,
        inici = input$inicio1,
        final = input$final,
        n_items1 = input$n_items1
      )
      items <- df |> select(starts_with(input$prefijo1))
    }
    
    if (input$n_escalas == 2) {
      req(input$prefijo1, input$prefijo2, input$n_items1, input$n_items2)
      df <- rename_items2(
        datos_filtrados(),
        prefix1 = input$prefijo1,
        prefix2 = input$prefijo2,
        inici = input$inicio1,
        final = input$final,
        n_items1 = input$n_items1,
        n_items2 = input$n_items2
      )
      items <- df |> select(starts_with(input$prefijo1), starts_with(input$prefijo2))
    }
    
    if (input$n_escalas == 3) {
      req(input$prefijo1, input$prefijo2, input$prefijo3,
          input$n_items1, input$n_items2, input$n_items3)
      df <- rename_items3(
        datos_filtrados(),
        prefix1 = input$prefijo1,
        prefix2 = input$prefijo2,
        prefix3 = input$prefijo3,
        inici = input$inicio1,
        final = input$final,
        n_items1 = input$n_items1,
        n_items2 = input$n_items2,
        n_items3 = input$n_items3
      )
      items <- df |> select(starts_with(input$prefijo1), starts_with(input$prefijo2), starts_with(input$prefijo3))
    }
    
    datos_filtrados(df)
    items_seleccionados(items)
  })
  
  output$tabla_items_renombrados <- DT::renderDataTable({
    req(items_seleccionados())
    DT::datatable(items_seleccionados(), options = list(scrollX = TRUE))
  })
  
  # Paso 5: Codificación
  observeEvent(input$codificar_respuestas, {
    req(datos_filtrados(), input$n_escalas)
    df <- datos_filtrados()
    
    if (input$n_escalas == 1) {
      alt1 <- eval(parse(text = paste0("c(", input$alternativa1, ")")))
      df <- df |> mutate(across(starts_with(input$prefijo1), ~ unname(alt1[.])))
      items <- df |> select(starts_with(input$prefijo1))
    }
    
    if (input$n_escalas == 2) {
      alt1 <- eval(parse(text = paste0("c(", input$alternativa1, ")")))
      alt2 <- eval(parse(text = paste0("c(", input$alternativa2, ")")))
      df <- df |> 
        mutate(across(starts_with(input$prefijo1), ~ unname(alt1[.]))) |>
        mutate(across(starts_with(input$prefijo2), ~ unname(alt2[.])))
      items <- df |> select(starts_with(input$prefijo1), starts_with(input$prefijo2))
    }
    
    if (input$n_escalas == 3) {
      alt1 <- eval(parse(text = paste0("c(", input$alternativa1, ")")))
      alt2 <- eval(parse(text = paste0("c(", input$alternativa2, ")")))
      alt3 <- eval(parse(text = paste0("c(", input$alternativa3, ")")))
      df <- df |> 
        mutate(across(starts_with(input$prefijo1), ~ unname(alt1[.]))) |>
        mutate(across(starts_with(input$prefijo2), ~ unname(alt2[.]))) |>
        mutate(across(starts_with(input$prefijo3), ~ unname(alt3[.])))
      items <- df |> select(starts_with(input$prefijo1), starts_with(input$prefijo2), starts_with(input$prefijo3))
    }
    
    datos_filtrados(df)
    items_codificados(items)
  })
  
  # Vista previa
  output$preview_codificado_dt <- DT::renderDT({
    req(items_codificados())
    DT::datatable(head(items_codificados(), 10), options = list(scrollX = TRUE))
  })
  
  
  #Paso 6
  output$seleccion_vars_participantes <- renderUI({
    req(datos_filtrados())
    nombres_vars <- names(datos_filtrados())
    selectizeInput("vars_participantes",
                   "Selecciona variables sociodemográficas:",
                   choices = nombres_vars,
                   multiple = TRUE)
  })
  
  # Lógica para generar tabla APA
  observeEvent(input$generar_analisis_participantes, {
    req(datos_filtrados(), input$vars_participantes)
    
    tabla <- PsyMetricTools::generate_summary(
      data = datos_socio(),
      variables = input$vars_participantes
    )
    tabla_APA <- PsyMetricTools::generate_table(tabla)
    
    output$tabla_participantes_APA <- DT::renderDataTable({
      DT::datatable(tabla_APA, options = list(scrollX = TRUE))
    })
  })
  
  #Descargar tabla participantes
  output$descargar_tabla_apa <- downloadHandler(
    filename = function() {
      "tabla_participantes.xlsx"
    },
    content = function(file) {
      tabla <- PsyMetricTools::generate_summary(data = datos_socio(), 
                                                variables = names(datos_socio()))
      tabla_formato <- PsyMetricTools::generate_table(tabla)
      openxlsx::write.xlsx(tabla_formato, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  
  
  # Paso 6: descargar
  output$descargar_excel <- downloadHandler(
    filename = function() {
      "datos_limpios.xlsx"  # o usa paste0("datos_limpios_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      df_final <- cbind(datos_socio(), items_codificados())
      openxlsx::write.xlsx(df_final, file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

