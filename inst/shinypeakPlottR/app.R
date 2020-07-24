library(shiny)

options(shiny.trace = FALSE)

ui <- fluidPage(

  titlePanel("select arrhythmic flies"),

  fluidRow(
    column(width = 8, class = "well",
           column(width = 4,
                  selectInput("genotype", label = "choose genotype",
                              choices = c(unique(metadata[, genotype])))),
           column(width = 4,
                  selectInput("entrainment", label = "choose entrainment",
                              choices = c(unique(metadata[, entrainment])))),
           column(width = 4,
                  selectInput("phase", label = "choose phase",
                              choices = c(unique(dt_curated[, phase])[grep("FR" ,unique(dt_curated[, phase]))])))),
    column(width = 4, class = "well",
           verbatimTextOutput("facet"),
           fluidRow(actionButton("returnArr", "return arrhythmic"),
                    actionButton("resetArr", "reset rhythm status")))),

  fluidRow(
    column(width = 12, class = "well",
           plotOutput("rhythmPlot", height = 700,
                      click = "fly_click")))

)




server <- function(input, output) {

  metadata <- dt_curated[, meta = TRUE]

  output$rhythmPlot <- renderPlot({
    shiny_peak_plottR(dt_curated, dt_peaks, gtype = input$genotype, ement = input$entrainment, FRphase = input$phase)
  })

  rvs <- reactiveValues(arr_flys = metadata[get(paste0(isolate(input$phase), "_rhythmic")) == FALSE, uid])

  observe({

    # add clicked
    rvs$arr_flys <- append(isolate(rvs$arr_flys), input$fly_click$panelvar1)
    # remove _all_ duplicates (toggle)
    # http://stackoverflow.com/a/13763299/3817004
    rvs$arr_flys <- isolate(rvs$arr_flys[!(duplicated(rvs$arr_flys) | duplicated(rvs$arr_flys, fromLast = TRUE))])

    #metadata[, paste0(isolate(input$phase), "_rhythmic") := ifelse(uid %in% rvs$arr_flys, FALSE, TRUE)]



  })

  output$facet <- renderPrint({
    if (is.null(rvs$arr_flys)) return()
    #paste0(input$phase, "_rhythmic")
    #metadata[paste0(input$phase, "_rhythmic") == FALSE, uid]
    rvs$arr_flys
  })

  observeEvent(input$returnArr, {

    metadata[, paste0(input$phase, "_rhythmic") := ifelse(uid %in% rvs$arr_flys, FALSE, TRUE)]

    rvs$arr_flys <- metadata[get(paste0(isolate(input$phase), "_rhythmic")) == FALSE, uid]

    ## re-link metadata to activity data
    dt_curated <- setmeta(dt_curated, metadata)
    dt_peaks <- setmeta(dt_peaks, metadata)

    ## replot
    output$rhythmPlot <- renderPlot({
      shiny_peak_plottR(dt_curated, dt_peaks, gtype = input$genotype, ement = input$entrainment, FRphase = input$phase)
    })
  })

  observeEvent(input$resetArr, {

    metadata[, paste0(input$phase, "_rhythmic") := TRUE]

    rvs$arr_flys <- metadata[get(paste0(isolate(input$phase), "_rhythmic")) == FALSE, uid]

    ## re-link metadata to activity data
    dt_curated <- setmeta(dt_curated, metadata)
    dt_peaks <- setmeta(dt_peaks, metadata)

    ## replot
    output$rhythmPlot <- renderPlot({
      shiny_peak_plottR(dt_curated, dt_peaks, gtype = input$genotype, ement = input$entrainment, FRphase = input$phase)
    })
  })

}

shinyApp(ui, server)
