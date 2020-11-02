library(data.table)
library(ggplot2)
library(shiny)
library(cowplot)
library(ggetho)


options(shiny.trace = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("activity data"),

    fluidRow(
        column(width = 12, class = "well",
               column(width = 4,
                      fluidRow(
                      sliderInput("bin", label = "bin width (s)",
                                  min = 60, max = 3600, value = 1800, step = 60)),
                      fluidRow(
                          column(width = 4,
                                 actionButton("saveDir", "directory")),
                          column(width = 4,
                                 actionButton("savePlot", "save"))
                      )),
               column(width = 8,
                      fluidRow(
                          column(width = 4,
                                 selectizeInput("entrainment", label = "choose entrainment",
                                             choices = unique(dt_curated[, xmv(entrainment)]),
                                             selected = unique(dt_curated[, xmv(entrainment)]),
                                             multiple = TRUE)),
                          column(width = 4,
                                 selectInput("genotype", label = "choose genotype",
                                             choices = unique(dt_curated[, xmv(genotype)]),
                                             selected = unique(dt_curated[, xmv(genotype)]),
                                             multiple = TRUE)),
                          column(width = 4,
                                 selectInput("phase", label = "choose phase",
                                             choices = unique(dt_curated[, phase]),
                                             selected = unique(dt_curated[, phase]),
                                             multiple = TRUE))
                      ),
                      fluidRow(
                          column(width = 2,
                                 selectInput("facetx", label = "facet formula",
                                             choices = c("none", unique(colnames(dt_curated[meta = TRUE]))),
                                             selected = "entrainment")),
                          column(width = 2,
                                 h1("~", align = "center")),
                          column(width = 2,
                                 selectInput("facety", label = "",
                                             choices = c(unique(colnames(dt_curated[meta = TRUE]))),
                                             selected = "genotype")),
                          column(width = 2,
                                 selectInput("nrows", label = "rows",
                                             choices = seq(1,16,1),
                                             selected = length(unique(dt_curated[, xmv(entrainment)])))),
                          column(width = 2,
                                 selectInput("ncols", label = "cols",
                                             choices = seq(1,16,1),
                                             selected = length(unique(dt_curated[, xmv(genotype)])))),
                          column(width = 2,
                                 selectInput("colour", label = "colour",
                                             choices = c(unique(colnames(dt_curated[meta = TRUE]))),
                                             selected = "genotype"))

                      )
               )
        )
    ),

    fluidRow(
        column(width = 12,
               plotOutput("actPlot", height = 700,
                          dblclick = "actPlot_dblclick",
                          brush = brushOpts(
                              id = "actPlot_brush",
                              resetOnNew = TRUE),
                          click = "actPlot_click")))


)




# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #reactive values

    rvs <- reactiveValues(x = NULL, y = NULL, directory = NULL)

    # light plot output
    output$actPlot <- renderPlot({

        if (input$facetx == "none") {

            shinyPlot <<- ggetho(dt_curated[phase %in% input$phase &
                                                xmv(entrainment) %in% input$entrainment &
                                                xmv(genotype) %in% input$genotype],
                                 summary_time_window = input$bin,
                                 aes_string(x = "t", y = "activity", colour = input$colour)) +
                stat_ld_annotations(height=1, alpha=0.1, outline = NA) +
                stat_pop_etho() +
                scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
                scale_fill_brewer(type = "qual", palette = "Set1", direction = -1) +
                coord_cartesian(xlim = rvs$x, ylim = rvs$y) +
                facet_wrap(. ~ get(input$facety), scales = "fixed", nrow = as.integer(input$nrows), ncol = as.integer(input$ncols)) +
                theme_minimal_hgrid(12) +
                theme(legend.position = "bottom")

        } else {

            shinyPlot <<- ggetho(dt_curated[phase %in% input$phase &
                                                xmv(entrainment) %in% input$entrainment &
                                                xmv(genotype) %in% input$genotype],
                                 summary_time_window = input$bin,
                                 aes_string(x = "t", y = "activity", colour = input$colour)) +
                stat_ld_annotations(height=1, alpha=0.1, outline = NA) +
                stat_pop_etho() +
                scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
                scale_fill_brewer(type = "qual", palette = "Set1", direction = -1) +
                coord_cartesian(xlim = rvs$x, ylim = rvs$y) +
                facet_wrap(get(input$facetx) ~ get(input$facety), scales = "fixed", nrow = as.integer(input$nrows), ncol = as.integer(input$ncols)) +
                theme_minimal_hgrid(12) +
                theme(legend.position = "bottom")

        }

        return(shinyPlot)

    })

    #

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$actPlot_dblclick, {
        brush <- input$actPlot_brush
        if (!is.null(brush)) {
            rvs$x <- c(brush$xmin, brush$xmax)
            rvs$y <- c(brush$ymin, brush$ymax)

        } else {
            rvs$x <- NULL
            rvs$y <- NULL
        }
    })

    observeEvent(input$saveDir, {

        rvs$directory <- dirname(file.choose())

    })

    observeEvent(input$savePlot, {

        if (is.null(rvs$directory)) {

            return(message("Set save directory first!"))

        }

        if (!dir.exists(paste0(rvs$directory, "/figures/actPlottR/"))) {

            dir.create(paste0(rvs$directory, "/figures/actPlottR/"), recursive = TRUE)

        }

        outFile <- paste0(rvs$directory, "/figures/actPlottR/", format(Sys.time(), "%Y%d%m_%H%M%S"), "_", input$facetx, "_X_", input$facety, "_activity.png")

        ggsave2(filename = outFile, plot = shinyPlot, width = 16, height = 9)

        message(paste0("Plot saved as ", outFile))

    })


}

# Run the application
shinyApp(ui = ui, server = server)
