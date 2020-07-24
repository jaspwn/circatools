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
                      sliderInput("bin", label = "bin width (s)",
                                  min = 10, max = 1800, value = 600, step = 10)),
               column(width = 8,
                      fluidRow(
                          column(width = 4,
                                 selectInput("entrainment", label = "choose entrainment",
                                             choices = c("all", unique(dt_curated[, xmv(entrainment)])))),
                          column(width = 4,
                                 selectInput("genotype", label = "choose genotype",
                                             choices = c("all", unique(dt_curated[, xmv(genotype)])))),
                          column(width = 4,
                                 selectInput("phase", label = "choose phase",
                                             choices = c("all", unique(dt_curated[, phase]))))
                      ),
                      fluidRow(
                          column(width = 3,
                                 selectInput("facetx", label = "facet formula",
                                             choices = c(unique(colnames(dt_curated[meta = TRUE]))),
                                             selected = "entrainment")),
                          column(width = 2,
                                 h1("~", align = "center")),
                          column(width = 3,
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
                                             selected = length(unique(dt_curated[, xmv(genotype)]))))

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

    rvs <- reactiveValues(x = NULL, y = NULL)



    # light plot output
    output$actPlot <- renderPlot({

        if(input$phase %in% unique(dt_curated[, phase])) {
            phases <- input$phase
        } else {
            phases <- unique(dt_curated[, phase])
        }

        if(input$genotype %in% unique(dt_curated[, xmv(genotype)])) {
            genotypes <- input$genotype
        } else {
            genotypes <- unique(dt_curated[, xmv(genotype)])
        }

        if(input$entrainment %in% unique(dt_curated[, xmv(entrainment)])) {
            entrainments <- input$entrainment
        } else {
            entrainments <- unique(dt_curated[, xmv(entrainment)])
        }

        ggetho(dt_curated[phase %in% phases][xmv(entrainment) %in% entrainments][xmv(genotype) %in% genotypes],
               summary_time_window = input$bin,
               aes(x = t, y = activity, colour = genotype)) +
            stat_ld_annotations(height=1, alpha=0.3, outline = NA) +
            stat_pop_etho() +
            scale_color_brewer(type = "qual", palette = "Set1", direction = -1) +
            scale_fill_brewer(type = "qual", palette = "Set1", direction = -1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$y) +
            facet_wrap(get(input$facetx) ~ get(input$facety), scales = "fixed", nrow = as.integer(input$nrows), ncol = as.integer(input$ncols)) +
            theme_minimal_hgrid(12) +
            theme(legend.position = "bottom")

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


}

# Run the application
shinyApp(ui = ui, server = server)
