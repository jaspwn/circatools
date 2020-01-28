library(data.table)
library(ggplot2)
library(shiny)

options(shiny.trace = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("environmental data"),

    fluidRow(
        column(width = 12, class = "well",
               plotOutput("plot1", height = 800,
                          dblclick = "plot1_dblclick",
                          brush = brushOpts(
                              id = "plot1_brush",
                              resetOnNew = TRUE),
                          click = "plot_click"))),
    fluidRow(
        column(width = 12, class = "well",
               column(width = 4,
                      h4("choose directory"),
                      actionButton("selectDir", "directory")),
               column(width = 4,
                      selectInput("selectMon", "choose env monitor file",
                                  choices = c())),
               column(width = 4,
                      h4("test out"),
                      verbatimTextOutput("info"))),
               )
    )





# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #reactive values

    rvs <- reactiveValues(x = NULL, y = NULL, directory = NULL)

    #select working directory
    observeEvent(input$selectDir, {

        rvs$directory <- dirname(file.choose())

    })

    observe({

        if (is.null(rvs$directory)) {
            rvs$directory <- character(0)
        }

        files <- list.files(rvs$directory)

        updateSelectInput(session, "selectMon", choices = files)

    })

    output$info <- renderPrint({

        # flybydt <<- nearPoints(res_dt[variable == "bpfiltered_slidingFitfreq"][n_fits >= input$range[1]][n_fits <= input$range[2]],
        #                        input$plot_click, xvar = "evtime", yvar = "median")
        rvs$directory
        rvs$x
        rvs$y
    })



    output$plot1 <- renderPlot({

        if (!is.null(rvs$x)) {
            rvs$x <- as.POSIXct(rvs$x, origin = "1970-01-01")
        }

        #monitorID <- basename(tools::file_path_sans_ext(filename))

        ## read in raw data file

        data <- fread(paste0(rvs$directory, "/", input$selectMon), stringsAsFactors = FALSE)

        ## convert time in data file to POSIX datetime

        data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

        ## grab data columns and rename

        data[, light := rowMeans(data[, 12:15])]
        data[, temp := rowMeans(data[, 17:20])/10]
        data[, humidity := rowMeans(data[, 22:25])]
        data[, error := as.numeric(V4)]
        data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

        ## melt data

        moltendata <- melt(data, id = c('time'), variable.name = 'env.cond', value.name = 'value')



        ggplot(data = moltendata, aes(x = time, y = value, colour = env.cond)) +
            geom_line(size = 1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$y) +
            scale_x_datetime(expand = c(0,0)) +
            facet_wrap( ~ env.cond, ncol = 1, scales = "free_y", labeller = label_wrap_gen(multi_line=FALSE)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title = element_blank(),
                  legend.position = "none")


    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
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
