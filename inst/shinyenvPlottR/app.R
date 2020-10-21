library(data.table)
library(ggplot2)
library(shiny)
library(cowplot)

options(shiny.trace = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("environmental data"),

    fluidRow(
        column(width = 12, class = "well",
               column(width = 2,
                      h4("choose directory"),
                      actionButton("selectDir", "directory")),
               column(width = 2,
                      h4("set experimental range"),
                      actionButton("setRange", "set time")),
               column(width = 2,
                      h4("save plot"),
                      actionButton("savePlot", "save")),
               column(width = 3,
                      selectInput("selectMon", "choose env monitor file",
                                  choices = c())),
               column(width = 3,
                      h4("test out"),
                      verbatimTextOutput("info")))),

    fluidRow(
        column(width = 12,
               plotOutput("lightPlot", height = 250,
                          dblclick = "lightPlot_dblclick",
                          brush = brushOpts(
                              id = "lightPlot_brush",
                              resetOnNew = TRUE),
                          click = "lightPlot_click"))),
    fluidRow(
        column(width = 12,
               plotOutput("tempPlot", height = 250,
                          dblclick = "tempPlot_dblclick",
                          brush = brushOpts(
                              id = "tempPlot_brush",
                              resetOnNew = TRUE),
                          click = "tempPlot_click"))),
    fluidRow(
        column(width = 12,
               plotOutput("humdPlot", height = 250,
                          dblclick = "humdPlot_dblclick",
                          brush = brushOpts(
                              id = "humdPlot_brush",
                              resetOnNew = TRUE),
                          click = "humdPlot_click"))),
    fluidRow(
        column(width = 12,
               plotOutput("errPlot", height = 75,
                          dblclick = "errPlot_dblclick",
                          brush = brushOpts(
                              id = "errPlot_brush",
                              resetOnNew = TRUE),
                          click = "errPlot_click")))

)





# Define server logic required to draw a histogram
server <- function(input, output, session) {

    #reactive values

    rvs <- reactiveValues(x = NULL, ylight = NULL, ytemp = NULL, yhumd = NULL, yerr = NULL, directory = NULL)

    #select working directory
    observeEvent(input$selectDir, {

        rvs$directory <- dirname(file.choose())

    })

    observeEvent(input$setRange, {

        rvs$x <- c(unique(metadata[basename(env_monitor) == input$selectMon, start_datetime]),
                   unique(metadata[basename(env_monitor) == input$selectMon, stop_datetime]))

    })

    observe({

        ## to try and implement later in order to automatically locate env mon files within
        ## a project - shiny apps set wd to their install dir!
        # if (exists("metadata")) {
        #
        #     rvs$directory <- paste0("./data/", unique(dirname(metadata[, env_monitor])))
        #
        # }

        if (is.null(rvs$directory)) {
            rvs$directory <- character(0)
        }

        files <- list.files(rvs$directory, pattern = "*.txt")

        updateSelectInput(session, "selectMon", choices = files)

    })

    output$info <- renderPrint({

        if (exists("metadata")) {

            #print(unique(dirname(metadata[, env_monitor])))
            print(input$selectMon)
            print(rvs$directory)
            print(unique(metadata[basename(env_monitor) == input$selectMon, start_datetime]))
            print(unique(metadata[basename(env_monitor) == input$selectMon, stop_datetime]))

        }


    })


    # light plot output
    output$lightPlot <- renderPlot({

        if (!is.null(rvs$x)) {
            rvs$x <- as.POSIXct(rvs$x, origin = "1970-01-01")
        }

        ## read in raw data file

        filename <- paste0(rvs$directory, "/", input$selectMon)

        data <- fread(filename, stringsAsFactors = FALSE)

        ## convert time in data file to POSIX datetime

        data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

        ## grab data columns and rename

        data[, light := rowMeans(data[, 12:15])]
        data[, temp := rowMeans(data[, 17:20])/10]
        data[, humidity := rowMeans(data[, 22:25])]
        data[, error := as.numeric(V4)]
        data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

        monitorID <- basename(tools::file_path_sans_ext(filename))

        if (exists("metadata")) {

            expID <- unique(metadata[basename(env_monitor) == input$selectMon, exp_ID])
            title <- paste(monitorID, expID, rvs$x[1], "-", rvs$x[2], sep = " ")

        } else {

            title <- paste(monitorID, rvs$x[1], "-", rvs$x[2], sep = " ")

        }

        lPlot <<- ggplot(data = data, aes(x = time, y = light, colour = "light")) +
            geom_line(size = 1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$ylight) +
            scale_colour_manual(values = c("light" = "#ff7f00")) +
            scale_x_datetime(expand = c(0,0)) +
            ylab("light") +
            scale_y_continuous(labels = function(x) formatC(x, width = 5)) +
            ggtitle(title) +
            theme_minimal_grid(font_size = 12) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(hjust = 0, debug = FALSE),
                  legend.position = "none")

        return(lPlot)

    })

    # temperature plot output
    output$tempPlot <- renderPlot({

        if (!is.null(rvs$x)) {
            rvs$x <- as.POSIXct(rvs$x, origin = "1970-01-01")
        }

        ## read in raw data file

        filename <- paste0(rvs$directory, "/", input$selectMon)

        data <- fread(filename, stringsAsFactors = FALSE)

        ## convert time in data file to POSIX datetime

        data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

        ## grab data columns and rename

        data[, light := rowMeans(data[, 12:15])]
        data[, temp := rowMeans(data[, 17:20])/10]
        data[, humidity := rowMeans(data[, 22:25])]
        data[, error := as.numeric(V4)]
        data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

        tPlot <<- ggplot(data = data, aes(x = time, y = temp, colour = "temp")) +
            geom_line(size = 1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$ytemp) +
            scale_colour_manual(values = c("temp" = "#e41a1c")) +
            scale_x_datetime(expand = c(0,0)) +
            ylab("temperature") +
            scale_y_continuous(labels = function(x) formatC(x, width = 5)) +
            theme_minimal_grid(font_size = 12) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(hjust = 0, debug = FALSE),
                  legend.position = "none")

        return(tPlot)

    })

    #humdity plot output
    output$humdPlot <- renderPlot({

        if (!is.null(rvs$x)) {
            rvs$x <- as.POSIXct(rvs$x, origin = "1970-01-01")
        }

        ## read in raw data file

        filename <- paste0(rvs$directory, "/", input$selectMon)

        data <- fread(filename, stringsAsFactors = FALSE)

        ## convert time in data file to POSIX datetime

        data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

        ## grab data columns and rename

        data[, light := rowMeans(data[, 12:15])]
        data[, temp := rowMeans(data[, 17:20])/10]
        data[, humidity := rowMeans(data[, 22:25])]
        data[, error := as.numeric(V4)]
        data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

        hPlot <<- ggplot(data = data, aes(x = time, y = humidity, colour = "humidity")) +
            geom_line(size = 1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$yhumd) +
            scale_colour_manual(values = c("humidity" = "#377eb8")) +
            scale_x_datetime(expand = c(0,0)) +
            ylab("humidity") +
            scale_y_continuous(labels = function(x) formatC(x, width = 5)) +
            theme_minimal_grid(font_size = 12) +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(hjust = 0, debug = FALSE),
                  legend.position = "none")

        return(hPlot)


    })

    #error plot output
    output$errPlot <- renderPlot({

        if (!is.null(rvs$x)) {
            rvs$x <- as.POSIXct(rvs$x, origin = "1970-01-01")
        }

        ## read in raw data file

        filename <- paste0(rvs$directory, "/", input$selectMon)

        data <- fread(filename, stringsAsFactors = FALSE)

        ## convert time in data file to POSIX datetime

        data[, time :=  as.POSIXct(paste(V2,V3), format="%d %b %y %H:%M:%S")]

        ## grab data columns and rename

        data[, light := rowMeans(data[, 12:15])]
        data[, temp := rowMeans(data[, 17:20])/10]
        data[, humidity := rowMeans(data[, 22:25])]
        data[, error := as.numeric(V4)]
        data <- data[, c("time", "light", "temp", "humidity", "error"), with = FALSE]

        ## melt data

        #moltendata <- melt(data, id = c('time'), variable.name = 'env.cond', value.name = 'value')

        ePlot <<- ggplot(data = data, aes(x = time, y = error, colour = "error")) +
            geom_line(size = 1) +
            coord_cartesian(xlim = rvs$x, ylim = rvs$yerr) +
            scale_colour_manual(values = c("error" = "#984ea3")) +
            scale_x_datetime(expand = c(0,0)) +
            ylab("error") +
            scale_y_continuous(labels = function(x) formatC(x, width = 5)) +
            theme_minimal_grid(font_size = 12) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(hjust = 0, debug = FALSE),
                  legend.position = "none")

        return(ePlot)

    })

    observeEvent(input$savePlot, {

        outPlot <- plot_grid(lPlot, tPlot, hPlot, ePlot, ncol = 1)

        filename <- paste0(rvs$directory, "/", input$selectMon)
        monitorID <- basename(tools::file_path_sans_ext(filename))

        sTime <- strftime(rvs$x[1], format = "%Y%m%d_%H%M")
        eTime <- strftime(rvs$x[2], format = "%Y%m%d_%H%M")

        if (exists("metadata")) {

            expID <- unique(metadata[basename(env_monitor) == input$selectMon, exp_ID])

            outFile <- paste(monitorID, expID, sTime, eTime, sep = "-")

        } else {

            outFile <- paste(monitorID, sTime, eTime, sep = "-")

        }



        outFile <- paste0(rvs$directory, "/", outFile, ".png")
        ggsave2(filename = outFile, plot = outPlot, width = 16, height = 9)

    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$lightPlot_dblclick, {
        brush <- input$lightPlot_brush
        if (!is.null(brush)) {
            rvs$x <- c(brush$xmin, brush$xmax)
            rvs$ylight <- c(brush$ymin, brush$ymax)

        } else {
            rvs$x <- NULL
            rvs$ylight <- NULL
        }
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$tempPlot_dblclick, {
        brush <- input$tempPlot_brush
        if (!is.null(brush)) {
            rvs$x <- c(brush$xmin, brush$xmax)
            rvs$ytemp <- c(brush$ymin, brush$ymax)

        } else {
            rvs$x <- NULL
            rvs$ytemp <- NULL
        }
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$humdPlot_dblclick, {
        brush <- input$humdPlot_brush
        if (!is.null(brush)) {
            rvs$x <- c(brush$xmin, brush$xmax)
            rvs$yhumd <- c(brush$ymin, brush$ymax)

        } else {
            rvs$x <- NULL
            rvs$yhumd <- NULL
        }
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$errPlot_dblclick, {
        brush <- input$errPlot_brush
        if (!is.null(brush)) {
            rvs$x <- c(brush$xmin, brush$xmax)
            rvs$yerr <- c(brush$ymin, brush$ymax)

        } else {
            rvs$x <- NULL
            rvs$yerr <- NULL
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
