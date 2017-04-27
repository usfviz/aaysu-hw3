library(shiny)
library(ggvis)
library(ggplot2)
library(lattice)
library(magrittr)
library(MASS)

fb <- read.csv('dataset_Facebook.csv', sep=';')
fb <- fb[ , !(names(fb) %in% c('Category'))]
names(fb) <- c('Total_Likes', 'Content_Type', 'Month_Posted', 'Weekday_Posted', 'Hour_Posted',
               'Paid', 'Lifetime_Post_Reach', 'Lifetime_Post_Impressions',
               'Lifetime_Engaged_Users', 'Lifetime_Post_Consumers',
               'Lifetime_Post_Consumption', 'Lifetime_Following_Post_Impressions',
               'Lifetime_Following_Post_Reach', 'Lifetime_Following_Engaged_Users',
               'Comments', 'Likes', 'Shares', 'Interactions')
cols <- names(fb)
quant <- cols[c(1, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)]
qual <- cols[c(2, 4, 5, 6)] # paid is giving me issues for some reason??

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- pageWithSidebar(
  headerPanel("Exploring Facebook Page Data"),
  mainPanel(
    tabsetPanel(
      tabPanel("Bubble Plot", plotOutput("plot1")),
      tabPanel("Scatterplot Matrix", plotOutput("plot2")),
      tabPanel("Parallel Coordinates", plotOutput("plot3"))
    )
  ),
  sidebarPanel(
    selectInput('color', 'Color PC by:', qual, selected = qual[1], multiple = FALSE,
                selectize = TRUE, width = NULL, size = NULL),
    #selectInput('size', 'Size scale:', quant, selected = quant[1], multiple = FALSE,
    #            selectize = TRUE, width = NULL, size = NULL),
    checkboxGroupInput('vars', 'Scatterplot Matrix Variables',
                       choices = sort(quant),
                       selected = c('Comments', 'Likes', 'Shares', 'Interactions')),
    checkboxGroupInput('types', 'Post Types',
                       choices = sort(unique(fb$Content_Type)),
                       selected = c('Link', 'Status')),
    sliderInput('month_s', 'From month:', value=1, min=1, max=12, sep = "", ticks=FALSE),
    sliderInput('month_e', 'To month:', value=12, min=1, max=12, sep = "", ticks=FALSE),
    sliderInput('wday_s', 'From weekday:', value=1, min=1, max=7, sep = "", ticks=FALSE),
    sliderInput('wday_e', 'To weekday:', value=7, min=1, max=7, sep = "", ticks=FALSE)
  )
)

unique(fb$`Content Type`)

server <- function(input, output) {
  
  selectedData <- reactive({
    df <- subset(fb, Month_Posted >= input$month_s)
    df <- subset(df, Month_Posted <= input$month_e)
    df <- subset(df, Month_Posted >= input$wday_s)
    df <- subset(df, Month_Posted <= input$wday_e)
    df <- subset(df, Content_Type %in% input$types)
    df
  })

  output$plot1 <- renderPlot({
    theme <- theme(
      plot.title = element_text(size = 24, hjust = 0.5),
      panel.background = element_rect(fill = 'white', colour = 'gray'),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(colour='gray'),
      panel.grid.major.y = element_line(colour='gray'),
      panel.border = element_blank()
    )
    p <- ggplot(data=selectedData(), aes(x=Lifetime_Post_Reach, y=Likes, colour=Content_Type))
    p <- p + geom_point(aes(fill=Content_Type, size=Lifetime_Engaged_Users))
    #p <- p + geom_point(aes(fill=input$color), alpha=0.3,
    #                    colour='black', shape = 21, stroke=0.2)
    # something with scale_size_continuous to make sizes bigger, but not working :(
    p <- p + scale_size(guide = "none") + theme
    #p <- p + scale_x_continuous('Life Expectancy', breaks=c(20, 40, 60, 80), limits=c(20,80))
    #p <- p + scale_y_continuous('Fertility Rate', breaks=seq(1:9), limits=c(1,9))
    #p <- p + labs(fill='Region')
    p
  })
  
  output$plot2 <- renderPlot({
    super.sym <- trellis.par.get("superpose.symbol")
    splom(~selectedData()[input$vars], groups = Content_Type, data = selectedData(),
          panel = panel.superpose)
  })

  output$plot3 <- renderPlot({
    parallelplot(~selectedData()[input$vars], selectedData(),
                 groups = selectedData()[,input$color],
                 horizontal.axis = FALSE, scales = list(x = list(rot = 0)))
  })
}

shinyApp(ui = ui, server = server)