library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")

dat <- readRDS("dat.RDS")
by_date <- dat %>%
    group_by(queue_type, sched_date, sched_day)

summ <- by_date %>%
    summarise(
        average_delay_minutes = mean(delay_min),
        total_delay_minutes = sum(delay_min),
        average_execution_minutes = mean(secs / 60),
        total_execution_minutes = sum(secs / 60),
        max_delay_minutes = max(delay_min),
        count = n()
    )

min_date <- as.Date(dat$scheduledFor[1])
max_date <- as.Date(dat$scheduledFor[nrow(dat)])

plot_by_date <- function(x, y, desc) {
        
    what <- gsub("_", " ", y)
    title <- paste("By date:", what, "(", desc, ")")
    
    print(title)
    
    #idx_rel1 <- which(x$sched_date == "2021/04/18")
    #idx_rel2 <- which(x$sched_date == "2021/05/06")
    
    day_colors = c("Mon" = "#460054", "Tue" = "#443a83", "Wed" = "#2f698e", "Thu" = "#1c928c", "Fri" = "#2eba76", "Sat" = "#8ed940", "Sun" = "#fae920")
    
    p <- x %>%
        ggplot(aes(x = sched_date)) +
        geom_col(aes_string(y = y, fill = "sched_day")) +
        theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) +
        #scale_fill_manual(values = cbp1) +
        xlab("Date") +
        ylab(what) +
        ggtitle(title) +
        #geom_vline(aes(xintercept = c(idx_rel1), color="Rel1"), size=1, linetype="dashed") +
        #geom_vline(aes(xintercept = c(idx_rel2), color="Rel2"), size=1, linetype="dashed") +
        #scale_color_manual(name = "Release", values = c(Rel1 = "blue", Rel2 = "black")) +
        scale_fill_manual(values = day_colors) +
        scale_x_discrete(breaks = x$sched_date[seq(1, nrow(x), 2)])
    
    print(p)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Darla Jobs Analysis"),
    
    # Sidebar with a slider input for number of bins
    selectInput(
        "queue_type",
        "Select Queue",
        choices = c("critical", "secondary", "tertiary")
    ),
    selectInput(
        "stat",
        "Select statistic",
        choices = c("average_delay_minutes", "total_delay_minutes", "average_execution_minutes", "total_execution_minutes", "max_delay_minutes", "count")
    ),
    
    checkboxGroupInput(
        "exclude_days",
        "Exclude day of week",
        choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
        inline = TRUE
    ),
    
    airDatepickerInput(
        "exclude_dates",
        "Exclude dates",
        minDate = min_date,
        maxDate = max_date,
        multiple = TRUE,
        dateFormat = 'yyyy/mm/dd'
       ),
    
    # Show a plot
    plotOutput("date_plot", height = "600"),
    
    dataTableOutput("summ_by_date")
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    print("")
    print("##################################")
    
    
    queue_type_in <- reactive({ input$queue_type })
    stat <- reactive({ input$stat })
    
    exclude_days <- reactive({
        exclude_days <- input$exclude_days
        if(length(input$exclude_days) > 6) exclude_days <- ""
        exclude_days
    })

    exclude_dates <- reactive({
        format(input$exclude_dates, "%Y/%m/%d")
    })
    
    title <- reactive({
        paste0(
            queue_type_in(),
            ifelse(
                toString(exclude_days()) != "",
                paste(", excluded day(s)", toString(exclude_days())),
                ""
            ),
            ifelse(
                exclude_dates() != "NULL",
                paste(", excluded date(s):", toString(exclude_dates())),
                ""
            )
        )
    })
        
    summ_by_date <- reactive({
        summ %>%
            filter(queue_type == queue_type_in()) %>%
            filter(!sched_day %in% exclude_days()) %>%
            filter(!sched_date %in% exclude_dates())
    })
    
    output$date_plot <- renderPlot({
        print(paste("Inputs:", queue_type_in(), stat()))
        print(paste("Inputs: exclude days", toString(exclude_days())))
        print(paste("Inputs: exclude dates", toString(exclude_dates())))
 
        plot_by_date(summ_by_date(), stat(), title())
    })
    
    output$mysession = renderPrint({c("<pre>", session, "</pre>")})
    #print(session)
    
    output$summ_by_date <- renderDataTable(summ_by_date())
}

# Run the application 
shinyApp(ui = ui, server = server)
