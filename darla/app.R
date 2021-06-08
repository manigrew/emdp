library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)

DEBUG <- 0

#setwd("C:/Users/manish.grewal/git-emdp/emdp/darla")

# load data
events = readRDS("events.RDS")
dat <- readRDS("dat.RDS")

# events = list()
# events$critical <- dat %>% filter(queue_type == "critical") %>% select(eventTypeEnum) %>% unique %>% arrange(eventTypeEnum)
# events$secondary <- dat %>% filter(queue_type == "secondary") %>% select(eventTypeEnum) %>% unique %>% arrange(eventTypeEnum)
# events$tertiary <- dat %>% filter(queue_type == "tertiary") %>% select(eventTypeEnum) %>% unique %>% arrange(eventTypeEnum)

if(DEBUG) {
    dat <- dat[1:300000,]
    options(shiny.reactlog = TRUE)
}

min_date <- as.Date(dat$scheduledFor[1])
max_date <- as.Date(dat$scheduledFor[nrow(dat)])

day_colors = c("Mon" = "#460054", "Tue" = "#443a83", "Wed" = "#2f698e", "Thu" = "#1c928c", "Fri" = "#2eba76", "Sat" = "#8ed940", "Sun" = "#fae920")
    
plot_by_date <- function(x, y, desc) {
    what <- gsub("_", " ", y)
    title <- paste("By date:", what, desc)
    
    print(title)
    
    #idx_rel1 <- which(x$sched_date == "2021/04/18")
    #idx_rel2 <- which(x$sched_date == "2021/05/06")
    
    #day_colors = c("Mon" = "#460054", "Tue" = "#443a83", "Wed" = "#2f698e", "Thu" = "#1c928c", "Fri" = "#2eba76", "Sat" = "#8ed940", "Sun" = "#fae920")
    
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

plot_by_time <- function(x, y, desc) {
    what <- gsub("_", " ", y)
    title <- paste("By time:", what, desc)
    
    print(title)
    
    p <- x %>%
        ggplot(aes(x = sched_time_30)) +
        geom_col(aes_string(y = y, fill = "sched_day"), position = "dodge") +
        theme(axis.text.x = element_text(angle = 90, size = 11, vjust = 0.7)) +
        xlab("Scheduled Time") +
        ylab(what) +
        ggtitle(title) +
        scale_fill_manual(values = day_colors) +
        scale_x_discrete(breaks = x$sched_time_30[seq(1, nrow(x), 2)])
    
    print(p)
}

###########################
# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("Darla Jobs Analysis"),
    
    wellPanel(
        fluidRow(
            column(4,
                selectInput("queue_type", "Select Queue",
                    choices = c("critical", "secondary", "tertiary")
            )),
            column(4,
                selectInput("stat", "Select statistic",
                    choices = c("average_delay_minutes", "total_delay_minutes", "average_execution_minutes", "total_execution_minutes", "max_delay_minutes", "count")
                    #choices = names(summ_date)[-(1:3)]
            )),
        
            column(4,
                selectInput("job_types", 
                               "Filter by job type",
                               choices = c("a", "b"),
                               multiple = TRUE)
                #uiOutput( "job_types", )
            ),
        
            column(4,
                selectInput("exclude_days", "Exclude day of week",
                    choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                    multiple = TRUE
            )),
        
            column(4,
                airDatepickerInput("exclude_dates", "Exclude dates",
                    minDate = min_date,
                    maxDate = max_date,
                    multiple = TRUE,
                    dateFormat = 'yyyy/mm/dd'
            ))
        )
    ),
    
    tabsetPanel  (id ="tsp", type = "tabs",
        tabPanel(title = "Date",
                plotOutput("date_plot"),
            dataTableOutput("summ_filtered_date")
        ),
        
        tabPanel(title = "Time",
                plotOutput("time_plot"),
            dataTableOutput("summ_filtered_time")
        )
    
    )
)


#####################
# Define server logic
server <- function(input, output, session) {
    print("")
    print("##################################")
    
    job_types <- reactiveVal(NULL)
    
    observeEvent(input$job_types, {
        job_types(input$job_types)
    } )
    
    observeEvent(input$queue_type, {
        job_types(NULL)
        updateSelectInput(session, "job_types", 
                           "Filter by job type",
                           choices = events[[queue_type()]]
                          )
    })
    
    queue_type <- reactive({ input$queue_type })
    stat <- reactive({ input$stat })
    
    exclude_days <- reactive({
        exclude_days <- input$exclude_days
        if(length(exclude_days) > 6) exclude_days <- ""
        exclude_days
    })

    exclude_dates <- reactive({
        format(input$exclude_dates, "%Y/%m/%d")
    })
    
    title <- reactive({
        title <- paste0("\nQueue:", queue_type(),
            ifelse(
                   job_types() != "NULL",
                   paste("\nJob types:", toString(job_types())),
                   ""
            ),
            ifelse(
                toString(exclude_days()) != "",
                paste("\nExcluded days:", toString(exclude_days())),
                ""
            ),
            ifelse(
                exclude_dates() != "NULL",
                paste("\nExcluded dates:", toString(exclude_dates())),
                ""
            )
        )
        print(paste("Calculating title: ", title))
        title
    })
        
    by_date <- reactive({
        print("Calculating by_date")
        if(!is.null(job_types())) {
            print("\tfiltering by date by job_types")
            dat %>%
                filter( eventTypeEnum %in% job_types()) %>%
                group_by(queue_type, sched_date, sched_day)
        } else {
            dat %>%
                group_by(queue_type, sched_date, sched_day)
        }
        
    })
    
    by_time <- reactive({
        print("Calculating by_time")
        if(!is.null(job_types())) {
            print("\tfiltering by time by job_types")
            dat %>%
                filter( eventTypeEnum %in% job_types()) %>%
                group_by(queue_type, sched_time_30, sched_day)
        } else {
            dat %>%
                group_by(queue_type, sched_time_30, sched_day)
        }
        
    })
    
    
    summ_date <- reactive({
        print(paste("Summarizing #rows:", nrow(by_date())))
        by_date() %>% 
            summarise(
                    average_delay_minutes = mean(delay_min),
                    total_delay_minutes = sum(delay_min),
                    average_execution_minutes = mean(secs / 60),
                    total_execution_minutes = sum(secs / 60),
                    max_delay_minutes = max(delay_min),
                    count = n()
           )
    })
    
    summ_time <- reactive({
        print(paste("Summarizing #rows:", nrow(by_time())))
        by_time() %>% 
            summarise(
                    average_delay_minutes = mean(delay_min),
                    total_delay_minutes = sum(delay_min),
                    average_execution_minutes = mean(secs / 60),
                    total_execution_minutes = sum(secs / 60),
                    max_delay_minutes = max(delay_min),
                    count = n()
           )
    })
        
    
    summ_filtered_date <- reactive({
        print("Filtering")
        summ_date() %>%
            filter(queue_type == queue_type()) %>%
            filter(!sched_day %in% exclude_days()) %>%
            filter(!sched_date %in% exclude_dates()) #%>%
            
    })
    
    summ_filtered_time <- reactive({
        print("Filtering")
        summ_time() %>%
            filter(queue_type == queue_type()) %>%
            filter(!sched_day %in% exclude_days()) #%>%
            #filter(!sched_time_30 %in% exclude_dates()) #%>%
            
    })
    
    output$date_plot <- renderPlot({
        plot_by_date(summ_filtered_date(), stat(), title())
        #plot_by_date(summ_filtered(), stat(), "title()")
    })
    
    output$time_plot <- renderPlot({
        plot_by_time(summ_filtered_time(), stat(), title())
        #plot_by_date(summ_filtered(), stat(), "title()")
    })
    
    output$summ_filtered_date <- renderDataTable(summ_filtered_date())
    output$summ_filtered_time <- renderDataTable(summ_filtered_time())
    
    #output$mysession = renderPrint({c("<pre>", session, "</pre>")})
    #print(session)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
