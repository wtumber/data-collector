library(shiny)
library(shinyauthr)
library(mongolite)
library(ggplot2)
library(shinythemes)
library(dplyr)

# App credentials from external files
app_credentials <- read.csv("app_credentials.csv")
mongo_credentials <- read.csv("mongo_credentials.csv")

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
    user = app_credentials$user,
    password = sapply(app_credentials$password, sodium::password_store),
    name = c(app_credentials$name)
)

# mongoDB connection settings
options(mongodb = list(
    "host" = mongo_credentials$host,
    "username" = mongo_credentials$username,
    "password" = mongo_credentials$password
))
databaseName <- mongo_credentials$databaseName
collectionName <- mongo_credentials$collectionName

# mongoDB connection functions
saveData <- function(data) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                    "mongodb+srv://%s:%s@%s/%s",
                    options()$mongodb$username,
                    options()$mongodb$password,
                    options()$mongodb$host,
                    databaseName
                ),
                options = ssl_options(weak_cert_validation = TRUE))
    # Insert the data into the mongo collection as a data.frame
    data <- data.frame(data)
    db$insert(data)
}

loadData <- function() {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                    "mongodb+srv://%s:%s@%s/%s",
                    options()$mongodb$username,
                    options()$mongodb$password,
                    options()$mongodb$host,
                    databaseName
                ),
                options = ssl_options(weak_cert_validation = TRUE))
    # Read all the entries
    data <- db$find()
    data
}


#===== App UI ======
ui <- fluidPage(theme = shinytheme("superhero"),
    tags$head(
        tags$style(
            HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             }
             "
            )
        )
    ),


    # logout button
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    shinyauthr::loginUI(id = "login"),

    uiOutput("ui")

)

#===== App Server ======
server <- function(input, output, session) {

    # user login credentials
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        sodium_hashed = TRUE,
        log_out = reactive(logout_init())
    )

    # Logout to hide
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
    )

    # render UI
    output$ui <- renderUI({
        req(credentials()$user_auth)
        shiny::tabsetPanel(id = "tabSwitch",
           shiny::tabPanel("Add data",
                shiny::tagList(
                    shiny::fluidRow(
                        column(12, align = "center",
                            shiny::numericInput("ambientTemp",label =,"Ambient Temperature:",value = 17,width = "40%"),
                            shiny::sliderInput("ambientHumidity",label ="Ambient Humidity:",
                                               min = 30,max=100,step = 1,value = 60),
                            shiny::numericInput("GHTemp",label ="Greenhouse Temperature:",value = 17,width = "40%"),
                            shiny::sliderInput("GHHumidity",label ="Greenhouse Humidity:",
                                               min = 30,max=100,step = 1,value = 60),
                            shiny::br(),
                            shiny::actionButton("submitData","Submit Reading")
                        )
                    )
                )
            ),
            shiny::tabPanel("Visualisations",value = "vistab",
                column(12, align="center",
                    #shiny::selectInput("plot_type",label = "Select Plot",choices = c("Line"),selected="Line"),
                    shiny::selectInput("axisContent",label = "Show on axis:",choices = c("Humidity","Temp"),selected="Humidity",multiple = F),
                    #shiny::selectInput("timeSpan",label = "Timespan:",choices = c("1 day","3 days","1 Week"),selected="3 days",multiple = F),
                    shiny::plotOutput("visPlot")
                )

            )
        )
    })


    shiny::observeEvent(input$submitData, {
        record <- data.frame(
            readingTime=Sys.time(),
            ambientTemp=input$ambientTemp,
            ambientHumidity=input$ambientHumidity,
            GHTemp=input$GHTemp,
            GHHumidity=input$GHHumidity
        )

        saveData(data = record)
        showNotification(paste("submitted to database."), duration = 2)

    })

    r <- shiny::reactiveValues(mongoData = data.frame())

    shiny::observeEvent(input$tabSwitch, {
        req(input$tabSwitch == "vistab")

        r$mongoData <- data.frame(loadData())

    })

    output$visPlot <- shiny::renderPlot({
        req(input$axisContent)

        pl <- ggplot(r$mongoData)

        if("Humidity" %in% input$axisContent) {
            pl <- pl +
            geom_ribbon(aes(x =readingTime, ymin=ambientHumidity - 5,ymax=ambientHumidity + 5,color="Ambient"),alpha = 0.4,fill="brown2",color="red") +
            geom_ribbon(aes(x =readingTime,ymin=GHHumidity - 5,ymax=GHHumidity + 5,color="GH"),alpha = 0.4,fill="lightblue",color="blue") +
            geom_line(aes(x =readingTime, y=ambientHumidity,color="Ambient"),size=1,color="red") +
            geom_line(aes(x =readingTime,y=GHHumidity,color="GH"),size=1,color="blue") +
            geom_point(aes(x =readingTime, y=ambientHumidity,color="Ambient"),size=3,color="red") +
            geom_point(aes(x =readingTime,y=GHHumidity,color="GH"),size=3,color="blue") +
                theme_minimal()
                }

        if("Temp" %in% input$axisContent) {
            pl <- pl +
                geom_ribbon(aes(x =readingTime, ymin=ambientTemp - 1,ymax=ambientTemp + 1,color="Ambient"),alpha = 0.4,fill="brown2",color="red") +
                geom_ribbon(aes(x =readingTime,ymin=GHTemp - 1,ymax=GHTemp + 1,color="GH"),alpha = 0.4,fill="lightblue",color="blue") +
                geom_line(aes(x =readingTime, y=ambientTemp,color="Ambient"),size=1,color="red") +
                geom_line(aes(x =readingTime,y=GHTemp,color="GH"),size=1,color="blue") +
                geom_point(aes(x =readingTime, y=ambientTemp,color="Ambient"),size=3,color="red") +
                geom_point(aes(x =readingTime,y=GHTemp,color="GH"),size=3,color="blue") + theme_minimal()
            }

        pl
    })

}

shinyApp(ui = ui, server = server)
