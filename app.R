library(shiny)
library(shinyauthr)
library(mongolite)
library(ggplot2)

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
ui <- fluidPage(
    tags$head(
        tags$style(
            HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50% - 150px));
             }
             "
            )
        )
    ),


    # logout button
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    shinyauthr::loginUI(id = "login"),

    uiOutput("ui"),

    shiny::actionButton("browser","browser")


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

    # TEMP
    shiny::observeEvent(input$browser,{browser()})

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
                    shiny::selectInput("plot_type",label = "Select Plot",choices = c("Line"),selected="Line"),
                    shiny::plotOutput("visPlot"),
                    shiny::tableOutput("table")
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

        r$mongoData <- loadData()

    })

    output$visPlot <- shiny::renderPlot({
        ggplot(data.frame(r$mongoData)) +
            geom_point(aes(x =readingTime, y=ambientHumidity,color="Ambient")) +
            geom_point(aes(x =readingTime,y=GHHumidity,color="GH"))
    })

    output$table <- shiny::renderTable({
        req(nrow(r$mongoData) > 1)

        data.frame(r$mongoData)
    })

}

shinyApp(ui = ui, server = server)
