library(shiny)

shinyUI(fluidPage(
        titlePanel("Predict review's rating"),
        h4(span("* Please wait for a while for first prediction. The app has to perform a lot\n",
                "of operations each time you change the parameters.", style = "color:darkred")),
        h4(" - The predicted review rating may be a bit different each time for the same\n",
        "business. Reason for difference is the review data selected for model building\n",
        "through sampling each time."),
        fluidRow(
                column(4,
                       radioButtons("count",
                                    label = h3("Choose the number of reviews to examine"),
                                    choices = list("100 or more reviews" = 100,
                                                   "200 or more reviews" = 200,
                                                   "300 or more reviews" = 300,
                                                   "400 or more reviews" = 400,
                                                   "500 or more reviews" = 500),
                                    selected = 200)),
                column(3,
                       radioButtons("busID",
                                    label = h3("Select Business ID"),
                                    choices = list("Random", "First record", "Last record"),
                                    selected = "Random")),
                br(),
                column(5,
                       span(h4(textOutput("bus_id")), style = "color:black"),
                       span(h4(textOutput("name")), style = "color:black"),
                       span(h4(textOutput("predicted_rating")), style = "color:mediumaquamarine"),
                       span(h4(textOutput("actual_rating"))), style = "color:orangered"),
                mainPanel(
                      plotOutput("RatingPlot")  
                )
        )))