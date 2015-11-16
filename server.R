## Set working directory
setwd("~/Documents/Coursera/Data Science/Capstone Project/Yelp_6_Shiny_1/")
## Load required packages
library(downloader); library(jsonlite); library(dplyr);
library(RTextTools); library(tidyr); library(shiny); library(ggplot2)

# Load data from the working directory
if(!exists("business")) business_100 <- readRDS("./business_100.rds")
if(!exists("review")) review_100 <- readRDS("./review_100.rds")

## Change review count
bus_ids <- business_100$business_id
n <- length(bus_ids)
for(i in 1:n) {
        id <- business_100[i, 1]           ## Identify business id in ith row
        rev_id <- filter(review_100, business_id == id)
        business_100$review_count[i] <- dim(rev_id)[1]}
business <- business_100 %>% filter(review_count >= 100)
bus_ids <- business$business_id
review <- review_100 %>% filter(business_id %in% bus_ids)

shinyServer(function(input, output){
        
        ## Create a business data having review count >= 100
        bus_100 <- reactive({
                count <- as.integer(input$count)
                if(count == 100) business})
        
        ## Create a business data having review count >= 200
        bus_100plus <- reactive({
                count <- as.integer(input$count)
                if(count >= 200){
                        sub_bus <- business %>% filter(review_count >= count)
                        n <- dim(sub_bus)[1]
                        bus_data <- data.frame()
                        for(i in 1:n) {
                                id <- sub_bus[i, 1]       ## Identify business id in ith row
                                bus_cat <- sub_bus[i, 2]  ## Identify business categories in ith row
                                sim_cat <- filter(sub_bus, categories %in% bus_cat)
                                k <- dim(sim_cat)[1]
                                if(k > 5) bus_data <- rbind(bus_data, sub_bus[i,])
                        }
                        bus_data}
        })
        
                ## Create a business ID list of selected data
        bus_row <- reactive({
                if(as.integer(input$count) == 100) bus_data <- bus_100()
                if(as.integer(input$count) >= 200) bus_data <- bus_100plus()
                revID <- bus_data$business_id
                
                ## Select the business ID
                if(input$busID == "Random") bus_id <- sample(revID, size = 1)
                if(input$busID == "First record") bus_id <- revID[1]
                len_revID <- length(revID)
                if(input$busID == "Last record") bus_id <- revID[len_revID]
                
                ## Generate selected business information
                bus_data %>% filter(business_id == bus_id)})
        
        ## Generate selected business ID
        id <- reactive({
                row_id <- bus_row()
                row_id$business_id})
        
        ## Generate name of selected business
        name <- reactive({
                row_id <- bus_row()
                row_id$name})
        
        ## Generate actual rating of selected business
        rating <- reactive({
                bus_id <- id()
                bus_rev <- review %>% filter(business_id == bus_id) %>% select(stars, text)
                round(mean(extract_numeric(bus_rev$stars)), 1)})
        
        # Build a prediction model to predict user rating for the business
        prediction <- reactive({
                bus_id <- id()
                bus_row <- bus_row()
                if(input$count == 100) bus_data <- bus_100()
                if(input$count >= 200) bus_data <- bus_100plus()
                bus_rev <- review %>% filter(business_id == bus_id) %>% select(stars, text)
                bus_rev$stars <- NA       ## Move NAs to all observations of variable 'stars'
                
                ## Identify the catagories of selected business
                bus_cat <- bus_row$categories
                
                # Build a prediction model to predict user rating for the business
                ## Filter businesses having similar catagories
                sim_bus <- filter(bus_data, categories %in% bus_cat)
                
                ## Create a sample ID list of 5 businesses of similar catagories
                ## excluding the selected business
                sim_bus <- sim_bus %>% filter(business_id != bus_id)
                id_list <- sample(sim_bus$business_id, 5)
                
                ## Filter review data relating to businesses in 'id_list'
                model_rev <- review %>% filter(business_id %in% id_list) %>% select(stars, text)
                
                ## Calculate the number of observations to be examined
                bus_n <- dim(bus_rev)[1]
                model_n <- dim(model_rev)[1]
                if(bus_n > model_n) n <- model_n
                if(bus_n < model_n) n <- bus_n
                
                ## Sample observations from 'model_rev' equal to observations in 'bus_rev'
                if(bus_n < model_n) model_rev <- sample_n(model_rev, n)
                if(bus_n > model_n) bus_rev <- sample_n(bus_rev, n)
                
                ## Create Document Term Matrix of model data
                Doc_Matrix <- create_matrix(model_rev$text, removeNumbers = T,
                                            removeSparseTerms = .998, stemWords = T)
                
                ## Create a container of model data
                container <- create_container(Doc_Matrix, model_rev$stars,
                                              trainSize = 1:(n-1), testSize = n, virgin = F)
                # Prediction Model
                MAXENT <- train_model(container, "MAXENT")
                
                ## Create Document Term Matrix of selected business
                Doc_Matrix <- create_matrix(bus_rev$text, removeNumbers = T,
                                            removeSparseTerms = .998, stemWords = T)
                
                ## Create a container of selected business
                container <- create_container(Doc_Matrix, bus_rev$stars,
                                              trainSize = 1, testSize = 2:n, virgin = T)
                ## Classify the selected business
                CLASSIFY_MAXENT <- classify_model(container, MAXENT)
                
                ## Calculate/predict average stars of business
                round(mean(extract_numeric(CLASSIFY_MAXENT$MAXENTROPY_LABEL)), 1)})
        
        ## Generate actual business information and predicted review rating
        output$bus_id <- renderPrint(cat(paste("Business ID:", id())))
        output$name <- renderPrint(cat(paste("Business Name:", name())))
        output$predicted_rating <- renderPrint(cat(paste("Predicted Rating:",
                                                         prediction(), "stars")))
        output$actual_rating <- renderPrint(cat(paste("Actual Rating:",
                                                      rating(), "stars")))
        
        ## Draw a barplot to show atual and predicted rating of the selected business
        output$RatingPlot <- renderPlot({
                review_rating <- data.frame(Type = c("Predicted", "Actual"),
                                            Rating = c(prediction(), rating()))
                row.names(review_rating) <- NULL
                ggplot(review_rating, aes(x = Type, y = Rating, fill = Type)) +
                        geom_bar(stat = "identity", col = "black", position = "dodge") +
                        geom_text(aes(label = Rating), vjust = 3, col = "white", size = 10) +
                        labs(x = "Type of rating", y = "Review rating (stars)")
        })
})