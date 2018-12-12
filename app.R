### Libraries to be used throughout the program
install.packages("stringr")
library(stringr)
install.packages("syuzhet")
library(syuzhet)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)
install.packages("choroplethr")
library(choroplethr)
install.packages("choroplethrMaps")
library(choroplethrMaps)
install.packages("sqldf")
library(sqldf)
install.packages("wordcloud")
library(wordcloud)
install.packages("sp")
library(sp)

sb_data <- read.csv("http://www.users.miamioh.edu/rajkumtm/sentiments/StarbucksBlonde.csv")
head(sb_data)

pepsi_data <- read.csv("http://www.users.miamioh.edu/rajkumtm/sentiments/PepsiNext.csv")
head(pepsi_data)

sb_data$activity_text <- iconv(sb_data$activity_text, "latin1", "ASCII", sub="")
pepsi_data$activity_text <- iconv(pepsi_data$activity_text, "latin1", "ASCII", sub="")

### Calculates the sentiment scores using the Bing method for sb_data
sb_data$sb_bing <- get_sentiment(sb_data$activity_text, method="bing")
head(sb_data)

### Calculates the sentiment scores using the Bing method for pepsi_data
pepsi_data$pepsi_bing <- get_sentiment(pepsi_data$activity_text, method="bing")
head(pepsi_data)

### Displays the results of the sentiment analysis by state
library(dplyr)
sb_state_dply <- sb_data %>% 
  group_by(state) %>%
  summarise(mean(sb_bing),sd(sb_bing))
sb_state_dply

pepsi_state_dply <- pepsi_data %>%
  group_by(state) %>%
  summarise(mean(pepsi_bing),sd(pepsi_bing))
pepsi_state_dply

### Displays the results of the sentiment analysis by gender
sb_gender_dply <- sb_data %>%
  group_by(gender) %>%
  summarise(mean(sb_bing),sd(sb_bing))
sb_gender_dply

pepsi_gender_dply <- pepsi_data %>%
  group_by(gender) %>%
  summarise(mean(pepsi_bing),sd(pepsi_bing))
pepsi_gender_dply

### Displays the results of the sentiment analysis by age group
sb_age_dply <- mutate(sb_data, agegrp= ifelse (age <35, '1-Millenials',
                                                   ifelse (36 <= age & age <49, '2-Generation X',
                                                           ifelse (50<=age, '3-BabyBoomers', "Other"))))
head(sb_age_dply)

new_sb_age_dply <- sb_age_dply %>%
  group_by(agegrp) %>%
  summarise(mean(sb_bing),sd(sb_bing))
new_sb_age_dply

pepsi_age_dply <- mutate(pepsi_data, agegrp= ifelse (age <35, '1-Millenials',
                                                   ifelse (36 <= age & age <49, '2-Generation X',
                                                           ifelse (50<=age, '3-BabyBoomers', "Other"))))
head(pepsi_age_dply)

new_pepsi_age_dply <- pepsi_age_dply %>%
  group_by(agegrp) %>%
  summarise(mean(pepsi_bing),sd(pepsi_bing))
new_pepsi_age_dply

### Produces a bar graph of mean sentiment by age group
sb_gen_plot <- ggplot(new_sb_age_dply, aes(new_sb_age_dply$agegrp,
                                           new_sb_age_dply$`mean(sb_bing)`)) +
  geom_bar(stat="identity") +
  ggtitle("Mean Sentiments by Generation") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Mean Sentiment")
sb_gen_plot

pepsi_gen_plot <- ggplot(new_pepsi_age_dply, aes(new_pepsi_age_dply$agegrp,
                                           new_pepsi_age_dply$`mean(pepsi_bing)`)) +
  geom_bar(stat="identity") +
  ggtitle("Mean Sentiments by Generation") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Mean Sentiment")
pepsi_gen_plot

### Makes gen_plot interactive
sb_gen_plotly <- plot_ly(sb_gen_plot)
sb_gen_plotly

pepsi_gen_plotly <- plot_ly(pepsi_gen_plot)
pepsi_gen_plotly

### Produces a bar graph of mean sentiment by gender
sb_gender_plot <- ggplot(sb_gender_dply, aes(sb_gender_dply$gender,
                                       sb_gender_dply$`mean(sb_bing)`)) +
  geom_bar(stat="identity") +
  ggtitle("Mean Sentiments by Gender") +
  scale_x_discrete("Gender") +
  scale_y_continuous("Mean Sentiment")
sb_gender_plot

pepsi_gender_plot <- ggplot(pepsi_gender_dply, aes(pepsi_gender_dply$gender,
                                             pepsi_gender_dply$`mean(pepsi_bing)`)) +
  geom_bar(stat="identity") +
  ggtitle("Mean Sentiments by Gender") +
  scale_x_discrete("Gender") +
  scale_y_continuous("Mean Sentiment")
pepsi_gender_plot

### Makes gender_plot interactive
sb_gender_plotly <- plot_ly(sb_gender_plot)
sb_gender_plotly

pepsi_gender_plotly <- plot_ly(pepsi_gender_plot)
pepsi_gender_plotly

### Lists the conditions for the choropleth map
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('black')
)

### Produces a choropleth map of mean sentiment by State
sb_state_plot <- plot_ly(sb_state_dply, z = sb_state_dply$`mean(sb_bing)`,
                      #locations = state, 
                      type = 'choropleth',
                      locationmode = 'USA-states',  colors = 'Purples',
                      colorbar = list(title = "Mean Sentiment")
) %>%
  layout(title = 'Mean Sentiment by State', geo = g)
sb_state_plot

pepsi_state_plot <- plot_ly(pepsi_state_dply, z = pepsi_state_dply$`mean(pepsi_bing)`,
                         #locations = state, 
                         type = 'choropleth',
                         locationmode = 'USA-states',  colors = 'Purples',
                         colorbar = list(title = "Mean Sentiment")
) %>%
  layout(title = 'Mean Sentiment by State', geo = g)
pepsi_state_plot

### Produces a barplot of the emotions for the product
sb_emoData <- get_nrc_sentiment(sb_data$activity_text)
sb_emo_sort <- sort(colSums(prop.table(sb_emoData[,1:8])))
sb_emo_names <- names(sb_emo_sort)
sb_df <- data.frame(sb_emo_names, Percentage=sb_emo_sort,row.names=NULL)
sb_emo_plot <- plot_ly(sb_df, x=Percentage, y=sb_emo_names,
                       name = "Emotion Percentage",
                       type = "bar",
                       orientation = "h"
) %>% layout(
  title="Frequency of Emotions for Starbuck's Blonde Drink",
  xaxis=list(title="Percentage for Blonde Drink"),
  yaxis=list(title="Emotion"))
sb_emo_plot

pepsi_emoData <- get_nrc_sentiment(pepsi_data$activity_text)
pepsi_emo_sort <- sort(colSums(prop.table(pepsi_emoData[,1:8])))
pepsi_emo_names <- names(pepsi_emo_sort)
pepsi_df <- data.frame(pepsi_emo_names, Percentage=pepsi_emo_sort,row.names=NULL)
pepsi_emo_plot <- plot_ly(pepsi_df, x=Percentage, y=pepsi_emo_names,
                          name = "Emotion Percentage",
                          type = "bar",
                          orientation = "h"
) %>% layout(
  title="Frequency of Emotions for Pepsi's Pepsi Next Drink",
  xaxis=list(title="Percentage for Pepsi Next Drink"),
  yaxis=list(title="Emotion"))
pepsi_emo_plot

### Creates a word cloud of the sb_data
wordcloud(
  sb_data$activity_text, scale = c(5,0.5), max.words = 100, random.order = FALSE, rot.per =
    0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2")
)

### Creates a word cloud of the pepsi_data
wordcloud(
  pepsi_data$activity_text, scale = c(5,0.5), max.words = 100, random.order = FALSE, rot.per =
    0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2")
)

######################################################################################################
### Construction of the Shiny Dashboard
install.packages("shiny")
library(shiny)
install.packages("shinydashboard")
library(shinydashboard)

shiny_header <- dashboardHeader(title = 'Product Sentiment Analysis')
shiny_product <-  selectInput("product", "Product:", 
                        choices = c("Starbucks",
                                    "Pepsi"), selected = "Starbucks")
shiny_sidebar <- dashboardSidebar(fluidRow(shiny_product))
shiny_state_box <- box(title="Sentiment Analysis by State", 
                       width = 12, height = NULL, plotlyOutput("State"))
shiny_gender_box <- box(title="Sentiment Analysis by Gender", 
                       width = 12, height = NULL, plotlyOutput("Gender"))
shiny_age_box <- box(title="Sentiment Analysis by Age Group", 
                     width = 12, height = NULL, plotlyOutput("Age"))
shiny_emotions_box <- box(title="Emotions Analysis", 
                          width = 12, height = NULL, plotlyOutput("Emotions"))
shiny_wordcloud_box <- box(title="Wordcloud Analysis", 
                               width = 12, height = NULL, plotlyOutput("Wordcloud"))
shiny_recomend_box <- box(h3("Recommendations"), 
                          htmlOutput("Recommend", inline=FALSE))

### Create tabs to displayed within dashboard
tabOutput<-
  tabBox(width=12,
         title = "Select Ouput Display", 
         id = "tabset1", 
         tabPanel("Sentiment", shiny_state_box, shiny_gender_box, shiny_age_box),
         tabPanel("Emotions", shiny_emotions_box),
         tabPanel("Wordcloud", shiny_wordcloud_box),
         tabPanel("Recommendations", shiny_recomend_box)
  )

shiny_body <- dashboardBody(fluidRow(tabOutput))
ui <- dashboardPage(shiny_header,shiny_sidebar,shiny_body)

server <- function(input, output) {
  
  output$State <- renderPlotly({
    if(input$product=='Starbucks') {
      sb_state_plot
      }
    else {
      pepsi_state_plot
      }
  })
  
  output$Gender <- renderPlotly({
    if(input$product=='Starbucks') {
      sb_gender_plotly
    }
    else {
      pepsi_gender_plotly
    }
  })
  
  output$Age <- renderPlotly({
    if(input$product=='Starbucks') {
      sb_gen_plotly
    }
    else {
      pepsi_gen_plotly
    }
  })
  
  output$Emotions <- renderPlotly({
    if(input$product=='Starbucks') {
      sb_emo_plot
    }
    else {
      pepsi_emo_plot
    }
  })
  
  output$Wordcloud <- renderPlotly({
    if(input$product=='Starbucks') {
      wordcloud(
        sb_data$activity_text, scale = c(5,0.5), max.words = 100, random.order = FALSE, rot.per =
          0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2")
      )
    }
    else {
      wordcloud(
        pepsi_data$activity_text, scale = c(5,0.5), max.words = 100, random.order = FALSE, rot.per =
          0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2")
      )
    }
  })
  
  output$Recommendations <- renderPlotly({
    if(input$product=='Starbucks') {
      p("Based on the plots that were produced from the various analyses, in order to optimize 
        the appeal of the product, the target market would be female baby boomers who live in Montana.
        However, it is key to note that there is a smaller sample size for baby boomers in the data size,
        thus this might skew the results that were given. If baby boomers were taken out of the discussion,
        the three other generations (Those with no name = 0, Millenials, and Generation X) would all
        relatively have the same appeal for the product. It is also important to note that there are far
        more females in the data set which could possibly be the cause for the higher sentiment score, but
        more data would be needed from male respondants to make this conclusion. Other states like Idaho
        and South Dakota also have relatively higher scores then most other states. According to the word
        map, the most occurring words are 'coffee', 'starbucks', 'roast' and 'blonde'.")
    }
    else {
      p("No Comment")
    }
  })
}

# launch app
shinyApp(ui, server)
