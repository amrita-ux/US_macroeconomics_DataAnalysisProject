#Data Loading & Understanding

data(longley)
print("Dataset Structure:")
str(longley)
print("Summary Statistics:")
summary(longley)
print("First 6 Observations:")
head(longley)
print("Last 6 Observations:")
tail(longley)

print(paste("Number of Observations (Years):", nrow(longley)))
print(paste("Number of Variables:", ncol(longley)))

#Data Cleaning / Preprocessing

install.packages("tidyverse")
library(tidyverse)

print("Total Missing Values:")
sum(is.na(longley))

longley_long <- longley %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(longley_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Outlier Detection using Boxplots",
       y = "Value (in respective units)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

longley_scaled <- longley %>%
  mutate(
    Year_Centered = scale(Year), # Example of a derived field (centering 'Year')
    GNP_deflator_s = scale(GNP.deflator),
    GNP_s = scale(GNP),
    Unemployed_s = scale(Unemployed),
    Armed.Forces_s = scale(Armed.Forces),
    Population_s = scale(Population)
  )

print("Structure of Scaled Dataset:")
str(longley_scaled)

#Exploratory Data Analysis (EDA)

install.packages("ggplot2")
install.packages("corrplot")
library(ggplot2)
library(corrplot)

ggplot(longley, aes(x = Year, y = Employed)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "Trend of Total Employment (Target Variable)",
       x = "Year", y = "Employed (in thousands)") +
  theme_classic()

plot_bivar <- ggplot(longley, aes(x = GNP, y = Employed)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Employed vs. GNP",
       x = "Gross National Product (GNP)", y = "Employed") +
  theme_minimal()

print(plot_bivar)

M <- cor(longley)

corrplot(M, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Longley Variables\n(Checking for Multicollinearity)",
         mar=c(0,0,1,0))

#Statistical Analysis / Modeling

model_ols <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, 
                data = longley)

print("Ordinary Least Squares (OLS) Model Summary:")
summary(model_ols)

library(car)

print("Variance Inflation Factors (VIFs):")
vif(model_ols)

model_reduced <- lm(Employed ~ Year + Unemployed + Armed.Forces, data = longley)
print("Reduced Model Summary:")
summary(model_reduced)
print("Reduced Model VIFs:")
vif(model_reduced)

#Interactive Visualization (Mandatory)

install.packages("shiny")
library(shiny)
library(ggplot2)

data(longley)
model_reduced <- lm(Employed ~ Year + Unemployed + Armed.Forces, data = longley)

ui <- fluidPage(
  titlePanel("Longley Dataset: Employment Prediction (1947-1962)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Input Sliders"),
      
      sliderInput("unemployed_input",
                  "Unemployed (in thousands):",
                  min = min(longley$Unemployed),
                  max = max(longley$Unemployed),
                  value = median(longley$Unemployed),
                  step = 1),
      
      sliderInput("armedforces_input",
                  "Armed Forces (in thousands):",
                  min = min(longley$Armed.Forces),
                  max = max(longley$Armed.Forces),
                  value = median(longley$Armed.Forces),
                  step = 1),
      
      h4("Predicted Employment:"),
      textOutput("prediction")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Time-Series Trend", plotOutput("trendPlot")),
        tabPanel("Model Summary", verbatimTextOutput("modelSummary"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$prediction <- renderText({
    new_data <- data.frame(
      Year = 1962,
      Unemployed = input$unemployed_input,
      Armed.Forces = input$armedforces_input
    )
    
    predicted_employed <- predict(model_reduced, newdata = new_data)
    
    paste(round(predicted_employed, 3), "thousands")
  })
  
  output$trendPlot <- renderPlot({
    ggplot(longley, aes(x = Year, y = Employed)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "darkblue", size = 3) +
      # Add a point for the year 1962 (the last observation)
      geom_point(data = longley[longley$Year == 1962, ], 
                 aes(x = Year, y = Employed), 
                 color = "red", size = 5) +
      labs(title = "Total Employment Trend (1947-1962)",
           caption = "Red point is the last observation in the dataset (1962)") +
      theme_minimal()
  })
  
  output$modelSummary <- renderPrint({
    summary(model_reduced)
  })
}