# Load the diabetes dataset
library(shiny)
library(readr)

Final_clean <- read_csv("diabetes_clean.csv")

# Train a simple logistic regression model
model <- glm(Outcome ~ Pregnancies + Glucose + Blood_Pressure + Skin_Thickness + Insulin +
               BMI + Diabetes_Pedigree_Function + Age, 
             data = Final_clean, family = binomial)

# Define UI
ui <- fluidPage(
  
  # Set background color
  tags$style(HTML("body { background-color: #ADD8E6; }")),
  
  # Header
  titlePanel(div(h1("Diabetes Risk Prediction", style = "color: #00008B;"))),
  
  fluidRow(
    column(12,
           h4("Developer: Shemelis Aragaw Yesuf"),
           h5("Submitted for: Prof. Dominik Böhler"),
           h5("Deggendorf Institute of Technology"),
           h5("GPH Masters - Digital Health Course")
    )
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      h3("Adjust Risk Factors"),
      sliderInput("Pregnancies", "Pregnancies:", min = 0, max = 20, value = 1, step = 1),
      sliderInput("Glucose", "Glucose Level:", min = 50, max = 200, value = 100, step = 1),
      sliderInput("Blood_Pressure", "Blood Pressure (mm Hg):", min = 40, max = 120, value = 80, step = 1),
      sliderInput("Skin_Thickness", "Skin Thickness (mm):", min = 0, max = 100, value = 20, step = 1),
      sliderInput("Insulin", "Insulin Level (µU/mL):", min = 0, max = 600, value = 100, step = 1),
      sliderInput("BMI", "BMI:", min = 10, max = 60, value = 25, step = 0.1),
      sliderInput("Diabetes_Pedigree_Function", "Diabetes Pedigree Function:", 
                  min = 0.0, max = 2.5, value = 0.5, step = 0.01),
      sliderInput("Age", "Age (years):", min = 10, max = 100, value = 30, step = 1)
    ),
    
    mainPanel(
      h3("Diabetes Risk Prediction"),
      verbatimTextOutput("risk_output"),
      plotOutput("risk_plot"),
      h4("Recommendation:"),
      verbatimTextOutput("recommendation")  # Added missing output
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to compute diabetes risk
  diabetes_risk <- reactive({
    new_data <- data.frame(
      Pregnancies = input$Pregnancies,
      Glucose = input$Glucose,
      Blood_Pressure = input$Blood_Pressure,
      Skin_Thickness = input$Skin_Thickness,
      Insulin = input$Insulin,
      BMI = input$BMI,
      Diabetes_Pedigree_Function = input$Diabetes_Pedigree_Function,
      Age = input$Age
    )
    prob <- predict(model, new_data, type = "response")
    return(prob * 100)  # Convert probability to percentage
  })
  
  # Assign risk category based on probability
  risk_category <- reactive({
    risk <- diabetes_risk()
    if (risk < 30) {
      return("Low Risk")
    } else if (risk >= 30 & risk < 60) {
      return("Moderate Risk")
    } else if (risk >= 60 & risk < 85) {
      return("High Risk")
    } else {
      return("Very High Risk")
    }
  })
  
  # Assign color based on risk category
  risk_color <- reactive({
    if (diabetes_risk() < 30) {
      return("green")
    } else if (diabetes_risk() >= 30 & diabetes_risk() < 60) {
      return("yellow")
    } else if (diabetes_risk() >= 60 & diabetes_risk() < 85) {
      return("orange")
    } else {
      return("red")
    }
  })
  
  # Display risk as a percentage
  output$risk_output <- renderText({
    risk <- diabetes_risk()
    paste("Your estimated risk of diabetes is:", round(risk, 2), "% (", risk_category(), ")")
  })
  
  # Provide recommendations based on risk level
  output$recommendation <- renderText({
    if (diabetes_risk() < 30) {
      return("Maintain a healthy lifestyle with balanced nutrition and regular exercise.")
    } else if (diabetes_risk() >= 30 & diabetes_risk() < 60) {
      return("Monitor your diet and exercise regularly. Consider consulting a healthcare provider.")
    } else if (diabetes_risk() >= 60 & diabetes_risk() < 85) {
      return("You are at high risk. Consult a doctor for further assessment and preventive measures.")
    } else {
      return("Immediate medical attention is recommended. Please consult a healthcare professional.")
    }
  })
  
  # Plot risk with dynamic colors
  output$risk_plot <- renderPlot({
    risk <- diabetes_risk()
    barplot(
      height = risk,
      names.arg = "Risk of Diabetes",
      col = risk_color(),  # Fixed color rendering
      ylim = c(0, 100),
      main = "Predicted Risk of Diabetes",
      ylab = "Risk Percentage (%)"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


