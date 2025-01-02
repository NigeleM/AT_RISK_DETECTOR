# Clear the environment
rm(list = ls())
if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(glmnet)) install.packages("glmnet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(e1071)) install.packages("e1071") 
if (!require(rpart)) install.packages("rpart") 
if (!require(FSelectorRcpp)) install.packages("FSelectorRcpp")

library(caret)
library(glmnet)
library(dplyr)
library(e1071)
library(rpart)
library(FSelectorRcpp)
library(shiny)

# User interface to collect Patient Info
Health_UI <- fluidPage(
  titlePanel("At Risk Detector"),
  
  sidebarLayout(
    sidebarPanel(
      # Mutual Information Columns from analysis
      textInput("PatientName", "Patient Name:", ""),
      numericInput("GeneticRisk", "Genetic Risk:", value = NA, min = 0, max = 1, step = 0.01),
      numericInput("HealthcareCost", "Health Care Cost:", value = NA, min = 0, max = 500000, step = 0.01),
      numericInput("CholesterolLevel", "Cholesterol Level (mg/dL):", value = NA, min = 0, max = 400, step = 0.1),
      selectInput("Gender", "Gender:", choices = c("Female", "Male"), selected = NULL),
      numericInput("BMI", "BMI:", value = NA, min = 0, max = 100, step = 0.01),
      selectInput("SmokingStatus", "Smoking Status:", choices = c("Never", "Former", "Current"), selected = NULL),
      selectInput("ExerciseFrequency", "Exercise Frequency:", choices = c("Never", "Rarely", "Sometimes", "Often"), selected = NULL),
      numericInput("Systolic", "Systolic Blood Pressure (mmHg):", value = NA, min = 0, max = 300, step = 1),
      numericInput("Diastolic", "Diastolic Blood Pressure (mmHg):", value = NA, min = 0, max = 200, step = 1),
      selectInput("Diabetes", "Diabetes:", choices = c("No", "Yes"), selected = NULL),
      selectInput("MedicationAdherence", "Medication Adherence:", choices = c("Low", "Medium", "High"), selected = NULL),
      selectInput("AlcoholConsumption","Alcohol Consumption:",choices = c("Never","Occasionally","Regularly"),selected = NULL),
      # Buttons Run Prediction for running analysis and to predict patients health status
      actionButton("RunPrediction", "Predict Health Status"),
      # Clear the Prediction data
      actionButton("ClearTable","Clear Prediction"),
      br()
      
    ),
    
    mainPanel(
      tableOutput("patientDataTable"), # Display the table of patient data
      verbatimTextOutput("PatientsPredictied_Diagnosis")
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # Metrics 
  metrics <- reactiveVal(data.frame(
    PatientName = character(),
    GeneticRisk = numeric(),
    HealthcareCost = numeric(),
    Gender = character(),
    BMI = numeric(),
    SmokingStatus = character(),
    ExerciseFrequency = character(),
    Diabetes = character(),
    MedicationAdherence = character(),
    SystolicCategory = character(),
    DiastolicCategory = character(),
    CholesterolCategory = character(),
    BMICategory = character(),
    AlcoholConsumption = character()
  ))
  
  observeEvent(input$RunPrediction, {
    
    # Daphney's code modified to change input data to model
    # Categorize inputs to match the data set and patient data
    SystolicCategory <- ifelse(input$Systolic <= 120, "Normal",
                               ifelse(input$Systolic <= 129, "Elevated",
                                      ifelse(input$Systolic <= 139, "Hypertension_sg1", "Hypertension_sg2")))
    DiastolicCategory <- ifelse(input$Diastolic <= 80, "Normal",
                                ifelse(input$Diastolic <= 89, "Hypertension_sg1", "Hypertension_sg2"))
    
    CholesterolCategory <- ifelse(input$CholesterolLevel < 200, "Healthy",
                                  ifelse(input$CholesterolLevel <= 239, "At-risk", "High"))
    BMICategory <- ifelse(input$BMI < 18.5, "Underweight",
                          ifelse(input$BMI <= 24.9, "Normal weight",
                                 ifelse(input$BMI <= 29.9, "Overweight", "Obese")))
    
    Systolic <- factor(SystolicCategory, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
    Diastolic <- factor(DiastolicCategory, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
    
    # Add new data to display in a row with formatted data to reflect categories
    new_row <- data.frame(
      PatientName = input$PatientName,
      GeneticRisk = input$GeneticRisk,
      HealthcareCost = input$HealthcareCost,
      Gender = input$Gender,
      BMI = input$BMI,
      SmokingStatus = input$SmokingStatus,
      ExerciseFrequency = input$ExerciseFrequency,
      Diabetes = input$Diabetes,
      MedicationAdherence = input$MedicationAdherence,
      SystolicCategory = SystolicCategory,
      DiastolicCategory = DiastolicCategory,
      CholesterolCategory = CholesterolCategory,
      BMICategory = BMICategory,
      AlcoholConsumption = input$AlcoholConsumption
    )
    metrics(rbind(metrics(), new_row))
    
    # Preprocess health data
    health_data <- read.csv('health_struct.csv', header = TRUE)
    # Pre processing Code largely Daphney's Code
    preprocess_health_data <- function(health_data) {
      
      # Daphney's Code
      health_data$Gender <- as.factor(health_data$Gender)
      health_data$SmokingStatus <- as.factor(health_data$SmokingStatus)
      health_data$AlcoholConsumption <- as.factor(health_data$AlcoholConsumption)
      health_data$ExerciseFrequency <- as.factor(health_data$ExerciseFrequency)
      health_data$Diabetes <- as.factor(health_data$Diabetes)
      health_data$HeartDisease <- as.factor(health_data$HeartDisease)
      health_data$PhysicalActivityLevel <- as.factor(health_data$PhysicalActivityLevel)
      health_data$DietQuality <- as.factor(health_data$DietQuality)
      health_data$MedicationAdherence <- as.factor(health_data$MedicationAdherence)
      
      health_data$Systolic <- factor(health_data$Systolic, levels = c("Normal", "Elevated", "Hypertension_sg1", "Hypertension_sg2"))
      health_data$Diastolic <- factor(health_data$Diastolic, levels = c("Normal", "Hypertension_sg1", "Hypertension_sg2"))
      health_data$Cholesterol <- factor(health_data$Cholesterol, levels = c("Healthy", "At-risk", "High"))
      health_data$Bmi <- factor(health_data$Bmi, levels = c("Underweight", "Normal weight", "Overweight", "Obese"))
      
      health_data$AlcoholConsumption <- factor(health_data$AlcoholConsumption, levels = c("Never", "Occasionally", "Regularly"))
      health_data$ExerciseFrequency <- factor(health_data$ExerciseFrequency, levels = c("Never", "Rarely", "Sometimes", "Often"))
      health_data$PhysicalActivityLevel <- factor(health_data$PhysicalActivityLevel, levels = c("Low", "Medium", "High"))
      health_data$DietQuality <- factor(health_data$DietQuality, levels = c("Poor", "Average", "Good"))
      health_data$MedicationAdherence <- factor(health_data$MedicationAdherence, levels = c("Low", "Medium", "High"))
      
      health_data$Outcome <- factor(health_data$Outcome,
                                    levels = c("At Risk", "Critical", "Healthy"),
                                    labels = c("At_Risk", "Critical", "Healthy"))
      
      # Darren's Code 
      healthy <- health_data %>% filter(Outcome == "Healthy")
      at_risk <- health_data %>% filter(Outcome == "At_Risk")
      critical <- health_data %>% filter(Outcome == "Critical")
      
      # Set seed for reproducibility
      set.seed(6203)
      
      # Generate random samples to match the size of the smallest class (at_risk)
      samples_critical <- critical %>% sample_n(size = nrow(at_risk)/3)
      samples_healthy <- healthy %>% sample_n(size = nrow(at_risk)/3)
      
      # Bind into one balanced dataframe and shuffle rows
      health_data_balanced <- bind_rows(samples_healthy, at_risk, samples_critical)
      health_data_balanced <- health_data_balanced[sample(nrow(health_data_balanced)), ]
      
      return(health_data_balanced)}
    
    health_data_balanced <- preprocess_health_data(health_data)
    
    # Nigele's code
    # Train the models
    set.seed(123)
    
    # provide a partition data of the data with the same distribution as the dataset
    trainIndex <- createDataPartition(health_data_balanced$Outcome, p = .7, list = FALSE)
    train_data <- health_data_balanced[trainIndex, ]
    test_data <- health_data_balanced[-trainIndex, ]
    
    # information gain or mutual information select the top 10 features for us to use for tree
    weights <- information_gain(Outcome ~ ., data = train_data)
    top_10_features <- weights %>% arrange(desc(importance)) %>% slice(1:10) %>% pull(1)
    
    # Prepare data with selected features
    selected_train_data <- train_data[, c(top_10_features, "Outcome")]
    selected_test_data <- test_data[, c(top_10_features, "Outcome")]
    
    # Convert to matrices for logistic regression
    X_train <- model.matrix(Outcome ~ . - 1, data = selected_train_data)
    y_train <- selected_train_data$Outcome
    X_test <- model.matrix(Outcome ~ . - 1, data = selected_test_data)
    y_test <- selected_test_data$Outcome
    
    # Build Logistic Model 
    logAtRisk_Model <- cv.glmnet(X_train, y_train, family = "multinomial", type.measure = "class", alpha = 1)
    
    # Daphney's Code Label action plans 
    train_data$ActionPlan <- ifelse(train_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                                    ifelse(train_data$Outcome == "At Risk", "Preventive Care", "Urgent medical attention"))
    train_data$ActionPlan <- as.factor(train_data$ActionPlan)
    
    test_data$ActionPlan <- ifelse(test_data$Outcome == "Healthy", "Maintain healthy lifestyle",
                                   ifelse(test_data$Outcome == "At Risk", "Preventive Care", "Urgent medical attention"))
    test_data$ActionPlan <- as.factor(test_data$ActionPlan)
        
    
    
    # Build decision trees
    trees <- list()
    for (feature in top_10_features) {
      formula <- as.formula(paste("ActionPlan ~", feature))
      tree_model <- rpart(formula, data = train_data, method = "class")
      trees[[feature]] <- tree_model
    }
    
    # Build patient data
    patient_data <- data.frame(
      GeneticRisk = input$GeneticRisk,
      HealthcareCost = input$HealthcareCost,
      Cholesterol = factor(CholesterolCategory, levels = levels(health_data_balanced$Cholesterol)),
      Bmi = factor(BMICategory, levels = levels(health_data_balanced$Bmi)),
      SmokingStatus = factor(input$SmokingStatus, levels = levels(health_data_balanced$SmokingStatus)),
      ExerciseFrequency = factor(input$ExerciseFrequency, levels = levels(health_data_balanced$ExerciseFrequency)),
      Diabetes = factor(input$Diabetes, levels = levels(health_data_balanced$Diabetes)),
      MedicationAdherence = factor(input$MedicationAdherence, levels = levels(health_data_balanced$MedicationAdherence)),
      Systolic = Systolic,
      Diastolic = Diastolic
    )
    # print(patient_data)
    # Generate patient_data_matrix and align with X_train
    patient_data_matrix <- model.matrix(~ . - 1, data = patient_data[, top_10_features, drop = FALSE])
    # print(patient_data_matrix)
    # Add missing columns to match X_train
    missing_cols <- setdiff(colnames(X_train), colnames(patient_data_matrix))
    for (col in missing_cols) {
      patient_data_matrix <- cbind(patient_data_matrix, setNames(rep(0, nrow(patient_data_matrix)), col))
    }
    
    # Ensure the columns are in the same order as X_train
    patient_data_matrix <- patient_data_matrix[, colnames(X_train), drop = FALSE]
    
    # Debugging logs issues with model during testing
    
    # cat("Dimensions of X_train:", dim(X_train), "\n")
    # cat("Dimensions of patient_data_matrix:", dim(patient_data_matrix), "\n")
    # cat("Columns in patient_data_matrix:", colnames(patient_data_matrix), "\n")
    # cat("Logistic regression prediction:\n")
    patientStatus <- predict(logAtRisk_Model, newx = patient_data_matrix, s = "lambda.min", type = "class")
    # print(patientStatus)
    
    
    # Make predictions
    predictions <- sapply(top_10_features, function(feature) {
      predict(trees[[feature]], newdata = patient_data, type = "class")
    })
    
    
    output$PatientsPredictied_Diagnosis <- renderText({
      # format and fix issue with columns with numbers in it
      feature_details <- paste(
        paste0("  - ", gsub("\\.\\d+$", "", names(predictions)), ": Reccomendation : ", predictions, ": Patient Data : ", 
               sapply(names(predictions), function(x) as.character(patient_data[[gsub("\\.\\d+$", "", x)]]))),
        collapse = "\n"
      )
      
      # 
      paste0(
        "Patient ", input$PatientName, " is (", as.character(patientStatus), "):\n",
        feature_details
      )
    })
  })
  
  
  # Display patient data
  output$patientDataTable <- renderTable({
    metrics()
  })
  
  
  # Clear Predictions data
  observeEvent(input$ClearTable,{
    output$PatientsPredictied_Diagnosis <- renderText({
      
    })
    # Clear data from table
    output$patientDataTable <- renderTable({
      metrics()
    })
    
  })
}

# Build Shiny Application
shinyApp(ui = Health_UI, server = server)