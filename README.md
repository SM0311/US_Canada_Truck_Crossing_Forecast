# US-Canada Border Truck Crossing Forecasting Project
Repository URL:
(https://surajm0311.shinyapps.io/FileDeployementFiles/)


# Project Overview
This repository contains all the resources for a time series forecasting project designed to predict truck crossings at the US-Canada border. This project is carried out in collaboration with the Eastern Border Transportation Coalition (EBTC), a non-profit group of representatives from Michigan's state and provincial transportation agencies. The goal is to ensure adequate facilities and personnel to accommodate the continued growth in trade between the two countries.

The key objective of the forecasting approach is to develop a straightforward and efficient model that produces defensible forecasts based on historical trends.

# Key Features:
1. Time Series Forecasting: Implements ARIMA and ETS models to predict future truck crossings.
2. Exploratory Data Analysis (EDA): Includes various visualizations and summaries of historical data.
3. Power BI Dashboards: Provides interactive Power BI visualizations for further data exploration.
4. Shiny Web Application: An interactive Shiny app where users can explore forecasts and trends in the data.
5. Presentation File: A PowerPoint presentation summarizing the project approach, findings, and recommendations.

# Contents of the Repository
1. Literature_Review.pdf: Comprehensive literature review outlining theoretical background and related works.
2. Presentation.pptx: A presentation file summarizing the project for the Eastern Border Transportation Coalition (EBTC), including the methodology and forecast results.
3. PowerBI_Visualizations.pbix: Power BI file that provides dynamic visualizations of the truck crossing data.
4. Border_Crossing_Entry_Data.csv: The historical dataset used for forecasting.
5. App Files: Files needed to run the Shiny web application:
6. app.R: Main script for the Shiny app.
7. forecasting_code.R: Code for data preprocessing and time series model development.

# How to Run the Application Locally
To run the Shiny app locally, follow these steps:

Clone this repository to your machine.
Install the necessary R packages:

install.packages(c("shiny", "DT", "lubridate", "forecast", "bslib", "shinyjs"))

Launch the Shiny app by running the app.R file:

shiny::runApp('app.R')

![image](https://github.com/user-attachments/assets/41bfb4a1-3316-4117-8fa7-013121196dc3)


# Power BI Visualizations
The PowerBI_Visualizations.pbix file contains visualizations that provide a deeper look at the truck crossing data and the resulting forecasts. To access the Power BI dashboards:


# Data
The dataset, Border_Crossing_Entry_Data.csv, contains historical records of truck crossings at the US-Canada-Mexico border. 

# Literature Review
The Literature_Review files document provides background information.

# Presentation
The Presentation.pptx file summarizes the key aspects of the project for the Eastern Border Transportation Coalition (EBTC). It outlines the methodology, explains the model selection process, and presents the forecasting results and recommendations.

# Conclusion
This project aims to develop an efficient and defensible forecasting model for predicting truck crossings at the US-Canada border. Through the use of ARIMA, ETS models, and Power BI dashboards, we provide actionable insights for planning and optimizing resources to meet the growing trade demands between the two nations.


