library(ggplot2)
library(dplyr)
library(fpp3)
library(tidyverse)
library(seasonal)

# Filtered data for Trucks across US-Canada Border
df=read.csv("Border_Crossing_Entry_Data.csv")
print(str(df))


df_clean <- df |> distinct()
# Remove duplicates based on specific columns
df_clean <- df |>
  distinct(Port.Name, State, Port.Code, Border, Date, Measure, Value, Latitude, Longitude, Point, .keep_all = TRUE)

sum(duplicated(df_clean))



US_Canada <- df_clean |>
  filter(df_clean$Border=="US-Canada Border" & df_clean$Measure=="Trucks")

df1<-US_Canada |>
  select('State','Measure','Value')

US_Canada

#Aggregated data for conversion to total number of truck for each state irrespective of the port of entry
agg_data=aggregate(df1$Value, by=list(df1$State,df1$Measure), FUN=sum)

agg_data$State=agg_data$`Group.1`
agg_data$Measure=agg_data$`Group.2`
agg_data$Value=agg_data$x

agg_data<- agg_data|>
  select('State','Measure','Value')
agg_data

options(scipen = 999)
library(scales)

#Bar plot to visualize the aggregated data for Trucks per state
ggplot(data=agg_data, aes(x=State, y=Value)) + geom_col() +
  labs(title='Frequency of Trucks from different States',
       x='State',
       y='Number of Trucks') +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

#As evident from the aggregation table and the histogram, Michigan seems to have highest movement of trucks across the border and hence we would wish to study the trend and accordingly forecast the traffic for next 6 months.

#Filtering Michigan from the Truck dataframe we extracted at the beginning for US-Canada border
US_Canada_MI <- US_Canada |>
  filter(State=="Michigan")

US_Canada_MI

#Box plot view of Trucks values at all ports of entry located in Michigan
boxplot(Value~Port.Name, data=US_Canada_MI,main="Box plot of the trucks across border ports located in Michigan") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

#Aggregating data to ignore Port of entries and combine all truck values in Michigan as per monthly frequency
agg_data=aggregate(US_Canada_MI$Value, by=list(US_Canada_MI$Port.Name,US_Canada_MI$Measure), FUN=sum)
print(agg_data)

MI_TimeSeries <- aggregate(US_Canada_MI$Value, by=list(US_Canada_MI$Date,US_Canada_MI$Measure), FUN=sum)
print(MI_TimeSeries)

#We get total of 341 observations representing 341 months of data of number of trucks coming from Canada and entering US through Michigan border. This becomes are base dataset that we will convert to tsibble and plot the time series using Date Index.

MI_TimeSeries<- MI_TimeSeries |>
  rename(Date=Group.1, Measure=Group.2, Value=x)

str(MI_TimeSeries)

#Converting date index into year-month format using mutate for easier index reference at monthly frequency
MI_TimeSeries<- MI_TimeSeries %>%
  mutate(Date = yearmonth(Date))

str(MI_TimeSeries)

#Converted data set to time sibble object and used autoplot for plotting timeseries data
MI_TimeSeries_Plot <- as_tsibble(MI_TimeSeries, index="Date")

options(repr.plot.width = 15, repr.plot.height = 8)

MI_TimeSeries_Plot |>
  autoplot(Value) +
  labs(title="Monthly data of trucks crossing US-CAN border from Michigan") +
  scale_x_yearmonth(date_breaks="1 year", date_labels="%Y") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#OBSERVATIONS FROM THE INITIAL TIME SERIES PLOT

#1. Data has rising trends within periods 1996 Jan till Dec 2008 and from Dec 2009 till Dec 2019.
#2. There is an evident seasonality after every 6 months as we can see a dip after every 6 months in trucks data across all the years
#3. There are two significant and sharp dips at year 2009 and 2020 representing Economic Depression and COVID-19 events that disrupted movement of transportation across US and Canada due to economic and legal restrictions.
#4. There is high variability as in trucks data from 1996 -2009 as compared to period 2010-2024. This is evident from the height of spikes that is significantly higher in 1996-2009. Hence this shows unstable variance in data that we would need to correct in order to get a better visualization of trends and seasonality.
#5. There is no stationarity in data as there is trend with seasonality

MI_TimeSeries_Qtr<- MI_TimeSeries %>%
  mutate(Date = yearquarter(Date))

MI_TimeSeries_Qtr <- MI_TimeSeries_Qtr[order(MI_TimeSeries_Qtr$Date),]

MI_TimeSeries_Qtr <- MI_TimeSeries_Qtr %>% group_by(Date) %>%
  summarise(Total_Value=sum(Value),
            .groups = 'drop')


str(MI_TimeSeries_Qtr)

#Converted quarterly data set to time sibble object and used autoplot for plotting timeseries data
MI_TimeSeries_Qtr <- as_tsibble(MI_TimeSeries_Qtr, index="Date")

options(repr.plot.width = 15, repr.plot.height = 8)

ggplot(data=MI_TimeSeries_Qtr, aes(x=Date, y=Total_Value)) + geom_line() +
  labs(title='Quarterly Frequency of Trucks from different States ') +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  scale_x_yearquarter(date_breaks = "2 years",
                      date_minor_breaks = "1 year") +
  scale_y_continuous(labels = scales::comma)

#ACF PLOTS AND DECOMPOSITION TECHNIQUES

ACF_plot<- MI_TimeSeries_Plot |>
  select('Date', 'Measure','Value')

ACF_plot <- ACF_plot[order(ACF_plot$Date),]

ACF_plot

options(repr.plot.width = 12, repr.plot.height = 8)

ACF_plot |>
  ACF(ACF_plot$Value, lag_max=48) |>
  autoplot() +
  labs(title="ACF plot of monthly trucks data", scale=c(1,10)) +
  theme_gray(base_size = 18)

#The ACF plot above shows clear positive autocorrelation which shows that the future value of number of trucks crossing US-CAN border is positively correlated with past values.

#Code to display ACF and PACF plot against the original time series
ACF_plot |> gg_tsdisplay(Value, plot_type = "partial") +
  labs(title="GG Residual Plots for the Time Series", scale=c(1,10))

#Data in the ACF plot without difference shows some signs of trends with less stationarity.
#Another insight we can see is that trucks number have dropped fairly after 2010 with highest figure of 250,000 around year 2005 while highest figure of 220,000 around year 2008

#Plotting ACT plot for first difference of Measure variable

ACF_plot$diff <- difference(ACF_plot$Value)
ACF_plot |>
  ACF(ACF_plot$diff, lag_max=48) |>
  autoplot() +
  labs(title="ACF plot of first difference of trucks data", scale=c(1,10)) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ACF_plot |> gg_tsdisplay(diff, plot_type = "partial") +
  labs(title="GG display view of ACF and PACF Plots", width=800, height=500)

#After first difference the data looks quite stationary and it does not seem that we would need 2nd difference. But to avoid any assumptions we will try to take second difference and see if there is any change to the stationarity of data.

install.packages("urca")
library(urca)

# KPSS test for first difference
ACF_plot |> features(diff, unitroot_kpss)

ACF_plot |>
  ACF(difference(ACF_plot$diff), lag_max=48) |>
  autoplot() +
  labs(title="ACF plot of second difference of trucks data", scale=c(1,10)) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ACF_plot |> gg_tsdisplay(difference(diff), plot_type = "partial") +
  labs(title="ACF and PACF plots for secon differenced data")

#KPSS test for the second difference
ACF_plot |> features(difference(diff), unitroot_kpss)

#From the visual review and hypothesis test of stationarity, it is fairly to make below assumptions :
  
#1. ACF plot of original data shows positive autocorrelation which indicates that present values of trucks crossing border have a correlation with past values.

#2. There seems to be a slight trend in the line plot of original data and some seasonal variations as well. We will use decomposition plots for check for trend and seasonality in order to check what time series models will be appropriate to fit the data.

#3. For ARIMA models, since stationarity is one of the requirements, we see that ggplot of first difference visually and through kpss test shows good amount of stationarity. On the other hand the second difference is very much similar to first one , hence we conclude that first different would be sufficient to build ARIMA and SARIMA models.

MI_TimeSeries_Plot |>
  autoplot(log(Value)) +
  labs(title="Log transformation of Monthly data of trucks") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

MI_TimeSeries_Plot |>
  autoplot(Value) +
  labs(title="Untransformed data of trucks crossing US-CAN border from Michigan") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#Box Cox Transformation parameter tuning
lambda<- MI_TimeSeries_Plot |>
  features(Value,features = guerrero) |>
  pull(lambda_guerrero)

#Data plot after box cox transformation
MI_TimeSeries_Plot |>
  autoplot(box_cox(Value,lambda)) +
  labs(title="Box Cox transformer for Measure of Trucks") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

print("optimal Lambda for boxcox",lambda)

#From Transformations we can see that Box Cox transformations hardly had any effect variance in the values across the months. Although log trasnformations had shown some significant improvement in capturing the data variance over time and stablized it to some extent. However, the data is still non-stationary.

MI_TimeSeries_Plot$Log_adjust <- log(MI_TimeSeries_Plot$Value)

MI_TimeSeries_Plot

# STL and classical decomposition of the log adjusted values
MI_dcmp1 <- MI_TimeSeries_Plot|>
  model(
    classical = classical_decomposition(Value, type="additive"),
    stl1 = STL(Value)
  )

# Plotting STL of the log adjusted values
stlcomp1<- MI_dcmp1|> select(stl1) |> components(MI_dcmp1)
stlcomp1 |> autoplot()+
  labs(title = "STL decomposition of Value data of Trucks ") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# Plotting Classical decomposition of the log adjusted values
clscomp1<- MI_dcmp1|> select(classical) |> components(MI_dcmp1)
clscomp1 |> autoplot()+
  labs(title = "Classical decomposition of Value data of Trucks ") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

MI_TimeSeries_Train <- MI_TimeSeries_Plot |>
  filter(year(Date) < 2024)

ETS_fit <- MI_TimeSeries_Train |>
  model(
    ETS(Value ~ error("A")+trend("Ad")+season("A"))
  )

tidy(ETS_fit) |> select(term,estimate)

fc <- ETS_fit |>
  forecast(h=12)

fc |>
  autoplot(MI_TimeSeries_Plot) +
  labs(title= "ETS Model Forecast plot for 12 months ") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))


ETS_fit |>  report()

result_table <- fc %>%
  as_tibble %>%
  select(Date, .mean)

result_table

MI_TimeSeries_Test <- MI_TimeSeries_Plot |>
  filter(year(Date) == 2024)

MI_TimeSeries_Test

install.packages("PerMat")
library(PerMat)

actual <- MI_TimeSeries_Test$Value
predicted<- result_table$.mean

RMSE(actual, predicted)
MAPE(actual, predicted)
MAE(actual, predicted)

ggplot(result_table, aes(x = Date, y = .mean)) + geom_line()



#XTL DECOMPOSITION

xtl_dcmp<- MI_TimeSeries_Train |>
  model(
    xtl= X_13ARIMA_SEATS(Value ~ x11()), #multiple options but we use x11
  )

xtl_dcmp<- xtl_dcmp|> select(xtl) |> components(xtl_dcmp)
xtl_dcmp

library(knitr)

xtl_dcmp |> tail() |> kable()
xtl_dcmp |> autoplot()

#BASIC MOVING AVERAGE FORECAST USING NAIVE, DRIFT AND SEASONAL NAIVE

#Moving Averages Forecast
MI_Trucks_fit<- MI_TimeSeries_Plot |>
  model(
    MI_Mean= MEAN(Value),
    MI_Naive=NAIVE(Value),
    MI_Drift= NAIVE(Value~drift())
  )

MI_Trucks_fit |> forecast(h=30) |> autoplot(MI_TimeSeries_Plot, level=NULL) +
  labs(title="Forecast of basic Moving Averages methods ") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

naive_fit <- MI_Trucks_fit |> select(MI_Naive)
naive_fit |> gg_tsresiduals()

drift_fit <- MI_Trucks_fit |> select(MI_Drift)
drift_fit |> gg_tsresiduals()

MI_seasonal<-MI_TimeSeries_Plot |> model(SNAIVE(Value))
MI_seasonal |> forecast(h=20) |> autoplot(MI_TimeSeries_Plot) +
  labs(title="Seasonal Naive Forecast for next 20 months") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#Linear Regression Using XTL
xtl_dcmp

Dcmp_TS_MI <- xtl_dcmp |>
  select(Date, Value, trend, seasonal, irregular, season_adjust)

Dcmp_TS_MI<- Dcmp_TS_MI |>
  as_tsibble(index=Date)

#MULTIPE REGRESSION TIME SERIES MODELING USING TREND AND SEASONALITY

#Multiple Linear Regression of Value with trend and seasonal adjusted values

Linear_XTL_train <- Dcmp_TS_MI |>
  filter(year(Date) < 2023)
Linear_XTL_test <- Dcmp_TS_MI |>
  filter(year(Date) >= 2023)

Linear_XTL_Multiple <- Linear_XTL_train |>
  model(TSLM(Value ~ trend + seasonal))


Linear_XTL_Multiple |> report()

Linear_XTL_Multiple |> gg_tsresiduals()

Linear_XTL_Multiple |> report()

fc <- forecast(Linear_XTL_Multiple, new_data = Linear_XTL_test)

Dcmp_TS_MI |>
  autoplot(Value) +
  labs(title="Multiple Linear Regression Forecast for 12 months from Jan-Dec 2023") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  autolayer(fc)

fc

Multiple_Linear_Predict <- fc$.mean
Multiple_Linear_Actual<-Linear_XTL_test$Value

as.data.frame(Multiple_Linear_Predict)

#MODEL EVALUATION METRICS FOR MULTIPLE LINEAR REGRESSION
RMSE(Multiple_Linear_Actual,Multiple_Linear_Predict)
MAPE(Multiple_Linear_Actual,Multiple_Linear_Predict)
MAE(Multiple_Linear_Actual,Multiple_Linear_Predict)


#ARIMA MODEL
#We Checked stationarity of the timeseries data earlier in this Code file and we will now use it for Modeling.

#So calling the difference codes again to show ACF and PACF plots of first difference since we saw that first difference was sufficient to get sufficient stationarity in the data.

#Plotting ACT plot for first difference of Measure variable

ACF_plot$diff <- difference(ACF_plot$Value)
ACF_plot |>
  ACF(ACF_plot$diff, lag_max=48) |>
  autoplot() +
  labs(title="ACF plot of first difference of trucks data", scale=c(1,10)) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

ACF_plot |> gg_tsdisplay(diff, plot_type = "partial")

#For ARIMA we need to choose below values:
  
#AR model inputs:
#p= 1 or 2
#d= 1 , since we took first difference and we can also try with 0
#q= 1 or 2


#P= 0
#D=1
#Q=1 or 2


#MODEL FITTING FOR ARIMA MODELS
ARIMA_fit1 <- MI_TimeSeries_Train |>
  model(
    AR1=ARIMA(Value),
    AR2=ARIMA(Value~pdq(1,1,1)),
    AR3=ARIMA(Value~pdq(2,1,2)),
    AR4=ARIMA(Value~pdq(0,1,1)),
    AR5=ARIMA(Value~pdq(0,1,2))
  )

ARIMA_fit1 |> select(AR1) |>
  report()

ARIMA_fit1 |> select(AR2) |>
  report()

ARIMA_fit1 |> select(AR3) |>
  report()

ARIMA_fit1 |> select(AR4) |>
  report()

ARIMA_fit1 |> select(AR5) |>
  report()

#MODEL FORECATING METRICS USING ARIMA MODEL WITH LEAST AICc
fit<-forecast(ARIMA_fit1$AR3, h=6)
fit

#MODEL FITTING AND FORECATING USING MOST OPTIMAL ARIMA MODEL WITH LOWEST AICc

best_fit1 <- MI_TimeSeries_Train %>% model(best = ARIMA(Value~pdq(2,1,2)))
best_fit1 |> report()

forecast_AR<- best_fit1 %>% forecast(h = 12)

forecast_AR |>
  autoplot(MI_TimeSeries_Train) +
  labs(title="\nForecast plots for ARIMA(2,1,2) for next 12 months") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

forecast_AR

predicted=forecast_AR$.mean
observed=MI_TimeSeries_Test$Value

#MODELING EVALUATION METRICS FOR ARIMA(2,1,2)
RMSE(predicted,observed)
MAPE(predicted,observed)
MAE(predicted,observed)

#FINDING BEST ARIMA MODEL USING STEPWISE AND SEARCH GRID

ARIMA_fit2 <- MI_TimeSeries_Train |>
  model(
    stepwise=ARIMA(Value),
    search=ARIMA(Value, stepwise = FALSE)
  )

ARIMA_fit2

best_fit2 <- MI_TimeSeries_Train %>% model(best2 = ARIMA(Value~pdq(2,0,2)))
report(best_fit2)

forecast_AR2 <- best_fit2 %>% forecast(h = 12)

forecast_AR2 |>
  autoplot(MI_TimeSeries_Train) +
  labs(title="\nForecast plots for ARIMA(2,0,2) for next 12 months") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

predicted <- forecast_AR2$.mean
observed <- MI_TimeSeries_Test$Value

#MODEL EVALUATION METRICS FOR ARIMA(2,0,2)
predicted <- forecast_AR2$.mean
observed <- MI_TimeSeries_Test$Value

RMSE(predicted,observed)
MAPE(predicted,observed)
MAE(predicted,observed)

#SEASONAL ARIMA MODELING

#Seasonal first difference ACF for lag 12

MI_TimeSeries_Plot |>
  gg_tsdisplay(difference(Value,12), plot_type="partial", lag=48) +
  labs(title="Seasonally differenced", y="") +
  labs(title="\nForecast plots for ARIMA(2,1,2) for next 12 months")

#Seasonal second differencing ACF for lag 12
MI_TimeSeries_Plot |>
  gg_tsdisplay(difference(Value,12) |>
                 difference(), plot_type="partial", lag=48) +
  labs(title="Seasonally second differenced", y="")

#Here we can see that the second difference of seasonal first difference looks stationary and both PACF and ACF have controlled spikes.

#Seasonal MA(2) since we two significant spikes at ACF plot at 12 and 24 months

#Non-seasonal difference, we can visually see in ACF that we have first two spikes going down and in PACF we have first 3 significant spikes showing downward trend . so we choose non-seasonal AR(2) or AR (3)

SARIMA_fit1 <- MI_TimeSeries_Train |>
  model(
    SAR1=ARIMA(Value),
    SAR2=ARIMA(Value~pdq(0,1,2) + PDQ(0,1,2)),
    SAR3=ARIMA(Value~pdq(2,1,0) + PDQ(0,1,2))
  )

SARIMA_fit1 |> select(SAR1) |>
  report()

SARIMA_fit1 |> select(SAR2) |>
  report()

SARIMA_fit1 |> select(SAR3) |>
  report()

#OPTIMAL MODEL USING STEPWISE & SEARCH GRID

SARIMA_fit2 <- MI_TimeSeries_Train |>
  model(
    stepwise=ARIMA(Value),
    search=ARIMA(Value, stepwise = FALSE)
  )
SARIMA_fit2

SARIMA_best_fit2 <- MI_TimeSeries_Train |>
  model(
    SAR4=ARIMA(Value~pdq(2,0,2) + PDQ(0,1,2))
  )
SARIMA_best_fit2 |> select(SAR4) |>
  report()

forecast_SAR1 <- SARIMA_best_fit2 |> select(SAR4) |>
  forecast(h = 12)

forecast_SAR1 |>
  autoplot(MI_TimeSeries_Train) +
  labs(title="FORECAST PLOT FOR SARIMA (2,0,2), (0,1,2)") +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle=45, hjust = 1))

#MODEL EVALUATION METRICS FOR SARIMA (2,0,2) (0,1,2)
predicted <- forecast_SAR1$.mean
observed <- MI_TimeSeries_Test$Value

RMSE(predicted,observed)
MAPE(predicted,observed)
MAE(predicted,observed)




























