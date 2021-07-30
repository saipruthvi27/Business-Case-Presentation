#calling the libraries required for the case study
library(readxl)
library(plotly)
library(ggplot2)
library(caret)
library(class)
library(ROCR)
library(dplyr)

#assigning the excel sheet to airF data
airF <- Air_France <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls", 
           sheet = "DoubleClick")
View(airF)  #viewing the dataset

#convert to dataframe
airF <- as.data.frame(airF)

################################################################################
########################### Cleaning the data ##################################
#Keyword ID data 
airF$`Keyword ID` <- gsub("43000", "0", airF$`Keyword ID`)

#editing square brackets from Keywords
airF$Keyword <- gsub("\\[|\\]", "", airF$Keyword)

#changing NAs values
for(x in 1:ncol(airF)){ #loop over each column, replace missing values to NA
  airF[, x] <- gsub("N/A|Unassigned|uncategorized|Unavailable", NA, airF[, x])
}

#Convertion of numeric data type
for (x in c(3, 12:23) ){
  airF[, x] <- as.numeric(airF[, x])
}

#cleaning format of Bid strategy 
airF$`Bid Strategy` <-gsub(".*(\\d)\\s*(-)\\s*(\\d*).*", "\\1\\2\\3", airF$`Bid Strategy`)

#deleting columns that are redundant 
airF <- airF[-10, -1]
View(airF)

# Investigating if there are any missing values
is.na(airF)

# Counting how many missing values in the each variable dataframe
colSums(is.na(airF))

# Viewing the types of variable in the dataframe
str(airF)

##Bid Strategy variable has missing values and is of character type. It will not be considered for the analysis 

################################################################################
############################## Descriptive Statistics ##########################
#normalising the data
my_normalize <- function(x){
  my_min <- min(x, na.rm = TRUE)
  my_max <- max(x, na.rm = TRUE)
  min_max <- (x - my_min)/(my_max - my_min)
  return(min_max)
}

#normalise the data
airF$sebid <- my_normalize(x = airF$`Search Engine Bid`)
airF$c <- my_normalize(x = airF$Clicks)
airF$cc <- my_normalize(x = airF$`Click Charges`)
airF$avgcpc <- my_normalize(x = airF$`Avg. Cost per Click`)
airF$im <- my_normalize(x = airF$Impressions)
airF$ect <- my_normalize(x = airF$`Engine Click Thru %`)
airF$avgp <- my_normalize(x = airF$`Avg. Pos.`)
airF$tc <- my_normalize(x = airF$`Trans. Conv. %`)
airF$tct <- my_normalize(x = airF$`Total Cost/ Trans.`)
airF$amt <- my_normalize(x = airF$Amount)
airF$totc <- my_normalize(x = airF$`Total Cost`)
airF$tvb <- my_normalize(x = airF$`Total Volume of Bookings`)

#creating binary code
for(x in 1:nrow(airF)){
  if(airF$`Total Volume of Bookings`[x] <= 1){
    airF$Business_outcome[x] <- as.numeric(0)
  } else if(airF$`Total Volume of Bookings` > 1){
    airF$Business_outcome[x] <- as.numeric(1)
  }
}

#regression model
my_log_norm <- glm(Business_outcome ~ sebid + c + cc + avgcpc + im + ect + avgp + tc + tct + amt, data = airF, family = "binomial")
summary(my_log_norm)

#removing insignificant variables from the above glm formula
my_log_norm <- glm(Business_outcome ~ c + avgcpc + im + tct + amt, data = airF, family = "binomial")
summary(my_log_norm)

#regression without normalisation
my_log <- glm(Business_outcome ~ Clicks + `Avg. Cost per Click`+ Impressions+ `Total Cost/ Trans.`+ Amount,
              data = airF, family = "binomial")
summary(my_log)
exp(3.965e-03)-1
exp(3.241e-01)-1
exp(-2.739e-06)-1
exp(-1.926e-03)-1
exp(2.700e-03)-1
#the average cost per click has the highest coefficient (i.e. 38.2%) among all the other significant variables

#profit calculation by assuming amount as the revenue generated
airF$Profits <- airF$Amount - airF$`Total Cost`
summary(airF$Profits)

# Creating a udf for Random sampling
random_sample <- function(df, n) {
  training_index <- sample(1:nrow(df), nrow(df)*n)
  training_dataset <- df[training_index,]
  testing_dataset <- df[-training_index,]
  return(list(training_dataset, testing_dataset))
}#closing random sampling

my_random_output <- random_sample(df = airF, n = 0.8)
training_data <- my_random_output[[1]] #getting training data from list
testing_data <- my_random_output[[2]] #getting testing from a list

###Confusion matrix for unit model (not normalized)
my_prediction <- predict(my_log,training_data,type="response") 

confusionMatrix(data = as.factor(as.numeric(my_prediction>0.5)),
                reference = as.factor(as.numeric(training_data$Business_outcome)))
#confusion matrix takes the predicted model from data=, matches them with their actual values put in reference=,

#here we take the model we have built on training data and test it on
#observations it hasn't seen yet to see how good it is
my_prediction_testing <- predict(my_log,testing_data,type="response") 

confusionMatrix(data = as.factor(as.numeric(my_prediction_testing>0.5)),
                reference = as.factor(as.numeric(testing_data$Business_outcome)))

#confusion matrix for normalized data
my_prediction_testing_norm <- predict(my_log_norm,testing_data,type="response") 

confusionMatrix(data = as.factor(as.numeric(my_prediction_testing_norm>0.5)),
                reference = as.factor(as.numeric(testing_data$Business_outcome)))
#result "IT'S THE SAME WITH THE ORIGINAL DISTRIBUTION"

my_prediction_testing <- predict(my_log,testing_data,type="response") 

pred_val_logit <- prediction(my_prediction_testing, testing_data$Business_outcome)

perf_logit <- performance(prediction.obj = pred_val_logit,measure = "tpr",x.measure = "fpr")
plot(perf_logit, col="blue")

#plotting average cost per click and average profit per publisher
avg_publisher <- tapply(airF$Profits, airF$`Publisher Name`, FUN = mean)
avg_publisher <- cbind.data.frame(avg_publisher, unlist(tapply(airF$Profits, airF$`Publisher Name`, FUN = mean)))
names(avg_publisher) <- c("Average Profit", "Average Cost per Click")
label_names <- row.names(avg_publisher)
myavg_pub <- ggplot()+
  geom_point(data = avg_publisher, aes(x =  `Average Cost per Click`,
                                       y =  `Average Profit`, 
                                       color=label_names), size = 5)
ggplotly(myavg_pub)


##Plotting Profit vs impressions
im_profit <- tapply(airF$Profits, airF$`Publisher Name`, FUN=mean)
im_profit <- cbind.data.frame( im_profit, unlist(tapply(airF$`Impressions`, airF$`Publisher Name`, FUN=mean)))
names(im_profit) <- c("Average Profit", "Impressions") 

improfit_plot <- ggplot() +
  geom_point(data = airF, aes(x = `Impressions` , y = Profits))
my_label <- row.names(im_profit)
improfit_avg <- ggplot() +
  geom_point(data = im_profit, aes(x =  `Impressions`, y =  `Average Profit`, color = my_label), size = 5)
ggplotly(improfit_avg)

#Calculating Revenue/ Clicks  to compare it with Average cost per click
#This will show the efficiency of each Search engine with respect to
#clicks

airF$`Avg(Revenue/Click)` <- airF$Amount / airF$Clicks
for (x in 1:nrow(airF)){
  if (airF$Clicks[x] == 0){
    airF$`Avg(Revenue/Click)`[x] <- 0
  }
}
summary(airF$`Avg(Revenue/Click)`)

#Comparing profit with cost per click for every search engine
#Average cost per click for Yahoo - US (publisher)
rc_avgcost <- tapply(airF$`Avg(Revenue/Click)`, airF$`Publisher Name`, FUN=mean)
rc_avgcost <- cbind.data.frame(rc_avgcost, unlist(tapply(airF$`Avg. Cost per Click`, airF$`Publisher Name`, FUN=mean)))
names(rc_avgcost) <- c("Avg(Revenue/Click)", "Average Cost Per Click") 

##Plotting Average Revenue/Clicks vs average cost per click
af_label <- row.names(rc_avgcost)
rc_avgcost_plt <- ggplot() +
  geom_point(data = rc_avgcost, aes(x = `Average Cost Per Click` , y = `Avg(Revenue/Click)`, color = my_label), size = 5)
ggplotly(rc_avgcost_plt) 


### To know if Kayak is a viable option as part of 
### the SEM strategy of Air France, we will compare the performance of each publisher 
### to Kayak's performance using return on advertising (ROA) spend.

# Creating another variable to compute for the ROA of all observations
airF$RoA <- airF$Amount / airF$`Total Cost`

# Remove an observation with 0 total cost to avoid an error
airF <- airF[airF$`Total Cost` != 0, ]

# Creating a pivot table to analyze the ROA performance of each advertising platform
pub_roa <- airF%>%group_by(`Publisher Name`)%>% summarize(avg_roa = mean(RoA))
#View(pub_roa)

# Putting Kayak's advertising performance in a dataframe
df_kay <- data.frame("Clicks" = 2839, "Media Cost" = 3567, "Total Bookings" = 208, "Avg Ticket" = 1124, "Total Revenue" = 233694, "Net Revenue" = 230127)

# Computing for ROA of Kayak based on the created dataframe
roa_kayak <- df_kay[1,5] / df_kay[1,2]

# Creating a dataframe for kayak_ROA
roa_df_kay <- data.frame("Kayak", roa_kayak)

# Adding variable names to the kayak_ROA_df dataframe
names(roa_df_kay) <- c("Publishers", "RoA")

# Adding names to the publishers_ROA_df dataframe
names(pub_roa) <- c("Publishers", "RoA")

# Combining kayak dataframe to publishers dataframe
df_kayak <- rbind(pub_roa, roa_df_kay)

# Rounding ROA variable up to 2 decimal places
df_kayak[,-1] <- round(df_kayak[,-1],2)
#View(df_kayak)

# Visualizing the ROA comparisons between other publishers and creating base plot
ggplot(df_kayak, aes(Publishers, RoA)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange", color = "white") +
  geom_text(aes(label=RoA),position=position_dodge(width=1.5), vjust=-0.25) +
  ggtitle("Publisher ROAs") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


airF$Probability <- ( airF$`Trans. Conv. %` * airF$`Engine Click Thru %`) /100
airF_plot <- airF %>% group_by(`Publisher Name`) %>% summarize(
  total_records = n(),
  total_amount = sum(`Total Cost`),
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_prob = mean(Probability),
  avg_roa = mean(RoA)
)
summary(airF_plot)

prob_plt <- plot_ly(airF_plot, x = ~avg_prob, y = ~avg_roa,
                textposition = "auto",
                type = 'scatter', 
                mode = 'markers', 
                size = ~avg_cpc, 
                color = ~`Publisher Name`, 
                colors = 'Paired',
                marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Publisher Strategy',
         xaxis = list(title = "Booking Probability", showgrid = TRUE),
         yaxis = list(title = "Average RoA", showgrid = TRUE),
         showlegend = TRUE)
prob_plt
summary(airF_plot)

################################################################################
