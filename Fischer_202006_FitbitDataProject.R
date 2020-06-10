#load needed libraries
if(!require(dslabs)) install.packages("dslabs")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(lubridate)) install.packages("lubridate")
if(!require(anytime)) install.packages("anytime")
if(!require(readxl)) install.packages("readxl")
if(!require(naniar)) install.packages("naniar")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(grid)) install.packages("grid")
if(!require(rpart,rpart.plot)) install.packages("rpart,rpart.plot")
if(!require(reshape2)) install.packages("reshape2")

#load data
sleepdata<- tempfile()
download.file("https://github.com/mafis103/RFiles/raw/master/fitbit_export_sleep_20200608.xlsx", sleepdata, mode="wb")
fitbit_sleep<-read_excel(sleepdata)
activitydata<- tempfile()
download.file("https://github.com/mafis103/RFiles/raw/master/fitbit_export_20200608.xlsx", activitydata, mode="wb")
fitbit_activities<-read_excel(activitydata)


################################Data Wrangling##################################

#replace String NA's with real NAs
str_na <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "Not available")
fitbit_sleep_data <- fitbit_sleep  %>% replace_with_na_all(condition = ~.x %in% str_na)

#add Date Column to join by for both DFs
fitbit_sleep_data <- fitbit_sleep_data %>% 
  mutate(Date=as.Date(Starttime_sleep, tryFormats= "%d.%m.%Y"),Minutes_REM= as.integer(Minutes_REM), Minutes_light_sleep=as.integer(Minutes_light_sleep), Minutes_deep_sleep=as.integer(Minutes_deep_sleep), Minutes_slept=as.integer(Minutes_slept), Minutes_awake=as.integer(Minutes_awake), Wakeup_amount=as.integer(Wakeup_amount), Time_in_bed= as.integer(Time_in_bed))
fitbit_sleep_data <- fitbit_sleep_data %>%
  select(Minutes_slept, Minutes_awake, Wakeup_amount, Time_in_bed, Minutes_REM, Minutes_light_sleep, Minutes_deep_sleep, Date)
fitbit_activities_data<-fitbit_activities%>%transform(Date=as.Date(Date, tryFormats
                                                                   = "%d.%m.%Y"))
fitbit_activities_data<-fitbit_activities_data%>%mutate(weekday=weekdays(Date))

#join both tables by date (MAIN SET)
fitbit_data<-full_join(fitbit_activities_data,fitbit_sleep_data,by="Date")

#create test and train set for later (MAIN SET)
set.seed(755)
test_index <- createDataPartition(y = fitbit_data$Calories_Burned, times = 1,
                                  p = 0.2, list = FALSE)
fitbit_train <- fitbit_data[-test_index,]
fitbit_test <- fitbit_data[test_index,]
fitbit_test<-na.omit(fitbit_test)

#create a separate set for sleep prediction that avoids overprediction by existing values
fitbit_traindata_stripped <- fitbit_train %>%
  select(-Minutes_awake,-Minutes_REM,-Minutes_light_sleep,
         -Minutes_deep_sleep,-Wakeup_amount,-Time_in_bed,-Date)



######Side Set with NA values filled
#see how many rows contain NA's
sum(is.na(fitbit_data))

#create one separate set that does NA-handling
fitbit_no_na<-fitbit_data
fitbit_no_na <- fitbit_no_na %>%
  mutate(Minutes_REM = ifelse(is.na(Minutes_REM), rnorm(n=sum(is.na(Minutes_REM)), mean=mean(Minutes_REM,na.rm = TRUE), sd=sd(Minutes_REM, na.rm = TRUE)), Minutes_REM),
         Minutes_light_sleep = ifelse(is.na(Minutes_light_sleep), rnorm(n=sum(is.na(Minutes_light_sleep)), mean=mean(Minutes_light_sleep,na.rm = TRUE), sd=sd(Minutes_light_sleep, na.rm = TRUE)), Minutes_light_sleep),
         Minutes_deep_sleep = ifelse(is.na(Minutes_deep_sleep), rnorm(n=sum(is.na(Minutes_deep_sleep)), mean=mean(Minutes_deep_sleep,na.rm = TRUE), sd=sd(Minutes_deep_sleep, na.rm = TRUE)), Minutes_deep_sleep),
         Minutes_slept = ifelse(is.na(Minutes_slept), rnorm(n=sum(is.na(Minutes_slept)), mean=mean(Minutes_slept,na.rm = TRUE), sd=sd(Minutes_slept, na.rm = TRUE)), Minutes_slept),
         Minutes_awake = ifelse(is.na(Minutes_awake), rnorm(n=sum(is.na(Minutes_awake)), mean=mean(Minutes_awake,na.rm = TRUE), sd=sd(Minutes_light_sleep, na.rm = TRUE)), Minutes_awake),
         Time_in_bed = ifelse(is.na(Time_in_bed), rnorm(n=sum(is.na(Time_in_bed)), mean=mean(Time_in_bed,na.rm = TRUE), sd=sd(Minutes_light_sleep, na.rm = TRUE)), Time_in_bed),
         Wakeup_amount = ifelse(is.na(Wakeup_amount), rnorm(n=sum(is.na(Wakeup_amount)), mean=mean(Wakeup_amount,na.rm = TRUE), sd=sd(Minutes_light_sleep, na.rm = TRUE)), Wakeup_amount)
  )
#check if there are still NAs in set
sum(is.na(fitbit_no_na))

#split set
set.seed(755)
fitbit_train_no_na <- fitbit_no_na[-test_index,]
fitbit_test_no_na <- fitbit_no_na[test_index,]


#######Side Set Using Mean
#create one separate set that does NA-handling
fitbit_mean<-fitbit_data
fitbit_mean <- fitbit_mean %>%
  mutate(Minutes_REM = ifelse(is.na(Minutes_REM), mean(Minutes_REM,na.rm = TRUE), Minutes_REM),
         Minutes_light_sleep = ifelse(is.na(Minutes_light_sleep), mean(Minutes_light_sleep,na.rm = TRUE), Minutes_light_sleep),
         Minutes_deep_sleep = ifelse(is.na(Minutes_deep_sleep), mean(Minutes_deep_sleep,na.rm = TRUE), Minutes_deep_sleep),
         Minutes_slept = ifelse(is.na(Minutes_slept), mean(Minutes_slept,na.rm = TRUE), Minutes_slept),
         Minutes_awake = ifelse(is.na(Minutes_awake), mean(Minutes_awake,na.rm = TRUE), Minutes_awake),
         Time_in_bed = ifelse(is.na(Time_in_bed), mean(Time_in_bed,na.rm = TRUE), Time_in_bed),
         Wakeup_amount = ifelse(is.na(Wakeup_amount), mean(Wakeup_amount,na.rm = TRUE), Wakeup_amount)
  )
#check if there are still NAs in set
sum(is.na(fitbit_mean))

#split set
set.seed(755)
fitbit_train_mean <- fitbit_mean[-test_index,]
fitbit_test_mean <- fitbit_mean[test_index,]

############# General Data Exploration ##########################
##calculate averages 
average_sleep <- mean(fitbit_sleep_data$Minutes_slept)
average_steps<-mean(fitbit_activities_data$Steps)
average_calories<-mean(fitbit_activities_data$Calories_Burned)
average_activities<-mean(fitbit_activities_data$Minutes_medium_activity)+mean(fitbit_activities_data$Minutes_high_activity)
average_sitting_time<-mean(fitbit_activities_data$Minutes_sitting)
average_light_activity<-mean(fitbit_activities_data$Minutes_light_activity)

#filter out NA values
index<-which(is.na(fitbit_data$Minutes_deep_sleep))
#filter out those, that are not coherent
i<-1
index_refined <- vector()
for(i in 1:length(index)){
  ifelse(index[i+1]==index[i]+1,i<-i+2,{
    index_refined<-append(index_refined,index[i]) 
    i<-i+1
  }
  )
}

#plot NA-Sleep versus average sleep 
na_values<-slice(fitbit_data,index_refined)
na_values<-as.data.frame(na_values)

fitbit_data %>% 
  qplot(Date, Minutes_slept, data = ., geom = "point", aes(group=Date)) +
  ggtitle("Minutes slept with NA values highlighted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_blank()) + 
  ylim(0,700) + 
  geom_point(data=na_values, aes(Date, Minutes_slept, color="red"), size=2) + 
  geom_hline(yintercept=average_sleep, color="blue", size=1)+ 
  ylab("Minutes slept") + 
  scale_fill_discrete(name = "", labels = c("NA values"))+
  scale_colour_manual("",values=c("red"), labels=c("NA values"))  


#Explore weekday effect and plot in a 3*2 grid
sleep_weekday_plot<-fitbit_data %>% 
  qplot(weekday, Minutes_slept, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=average_sleep, color="blue", size=1)+ 
  ggtitle(label="Sleep") + 
  theme(axis.title.x = element_blank())  + 
  ylab("Minutes slept")
calories_weekday_plot<-fitbit_data %>%
  qplot(weekday, Calories_Burned, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle(label="Calorie expenditure") + 
  geom_hline(yintercept=average_calories, color="blue", size=1)+ 
  theme(axis.title.x = element_blank()) + 
  ylab("Calories burned in kcal")
steps_weekday_plot<-fitbit_data %>%
  qplot(weekday, Steps, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=average_steps, color="blue", size=1)+ 
  ggtitle(label="Steps") + 
  theme(axis.title.x = element_blank()) + 
  ylab("Amount of steps")
activity_weekday_plot<-fitbit_data %>% mutate(activity=(Minutes_medium_activity+Minutes_high_activity)) %>%
  qplot(weekday, activity, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=average_activities, color="blue", size=1)+ 
  ggtitle(label="Activity (medium to high)") + 
  theme(axis.title.x = element_blank()) + 
  ylab("Time (min) of medium or high activity")
light_activity_weekday_plot<-fitbit_data %>% 
  qplot(weekday, Minutes_light_activity, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=average_light_activity, color="blue", size=1)+ 
  ggtitle(label="Activity (low)") + 
  theme(axis.title.x = element_blank()) + 
  ylab("Time (min) of low activity")
sitting_weekday_plot<-fitbit_data %>% 
  qplot(weekday, Minutes_sitting, data = ., geom = "boxplot", aes(group=weekday)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept=average_sitting_time, color="blue", size=1)+ 
  ggtitle(label="Inactivity") + 
  theme(axis.title.x = element_blank()) + 
  ylab("Time (min) sitting")

grid.arrange(sleep_weekday_plot,calories_weekday_plot,steps_weekday_plot,activity_weekday_plot,light_activity_weekday_plot, sitting_weekday_plot, ncol=3, top = textGrob("Weekday Effect",gp=gpar(fontsize=20,color="blue")))



##Explore relationships between sleep details
wakeups<-fitbit_data %>%
  qplot(Wakeup_amount, Minutes_slept, data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Interrupted sleep") + 
  xlab("Wakeups") + 
  ylab("Time (min) slept")
waketime<-fitbit_data %>%
  qplot(Wakeup_amount, Minutes_awake, data = ., geom = "point", aes(group=Minutes_awake)) + 
  ggtitle(label="Time awake") + 
  xlab("Wakeups") + 
  ylab("Time (min) awake")
deepsleep<-fitbit_data %>%
  qplot(Minutes_deep_sleep, Minutes_slept, data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Deep sleep") + 
  xlab("Time (min) deep sleep") + 
  ylab("Time (min) slept")
lightsleep<-fitbit_data %>%
  qplot(Minutes_light_sleep, Minutes_slept, data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Light sleep") + 
  xlab("Time (min) light sleep") + 
  ylab("Time (min) slept")

wake_activity<-fitbit_data %>%
  qplot(Wakeup_amount, Calories_activities, data = ., geom = "point", aes(group=Wakeup_amount)) + 
  ggtitle(label="Wakeups vs Activity") + 
  ylab("Calories burned by activity") + 
  xlab("Wakeups")
sleeptime_seated<-fitbit_data %>%
  qplot(Minutes_sitting, Minutes_slept,  data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Sleep amount vs Sitting") + 
  xlab("Time (min) sitting") + 
  ylab("Time (min) slept")
weight_sleep<-fitbit_data %>%
  qplot(Weight, Minutes_slept, data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Weight vs sleep") + 
  xlab("Weight") + 
  ylab("Time (min) slept")
activity_sleep<-fitbit_data %>%
  qplot(Calories_activities, Minutes_slept, data = ., geom = "point", aes(group=Minutes_slept)) + 
  ggtitle(label="Activity vs sleep") + 
  xlab("Calories burned during activities") + 
  ylab("Time (min) slept")

grid.arrange(wakeups,waketime,deepsleep,lightsleep, wake_activity,sleeptime_seated,weight_sleep, activity_sleep, ncol=2, top = textGrob("Sleepdetails",gp=gpar(fontsize=20,color="blue")))

##Gather values in regression tree that are driving calories burned and sleep
Calories_tree <- rpart(Calories_Burned ~ ., data = fitbit_data, control = rpart.control(maxdepth=3)) 
printcp(Calories_tree)
rpart.plot(Calories_tree,type=1, clip.right.labs = FALSE, branch = .3, under = TRUE)

#take out detailed data about sleep
fitbit_data_stripped<-fitbit_data%>%select(-Minutes_awake,-Minutes_REM,-Minutes_light_sleep,-Minutes_deep_sleep,-Wakeup_amount,-Time_in_bed,-Date)
Sleep_tree <- rpart(Minutes_slept ~ ., data = fitbit_data_stripped, control = rpart.control(maxdepth=3)) 
printcp(Sleep_tree)
rpart.plot(Sleep_tree,type=1, clip.right.labs = FALSE, branch = .3, under = TRUE, tweak=1.5)


################Prediction function####################
#transf transforms matrixes into the correct fromat
transf<-function(matrixes){
  matrixes<-t(matrixes)
  matrixes<-as.matrix(matrixes)
  colnames(matrixes)<-c("category","b","value")
  return(matrixes)
}

# calculate performance of model, assuming a certain variability allowance
# prepare data for plotting with several B's
# all functions take a numberic value B that defines tolerance, a dataframe df to modify
# and a test dataframe test_df to check against
# checkCol is the column to be checked and param is the model name
check_model<-function(B, df, df_test, checkCol, param){
  #find index of desired column (df$checkCol)
  index<-grep(param,colnames(df))
  #create data frame with size of df
  size<-nrow(df)
  temp<-data.frame(matrix(0, ncol=1, nrow=size))
  #set column name for this data frame to param_perf
  names(temp[1])<-paste(param,"_perf")
  #define column df_test$checkCol
  df_test_checkCol<-df_test[,checkCol]
  #check if actual data is in tolerance range and write to temp
  temp[1]<- df_test_checkCol<= df[,index]+B & df_test_checkCol>= df[,index]-B
  #transform to right format
  transformed<-transf(c(param,B,mean(temp[,1])))
  return(transformed)
}

# calculate normalized RMSE
NRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))/mean(true_ratings)
}

models <- c("rf", "glm", "svmLinear", "knn")
prediction<-function(tolerance, testset, trainset, column, average, plottitle, xtitle, ytitle){
  set.seed(5)
  form <- as.formula(paste(column, '~ .'))
    fitted <- lapply(models, function(model){ 
    print(model)
    train(form, method = model, data = trainset, na.action=na.exclude)
  }) 
  names(fitted) <- models
  prediction <- sapply(fitted, function(object) 
    predict(object, newdata = testset, na.action=na.exclude))
  #add to dataframe
  pred_df<-data.frame(prediction)
  # see how well predictions perform in average
  # add naive approach, just using average
  pred_df<- pred_df %>% mutate(naive_approach=average)

  #add naive to list of models
  models<-append(models, "naive")
  
  #create a list to put all model performances in
  dframes <- vector(mode = "list", length = length(models))
  #call function check model for all models with defined values
  for (i in 1:length(models)){
    dframes[[i]]<-sapply(tolerance,check_model,pred_df,testset,column,models[i])
    print(dframes[[i]])
  }
  
  #create dataframe from list for plotting
  plotting_data<-data.frame(matrix(unlist(dframes), ncol=3, byrow=T))
  #set column names
  colnames(plotting_data)<-c("category", "b", "value")
  print(names(plotting_data))
  
  #make sure the values are in right format
  plotting_data<-plotting_data%>% mutate(b=as.integer(b), value=as.numeric(value))
  #prepare ensemble
  ensemble<-plotting_data %>%
    group_by(b) %>% summarize(category="ensemble",value=mean(value))

  #create plot
  plotting_data<-rbind(plotting_data,as.data.frame(ensemble))
  plot <- plotting_data%>% mutate(b=as.integer(b), value=as.numeric(value)) %>%
    group_by(b) %>%
    ggplot(aes(b,value,col=category)) + geom_line()+ ggtitle(label=plottitle) +
    xlab(xtitle) +
    ylab(ytitle)
  
  #prepare NRMSE values
  test_checkCol<-testset[,column]
  nrmse_results<-data.frame()
  nrmse_results <- bind_rows(nrmse_results,
                            data_frame(method="Random Forest",
                                       NRMSE = NRMSE(pred_df$rf, test_checkCol) ))
  nrmse_results <- bind_rows(nrmse_results,
                             data_frame(method="General Linear Model",
                                        NRMSE = NRMSE(pred_df$glm, test_checkCol) ))
  nrmse_results <- bind_rows(nrmse_results,
                             data_frame(method="SVM Linear",
                                        NRMSE = NRMSE(pred_df$svmLinear, test_checkCol) ))
  nrmse_results <- bind_rows(nrmse_results,
                             data_frame(method="K-nearest Neighbor",
                                        NRMSE = NRMSE(pred_df$knn, test_checkCol) ))
  nrmse_results <- bind_rows(nrmse_results,
                             data_frame(method="Pure average",
                                        NRMSE = NRMSE(pred_df$naive_approach, test_checkCol) ))
  nrmse_results%>% knitr::kable()
  
  return(list(plot,nrmse_results%>% knitr::kable()))
}

#tolerance for calorie predictions
cal_tolerance<-seq(0,100,10)

prediction(cal_tolerance, fitbit_test, fitbit_train, "Calories_Burned",
           average_calories,
           "Model performance for prediction of calorie expenditure",
           "+-Tolerance (in kcal)", "Performance of model")

prediction(cal_tolerance, fitbit_test_no_na, fitbit_train_no_na, "Calories_Burned",
           average_calories,
           "Model performance for prediction of calorie expenditure (NAs substituted by rnorm)",
           "+-Tolerance (in kcal)", "Performance of model")

prediction(cal_tolerance, fitbit_test_mean, fitbit_train_mean, "Calories_Burned",
           average_calories,
           "Model performance for calorie expenditure (NAs substituted by mean)",
           "+-Tolerance (in kcal)", "Performance of model")

prediction(seq(0,30,2), fitbit_test, fitbit_traindata_stripped, "Minutes_slept",
           average_sleep,
           "Model performance for prediction of sleep duration",
           "+-Tolerance (in min)", "Performance of model")


