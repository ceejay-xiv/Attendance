install.packages('tidyverse')
install.packages('naniar')
install.packages('dplyr')
install.packages('PerformanceAnalytics')
install.packages('ggplot2')
install.packages('ggpubr')

#load the dataset absenteeism as abs
abs <- read.csv(file = 'Documents/absenteeism.csv', header=TRUE, stringsAsFactors = FALSE)
names(abs)[names(abs) == 'Work.load.Average.day'] <- 'time.to.work'
#check for missingness
library(naniar)
vis_miss(abs) 
mcar_test(abs)

#Duplicate check
dup_chk<-abs%>%distinct()

#Create a backup and remove duplicate data
library(dplyr)
library(ggplot2)
library(ggpubr)



#Age
Test_var<-abs
Test_var$Age<-as.character(abs$Age)

#amount spent per kilometre#######################################################################
abs$amount.spent.per.km<-abs$Transportation.expense/abs$Distance.from.Residence.to.Work
abs$amount.spent.per.km<- abs$amount.spent.per.km%>% dplyr::na_if(55.8)
amt_mean <- mean(abs$amount.spent.per.km, na.rm = TRUE)
abs$amount.spent.per.km <- ifelse(is.na(abs$amount.spent.per.km) == TRUE, amt_mean,abs$amount.spent.per.km)
##################################################################################################
#Join.age#########################################################################################
abs$Join_age<- abs$Age - abs$Service.time
##################################################################################################
#Work/productivity################################################################################
abs$productivity<-(abs$Hit.target*abs$time.to.work)/100
##################################################################################################
#Disciplinary.failure
Test_var$Disciplinary.failure<-ifelse(abs$Disciplinary.failure== 0,'No', 'Yes')
#Education
Test_var$Education<-ifelse(abs$Education==1, 'School',
                           ifelse(abs$Education==2, 'Undergrad',
                                  ifelse(abs$Education==3, 'Postgraduate','Doctorate'
                                  )))
#Children
children<- as.character(abs$Children)
#Pet
Pet<- as.character(abs$Pet)
#Responsibility
abs$Responsibilty<-abs$Children + abs$Pet
#Intake
smoking<-ifelse(abs$Smoker==1,2,abs$Smoker)
abs$Intake<-smoking + abs$Drinker



#plots
ggplot(data = abs, aes(x =Transportation.expense)) + geom_boxplot(fill = '#FFFFFF', color='#E9967A' ,outlier.color = "red")+ labs ( x='Transportation Expense', title = "Boxplot: Transportation Expense",caption = "Data: Absenteeism/ w21007844")
#Transportation Expense
ggplot(data = abs, aes(x =Distance.from.Residence.to.Work, y =Transportation.expense)) + geom_point()+ geom_smooth(method = lm, formula= y~x)+ labs (title = "Scatterplot: Transportation Expense vs Distance from work",caption = "Data: Absenteeism/ w21007844")
#Distance From work VS TE
ggplot(data = abs, aes(x =amount.spent.per.km)) + geom_boxplot(fill = '#FFFFFF', color='#E9967A' ,outlier.color = "red")+ labs ( x='Amount spent per kilometre', title = "Boxplot: Amount spent per kilometre",caption = "Data: Absenteeism/ w21007844")
#amount spent per kilometer
ggplot(data = abs, aes(x=Service.time, y =Age)) + geom_point()+ geom_smooth(method = lm, formula= y~x)+ labs (title = "Scatterplot: Age vs Service time",caption = "Data: Absenteeism/ w21007844")
ggplot(data = abs, aes(x = Join_age)) + geom_histogram(fill=1:12, binwidth= 2)+ labs (y= 'Number of Employees', x='Organisation joined age', title = "Histogram: Age when employees joined the organisation",caption = "Data: Absenteeism/ w21007844")
#Joined Age
ggplot(data = abs, aes(x =productivity)) + geom_boxplot(fill = '#FFFFFF', color='#E9967A' ,outlier.color = "red")+ labs ( x='Productivity', title = "Boxplot: Productivity",caption = "Data: Absenteeism/ w21007844")
#Productivity
ggplot(data = abs, aes(x =Body.mass.index)) + geom_boxplot(fill = '#FFFFFF', color='#E9967A' ,outlier.color = "red")+ labs ( x='BMI', title = "Boxplot: BMI",caption = "Data: Absenteeism/ w21007844")
#BMI
abs_backup1<-abs
abs<-abs_backup1

abs$Transportation.expense<-abs$amount.spent.per.km
abs$Distance.from.Residence.to.Work<-abs$productivity
abs$Service.time<-abs$Join_age
abs$time.to.work<-abs$Intake
abs$Age<-abs$Responsibilty
abs<-abs[-21][-20][-19][-18][-17][-14][-13][-12][-11][-10][-9][-6]

names(abs)[names(abs) == 'Transportation.expense'] <- 'Spent.per.km'
names(abs)[names(abs) == 'Distance.from.Residence.to.Work'] <- 'productivity'
names(abs)[names(abs) == 'Service.time'] <- 'Join_age'
names(abs)[names(abs) == 'time.to.work'] <- 'Intake'
names(abs)[names(abs) == 'Age'] <- 'Responsibilty'
names(abs)[names(abs) == 'Absenteeism.time.in.hours'] <- 'Absent'

#Min-Max Scaler
rs_function <- function(x){(x-min(x))/(max(x)-min(x))}
#This Scaler responds well if the standard deviation 
#is small and when a distribution is not Gaussian. This Scaler is sensitive to outliers.
#If the Coefficient of variation which is (SD/mean) is less than 1, then the SD is low.
abs_backup1<-abs
abs<-abs_backup


abs_hrs<-abs
abs_hrs<-abs_hrs%>%filter(Absent<8)
abs_days<-abs
abs_days<-abs_days%>%filter(Absent == 0| Absent>=8)
abs_days$Absent<-abs_days$Absent/8


#hours dataset
abs_hrs<-abs_hrs%>%filter(Absent<8)

#check the correlation
cor(abs_hrs, method = 'pearson')
library(PerformanceAnalytics)
chart.Correlation(abs_hrs, histogram = TRUE)

################################################################################
#days dataset
abs_days<-abs_days%>%filter(Absent == 0| Absent>=8)
abs_days$Absent<-abs_days$Absent/8

#check the correlation
cor(abs_days, method = 'pearson')
library(PerformanceAnalytics)
chart.Correlation(abs_days, histogram = TRUE)
################################################################################
#total hours and days dataset
abs_ndays$Absent<-abs_ndays$Absent/8
abs_ndays$Absent<- ifelse(abs_ndays$Absent>0 & abs_ndays$Absent<1, 0.5, abs_ndays$Absent)

#check the correlation
cor(abs_ndays, method = 'pearson')
library(PerformanceAnalytics)
chart.Correlation(abs_ndays, histogram = TRUE)

################################################################################
abs_pred_hrs_lm<-lm(Absent~Join_age+Spent.per.km+Education+Disciplinary.failure+Body.mass.index+Intake, data = trainset)
val_pred_abs_hrs<-predict(abs_pred_hrs_lm,testset)
cor(testset$Absent, val_pred_abs_hrs, method='pearson')

abs_pred_days_lm<-lm(Absent~Join_age+Spent.per.km+Disciplinary.failure+Body.mass.index+Intake, data = trainset)
val_pred_abs_days<-predict(abs_pred_days_lm,testset)
cor(testset$Absent, val_pred_abs_days, method='pearson')
################################################################################
#GLM
install.packages('mlbench')
install.packages('glmnet')
library(mlbench)
library(glmnet)
#Ridge for hours
glm_abs_hrs<-abs_hrs
set.seed(42)
glm_abs_hrs[,"train"] <- ifelse(runif(nrow(glm_abs_hrs))<0.8, 1, 0)
trainset <- glm_abs_hrs[glm_abs_hrs$train == "1",]
testset <- glm_abs_hrs[glm_abs_hrs$train == "0",]
trainset <- trainset[-10]
testset <- testset[-10]

train_matrix <- model.matrix(Absent~., trainset)
response <- trainset$Absent
absent_hrs_glm <- cv.glmnet(train_matrix, response, family = "gaussian", alpha = 1, nfolds = 10, type.measure = "mse")
lambda_min <- absent_hrs_glm$lambda.min
coef(absent_hrs_glm, s = lambda_min)
test_matrix <- model.matrix(Absent~., testset)
absent_hrs_pred_glm <- predict(absent_hrs_glm, newx = test_matrix, s =lambda_min)
cor(testset$Absent, absent_hrs_pred_glm, method = "pearson")

#Ridge for days
glm_abs_days<-abs_days
glm_abs_days2<-abs_days
set.seed(42)
glm_abs_days2[,"train"] <- ifelse(runif(nrow(glm_abs_days2))<0.8, 1, 0)
trainset <- glm_abs_days2[glm_abs_days2$train == "1",]
testset <- glm_abs_days2[glm_abs_days2$train == "0",]
trainset <- trainset[-10]
testset <- testset[-10]

train_matrix <- model.matrix(Absent~., trainset)
response <- trainset$Absent
absent_days_glm <- cv.glmnet(train_matrix, response, family = "poisson", alpha = 0, nfolds = 10, type.measure = "mse")
lambda_min <- absent_days_glm$lambda.min
coef(absent_days_glm, s = lambda_min)
test_matrix <- model.matrix(Absent~., testset)
absent_days_pred_glm <- predict(absent_days_glm, newx = test_matrix, s =lambda_min)
cor(testset$Absent, absent_days_pred_glm, method = "pearson")

glmhist_d<-ggplot(data = abs_hrs, aes(x = Absent)) + geom_histogram(fill=1:8, binwidth= 1)+ labs (y= 'Number of Employees', x='Absenteeism', title = "Histogram: Absenteeism in hours")
glmbp_d<-ggplot(data = abs_hrs, aes(x =Absent)) + geom_boxplot(fill = '#FFFFFF', color='#E9967A' ,outlier.color = "red")+ labs ( x='Absenteeism', title = "Boxplot: Absenteeism in hours",caption = "Data: Absenteeism/ w21007844")
ggarrange(glmhist_d, glmbp_d, ncol=2, nrow=1)
################################################################################
#XGB
install.packages('xgboost')
library(xgboost)
library(caret)
install.packages('Metrics')
library(Metrics)
rs_function <- function(x){(x-min(x))/(max(x)-min(x))}
xgb_abs_hrs<-scl_ft%>%filter(Absent<4)
set.seed(42)
indexes<-createDataPartition(xgb_abs_hrs, p = .85, list = F)
xgbtrain<-xgb_abs_hrs[indexes, ]
xgbtest<-xgb_abs_hrs[-indexes, ]

xgbtrain_x<- data.matrix(xgbtrain[, -9])
xgbtrain_y<- xgbtrain[, 9]

xgbtest_x<- data.matrix(xgbtest[, -9])
xgbtest_y<- xgbtest[, 9]

xgb_train<-xgb.DMatrix(data=xgbtrain_x, label = xgbtrain_y)
xgb_test<-xgb.DMatrix(data=xgbtest_x, label = xgbtest_y)

xgb_data<-xgboost(data = xgb_train, eta=0.7 , max.depth = 2, nrounds = 15)

pred_y_xgb<- predict(xgb_data, xgb_test)

mse = mean((xgbtest_y - pred_y_xgb)^2)
mae = caret::MAE(xgbtest_y, pred_y_xgb)
rmse = rmse(xgbtest_y, pred_y_xgb)
r2 = R2(xgbtest_y, pred_y_xgb, form = "traditional")

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(xgbtest_y)
plot(x, xgbtest_y, main="w21007844: Absenteeism in hours: XGBoost", xlab="Employees", ylab="Absenteeism", col = "red", type = "l")
lines(x, pred_y_xgb, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"),col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))


#days
xgb_abs_days<-abs_days
set.seed(42)
indexes<-createDataPartition(xgb_abs_days, p = .8, list = F)
xgbtrain<-xgb_abs_days[indexes, ]
xgbtest<-xgb_abs_days[-indexes, ]

xgbtrain_x<- data.matrix(xgbtrain[, -9])
xgbtrain_y<- xgbtrain[, 9]

xgbtest_x<- data.matrix(xgbtest[, -9])
xgbtest_y<- xgbtest[, 9]

xgb_train<-xgb.DMatrix(data=xgbtrain_x, label = xgbtrain_y)
xgb_test<-xgb.DMatrix(data=xgbtest_x, label = xgbtest_y)

xgb_data<-xgboost(data = xgb_train, eta=0.2, max.depth = 2, nrounds = 50)

pred_y_xgb<- predict(xgb_data, xgb_test)

mse = mean((xgbtest_y - pred_y_xgb)^2)
mae = caret::MAE(xgbtest_y, pred_y_xgb)
rmse = caret::RMSE(xgbtest_y, pred_y_xgb)
rmse = rmse(xgbtest_y, pred_y_xgb)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(xgbtest_y)
plot(x, xgbtest_y, main="w21007844: Absenteeism in days: XGBoost", xlab="Employees", ylab="Absenteeism",col = "red", type = "l")
lines(x, pred_y_xgb, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"),col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

################################################################################


###PCA
install.packages('factoextra')
library(factoextra)
library(caret)
abs_pca<-abs_hrs[c(1:8)]
pca_abs_hrs<-princomp(abs_pca, cor = TRUE, scores = TRUE)
fviz_eig(pca_abs_hrs)
fviz_pca_var(pca_abs_hrs, col.var = 'contrib', repel = TRUE)
fviz_pca_biplot(pca_abs_hrs, repel = TRUE)
fviz_cos2(pca_abs_hrs,choice='var',axes=1:2)
################################################################################
#SVR
install.packages('e1071')
library(e1071)
library(caret)
svm_abs_hrs<- abs_hrs
svm_indexes<-createDataPartition(svm_abs_hrs$Absent, p=0.85, list = F)
svmtrain<-svm_abs_hrs[svm_indexes, ]
svmtest<-svm_abs_hrs[-svm_indexes, ]

model_reg = svm(Absent~., data=svmtrain)
pred = predict(model_reg, svmtest)

x<-1:length(svmtest$Absent)
plot(x, svmtest$Absent,main="w21007844: Absenteeism in hours: SVR", xlab="Employees", ylab="Absenteeism", pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

# accuracy check 
mse = mse(svmtest$Absent, pred)
mae = MAE(svmtest$Absent, pred)
rmse = RMSE(svmtest$Absent, pred)
r2 = R2(svmtest$Absent, pred, form = "traditional")

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse, " R2: ", r2)

#days
svm_abs_days<- abs_days%>%filter(Absent<2)
svm_indexes<-createDataPartition(svm_abs_days$Absent, p=0.7, list = F)
svmtrain<-svm_abs_days[svm_indexes, ]
svmtest<-svm_abs_days[-svm_indexes, ]

model_reg = svm(Absent~., data=svmtrain)
pred = predict(model_reg, svmtest)

x<-1:length(svmtest$Absent)
plot(x, svmtest$Absent, main="w21007844: Absenteeism in days: SVR", xlab="Employees", ylab="Absenteeism",pch=18, col="red")
lines(x, pred, lwd="1", col="blue")

# accuracy check 
mse = mse(svmtest$Absent, pred)
mae = MAE(svmtest$Absent, pred)
rmse = RMSE(svmtest$Absent, pred)
r2 = R2(svmtest$Absent, pred, form = "traditional")

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse, " R2: ", r2)
#######

test<-abs

ggplot(data = test, aes(x=Absenteeism.time.in.hours)) + geom_histogram(fill=1:49, binwidth = 2.5) + labs(x='Absenteeism.time.in.hours', y='Number of employees', title = "Histogram: Absenteeism in hours",caption = "Data: Absenteeism/ w21007844")

test$Transportation.expense<-ifelse(test$Transportation.expense>=118 & test$Transportation.expense<=148,'118-148', 
                                    ifelse(test$Transportation.expense>=148 & test$Transportation.expense<=178,'148-178',  
                                           ifelse(test$Transportation.expense>=178 & test$Transportation.expense<=208,'178-208',  
                                                  ifelse(test$Transportation.expense>=208 & test$Transportation.expense<=238,'208-238',  
                                                         ifelse(test$Transportation.expense>=238 & test$Transportation.expense<=268,'238-268',  
                                                                ifelse(test$Transportation.expense>=268 & test$Transportation.expense<=298,'268-298',  
                                                                       ifelse(test$Transportation.expense>=298 & test$Transportation.expense<=328,'298-328',  
                                                                              ifelse(test$Transportation.expense>=328 & test$Transportation.expense<=358,'328-358','358-388' 
                                                                              ))))))))

ggplot(data = test, aes(x= Transportation.expense, fill=Transportation.expense)) + geom_bar() + labs ( y= 'Number of Employees', x='Transportation Expense', title = "Barplot: Transportation Expense",caption = "Data: Absenteeism/ w21007844")
plot1<-ggplot(data = Test_var, aes(x= Disciplinary.failure, fill=Disciplinary.failure)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Number of Employees', x='Disciplinary.failure', title = "Barplot: Disciplinary Failure in general")
plot2<-ggplot(data = ds_hrs, aes(x= Disciplinary.failure, fill=Disciplinary.failure)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= ' ', x='Disciplinary.failure', title = "Barplot: Disciplinary Failure for absenteeism less than 8 and Not 0")
plot3<-ggplot(data = ds_days, aes(x= Disciplinary.failure, fill=Disciplinary.failure)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Number of Employees', x='Disciplinary.failure', title = "Barplot: Disciplinary Failure for absenteeism greater than 8")
plot4<-ggplot(data = ds_0days, aes(x= Disciplinary.failure, fill=Disciplinary.failure)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= ' ', x='Disciplinary.failure', title = "Barplot: Disciplinary Failure for no absenteeism",caption = "Data: Absenteeism/ w21007844")

ggarrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)

prop.table(table(Test_var$Disciplinary.failure))
prop.table(table(ds_hrs$Disciplinary.failure))
prop.table(table(ds_days$Disciplinary.failure))
prop.table(table(ds_0days$Disciplinary.failure))

edu1<-ggplot(data = Test_var, aes(x= Education, fill=Education)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Number of Employees', x='Education', title = "Barplot: Education in general")
edu2<-ggplot(data = ds_hrs, aes(x= Education, fill=Education)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= ' ', x='Education', title = "Barplot: Education for absenteeism less than 8 and Not 0")
edu3<-ggplot(data = ds_days, aes(x= Education, fill=Education)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Number of Employees', x='Education', title = "Barplot: Education for absenteeism greater than 8")
edu4<-ggplot(data = ds_0days, aes(x= Education, fill=Education)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= ' ', x='Education', title = "Barplot: Education for no absenteeism",caption = "Data: Absenteeism/ w21007844")

ggarrange(edu1, edu2, edu3, edu4, ncol=2, nrow=2)

Test_var$Children<-as.character(Test_var$Children)
Test_var$Pet<-as.character(Test_var$Pet)
child<-ggplot(data = Test_var, aes(x = Children, fill=Children)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees with children', x='Children', title = "Barplot: Children")
pet<-ggplot(data = Test_var, aes(x = Pet, fill=Pet)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees with pets', x='Pets', title = "Barplot: Pets",caption = "Data: Absenteeism/ w21007844")

ggarrange(child, pet, ncol=2, nrow=1)


Test_var$Smoker<-ifelse(Test_var$Smoker==1,2,Test_var$Smoker)
Test_var$drugs<-Test_var$Smoker+Test_var$Drinker
Test_var$drugs<-ifelse(Test_var$drugs==0, 'No intake',
                      ifelse(Test_var$drugs==1, 'Drinks',
                            ifelse(Test_var$drugs==2, 'Smokes','Both')))


drugs1<-ggplot(data = Test_var, aes(x = drugs, fill=drugs)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees', x='', title = "Barplot: Intake in general")
drugs2<-ggplot(data = ds_hrs, aes(x = drugs, fill=drugs)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= '', x='', title = "Barplot: Intake for absenteeism less than 8 and Not 0")
drugs3<-ggplot(data = ds_days, aes(x = drugs, fill=drugs)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees', x='Intake', title = "Barplot: Intake for absenteeism greater than 8")
drugs4<-ggplot(data = ds_0days, aes(x = drugs, fill=drugs)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= '', x='Intake', title = "Barplot: Intake for no absenteeism",caption = "Data: Absenteeism/ w21007844")

ggarrange(drugs1, drugs2, drugs3, drugs4, ncol=2, nrow=2)

Test_var$Body.mass.index<-ifelse(Test_var$Body.mass.index<18.5, 'Underweight',
                            ifelse(Test_var$Body.mass.index>=18.5 & Test_var$Body.mass.index<=24.9, 'Normal',
                              ifelse(Test_var$Body.mass.index>=25 & Test_var$Body.mass.index<=29.9, 'Overweight',
                                ifelse(Test_var$Body.mass.index>=30 & Test_var$Body.mass.index<=34.9, 'Obese', 'Extremely Obese' 
                                     ))))

bmi1<-ggplot(data = Test_var, aes(x = Body.mass.index, fill=Body.mass.index)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees', x='', title = "Barplot: BMI all employees")
bmi2<-ggplot(data = ds_hrs, aes(x = Body.mass.index, fill=Body.mass.index)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= '', x='', title = "Barplot: BMI for absenteeism less than 8 and Not 0")
bmi3<-ggplot(data = ds_days, aes(x = Body.mass.index, fill=Body.mass.index)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= 'Employees', x='Intake', title = "Barplot: BMI for absenteeism greater than 8")
bmi4<-ggplot(data = ds_0days, aes(x = Body.mass.index, fill=Body.mass.index)) + geom_bar()+scale_fill_brewer(palette = 'Pastel2') + labs ( y= '', x='Intake', title = "Barplot: BMI for no absenteeism",caption = "Data: Absenteeism/ w21007844")

ggarrange(bmi1, bmi2, bmi3, bmi4, ncol=2, nrow=2)

prop.table(table(Test_var$Body.mass.index))
prop.table(table(Test_var$drugs))
ds_hrs<-Test_var%>%filter(Absenteeism.time.in.hours<8 & Absenteeism.time.in.hours>0)
ds_days<-Test_var%>%filter(Absenteeism.time.in.hours>=8)
ds_0days<-Test_var%>%filter(Absenteeism.time.in.hours==0)

boxplot(abs_days$Absent)

scl_ft<-abs
scl_ft$Spent.per.km<-rs_function(scl_ft$Spent.per.km)
scl_ft$productivity<-rs_function(scl_ft$productivity)
scl_ft$Join_age<-rs_function(scl_ft$Join_age)
scl_ft$Responsibilty<-rs_function(scl_ft$Responsibilty)
scl_ft$Intake<-rs_function(scl_ft$Intake)
scl_ft$Education<-rs_function(scl_ft$Education)
scl_ft$Body.mass.index<-rs_function(scl_ft$Body.mass.index)
