dataset=read.csv2(choose.files(), header = T)
str(dataset)
summary(dataset)
tim1= dataset$Time_Saved1
tim2= dataset$Time_Saved2
tim3= dataset$Time_Saved3
tim4= dataset$Time_Saved4
timalpha= data.frame(tim1,tim2,tim3,tim4)
library(psych)
psych::alpha(timalpha)
Ret1=dataset$Retention_1
Ret2=dataset$Retention_2
Retalpha= data.frame(Ret1, Ret2)
psych::alpha(Retalpha)
Mot1=dataset$Motivation_1
Mot2=dataset$Motivation_2
Mot3=dataset$Motivation_3
Mot4=dataset$Motivation_4
Motalpha= data.frame(Mot1, Mot2, Mot3, Mot4)
psych::alpha(Motalpha)
Pro1=dataset$Productivity_1
Pro2=dataset$Productivity_2
Pro3=dataset$Productivity_3
Pro4=dataset$Productivity_4
Proalpha= data.frame(Pro1, Pro2, Pro3, Pro4)
psych::alpha(Proalpha)
Abs1=dataset$Absenteeism_1
Abs2=dataset$Absenteeism_2
Absalpha= data.frame(Abs1, Abs2)
psych::alpha(Absalpha)

#in the end, i wouldn't consider Retention cause it has low cronbach, so i would just delete the variable.
#here I swapped the 2 methods to proceed, first one will be just adding the different values of each variable and dividing for the N* of questions. (Here i didn't swap the dichotomus, it still has values 1-2)
#second method would be of performing a PCA for each variable to reduce the variables for the final regression.

#First method
Times_Saved_Mean=(dataset$Time_Saved1 + dataset$Time_Saved2 + dataset$Time_Saved3 + dataset$Time_Saved4)/4
Motivation_Mean=(dataset$Motivation_1 + dataset$Motivation_2 + dataset$Motivation_3 + dataset$Motivation_4 + dataset$Motivation_5)/5
Productivity_Mean=(dataset$Productivity_1 + dataset$Productivity_2 + dataset$Productivity_3 + dataset$Productivity_4)/4
Absenteeism_Mean=(dataset$Absenteeism_1 + dataset$Absenteeism_2)/2
dataset_With_Means= cbind.data.frame(dataset, Times_Saved_Mean, Motivation_Mean, Productivity_Mean, Absenteeism_Mean)
str(dataset_With_Means)
#now calculate the model with the main variables
Model_H1_Means= lm(W.L_Diff_Employer ~ Healthcare_Expenses + Times_Saved_Mean + Motivation_Mean + Productivity_Mean + Absenteeism_Mean , data=dataset_With_Means)
summary(Model_H1_Means)
#Adding the control variables Age, gender, location,n°employee,sector
Model_H1_Means_Control= lm(W.L_Diff_Employer ~ Healthcare_Expenses + Times_Saved_Mean + Motivation_Mean + Productivity_Mean + Absenteeism_Mean + Age + Gender + Edu_Level + Employer_Sector + Employer_Location + N_employee_Employer , data=dataset_With_Means)
summary(Model_H1_Means_Control)


#Second method

#computing PCA on all the variables except healthcare that has single item and retention that we excluded with the cronbach

timesaved=princomp(dataset[,14:17], cor= TRUE, scores= TRUE)
summary(timesaved)
motivation=princomp(dataset[,20:24], cor= TRUE, scores= TRUE)
summary(motivation)
productivity=princomp(dataset[,25:28], cor= TRUE, scores= TRUE)
summary(productivity)
Absenteeism=princomp(dataset[,29:30], cor= TRUE, scores=TRUE)
summary(Absenteeism)

#from the PCA, we can see the cumulative proportion and aim at ~ 70% cumulative
timesaved_PCA_1=timesaved$scores[,1]
timesaved_PCA_2=timesaved$scores[,2]
motivation_PCA_1=motivation$scores[,1]
productivity_PCA_1=productivity$scores[,1]
productivity_PCA_2=productivity$scores[,2]
Absenteeism_PCA_1=Absenteeism$scores[,1]
PCA_add=data.frame(timesaved_PCA_1, timesaved_PCA_2, motivation_PCA_1, productivity_PCA_1, productivity_PCA_2, Absenteeism_PCA_1)
str(PCA_add)
Dataset_PCA=cbind.data.frame(dataset, PCA_add)
str(Dataset_PCA)

#to add validity we perform Correlations with the pca
cor(timesaved$scores[,1],dataset[, c(14:17)])
cor(timesaved$scores[,2],dataset[, c(14:17)])
cor(motivation$scores[,1],dataset[, c(20:24)])
cor(productivity$scores[,1], dataset[,25:28])
cor(productivity$scores[,1], dataset[,25:28])
cor(Absenteeism$scores[,1], dataset[, 29:30])


#now we estimate the regression model
Model_H1_PCA=lm(W.L_Diff_Employer ~ timesaved_PCA_1 + timesaved_PCA_2 + motivation_PCA_1 + productivity_PCA_1 +productivity_PCA_2 + Absenteeism_PCA_1 + Healthcare_Expenses, data=Dataset_PCA)
summary(Model_H1_PCA)

#now add the control variables Age, gender, location,n°employee,sector
Model_H1_PCA_Control=lm(W.L_Diff_Employer ~ timesaved_PCA_1 + timesaved_PCA_2 + motivation_PCA_1 + productivity_PCA_1 +productivity_PCA_2 + Absenteeism_PCA_1 + Healthcare_Expenses + Age + Gender + Edu_Level + Employer_Sector + Employer_Location + N_employee_Employer, data=Dataset_PCA)
summary(Model_H1_PCA_Control)



