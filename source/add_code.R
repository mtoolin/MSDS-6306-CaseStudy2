library("readxl", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")
library("dplyr", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")
library("tibble", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")
library("ggplot2", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")
library("GGally", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")
library("reshape2", lib.loc="C:/Users/HPProLaptop/Anaconda3/R/library")


#add columns to data frame to get groupings
Agecut <- cut(employeeData$Age, breaks=c(17,30,36,43,100), labels = c("18-30","30-36","36-43", "43+"))
MonthlyIncomecut <- cut(employeeData$MonthlyIncome, breaks=c(1008,2911,4919,8379,20000), labels = c("1009-2911","2912-4919","4920-8379", "8380+"))
cuts_employeeData <- cbind(employeeData, Agecut, MonthlyIncomecut)



#attrition monthly income grouped by years at company
ggplot(yacmonthly,aes(x = YearsAtCompany, y = MeanMonthly)) + 
  xlab("Years at Company") + 
  ylab("Average Monthly Income") +
  geom_bar(aes(fill = Attrition), stat = "identity", position = position_dodge(NULL)) +
  geom_text(aes(label = MeanMonthly, group=Attrition), position = position_dodge(.9),vjust = -.5, size=5) +
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.x = element_text(face = "bold"))



#code for cross-comparisons with jobsatisfaction, environmentsatisfaction, worklifebalance, relationshipsatisfaction
RelEnvSat <- lapply(employeeData, function(x){aggregate(EnvironmentSatisfaction~x, data=employeeData, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelWLB <- lapply(employeeData, function(x){aggregate(WorkLifeBalance~x, data=employeeData, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelJobSat <- lapply(employeeData, function(x){aggregate(JobSatisfaction~x, data=employeeData, FUN=function(x) c(mean=mean(x), count=length(x)))})
RelRelSat <- lapply(employeeData, function(x){aggregate(RelationshipSatisfaction~x, data=employeeData, FUN=function(x) c(mean=mean(x), count=length(x)))})

#notes on significant comparisons
#RelEnvSat:6,16
#montlyincome vs jobsatisfaciton
#***environmentsatisfaction vs years with current manager, couple 2s with long time employees
#Work Life Balance vs Age score trends down
#Work Life Balance vs Department best in HR
#Work Life Balance vs Performance PercentSalaryHike best is highest percent hike at 25
#Work Life Balance vs Years at company downward trend
#****Work Life Balance vs YearswithCurrManager 3 and 3.5 for longtime employees
#range of jobsatisfaction even across jobroles
#JOb satisfaction vs totalworking years trends down till end
#JOB SATISFaction vs yearsat company trends down till end
#job satisfaction years in current role al over the palce
#job satsifaction vs years with current manager a 1?
#all relations vs distance from home interesting scores further out
#relaionship satisfaction vs EducationField and JobRole human resource tops at 3 sales people lowest
#relationship satisfaction vs total working years 4 at end
