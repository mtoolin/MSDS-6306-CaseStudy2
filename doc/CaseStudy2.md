# Case Study2
Michalel Toolin, Jaime Villanueva,Venkat Kasarla  
11/23/2017  



```r
rawdata.xls <- '../data/CaseStudy2-data.xlsx'
rawdata.csv <- '../data/original.csv'
refine <- read_excel(rawdata.xls,1)
refine %>% data.table::fwrite(rawdata.csv)
dframe <- read.csv(rawdata.csv)
rawdata <- dframe
# Calculate Avg years on job vs job role
mjobrole <-tapply(rawdata$YearsAtCompany, rawdata$JobRole, mean)
mjobrole.df <-data.frame(JobRole=names(mjobrole),YearsWithCompany=mjobrole)
# Calculate Number Average years on Job vs Age
ma <- tapply(rawdata$YearsAtCompany, rawdata$Age, mean)
madf<-data.frame(Age=names(ma),YearsWithCompany=ma)
madf$AgeGroup = cut(as.numeric(madf$Age),c(0,12,19,26,43), 
                    labels=c("18-29","30-36","37-43","44-60"))
```
## Introduction  
We have been provided information describing the existing employees at DDSAnalytics.  This data provides a wide breadth of information.  The current project’s goal is to ascertain the three primary reasons for attrition at the company.  During this analysis the company is also interested in any trends regarding specific jobs that are uncovered.  

We are only examining past data and no predictive analysis is provided.  

## Executive Summary  
The top three indicators for people leave the company  

- Total compensation   
- Job satisfaction   
- Career move  
  
## Data Set
- The data provided contained information on 1470 individuals in the company.  Each individual’s   record contained 35 separate pieces of information.  
- Aside from company specific data such as payrate, the data contained details that ranged from   personal information such as age and marital status to professional information such education   level and number of years in the workforce.  
- Many of the variables are categorical.  Our approach is to identify key variables and compare the  percentage of people  who left the company vs those who stayed at the company.  
  
## Exploratory Phase  
Basic statistics of employees in the company 

-  1470 employees with 35 variables describing each person  
-  237 people have left the company, 1233 people remaining  
-  Average age of employee is just under 37 years old  
-  Employees have spent just over 7 years on average at the company and have spent almost 4.25 years in their current role.  

## Cleaning and Munging  
- Mapping of the various categorical values according to Data definition  
- The data was filtered based on different Categorical variables against Attrition  
- Some variables that act as categorical variables had too many categories to do an effective   comparison.  In these cases various groups were created for data exploration.  One example is Age, where the quantiles for Age were calculated and groups based on Age were created.  These are   discussed later on.  

## Attrition based on Monthly Income  
- Grouped by quantiles for number of years at company

```r
# Create category buckets for Age, MonthlyIncome, and YearsAtCompany based on quartiles
Agecut <- cut(rawdata$Age, breaks=c(17,30,36,43,100), labels = c("18-30","30-36","36-43", "43+"))
MonthlyIncomecut <- cut(rawdata$MonthlyIncome, breaks=c(1008,2911,4919,8379,20000), labels = c("1009-2911","2912-4919","4920-8379", "8380+"))
YearsAtCompanycut <- cut(rawdata$YearsAtCompany, breaks=c(-1,3,5,9,100), labels = c("0 to 3","3 to 5","5 to 9", "9+"))

# Tack new columns created in previous step onto rawdata
rawdata <- cbind(rawdata, Agecut, MonthlyIncomecut, YearsAtCompanycut)

# Create a series of mean variables for each category of Attrition & YearsAtCompanycut
zto3yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "0 to 3")
zto3yes_mean <- as.integer((mean(zto3yes$MonthlyIncome)))
threeto5yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "3 to 5")
threeto5yes_mean <- as.integer((mean(threeto5yes$MonthlyIncome)))
fiveto9yes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "5 to 9")
fiveto9yes_mean <- as.integer((mean(fiveto9yes$MonthlyIncome)))
nineplusyes <- filter(rawdata, Attrition == "Yes", YearsAtCompanycut == "9+")
nineplusyes_mean <- as.integer((mean(nineplusyes$MonthlyIncome)))
zto3no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "0 to 3")
zto3no_mean <- as.integer((mean(zto3no$MonthlyIncome)))
threeto5no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "3 to 5")
threeto5no_mean <- as.integer((mean(threeto5no$MonthlyIncome)))
fiveto9no <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "5 to 9")
fiveto9no_mean <- as.integer((mean(fiveto9no$MonthlyIncome)))
nineplusno <- filter(rawdata, Attrition == "No", YearsAtCompanycut == "9+")
nineplusno_mean <- as.integer((mean(nineplusno$MonthlyIncome)))

# Tack a column named MeanMonthly with a 1 to hold a numeric space
addblank <- rep.int(1,1470)
rawdata <- cbind(rawdata, addblank)
names(rawdata[,39]) <- "MeanMonthly"

# Write the appropriate monthly mean value into the MonthlyMean column
for (i in 1:1470) {
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "0 to 3") {rawdata$MeanMonthly[i] <- zto3yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "3 to 5") {rawdata$MeanMonthly[i] <- threeto5yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "5 to 9") {rawdata$MeanMonthly[i] <- fiveto9yes_mean}
  if (rawdata$Attrition[i] == "Yes" & rawdata$YearsAtCompanycut[i] == "9+") {rawdata$MeanMonthly[i] <- nineplusyes_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "0 to 3") {rawdata$MeanMonthly[i] <- zto3no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "3 to 5") {rawdata$MeanMonthly[i] <- threeto5no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "5 to 9") {rawdata$MeanMonthly[i] <- fiveto9no_mean}
  if (rawdata$Attrition[i] == "No" & rawdata$YearsAtCompanycut[i] == "9+") {rawdata$MeanMonthly[i] <- nineplusno_mean}
}

# Group Attrition, YearsAtCompanycut, and MeanMonthly 
yacmonthly <- group_by(rawdata, Attrition, YearsAtCompanycut, MeanMonthly)

# Create graph of Attrition vs MonthlyIncome grouped by Years at Company
ggplot(yacmonthly,aes(x = YearsAtCompanycut, y = MeanMonthly)) + 
  xlab("Years at Company") + 
  ylab("Average Monthly Income") +
  geom_bar(aes(fill = Attrition), stat = "identity", position = position_dodge(NULL)) +
  geom_text(aes(label = MeanMonthly, group=Attrition), position = position_dodge(.9),vjust = -.5, size=5) +
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.x = element_text(face = "bold"))
```

![](CaseStudy2_files/figure-html/MonthlyIncomeAttrition-1.png)<!-- -->

## Stock Option Level vs Attrition  
- Employees who were at a stop option level of zero tended to leave the company. 

```r
ggplot(rawdata,aes(StockOptionLevel, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Stock Level")+
  theme(axis.text.x = element_text(angle=0),
        legend.position="none")+
  scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/StockOptionAttrition-1.png)<!-- -->

## Overtime vs Attrition  
- Employees who worked more overtime left the company at a higher rate.  

```r
ggplot(rawdata,aes(OverTime, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="Overtime")+
  theme(axis.text.x = element_text(angle=0),
        legend.position="none")+
   scale_y_continuous(labels=scales::percent)+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/OvertimeAttrition-1.png)<!-- -->
  
## Marital Status vs Attrition  
- Single emplyees tended to leave the company  

```r
 ggplot(rawdata,aes(MaritalStatus, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
    facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/MaritalAttrition-1.png)<!-- -->
  
## Job Level vs Attrition  
- Employees at the lowest Job Level leave the company at a higher rate  

```r
 ggplot(rawdata,aes(JobLevel, group=Attrition))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=0, hjust=1),legend.position="none")+
    facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/JobLevelAttrition-1.png)<!-- -->
  
## Job Involvement  
- More employees who rated their job involvement as a 1 or 2 left the company than stayed with the company  
- The opposite is true for employees who rated their job involvement as a 3 or 4.

```r
 rawdata$JobInvolvement <- factor(rawdata$JobInvolvement,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
ggplot(rawdata,aes(JobInvolvement, group=Attrition,fill=JobInvolvement))+
    geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),
                  y=..prop..),stat="count",vjust=-.5)+
    labs(y="Percent")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
    facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/JobInvolvementAttrition-1.png)<!-- -->

## Job Role vs Attrtion  
-  Sales Representative, Laboratory Technician roles tend to leave

```r
ggplot(rawdata,aes(JobRole, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/JobRoleAttrition-1.png)<!-- -->

## Job Satisfaction vs Attrition
-  Less Satisfied employees tend to leave  

```r
rawdata$JobSatisfaction <- factor(rawdata$JobSatisfaction,levels = c(1, 2, 3,4), labels = c("Low","Medium","High","Very High"))
ggplot(rawdata,aes(JobSatisfaction, group=Attrition))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  geom_text(aes(label=scales::percent(..prop..),
                y=..prop..),stat="count",vjust=-.5)+
  labs(y="Percent",fill="JobSatisfaction")+
  scale_y_continuous(labels=scales::percent)+
  theme(axis.text.x = element_text(angle=60, hjust=1),legend.position="none")+
  facet_grid(~Attrition)
```

![](CaseStudy2_files/figure-html/JobSatisfactionAttrition-1.png)<!-- -->

##  Age groups within the Company  
- We have more middle age workers, yet older workers have been at the company longer, as expected  


```r
ggplot(madf, aes(x=Age, y=YearsWithCompany, fill = AgeGroup)) +
  geom_bar(stat ="identity",colour="black",position="dodge")+
  labs(fill="Age",y="Avg Years With Company")+
  theme(axis.text.x = element_text(angle=90,vjust=.5))
```

![](CaseStudy2_files/figure-html/Agesummary-1.png)<!-- -->

## Which job roles stay?  
-  Management has the longest tenure, our Sales Team the least  

```r
ggplot(mjobrole.df, aes(x=JobRole, y=YearsWithCompany,fill=JobRole)) +
  geom_bar(stat ="identity",colour="black",position="dodge")+
  labs(fill="Age",y="Avg Years With Company",x="Job Role")+
  theme(axis.text.x = element_text(angle=60, hjust=1),
        legend.position="none")
```

![](CaseStudy2_files/figure-html/WhichJobsStay-1.png)<!-- -->
  
## Conclusion  
- There are a variety of reasons why people leave the company.
-  Total Compensation package drives attrition  
    - Stock Options – give people ownership
    - Monthly Income – Need to be competitive in the industry. A competitive analysis requires more data  
- We have a much higher turnover rate in Sales than anywhere else in the company  
- A feeling of ownership and job satisfaction is a contributing factor  
- A better model is required to predict if a person with certain characteristics is at risk of leaving the company
