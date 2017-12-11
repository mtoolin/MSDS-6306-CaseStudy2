



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

# Create graph
ggplot(yacmonthly,aes(x = YearsAtCompanycut, y = MeanMonthly)) + 
  xlab("Years at Company") + 
  ylab("Average Monthly Income") +
  geom_bar(aes(fill = Attrition), stat = "identity", position = position_dodge(NULL)) +
  geom_text(aes(label = MeanMonthly, group=Attrition), position = position_dodge(.9),vjust = -.5, size=5) +
  scale_fill_brewer(palette="Paired") + 
  theme_minimal()+
  theme(axis.title = element_text(size=12, face = "bold"), axis.text.x = element_text(face = "bold"))
