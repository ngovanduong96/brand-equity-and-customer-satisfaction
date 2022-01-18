########## DISSERTATION R SCRIPT ##########

#####preparation
#set working directory
setwd("C:/Users/ngova/OneDrive/Máy tính/Dissertation")
.libPaths("C:/Users/ngova/OneDrive/Máy tính/data_test")

#==========================================
##### 1. Open the data set and get an overview

### import the data from an EXCEL file 
install.packages("readr")
install.packages("tidyverse")
library(readxl)   # to read an EXCEL file

cbbe <- read_excel("Dataset.xlsx")

### get an overview of the data
glimpse(cbbe)
head(cbbe)
summary(cbbe) # Dataset has a lot of non-integer values and not consistent with corresponding nominal values-> have to clean data set

#========================================
##### 2. Data preparation
#### Data cleaning
colSums(is.na(cbbe))
sum(is.na(cbbe)) #missing data or "NA" are recorded 
### creat a subset of cbbe without non-integer and inconsistent values
# Accepted values for Likert scale items
likert <- as.integer(c(1,2,3,4,5,6,7))
# Accepted values for GENDER variable
gender <- as.integer(c(1,2))
# Accepted values for AGE variable
age <- as.integer(c(1,2,3,4))
# Accepted values for PRO_CAT variable
product <- as.integer(c(1,2,3,4,5,6))
# Accepted values for EDUCATE variable
educate <- as.integer(c(1,2,3,4,5))
# Accepted values for INCOME variable
income <- as.integer(c(1,2,3,4,5,6,7,8))
#### Create a new subset including all items for research
final_cbbe <- subset(cbbe, cbbe$LOYAL1 %in% likert & cbbe$LOYAL2 %in% likert & cbbe$LOYAL3 %in% likert & cbbe$SACRIF1 %in% likert & cbbe$SACRIF2 %in% likert & cbbe$SACRIF3 %in% likert
                     & cbbe$AWAR1 %in% likert & cbbe$AWAR2 %in% likert & cbbe$AWAR3 %in% likert & cbbe$ASSOC1 %in% likert & cbbe$ASSOC2 %in% likert & cbbe$ASSOC3 %in% likert
                     & cbbe$ATTACH1 %in% likert & cbbe$ATTACH2 %in% likert & cbbe$SELF1 %in% likert& cbbe$SELF2 %in% likert& cbbe$RELEV3 %in% likert
                     & cbbe$QUAL1 %in% likert & cbbe$QUAL2 %in% likert & cbbe$QUAL3 %in% likert & cbbe$TRUST1 %in% likert & cbbe$TRUST2 %in% likert & cbbe$TRUST3 %in% likert
                     & cbbe$PRICE1 %in% likert & cbbe$PRICE2 %in% likert & cbbe$PRICE3 %in% likert
                     & cbbe$SATISFAC %in% likert & cbbe$RELIAB1 %in% likert & cbbe$RELIAB2 %in% likert & cbbe$RELIAB3 %in% likert
                     & cbbe$GENDER %in% gender
                     & cbbe$AGE %in% age 
                     & cbbe$PROD_CAT %in% product
                     & cbbe$EDUCATE %in% educate
                     & cbbe$INCOME %in% income)
summary(final_cbbe) # New data set is perfect for investigating , new obs N = 1512

#### Reliability Test
library(tidyverse)
library(psych) #for Cronbach's Alpha

### 2.1. Brand Loyalty (BL)
cor_brand_loyalty <- final_cbbe[, c("LOYAL1","LOYAL2","LOYAL3", "SACRIF1","SACRIF2","SACRIF3")]
alpha(cor_brand_loyalty, check.keys = TRUE)
#alpha = 0.95>0.7 => reliable => generate the composite score "BL"
final_cbbe$BL = rowMeans(final_cbbe[,c("LOYAL1","LOYAL2","LOYAL3", "SACRIF1","SACRIF2","SACRIF3")], na.rm = FALSE)
str(final_cbbe$BL)

### 2.2. Brand Awareness (BAw)
cor_awareness <- final_cbbe[, c("AWAR1","AWAR2","AWAR3", "ASSOC1","ASSOC2", "ASSOC3")]
alpha(cor_awareness, check.keys = TRUE)
#alpha = 0.84 > 0.7 => reliable => generate the composite score "BAw"
final_cbbe$BAw = rowMeans(final_cbbe[,c("AWAR1","AWAR2","AWAR3","ASSOC1","ASSOC2", "ASSOC3")], na.rm = FALSE)
str(final_cbbe$BAw)

### 2.3. Brand Association (BAs)
cor_association <- final_cbbe[, c("ATTACH1", "ATTACH2", "SELF1", "SELF2", "RELEV3")]
alpha(cor_association, check.keys = TRUE)
#alpha = 0.93 > 0.7 => reliable => generate the composite score "BAs"
final_cbbe$BAs = rowMeans(final_cbbe[,c("ATTACH1", "ATTACH2", "SELF1", "SELF2", "RELEV3")], na.rm = FALSE)
str(final_cbbe$BAs)

### 2.4. Perceived Quality (PQ)
cor_perceived_quality <- final_cbbe[, c("QUAL1","QUAL2","QUAL3", "TRUST1", "TRUST2", "TRUST3")]
cor_perceived_quality
alpha(cor_perceived_quality, check.keys = TRUE)
#alpha = 0.97 > 0.7 => reliable => generate the composite score "PQ"
final_cbbe$PQ = rowMeans(final_cbbe[,c("QUAL1","QUAL2","QUAL3", "TRUST1", "TRUST2", "TRUST3")], na.rm = FALSE)
str(final_cbbe$PQ)

### 2.5. Price Premium (Pri)
cor_price_premium <-  final_cbbe[, c("PRICE1","PRICE2", "PRICE3")]
cor_price_premium
alpha(cor_price_premium, check.keys = TRUE)
#alpha = 0.89 > 0.7 => reliable => generate the composite score "Pri"
final_cbbe$Pri <- rowMeans(final_cbbe[,c("PRICE1","PRICE2", "PRICE3")], na.rm = FALSE)
str(final_cbbe$Pri)

### 2.6. Customer Satisfaction (CS)
cor_customer_satisfaction <- final_cbbe[, c("SATISFAC","RELIAB1", "RELIAB2", "RELIAB3")]
cor_customer_satisfaction
alpha(cor_customer_satisfaction, check.keys = TRUE)
#alpha = 0.96 > 0.7 => reliable => generate the composite score "CS"
final_cbbe$CS <- rowMeans(final_cbbe[,c("SATISFAC","RELIAB1", "RELIAB2", "RELIAB3")], na.rm = FALSE)
str(final_cbbe$CS)

#========================================
##### 3. Descriptive Statistics and Demographic Analysis
install.packages("freqdist")
library(freqdist) #  Frequency Distribution
library(dplyr)    # for summary statistics and data cleaning
library(psych)    # for descriptive statistic

#### 3.1. Key variables (Outcome Variable + Explanatory Variables + Mediator)
new_cbbe <- final_cbbe[,c("BL","BAw", "BAs", "PQ", "Pri" ,"CS")]
str(new_cbbe)
descriptive_analysis <- describe(new_cbbe)
descriptive_analysis
write.csv(descriptive_analysis, "descriptive.csv")

#### 3.2. Demographic Profiles
demo_cbbe <- final_cbbe[,c("AGE", "GENDER", "EDUCATE","INCOME")]
str(demo_cbbe)
descriptive_demo <- describe(demo_cbbe)
descriptive_demo
write.csv(descriptive_demo, "descriptive_demo.csv")

## Age
class(final_cbbe$AGE) # AGE includes numeric values -> create new value "agegroup" that includes corresponding character values
final_cbbe$agegroup <- as.character(final_cbbe$AGE)
final_cbbe$agegroup<- ifelse(final_cbbe$agegroup == "1", "18-24",
                             ifelse(final_cbbe$agegroup == "2", "25-44",
                                    ifelse(final_cbbe$agegroup == "3", "45-64",
                                           ifelse(final_cbbe$agegroup == "4", "65 and older", NA))))
table(final_cbbe$agegroup)
freq_agegroup <- freqdist(final_cbbe$agegroup)
freq_agegroup
write.csv(freq_agegroup, "freq_agegroup.csv")

## Gender
class(final_cbbe$GENDER)
table(final_cbbe$GENDER) # GENDER includes numeric values -> create new value "sex" that includes corresponding character values

final_cbbe$sex <- as.character(final_cbbe$GENDER)
final_cbbe$sex <- ifelse(final_cbbe$sex == "1", "Male",
                         ifelse(final_cbbe$sex == "2", "Female",NA))
table(final_cbbe$sex)
freq_sex <- freqdist(final_cbbe$sex)
freq_sex
write.csv(freq_sex, "freq_sex.csv")

#Visualization of gender and age
library(ggplot2)  # for data visualization
table(final_cbbe$agegroup, final_cbbe$sex)
ggplot(final_cbbe, aes(x = agegroup, y = agegroup , fill= sex)) +
  geom_bar(position = "stack", stat = "identity")+
  theme(axis.title.y = element_blank())
ggsave("gender on age.png")

## Qualification
table(final_cbbe$EDUCATE)
class(final_cbbe$EDUCATE) # GENDER includes numeric values -> create new value "qualification" that includes corresponding character values

final_cbbe$qualification <- as.character(final_cbbe$EDUCATE)
final_cbbe$qualification <- ifelse(final_cbbe$qualification == "1", "Higher degree and postgraduate qualifications",
                                   ifelse(final_cbbe$qualification == "2", "Degree, or degree level equivalent",
                                          ifelse(final_cbbe$qualification == "3", "School leaving certificate",
                                                 ifelse(final_cbbe$qualification == "4", "Other qualifications",
                                                        ifelse(final_cbbe$qualification == "5", "No qualifications", NA)))))
table(final_cbbe$qualification)
freq_qualification <- freqdist(final_cbbe$qualification)
freq_qualification
write.csv(freq_qualification, "freq_qualification.csv")

## Income
table(final_cbbe$INCOME)
class(final_cbbe$INCOME) # INCOME includes numeric values -> create new value "Revenue" that includes corresponding character values
final_cbbe$revenue <- as.character(final_cbbe$INCOME)
final_cbbe$revenue<- ifelse(final_cbbe$revenue == "1", "Lower than £9,000",
                            ifelse(final_cbbe$revenue == "2", "£9,001-17,000",
                                   ifelse(final_cbbe$revenue == "3", "£17,001-25,000",
                                          ifelse(final_cbbe$revenue == "4", "£25,001- 34,000",
                                                 ifelse(final_cbbe$revenue == "5", "£34,001-42,000",
                                                        ifelse(final_cbbe$revenue == "6", "£42,001-50,000",
                                                               ifelse(final_cbbe$revenue == "7", "£50,001-59,000",
                                                                      ifelse(final_cbbe$revenue == "8", "More than £59,001",NA))))))))
table(final_cbbe$revenue)                                                     
freq_revenue <- freqdist(final_cbbe$revenue)
freq_revenue
write.csv(freq_revenue, "freq_revenue.csv")
# Visual income levels
table_revenue <- within(final_cbbe, 
                        revenue <- factor(revenue, 
                                          levels=names(sort(table(revenue), 
                                                            decreasing=FALSE))))

ggplot(table_revenue) +
  geom_bar(aes(y=revenue), width=0.5, fill="steelblue")
ggsave("revenue.png")


#### Compare Average score of BL and CS by control factors
library(dplyr)    # for summary statistics and data cleaning
# Mean of BL and CS by age
CS_by_age <- final_cbbe %>% group_by(agegroup) %>% summarize(Average_CS = mean(CS))
BL_by_age <- final_cbbe %>% group_by(agegroup) %>% summarize(Average_BL = mean(BL))
Mean_by_age <- rbind(cbind(setNames(CS_by_age, c("agegroup", "Mean")), source = "Average_CS"),
                        cbind(setNames(BL_by_age, c("agegroup", "Mean")), source = "Average_BL"))
write.csv(Mean_by_age, "Mean_by_age.csv")
ggplot(data = Mean_by_age) + 
  geom_col(mapping = aes(x= agegroup, y = Mean, fill = source), position = position_dodge())
ggsave("Mean_by_age.png")

# Mean of BL and CS by gender
CS_by_gender <- final_cbbe %>% group_by(sex) %>% summarize(Average_CS = mean(CS))
BL_by_gender <- final_cbbe %>% group_by(sex) %>% summarize(Average_BL = mean(BL))
Mean_by_gender <- rbind(cbind(setNames(CS_by_gender, c("sex", "Mean")), source = "Average_CS"),
                     cbind(setNames(BL_by_gender, c("sex", "Mean")), source = "Average_BL"))
write.csv(Mean_by_gender, "Mean_by_gender.csv")
ggplot(data = Mean_by_gender) + 
  geom_col(mapping = aes(x= sex, y = Mean, fill = source), position = position_dodge())
ggsave("Mean_by_gender.png")

# Mean of BL and CS by qualification
CS_by_qualification <- final_cbbe %>% group_by(qualification) %>% summarize(Average_CS = mean(CS))
BL_by_qualification <- final_cbbe %>% group_by(qualification) %>% summarize(Average_BL = mean(BL))
Mean_by_qualification<- rbind(cbind(setNames(CS_by_qualification, c("qualification", "Mean")), source = "Average_CS"),
                        cbind(setNames(BL_by_qualification, c("qualification", "Mean")), source = "Average_BL"))
write.csv(Mean_by_qualification, "Mean_by_qualification.csv")
ggplot(data = Mean_by_qualification) + 
  geom_col(mapping = aes(x= reorder(qualification, Mean), y = Mean, fill = source), width=0.5, position = position_dodge())+
  coord_flip()
ggsave("Mean_by_qualification.png")

# Mean of BL and CS by revenue
CS_by_revenue <- final_cbbe %>% group_by(revenue) %>% summarize(Average_CS = mean(CS))
BL_by_revenue <- final_cbbe %>% group_by(revenue) %>% summarize(Average_BL = mean(BL))
Mean_by_revenue<- rbind(cbind(setNames(CS_by_revenue, c("revenue", "Mean")), source = "Average_CS"),
                              cbind(setNames(BL_by_revenue, c("revenue", "Mean")), source = "Average_BL"))
write.csv(Mean_by_revenue, "Mean_by_revenue.csv")
ggplot(data = Mean_by_revenue) + 
  geom_col(mapping = aes(x= reorder(revenue, Mean), y = Mean, fill = source), width=0.5, position = position_dodge())+
  coord_flip()
ggsave("Mean_by_revenue.png")




#========================================
##### 4. Correlation Analysis Matrix
install.packages("corrplot")
library(corrplot)
library(dplyr)    # for summary statistics and data cleaning
library(corrr)    # for correlation matrix
library(tidyverse)# for print image with function ggsave()

new_cbbe <- final_cbbe[,c("BL","BAw", "BAs", "PQ", "Pri" ,"CS")]
str(new_cbbe)
##  Correlation Analysis Matrix
library("psych")
correlation_matrix <- corr.test (new_cbbe)
# Print Pearson Correlation Coefficient (r-value)
correlation_matrix_R <- correlation_matrix$r
write.csv(correlation_matrix_R, "r_value.csv")
# Print Sig. (P-value)
correlation_matrix_P <- correlation_matrix$p
write.csv(correlation_matrix_P, "P_value.csv")
#Visual Correlation Matrix
corrplot(correlation_matrix_R, method = "ellipse", type = "lower")
ggsave("Pearson.png")

#========================================
##### 6. Linear Mediation Analysis
library(tidyverse)# make available in current R session
library(broom) #to take the messy output of built-in functions in R

summary(final_cbbe$GENDER)
summary(final_cbbe$AGE)
summary(final_cbbe$EDUCATE)
summary(final_cbbe$INCOME)

# Create dummies for EDUCATION and GENDER variable
install.packages("fastDummies")
library("fastDummies") # for generating dummy variables for large categorical variables

final_cbbe <- dummy_cols(final_cbbe, select_columns = c("sex","qualification"))
model.Med  <- lm(CS ~ BAw + BAs + PQ + Pri + AGE + qualification + sex + INCOME, final_cbbe)
summary(model.Med) #Significant effect of BAs, PQ, Pri on CS # inSignificant effect of BAw on CS
                    #Significant effect of GENDER on CS # inSignificant effect of AGE, EDUCATE, INCOME on CS

#Print result
tidy_Med <- tidy(model.Med)
summary(tidy_Med)
write.csv(tidy_Med, "model.Med.csv")

model.Y <- lm(BL ~ BAw + BAs + PQ + Pri + AGE + qualification + sex + INCOME + CS, final_cbbe)
summary(model.Y) #Significant effect of BAw, BAs, PQ, Pri , CS on BL
#Print result
tidy_Y <- tidy(model.Y)
summary(tidy_Y)
write.csv(tidy_Y, "model.Y.csv")
#========================================
##### 6. Structural Equation Modeling (SEM)
install.packages("lavaan")
library(lavaan)

SEM.model <- '# create latent variables 
                B_Awareness =~ AWAR1+AWAR2+AWAR3+ASSOC1+ASSOC2+ASSOC3
                B_Association =~ ATTACH1+ATTACH2+SELF1+SELF2+RELEV3
                P_Quality =~ QUAL1+QUAL2+QUAL3+TRUST1+TRUST2+TRUST3
                C_Satisfaction =~ SATISFAC+RELIAB1+RELIAB2+RELIAB3
                B_Loyalty =~ LOYAL1+LOYAL2+LOYAL3+SACRIF1+SACRIF2+SACRIF3
                Price_premium =~ PRICE1 +PRICE2+PRICE3
              # Main model
                C_Satisfaction ~ a1*B_Awareness + a2*B_Association + a3*P_Quality + a4*Price_premium + AGE + sex + qualification + INCOME
                B_Loyalty ~ c1*B_Awareness + c2*B_Association + c3*P_Quality + c4*Price_premium + b*C_Satisfaction + AGE + sex + qualification + INCOME
              # Indirect effect (IDE)
                IDE_BAw := a1*b
                IDE_BAs := a2*b
                IDE_PQ  := a3*b
                IDE_Pri := a4*b
              # Total Effect (TE)
                TE_BAw := (a1*b) + c1
                TE_BAs := (a2*b) + c2
                TE_PQ  := (a3*b) + c3
                TE_Pri := (a4*b) + c4'
##Run model
SEM.fit <- sem(model = SEM.model, data = final_cbbe)
##Examine
summary(SEM.fit, standardized=TRUE, fit.measure=TRUE)
fitmeasures(SEM.fit, c("all")) #CFI = 0.877 TLI = 0.864; RMSEA = 0.094 => The model has a poor fit
##Modify Model
MI <- modificationindices(SEM.fit, sort=TRUE)  # add extra covariances into the original model

##--> Modified SEM Model
SEM.modified_model <- '# create latent variables 
                B_Awareness =~ AWAR1+AWAR2+AWAR3+ASSOC1+ASSOC2+ASSOC3
                B_Association =~ ATTACH1+ATTACH2+SELF1+SELF2+RELEV3
                P_Quality =~ QUAL1+QUAL2+QUAL3+TRUST1+TRUST2+TRUST3
                C_Satisfaction =~ SATISFAC+RELIAB1+RELIAB2+RELIAB3
                B_Loyalty =~ LOYAL1+LOYAL2+LOYAL3+SACRIF1+SACRIF2+SACRIF3
                Price_premium =~ PRICE1 +PRICE2+PRICE3
              # Main model
                C_Satisfaction ~ a1*B_Awareness + a2*B_Association + a3*P_Quality + a4*Price_premium + AGE + sex + qualification + INCOME
                B_Loyalty ~ c1*B_Awareness + c2*B_Association + c3*P_Quality + c4*Price_premium + b*C_Satisfaction + AGE + sex + qualification + INCOME
              # Indirect effect (IDE)
                IDE_BAw := a1*b
                IDE_BAs := a2*b
                IDE_PQ  := a3*b
                IDE_Pri := a4*b
              # Total Effect (TE)
                TE_BAw := (a1*b) + c1
                TE_BAs := (a2*b) + c2
                TE_PQ  := (a3*b) + c3
                TE_Pri := (a4*b) + c4
              # Covariates
                SACRIF1 ~~  SACRIF2
                ATTACH1 ~~  ATTACH2
                SACRIF2 ~~  SACRIF3
                QUAL1 ~~    QUAL2'
## Run modified model
SEM.modified_fit <- sem(model = SEM.modified_model, data = final_cbbe)
## Examine modified model
summary(SEM.modified_fit, standardized=TRUE, fit.measure=TRUE)
fitmeasures(SEM.modified_fit, c("all")) #CFI = 0.915; TLI = 0.906; RMSEA = 0.078 => The model fit is acceptable

tidy_SEM <- tidy(SEM.modified_fit)
summary(tidy_SEM)
write.csv(tidy_SEM, "model.SEM.csv")

##Visualization SEM
install.packages("semPlot")
library(semPlot)
semPaths(SEM.modified_fit,as.expression = c("nodes", "edges"), rotation=2,sizeMan=4,
         style="lisrel",curvePivot=TRUE, edge.color="black")
ggsave("SEM.png")

