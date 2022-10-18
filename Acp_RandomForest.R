```{r}
install.packages("randomForest")
library(randomForest)


```{r}
acp.train <- read.csv(file.choose())
str(acp.train)
acp.test <- read.csv(file.choose())
str(acp.test)
nrow(acp.test)
```{r}#combining the two data sets to make them compatible
acp.train$IsTrainSet <- TRUE
acp.test$IsTrainSet <- FALSE
acp.test$NonShort <- NA
acp.full <- rbind(acp.train, acp.test)
str(acp.full)

```{r} #cleaning data
acp.full[acp.full$Aged_d=='', "Aged_d"]
table(is.na(acp.full$Aged_d))
#S is the mode, replace the 2 blanks with the mode
acp.full[acp.full$Embarked=='', "Embarked"] <- 'S'
table(acp.full$Embarked)

#fix the age missing values
table(is.na(acp.full$StartAge))
age.median <- median(acp.full$StartAge, na.rm = TRUE)
acp.full[is.na(acp.full$StartAge), "StartAge"] <- age.median

#fix fare
table(is.na(acp.full$Fare))
fare.median <- median(acp.full$Fare, na.rm = TRUE)
acp.full[is.na(acp.full$Fare), "Fare"] <- fare.median


#check for null values
table(is.na(acp.full$StartAge))
table(is.na(acp.full$CoApp))
table(is.na(acp.full$ResidenceCounty))
table(is.na(acp.full$ClassificationType))
table(is.na(acp.full$DV))
table(is.na(acp.full$SA))
table(is.na(acp.full$STK))
table(is.na(acp.full$TFK))
table(is.na(acp.full$CJ))
table(is.na(acp.full$LEA))
table(is.na(acp.full$Gender))
table(is.na(acp.full$PRM))
table(is.na(acp.full$PL))
table(is.na(acp.full$OwnsPhone))
table(is.na(acp.full$Alias))
table(is.na(acp.full$Aged_d))
table(is.na(acp.full$Email))
table(is.na(acp.full$PRV))
table(is.na(acp.full$CoPRV))
table(is.na(acp.full$COA))


```{r} #cleaning the data with categorical casting, every column except survive
acp.full$StartAge <- as.integer(acp.full$StartAge)
acp.full$CoApp <- as.factor(acp.full$CoApp)
acp.full$ResidenceCounty <- as.factor(acp.full$ResidenceCounty)
acp.full$ClassificationType <- as.factor(acp.full$ClassificationType)
acp.full$Metro <- as.factor(acp.full$Metro)
acp.full$DV <- as.factor(acp.full$DV)
acp.full$SA <- as.factor(acp.full$SA)
acp.full$STK <- as.factor(acp.full$STK)
acp.full$TFK <- as.factor(acp.full$TFK)
acp.full$CJ <- as.factor(acp.full$CJ)
acp.full$LEA <- as.factor(acp.full$LEA)
acp.full$Gender <- as.factor(acp.full$Gender)
acp.full$PRM <- as.factor(acp.full$PRM)
acp.full$PL <- as.factor(acp.full$PL)
acp.full$OwnsPhone <- as.factor(acp.full$OwnsPhone)
acp.full$Alias <- as.factor(acp.full$Alias)
acp.full$Aged_d <- as.integer(acp.full$Aged_d)
acp.full$Email <- as.factor(acp.full$Email)
acp.full$PRV <- as.factor(acp.full$PRV)
acp.full$CoPRV <- as.factor(acp.full$CoPRV)
acp.full$COA <- as.factor(acp.full$COA)
acp.full$Rmail <- as.factor(acp.full$Rmail)
acp.full$Rpackage <- as.factor(acp.full$Rpackage)
str(acp.full)

```{r} #splitting them back apart
acp.train<-acp.full[acp.full$IsTrainSet==TRUE,]
acp.test<-acp.full[acp.full$IsTrainSet==FALSE,]

acp.train$NonShort <- as.factor(acp.train$NonShort)
str(acp.train)

```{r} #creating the formula and predictive model
NonShort.equation <- "NonShort ~ StartAge + Gender + CoApp + ResidenceCounty + ClassificationType + PRM + PL + OwnsPhone + Alias + Aged_d + Email + PRV + CoPRV + COA + LEA"
NonShort.formula <- as.formula(NonShort.equation)
acp.model <- randomForest(formula = NonShort.formula, data = acp.train, ntree = 500, mtry = 3, nodeside = 0.01 * nrow(acp.test))

features.equation <- "StartAge + Gender + CoApp + ResidenceCounty + ClassificationType + PRM + PL + OwnsPhone + Aged_d + Email + PRV + CoPRV + COA + LEA"
NonShort <- predict(acp.model, newdata = acp.test)

#narrowing
NonShort.equation <- "NonShort ~ StartAge + Gender + ResidenceCounty + ClassificationType + PRM + OwnsPhone + Aged_d + Email + PRV + CoPRV + COA + LEA + Metro + Rmail + Rpackage + Alias + DV + SA + STK + TFK + CJ + PL + CoApp"
NonShort.formula <- as.formula(NonShort.equation)
acp.model <- randomForest(formula = NonShort.formula, data = acp.train, ntree = 500, mtry = 3, nodeside = 0.01 * nrow(acp.test))

features.equation <- "StartAge + Gender + ResidenceCounty + ClassificationType + PRM + OwnsPhone + Aged_d + Email + PRV + CoPRV + COA + LEA + Metro + Rmail + Rpackage + Alias + DV + SA + STK + TFK + CJ + PL + CoApp"
NonShort <- predict(acp.model, newdata = acp.test)

#different variable tries
NonShort.equation <- "NonShort ~ StartAge + Gender + ResidenceCounty + ClassificationType + PRM + Email + PRV + CoPRV + COA + Rpackage + Alias + DV + SA + CJ + PL"
NonShort.formula <- as.formula(NonShort.equation)
acp.model <- randomForest(formula = NonShort.formula, data = acp.train, ntree = 500, mtry = 3, nodeside = 0.01 * nrow(acp.test))

features.equation <- "NonShort ~ StartAge + Gender + ResidenceCounty + ClassificationType + PRM + Email + PRV + CoPRV + COA + Rpackage + Alias + DV + SA + CJ + PL"
NonShort <- predict(acp.model, newdata = acp.test)



NonShort
table(NonShort)
PMB <- acp.test$ï..PMB
output.df <- as.data.frame(PMB)
output.df$NonShort <- NonShort
output.df

write.csv(output.df, file="", row.names = FALSE)


#Good Indicators 70%+ or 30%-
Email
CoPRV
PRV
Alias
Rpackage
CJ
PL
PRM


#meh indicators 60-69% or 30-39%
COA
SA
Gender
DV
Rmail

#bleh indicators 51-59% or 40-49%
STK
TFK
LEA
OwnsPhone
Metro


#bad, do not use ~50%
CoApp




