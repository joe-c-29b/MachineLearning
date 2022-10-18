```{r}
install.packages("randomForest")
library(randomForest)


```{r}
titanic.train <- read.csv(file.choose())
str(titanic.train)
titanic.test <- read.csv(file.choose())
str(titanic.test)

```{r}#combining the two data sets to make them compatible
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)
str(part.full)

```{r} #cleaning data
titanic.full[titanic.full$Embarked=='', "Embarked"]
table(titanic.full$Embarked)
#S is the mode, replace the 2 blanks with the mode
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'
table(titanic.full$Embarked)

#fix the age missing values
table(is.na(titanic.full$Age))
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#fix fare
table(is.na(titanic.full$Fare))
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median


```{r} #cleaning the data with categorical casting, every column except survive
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

str(titanic.full)

```{r} #splitting them back apart
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

```{r} #creating the formula and predictive model
Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
titanic.model <- randomForest(formula = Survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodeside = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
output.df


write.csv(output.df, file="", row.names = FALSE)





```{r} #categorical casting of the test group
part.new$CoApp <- as.factor(part.new$CoApp)
part.new$ResidenceCounty <- as.factor(part.new$ResidenceCounty)
part.new$DV <- as.factor(part.new$DV)
part.new$SA <- as.factor(part.new$SA)
part.new$STK <- as.factor(part.new$STK)
part.new$TFK <- as.factor(part.new$TFK)
part.new$CJ <- as.factor(part.new$CJ)
part.new$LEA <- as.factor(part.new$LEA)
part.new$Gender <- as.factor(part.new$Gender)
part.new$PRM <- as.factor(part.new$PRM)
part.new$PL <- as.factor(part.new$PL)
part.new$OwnsPhone <- as.factor(part.new$OwnsPhone)
part.new$StartAge <- as.numeric(part.new$StartAge)
part.new$NonShort <- as.factor(part.new$NonShort)




str(part.his)
str(part.new)
