#Import the Titanic Dataset from the link Titanic Data Set.

library(titanic)

Training <- titanic_train
str(Training)
Training$mode <- 'Training'

Testing <- titanic_test
str(Testing)
Testing$Survived <- NA
Testing$mode <- 'Testing'

GCM <- titanic_gender_class_model
GM <- titanic_gender_model

dataset <- rbind(Training, Testing)
dataset$mode <- as.factor(dataset$mode)
head(dataset$Name)

#Perform the following:

# a. Preprocess the passenger names to come up with a list of titles that represent families
# and represent using appropriate visualization graph.

x <- strsplit(as.character(dataset$Name), split = ', ')
head(x)

## preprocessing function
extractAndConvertTitles <- function(dataset) {
  titles <- apply(dataset, 1, function(row) {
    strsplit(strsplit(as.character(row['Name']), ', ')[[1]][2], '\\.')[[1]][1]
  })
  keep_titles <- c('Dr', 'Master', 'Miss', 'Mr', 'Mrs')
  replacementTitles <-
    list(
      Mlle = 'Miss',
      Mme = 'Mrs',
      Sir = 'Mr',
      Ms = 'Miss'
    )
  for (r_title in names(replacementTitles)) {
    titles[titles == r_title] <- replacementTitles[[r_title]]
  }
  # convert all the other remaining titles to 'Rare Title'
  titles[!titles %in% keep_titles] = 'Rare Title'
  dataset$Title <- as.factor(titles)
  invisible(dataset)
}
dataset <- extractAndConvertTitles(dataset)
dataset$Name <- NULL
summary(dataset$Title[dataset$mode == 'Training']) # Better!

head(dataset)

# b. Represent the proportion of people survived from the family size using a graph.

familySize <- dataset$SibSp + dataset$Parch + 1
familySizeClass = array(dim = length(familySize))
familySizeClass[familySize == 1] = 'Small'
familySizeClass[familySize >= 2 & familySize <= 4] = 'Medium'
familySizeClass[familySize > 4] = 'Big'

dataset$FamilySize <- as.factor(familySizeClass)

library(ggplot2)
ggplot(dataset, aes(dataset$FamilySize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('Family Size Impact on Survival') +
  labs(y = '%')

# c. Impute the missing values in Age variable using Mice Library, create two different
# graphs showing Age distribution before and after imputation.

library(magrittr)
library(dplyr)

Training %>% (Sex) %>%
  summarise(meanAge = mean(Age, na.rm = TRUE),
            medianAge = median(Age, na.rm = TRUE))

ageEvaluationSex_Pclass <- Training %>% group_by(Sex, Pclass) %>%
  summarise(meanAge = mean(Age, na.rm = TRUE),
            medianAge = median(Age, na.rm = TRUE))
ageEvaluationSex_Pclass


ageImputBySex_Pclass <- function(dataset, averageAgeStats) {
  calculateImputedAge <- function(dfRow, ageEvaluationSex_Pclass) {
    filterIndex <-
      ageEvaluationSex_Pclass$Sex == dfRow['Sex'] &
      ageEvaluationSex_Pclass$Pclass == dfRow['Pclass']
    impAge <- ageEvaluationSex_Pclass[filterIndex,]$meanAge
  }
  # send each age missing row for imputation
  dataset$Age[is.na(dataset$Age)] <-
    apply(dataset[is.na(dataset$Age),], 1, calculateImputedAge, ageEvaluationSex_Pclass)
  invisible(dataset)
}

dataset <- ageImputBySex_Pclass(dataset, averageAgeStats)
