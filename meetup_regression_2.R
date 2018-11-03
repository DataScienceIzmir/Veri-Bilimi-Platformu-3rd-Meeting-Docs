load.libraries <- c('tidyverse', 'forcats', 'stringr','DT', 'data.table', 'caTools', 'magrittr', 'janitor', 'scales', 'directlabels', 
                    'ggthemes', 'plotly', 'tidyr', 'dplyr', 'scales', 'grid', 'gridExtra', 'corrplot','VIM', 'knitr', 'vcd', 'caret', 'lubridate',
                    'ggrepel', 'reshape2', 'data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071',
                    "rCharts", "TTR", "shinydashboard", "forecast", 'MLmetrics', 'randomForest', 'rpart', 'rpart.plot', 'car', 'e1071',
                    'ROCR', 'pROC', 'glmnet', 'xgboost', 'h2o', 'irlba', 'moments', 'readr')
sapply(load.libraries, require, character = TRUE)


#Useful data quality function for missing values, I learned them from kaggle
checkColumn = function(df,colname){
  
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)
  
  
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
  
}
checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}

get_dup <- function(x) lapply(x, c) %>% duplicated %>% which 

#-------------------------------------------------

#read data

train <- read_csv("D:/MeetUp_AutoData.csv") 
#test <- read_csv("RAcredit_test.csv")
str(train)
numerical <- c("price", "wheel-base", "length", "width","height", "normalized-losses", "curb-weight","engine-size", "bore", "stroke", "compression-ratio", "horsepower",
               "peak-rpm", "city-mpg", "highway-mpg")

#recode as numeric
train[, which(names(train) %in% numerical)] <- sapply(train[, which(names(train) %in% numerical)], as.numeric)
train$symboling <- as.character(train$symboling)

str(train)
# Unique values per column
len <- t(data.frame(lapply(train[, -which(names(train) %in% numerical)], function(x) length(unique(x)))))
View(len)

#Check for Missing values
missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n()))
#missing_values <- test %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw() + labs(x='features', y='% missing', title='Percent missing data by feature')


#nice one to analyze the data
datatable(checkAllCols(train), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

cols <- sapply(train, class)

numeric_cols <- rownames(data.frame(cols[which(cols != "character")]))
char_cols <- rownames(data.frame(cols[which(cols == "character")]))

#impute missing variables with regression
library(mice)

miceMod2 <- mice(train[, !names(train) %in% "price"], diagnostics = T) #regression (pmm)

# miceOutput <- mice::complete(miceMod)
miceOutput2 <- mice::complete(miceMod2)

#check missings again
missdata <- data.frame(cbind(train$`normalized-losses`, miceOutput2$`normalized-losses`))
colnames(missdata) <- c("Originals", "LR_Model")

plot_ly(missdata, x =~1:nrow(missdata), y = ~Originals, type = "scatter", mode = 'lines', name = 'Originals') %>%
  #add_trace(y = ~RF_Model, name = 'RF_Model',mode = 'lines+markers') %>%
  add_trace(y = ~LR_Model, name = 'LR_Model',mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))


new_train <- miceOutput2
rm(miceOutput2)

new_train$price <- train$price

#Check for Missing values
missing_values <- new_train %>% summarize_all(funs(sum(is.na(.))/n()))
#missing_values <- test %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw() + labs(x='features', y='% missing', title='Percent missing data by feature')


#exclude the price nas
new_train <- new_train[!is.na(new_train$price),]

nrow(new_train[is.na(new_train$price),])
#seperated the data
char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]


#Categorical Variables

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(char_train, fun = plotHist, ii = 1:6, ncol = 2)
doPlots(char_train, fun = plotHist, ii = 7:11, ncol = 2)
new_train[["num-of-doors"]][new_train[["num-of-doors"]] == "?"] <-  "four"



plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data = data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}


doPlots(num_train, fun = plotDen, ii = 1:8, ncol = 2)
doPlots(num_train, fun = plotDen, ii = 9:15, ncol = 2)

#Box plot
ggplot(num_train, aes(x = char_train$make, num_train$price)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)

#Box plot
ggplot(num_train, aes(x = char_train$`fuel-system`, num_train$price)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)

#Box plot
ggplot(num_train, aes(x = char_train$`engine-type`, num_train$price)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)

#Box plot
ggplot(num_train, aes(x = char_train$`num-of-cylinders`, num_train$price)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)

#Box plot
p <- ggplot(num_train, aes(x = char_train$symboling, num_train$price)) + geom_boxplot(outlier.colour="red", outlier.shape=16, outlier.size=2, notch=FALSE)


get_dup(train)

#-------Extract the columns has 0 variance
options(scipen = 99)
vars <- sapply(new_train[, which((names(new_train) %in% numerical))], function(x) var(x, na.rm  = T))
extract_vars <- rownames(data.frame(vars[which(vars < 0.01)]))
extract_vars
#train <- train[ , -which(names(train) %in% extract_vars)]

char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]
#new_train <- train[!is.na(train$price),]
rm(miceMod2, missdata, missing_values,p)


#create correlation matrix, (model matrix creates a one hot encoding method)
m <- model.matrix(~.-1, num_train) %>% cor(method = "pearson")

corrplot(m, type = "upper", method="color", addCoef.col = "black", tl.cex = .7,cl.cex = .7, number.cex=.7)

cor_var <- findCorrelation(m, cutoff = 0.70, names = TRUE) %>% gsub("`", "", .)
cor_var

#MULTICOLLINEARITY !

#extract highly correlated ones
new_train %<>% dplyr::select(-one_of(cor_var[cor_var != "price"]))

library(GGally)
ggscatmat(num_train, columns = 1: ncol(num_train), corMethod = "pearson")


char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]
new_train <- new_train[!is.na(new_train$price),]


#-------------------------------------------LINEAR REGRESSION--------------------------------------------------

set.seed(1234)
parts <- createDataPartition(new_train$price, p = 0.8, list = F) %>% c()

#LAbel encoding
new_train[, -which(names(new_train) %in% numerical)] <- lapply(new_train[, -which(names(new_train) %in% numerical)], as.factor)

# Train & Test seperation
new_train_lm <- new_train[parts,]
new_test_lm <- new_train[-parts,]
#price <- new_train$price

#start regression
reg_auto <- lm(price~., new_train)
summary(reg_auto)
plot(reg_auto)
anova(reg_auto)

# test for autocorrelation on error terms, 
durbinWatsonTest(reg_auto,max.lag = 1)


ggplot(new_train, aes(x = new_train$horsepower, price)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("height") +
  ylab("Price") +
  ggtitle("Regression Plot") + 
  expand_limits(y=0) 


modelSummary <- summary(reg_auto)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients[,c(1,2)]  # model coefficients


p_val <- anova(reg_auto)[,c(1,5)]
p_val$`Pr(>F)` <- round(p_val$`Pr(>F)`,3)

AIC(reg_auto)
BIC(reg_auto)

#PREDICTION
# Stepwise Regression   Backward, forwar selection, depends on your data s volume
library(MASS)
reg_auto_sw <- lm(price~., new_train_lm)
step <- stepAIC(reg_auto_sw, direction="both")
step$anova # display results 
summary(reg_auto_sw) #

reg_auto_sw <- lm(log(price) ~ make + `drive-wheels` + `wheel-base` + height + `engine-type` + 
                    `num-of-cylinders` + bore + stroke + `compression-ratio` + 
                    horsepower, new_train)

summary(reg_auto_sw)


#PREDICTION
anova(reg_auto_sw)

library(mosaic)

mplot(reg_auto_sw, which = 7, multiplot = T, system = "ggplot2")

#reg_auto_sw <- lm(log(price) ~ ., new_train_lm)

pred1 <- predict.lm(reg_auto_sw, newdata = new_test_lm)
reg_auto_sw$xlevels[["make"]] <- union(reg_auto$xlevels[["make"]], levels(new_test_lm$make))

pred1 <- data.frame(pred1, row.names = 1:40)


plot_ly(pred1, y = exp(pred1$pred1), type = "scatter", mode = "lines", name = "Forecast") %>% 
  add_trace(y = ~new_test_lm$price, type = "scatter", mode = "lines", name = "Actual")

actuals_preds <- data.frame(cbind(actuals=new_test_lm$price, predicteds=exp(pred1$pred1)))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
# => 93%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape #9%






# K-fold cross-validation
#city mpg ile highway mpg yi birleştirip ortalamasını alıp ilerleyebilirsiniz.
# VIF inceleme
#"curb-weight" "length"      "width"       "engine-size" "highway-mpg" "price"       "city-mpg"    burada variable ları birleştirebilirsiniz,
# yani yeni variable oluşturmai ortalama mpg, boyutlar