#### Use this template file to solve the problem

###### DONOT CHANGE THIS ############
### First we download data files from github
url1 = "https://raw.githubusercontent.com/Auquan/qq-ic/master/training_data.csv"
url2 = "https://raw.githubusercontent.com/Auquan/qq-ic/master/test_data.csv"
if (!file.exists("training_data.csv")) {
  download.file(url1, "training_data.csv", method = "auto", quiet = FALSE, mode = "w")
}
if (!file.exists("test_data.csv")) {
  download.file(url2, "test_data.csv", method = "auto", quiet = FALSE, mode = "w")
}

checkInstall <- function(x){
  r = getOption("repos") # hard code the UK repo for CRAN
  r["CRAN"] = "http://cran.uk.r-project.org"
  options(repos = r)
  rm(r)
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

##### FIRST MENTION ANYPACKAGES YOU WANT TO USE HERE

requiredPackages <-c("reshape2" , "data.table", "MASS", "stats", "caret", "car", "elasticnet", "glmnet",
                     "mvtnorm", "tidyverse", "dplyr", "roll")
#  Then try/install packages...
checkInstall( requiredPackages )

### This command reads CSV into R
market_data <- read.csv(file="training_data.csv", header=TRUE, sep=",")
# print(market_data)


#### Now you can play with data

#### Variable Selection
##Pre-processing the data
market_data_1 <- market_data[,-1]

trans <- preProcess(market_data_1[,-2001], 
                   method=c("BoxCox", "center", 
                            "scale"))
pre_processed_data <- predict(trans, market_data_1[,-2001])
##Using lasso to perform a feature selection

lasso_trans <- cv.glmnet(as.matrix(pre_processed_data),market_data$Y)
#Selecting the variables
c<-coef(lasso_trans, s = lasso_trans$lambda.min)
inds<-which(c!=0)
variables<-row.names(c)[inds][-1]
#Creating the data frame with the feature selection done
lasso_data <- pre_processed_data %>% select(variables)

##Doing PCA to reduce even more the dimension
trans <- preProcess(lasso_data, 
                    method=c("pca"))
pca_data <- predict(trans, lasso_data)
#Data after selection
new_market_data_2 <- cbind(market_data$TimeStamp, lasso_data, market_data$Y)
colnames(new_market_data_2)[1] = "TimeStamp"
colnames(new_market_data_2)[dim(pca_data)[2]+2] = "Y"

new_market_data <- cbind(market_data$TimeStamp, pca_data, market_data$Y)
colnames(new_market_data)[1] = "TimeStamp"
colnames(new_market_data)[dim(pca_data)[2]+2] = "Y"

#### Assess Correlation 
fit <- roll_lm(as.matrix(pca_data), as.matrix(market_data$Y), width = 20 )
#### Assess change in correlation with time

#### Multi Linear regression Fitting the model
#fit = lm(F4~F1+F2+F3, data = market_data) #Create the linear regression
# summary(fit) # show results
# Other useful functions 
# coefficients(fit) # model coefficients
# confint(fit, level=0.95) # CIs for model parameters 
# fitted(fit) # predicted values
# residuals(fit) # residuals
# anova(fit) # anova table 
# vcov(fit) # covariance matrix for model parameters 
# influence(fit) # regression diagnostics

#### Compare Models
# fit1 <- lm(y~x_1+x_2+x_3, data = market_data)
# fit2 <- lm(y~x_1+x_2, data = market_data)
# anova(fit1, fit2)

#### Cross Validation - Remember this is time series data!! Don't use information from the future

#### Check Relative Importance

#### Try GLM/Nonlinear Regression



#### Make predictions for new data using model of your choice
## This assumes that model name is 'fit'

new.df <- read.csv(file="test_data.csv", header=TRUE, sep=",")

p<- 200
new.df <- read.csv(file="test_data.csv", header=TRUE, sep=",")
new.df.select <- new.df[, variables]
z <- matrix(rep(0, times = length(new.df.select[, 1])), ncol = 3, nrow = length(new.df.select[, 1]))

tmp.data <- cbind(market_data$Y[(360-p+1):360], lasso_data[(360-p+1):360, ])
colnames(tmp.data) <- c("Y", variables)
result <- lm(Y~., tmp.data)
z[1, ] = predict(result, newdata = new.df.select[1, ], interval='confidence')

for(i in 2:length(new.df.select[, 1]))
{
  tmp1 <- cbind(market_data$Y[(360-p+i):360], lasso_data[(360-p+i):360, ])
  colnames(tmp1) <- c("Y", variables)
  tmp2 <- cbind(z[1:(i-1), 1], new.df.select[1:(i-1), ])
  colnames(tmp2) <- c("Y", variables)
  
  tmp.data <- rbind(tmp1, tmp2)
  colnames(tmp.data) <- c("Y", variables)
  result <- lm(Y~., tmp.data)
  z[i, ] = predict(result, newdata = new.df.select[i, ], interval='confidence')
}
#z = predict(result, newdata=new.df, interval='confidence')
colnames(z) <- c("fit", "lwr", "")
print(z)

##### Save your prediction as CSV
write.csv(z, file = "predictions.csv",row.names=FALSE, na="")

