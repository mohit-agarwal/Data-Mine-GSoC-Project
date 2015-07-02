# Function to compute Positive and Negative Mean of the
# Residual values for a given dataset. 
compute_residual_means <- function(residual_values_list){
psum = 0
nsum = 0
pcnt = 0
ncnt = 0
for (x in residual_values_list)
{
if(x>=0)
{
psum = psum + x
pcnt = pcnt + 1
}
else
{
nsum = nsum + x
ncnt = ncnt + 1
}
}
# Mean of positive residual values. 
positive_residual_mean = psum/pcnt
# Mean of negative residual values.
negative_residual_mean = nsum/ncnt
return (data.frame(positive_residual_mean,negative_residual_mean))
}

# Load RMySQL library.
library('RMySQL')
# Connect to database.
mydb = dbConnect(MySQL(), user='root', password='pt1234', dbname='gsoc', host='localhost')
# regression_table is the MySQL table that contains all the necessary data about mails.
rs = dbSendQuery(mydb, "select * from regression_table order by rand()")
data = fetch(rs, n=-1)
# Divide data set into train and test sets.
train = data[1:2000,]
test = data[2001:2153,]

# Extract the dependent and independent variables.
# Independent vairbales -> sent_time, email_id, day
# Dependent variables -> open_time (We need to predict when the mail opens first.)
train <- train[,c('sent_time','day','email_id','open_time')]
test <- test[,c('sent_time','day','email_id','open_time')]

# Name the columns of the data frame.
colnames(train) <- c('sent_time','day','email_id','open_time')
colnames(test) <- c('sent_time','day','email_id','open_time')

# Randomly Shuffle Rows of Train set.
train <- train[sample(nrow(train)),]

# Test data
train.fit <- train[, -ncol(train)]
test.fit <- test[, -ncol(test)]

# Find range of data
range.train <- matrix(apply(train,2,range), nrow =2)

# Load the 'Fuzzy Rule based System for Classification and Regression'
library('frbs')
# Using Adaptive Neuro Fuzzy Inference System to build the prediction model.
method.type <- "ANFIS"
control.ANFIS <- list(num.labels = 5, max.iter = 10, step.size = 0.01, type.tnorm = "YAGER",
                     type.snorm = "YAGER", type.implication.func = "ZADEH", name = "Sim-0")
object.ANFIS <- frbs.learn(train, range.train, method.type, control.ANFIS)

# Predict the open_time for test data.
results.test <- predict(object.ANFIS, test.fit)

test.fit <- cbind(test.fit, predicted_open_time=0)
test.fit <- cbind(test.fit, error=0)

test.fit['predicted_open_time'] <- results.test
test.fit['error'] = test['open_time'] - test.fit['predicted_open_time']

# Predict the open_time for train data.
results.train <- predict(object.ANFIS, train.fit)

train.fit <- cbind(train.fit, predicted_open_time=0)
train.fit <- cbind(train.fit, error=0)

train.fit['predicted_open_time'] <- results.train
train.fit['error'] = train['open_time'] - train.fit['predicted_open_time']

# Compute Residual means for train and test sets
print('Residual mean values for Train set')
compute_residual_means(train.fit[,'error'])
print('Residual mean values for Test set')
compute_residual_means(test.fit[,'error'])
