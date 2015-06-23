# This script builds the model using linear regression techniques, applies it on the train and test data and prints the results to csv files. The Model stats are printed on the console.

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
mydb = dbConnect(MySQL(), user='root', password='your password', dbname='civicrm', host='localhost')
# regression_table is the MySQL table that contains all the necessary data about mails.
rs = dbSendQuery(mydb, "select * from mailing_data order by rand()")
data = fetch(rs, n=-1)
# Divide data set into train and test sets.
train = data[1:2000,]
test = data[2001:2153,]


library('DAAG')
# Build a predictive model using linear regression.
# Independent vairbales -> sent_time, email_id, day
# Dependent variables -> open_time (We need to predict when the mail opens first.)
model = lm(open_time ~ sent_time + email_id + day, data = train)

# Store Model coefficients, predicted values and residual values.
model_coeff <- coefficients(model)
predicted_values <- fitted(model)
residual_values <- residuals(model)

# Write  the results of model on traindata in a csv file.
train_data_results <- train[,c('id','sent_time','day','email_id','open_time')]
train_data_results <- cbind(train_data_results, predicted_values)
train_data_results <- cbind(train_data_results, residual_values)
write.csv(file='train_data_results.csv', x = train_data_results)

# Print Model summary.
print(summary(model))
# Print Residual Means of train data.
mean_values <- compute_residual_means(residual_values)
print(mean_values)


# Apply model on test data and predict the open_times for new data.
test_data_results <- cbind(test, predicted_open_time=0)
test_data_results <- cbind(test, error=0)
test_data_results['predicted_open_time'] = model_coeff['(Intercept)'] + (model_coeff['sent_time'] * test_data_results['sent_time']) + (model_coeff['email_id'] * test_data_results['email_id']) + (model_coeff['day'] * test_data_results['day'])
test_data_results['error'] = test_data_results['open_time'] - test_data_results['predicted_open_time']
test_data_results <- test_data_results[,c('id','sent_time','open_time','predicted_open_time','error')]
# Write the results to csv file.
write.csv(file='test_data_results.csv', x=test_data_results)

