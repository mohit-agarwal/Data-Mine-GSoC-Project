<!-- The file generates a prediction model and tests it on a dataset. It connects to MySQL database to fetch the dump (for training), runs a R script to generate the model (as per user specifications) and then tests the model on new data. It then shows the results. -->

<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
        <title>Data Mine Project</title>
    </head>

    <body>
        <h2><center> Data Mine GSoC Project </center> </h2>
        The php module builds linear prediction model that is able to make predictions related to mailings. It works on a dump that contains tables having information about mail opening rates, click rates and so on.
        <br/>
        <h3> Dump </h3>
        Machine learning is done on a dump of 1044 mails belonging to 15 different mailings, sent at different timestamps. The mails are sent at 25 unique sent_times (implying most of the mails are sent at same time, here I'm ignoring the date). The data has been extracted from <i>civicrm_mailing_event_delivered</i>, <i>civicrm_mailing_event_opened</i> and <i>civicrm_mailing_event_queue</i> mysql tables. I have the following information about the mails - sent time, open time, day on which it was sent, email_id and mailing id.

        <h3>Working</h3>
        <i>Choose the variable (independent) you want to predict and the factors (dependent) on which the prediction model would be based.</i>

        <br/><br/>
        <form id="prediction_model_form" name="prediction_model_form" method="post" action="">
            <label><b>Independent variable : </b>
            <select name ='independent'>
                <option value="open_time">Open time of Mail (since 00:00 hrs) in minutes</option>
                <option value="delay_seconds">Open time of Mail (since the time it was sent) in seconds</option>
            </select>  
            </label><br/><br/><br/>
            <label><b>Dependent variables : </b>
            <select name ='dependent[]' multiple>
                <option value="sent_time">Sent time of Mail (in minutes)</option>
                <option value="day">Day on which the mail was sent</option>
                <option value="email_id">Email Id</option>
                <option value="mailing_id">Mailing Id</option>
            </select> 
            </label>

            <p>
            <label>
            <input type="submit" name="submit" id="submit" value="Get Results" />
            </label>
            </p>
        </form>

        <hr>

<?php
$independent_variable = $_POST["independent"];
$dependent_variables = $_POST["dependent"];

$dependent_variables_string = join(' + ',$dependent_variables);
$query_for_model = $independent_variable.' ~ '.$dependent_variables_string;

$predict_query = "model_coeff['(Intercept)'] + ";

# String to predict the independent variable using the model coefficients and the dependent variables.
foreach ($dependent_variables as $variable){
    $predict_query = $predict_query . "(model_coeff['" . $variable . "'] * test_data_results['" . $variable . "']) + ";
}

$predict_query = substr($predict_query,0,-2);

# Deleting the output files.
shell_exec('rm -rf test.R out.txt train_data_results.csv test_data_results.csv');

# R Script to generate the predictive model and compute the results for train and test data.
$r_file = "compute_residual_means <- function(residual_values_list){
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
rs = dbSendQuery(mydb, 'select * from regression_table order by rand()')
data = fetch(rs, n=-1)
# Divide data set into train and test sets.
train = data[1:2000,]
test = data[2001:2153,]


library('DAAG')
# Build a predictive model using linear regression.
# Independent vairbales -> sent_time, email_id, day
# Dependent variables -> open_time (We need to predict when the mail opens first.)
model = lm(". $query_for_model. ", data = train)

# Store Model coefficients, predicted values and residual values.
model_coeff <- coefficients(model)
predicted_values <- fitted(model)
residual_values <- residuals(model)

# Write  the results of model on traindata in a csv file.
train_data_results <- train[,c('id','sent_time','day','email_id','mailing_id','open_time')]
train_data_results <- cbind(train_data_results, predicted_values)
train_data_results <- cbind(train_data_results, residual_values)
write.csv(file='train_data_results.csv', x = train_data_results)

out<-capture.output(summary(model))
cat(out,file='out.txt',sep='\n',append=TRUE)

# Print Residual Means of train data.
mean_values <- compute_residual_means(residual_values)
cat(mean_values[1,1])
cat(' ',mean_values[1,2])

# Apply model on test data and predict the open_times for new data.
test_data_results <- cbind(test, predicted_open_time=0)
test_data_results <- cbind(test, error=0)
test_data_results['predicted_open_time'] = " . $predict_query . "
test_data_results['error'] = test_data_results['open_time'] - test_data_results['predicted_open_time']
test_data_results <- test_data_results[,c('id','sent_time','open_time','predicted_open_time','error')]
# Write the results to csv file.
write.csv(file='test_data_results.csv', x=test_data_results)";

# Put the contents of R script in test.R file.
file_put_contents('test.R', $r_file);

# Run the R script using Rscript command.
$output = shell_exec('Rscript test.R');

echo "<h3> Summary of Generated Prediction Model </h3>"; 
# Model information is stored in out.txt file. Print the contents of this file.
$fh = fopen("out.txt", 'r');
$pageText = fread($fh, 25000);
echo nl2br($pageText);

$output = split(" ",$output);

echo "<b>Mean of Positive Residual Values : </b>". $output[0];
echo "<br/><b>Mean of Negative Residual Values : </b>". $output[2];

# Provide link to Results on Train and Test data.
echo "<hr> <h3> Results on Train data and Test data </h3>";
echo "<a href='train_data_results.csv'>Train Data Results</a><br/>";
echo "<a href='test_data_results.csv'>Test Data Results</a>";

?>
    </body>
</html>
