README

Given folder contains the following files:

**mailing_sendopen_analysis.sql** 

This sql file generates the table 'mailing_data_analytics' that contains all the data analytics(min, max, mean of delay(in seconds) in sending and opening of mails). It also generates a table 'mailing_data' that is used by model.R file in model_R_code folder to build the linear regression model.

Use this command to get the dump of the required tables :

$ *mysqldump -u user_name -p database_name mailing_data mailing_data_analytics > mail_dump.sql*


**mailing_clicks_analysis.sql**

This sql file generates the table 'clicks_data_analytics' that contains all the data analytics(min, max, mean of click rates for each mailing).  

Use this command to get the dump of the required tables :

$ *mysqldump -u user_name -p database_name clicks_data clicks_data_analytics > clicks_dump.sql*

**Run the above 2 scripts using the following command:**

$ *mysql -u user_name -p database_name < sql_file_name.sql*


**model_R_code**

This folder contains the R scripts to generate the linear regression model and apply it on train and test sets. It generates the results of the model on train and test sets in 'train_data_results.csv' and 'test_data_results.csv' files.

**php_code**

This folder contains php scripts to give end user an option to build the prediction model using the parameters he selects, and accordingly generate the results on train and test data.

**Results_dump**

This folder contains the analytics data stored in mailing_data_analytics and clicks_data analytics table (These are the results for the small dump.)



