-- mysql  Ver 14.14 Distrib 5.5.41, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: civicrm
--
-- Do 'SELECT * from mailing_data_analytics' to get the final results.

drop table if exists queue_mailing_mapping;
drop table if exists mailing_data;
drop table if exists mailing_data_analytics;

-- Create table 'queue_mailing_mapping' that maps every queue id to its mailing.
create table queue_mailing_mapping as select id, job_id from civicrm_mailing_event_queue;

alter table queue_mailing_mapping add column mailing_id int(10) unsigned;

update queue_mailing_mapping set mailing_id = (select mailing_id from civicrm_mailing_job where id = queue_mailing_mapping.job_id) where mailing_id is NULL and exists(select mailing_id from civicrm_mailing_job where id = queue_mailing_mapping.job_id);

-- Create table 'mailing_data' that stores information about the delay between sending and opening of mails in each mailing.
create table mailing_data as select * from civicrm_mailing_event_opened;

alter table mailing_data add column mailing_id int(10) unsigned;

update mailing_data set mailing_id = (select mailing_id from queue_mailing_mapping where id = mailing_data.event_queue_id) where mailing_id is NULL and exists(select mailing_id from queue_mailing_mapping where id = mailing_data.event_queue_id);

alter table mailing_data add column time_stamp_delivered datetime;

update mailing_data set time_stamp_delivered = (select time_stamp from civicrm_mailing_event_delivered where event_queue_id = mailing_data.event_queue_id) where time_stamp_delivered is NULL and exists(select time_stamp from civicrm_mailing_event_delivered where event_queue_id = mailing_data.event_queue_id);

alter table mailing_data add column delay_seconds int(10) unsigned;

update mailing_data set delay_seconds = TIMESTAMPDIFF(second, time_stamp_delivered, time_stamp);

alter table mailing_data add column email_id int(10) unsigned;
alter table mailing_data add column sent_time int(10) unsigned;
alter table mailing_data add column open_time int(10) unsigned;
alter table mailing_data add column day int(10) unsigned;

update mailing_data set email_id = (select email_id from civicrm_mailing_event_queue where id = mailing_data.event_queue_id) where email_id is NULL and exists(select email_id from civicrm_mailing_event_queue where id = mailing_data.event_queue_id);

-- Store sent_time and open_time in minutes since 00:00 hrs.
update mailing_data set sent_time = (TIME_TO_SEC(time_stamp_delivered))/60;
update mailing_data set open_time = (TIME_TO_SEC(time_stamp))/60;

-- Store index of day on which the mail was sent. Sunday:1, Monday:2 and so on..
update mailing_data set day = DAYOFWEEK(time_stamp_delivered);


-- Create table 'mailing_data_analytics' to store data pertaining to mail(send-open) analytics per mailing.
create table mailing_data_analytics as select mailing_id, min(delay_seconds) as min_delay, max(delay_seconds) as max_delay, avg(delay_seconds) as avg_delay from mailing_data group by mailing_id;

alter table mailing_data_analytics add column min_delay_time time;
alter table mailing_data_analytics add column max_delay_time time;
alter table mailing_data_analytics add column avg_delay_time time;

update mailing_data_analytics set min_delay_time = maketime(floor(min_delay/3600),floor(min_delay/60)%60,floor(min_delay%60));
update mailing_data_analytics set max_delay_time = maketime(floor(max_delay/3600),floor(max_delay/60)%60,floor(max_delay%60));
update mailing_data_analytics set avg_delay_time = maketime(floor(avg_delay/3600),floor(avg_delay/60)%60,floor(avg_delay%60));

