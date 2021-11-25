########################################################################
## 00. LOAD LIBRARIES
## ALSO SET WORKING DIRECTORY
########################################################################

library(dplyr)
library(tidyr)
library(reshape2)
library(ggthemes)
library(ggplot2)
library(lubridate)
library(MASS)
library(compositions)

DIRECTORY <- '/mnt/sda2/uoa/compsci791-dissertation/ap-osteoarthritis/script/'
setwd(DIRECTORY)

## assumption: most patients sleep between 9 PM to 5 AM (inclusive)
SLEEP_TIME <- c(21,22,23,0,1,2,3,4,5)
########################################################################
## 01. DATA IMPORT AND PREPARATION
## 01-1. ACTIVPAL
########################################################################

ACTIVPAL_FILE <- '../dataset/activpal_hourly_cleaned_duration_without_sleep_with_meta.csv'
df_activpal_hourly <- read.csv(ACTIVPAL_FILE)

df_activpal_hourly$datetime.hour <- as.POSIXct(df_activpal_hourly$datetime.hour)
df_activpal_hourly$hour <- as.factor(hour(df_activpal_hourly$datetime.hour))
df_activpal_hourly$day.of.week.string <- weekdays(df_activpal_hourly$datetime.hour)
df_activpal_hourly$day.of.week <- as.numeric(format(df_activpal_hourly$datetime.hour, '%w'))
df_activpal_hourly$date <- as.Date(df_activpal_hourly$datetime.hour, '%Y-%m-%d')

## create dummy variable to indicate baseline or follow up period
df_activpal_hourly$is.followup.period <- as.numeric(df_activpal_hourly$visit.info == 'Follow up')
## create dummy variable to indicate gender of the patient
df_activpal_hourly$is.female <- as.numeric(df_activpal_hourly$gender == 'female')

## add is.sleep.time as dummy attribute
df_activpal_hourly$is.sleep.time <- as.numeric(df_activpal_hourly$hour %in% SLEEP_TIME)
df_activpal_hourly$is.awake.time <- (df_activpal_hourly$is.sleep.time != 1)

## create time sequence for visualizing day-hour cycle
df_activpal_hourly <- df_activpal_hourly %>% 
  mutate(day.hour = paste(day.of.week, hour, sep=''))

## set the level to start on Monday, so that it's similar to Actigraph
## previously: day.of.week 0 = Sunday, 6 = Saturday
df_activpal_hourly$day.hour <- factor(
  df_activpal_hourly$day.hour, 
  levels = c(
    '10','11','12','13','14','15','16','17','18','19',
    '110','111','112','113','114','115','116','117','118','119',
    '120','121','122','123',
    '20','21','22','23','24','25','26','27','28','29',
    '210','211','212','213','214','215','216','217','218','219',
    '220','221','222','223',
    '30','31','32','33','34','35','36','37','38','39',
    '310','311','312','313','314','315','316','317','318','319',
    '320','321','322','323',
    '40','41','42','43','44','45','46','47','48','49',
    '410','411','412','413','414','415','416','417','418','419',
    '420','421','422','423',
    '50','51','52','53','54','55','56','57','58','59',
    '510','511','512','513','514','515','516','517','518','519',
    '520','521','522','523',
    '60','61','62','63','64','65','66','67','68','69',
    '610','611','612','613','614','615','616','617','618','619',
    '620','621','622','623',
    '00','01','02','03','04','05','06','07','08','09',
    '010','011','012','013','014','015','016','017','018','019',
    '020','021','022','023'
  )
)

df_activpal_hourly <- df_activpal_hourly %>% 
  group_by(day.hour, visit.info) %>%
  mutate(time.seq = ceiling(group_indices() / 2))

## there are patients without follow up period observations
## should be removed later before further analysis
activpal_patient_obs_check <- df_activpal_hourly %>% 
  group_by(patient.id) %>% 
  summarise(
    observation_count = length(unique(visit.info))
  )

print(
  paste(
    "Number of patients with valid Activpal data:",
    nrow(activpal_patient_obs_check)
  )
)

print(
  paste(
    "Number of patients with valid Activpal data AND have both baseline & follow-up data:",
    sum(activpal_patient_obs_check$observation_count == 2)
  )
)

activpal_patient_obs_check <- activpal_patient_obs_check %>% filter(observation_count == 2)
df_activpal_hourly_full_obs <- df_activpal_hourly %>% filter(patient.id %in% activpal_patient_obs_check$patient.id)
rm(df_activpal_hourly)

########################################################################
## 01. DATA IMPORT AND PREPARATION
## 01-2. ACTIGRAPH
########################################################################

ACTIGRAPH_FILE <- '../dataset/actigraph_combined_cleaned_with_consecutive_zero.csv'
df_actigraph <- read.csv(ACTIGRAPH_FILE)

## create dummy variable to indicate baseline or follow up period
df_actigraph$is.followup.period <- as.numeric(df_actigraph$directory_name == 'Follow up')
## create dummy variable to indicate gender of the patient
df_actigraph$is.female <- as.numeric(df_actigraph$gender == 'female')
## add is.sleep.time as dummy attribute
## assumption: most patients sleep between 9 PM to 5 AM (inclusive)
df_actigraph$is.sleep.time <- as.numeric(df_actigraph$hour %in% SLEEP_TIME)
df_actigraph$is.awake.time <- (df_actigraph$is.sleep.time != 1)


## only use patients with pre and post surgery data
actigraph_patient_obs_check <- df_actigraph %>% 
  filter(directory_name %in% c('Baseline','Follow up')) %>% 
  group_by(patient_id) %>% 
  summarise(
    observation_count = length(unique(directory_name))
  )

print(
  paste(
    'Number of patients - Actigraph:', 
    length(unique(df_actigraph$patient_id)),
    sep = ' '
  )
)

actigraph_patient_obs_check <- actigraph_patient_obs_check %>% filter(observation_count == 2)
print(
  paste(
    'Number of patients - Actigraph (have both baseline and follow-up data):', 
    length(actigraph_patient_obs_check$patient_id),
    sep = ' '
  )
)

df_actigraph_full_obs <- df_actigraph %>% 
  filter(df_actigraph$patient_id %in% actigraph_patient_obs_check$patient_id) %>% 
  mutate(day.hour = paste(day_of_week, hour, sep=''))

df_actigraph_full_obs$day.hour <- factor(
  df_actigraph_full_obs$day.hour, 
  levels = c(
    '00','01','02','03','04','05','06','07','08','09',
    '010','011','012','013','014','015','016','017','018','019',
    '020','021','022','023',
    '10','11','12','13','14','15','16','17','18','19',
    '110','111','112','113','114','115','116','117','118','119',
    '120','121','122','123',
    '20','21','22','23','24','25','26','27','28','29',
    '210','211','212','213','214','215','216','217','218','219',
    '220','221','222','223',
    '30','31','32','33','34','35','36','37','38','39',
    '310','311','312','313','314','315','316','317','318','319',
    '320','321','322','323',
    '40','41','42','43','44','45','46','47','48','49',
    '410','411','412','413','414','415','416','417','418','419',
    '420','421','422','423',
    '50','51','52','53','54','55','56','57','58','59',
    '510','511','512','513','514','515','516','517','518','519',
    '520','521','522','523',
    '60','61','62','63','64','65','66','67','68','69',
    '610','611','612','613','614','615','616','617','618','619',
    '620','621','622','623'
  )
)
rm(df_actigraph)

########################################################################
## 01. DATA IMPORT AND PREPARATION
## 01-2. ACTIGRAPH
## 01-2A. STORE HOURLY RECORD IN A DATAFRAME, TO BE USED ON GLM
########################################################################

## create new time variable, so that the aggregation is based on day-hour of week
## not the sequence from the start of each patient's observation period
## filter out identified "non-wear" time
## put "behavior_final" in the group by, since it is the indicator of the sedentary or non-sedentary
df_actigraph_hourly <- df_actigraph_full_obs %>% 
  filter(behavior_final != 'non_wear_time') %>% 
  group_by(
    date,
    datetime_hour,
    day_of_week, 
    day_of_week_string, 
    hour, 
    day.hour,
    patient_id, 
    is.female,
    age,
    height,
    weight,
    bmi,
    sf_physical_functioning,
    sf_role_physical,
    sf_bodily_pain,
    sf_social_functioning,
    sf_mental_health,
    sf_role_emotional,
    sf_vitality,
    sf_general_health,
    sf36_total,
    haq,
    ntx,
    oc,
    directory_name,
    is.followup.period,
    is.sleep.time,
    is.awake.time,
    behavior_final
  ) %>% 
  summarise(
    total_step = sum(steps),
    duration.minute = n()
  )

## remove Tue 10-13 baseline, Thu 10-13 follow up
## based on our observation that lots of patients might remove the devices in that period
## leading to fewer observed step counts
df_actigraph_hourly <- df_actigraph_hourly %>% 
  filter(
    (
      directory_name == 'Baseline' &
        !(day.hour %in% c('110','111','112','113'))
    ) |
      (
        directory_name == 'Follow up' &
          !(day.hour %in% c('310','311','312','313'))
      )
  )

df_actigraph_hourly <- df_actigraph_hourly %>% 
  group_by(day.hour, directory_name) %>% 
  mutate(time.seq = ceiling(group_indices() / 2))

## convert "behavior_final" into separate variables
df_actigraph_hourly <- df_actigraph_hourly %>% 
  spread(
    key = behavior_final,
    value = duration.minute
  ) %>% 
  group_by(
    date,
    datetime_hour,
    day.hour,
    patient_id, 
    is.female,
    age,
    height,
    weight,
    bmi,
    sf_physical_functioning,
    sf_role_physical,
    sf_bodily_pain,
    sf_social_functioning,
    sf_mental_health,
    sf_role_emotional,
    sf_vitality,
    sf_general_health,
    sf36_total,
    haq,
    ntx,
    oc,
    directory_name,
    time.seq, 
    is.followup.period, 
    is.sleep.time,
    is.awake.time
  ) %>% 
  summarise(
    sedentary_behavior = sum(sedentary_behavior, na.rm = TRUE),
    light_intensity = sum(light_intensity, na.rm = TRUE),
    moderate = sum(moderate, na.rm = TRUE),
    vigorous = sum(vigorous, na.rm = TRUE),
    total_step = sum(total_step)
  )

########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK
## 02-1. STEP COUNT BASED ON ACTIGRAPH -- INITIAL LINE PLOT
########################################################################

library(scales)
## plot the hourly one
df_actigraph_hourly$datetime_hour <- as.POSIXct(df_actigraph_hourly$datetime_hour, format = "%Y-%m-%d %H:%M:%S")
sub <- df_actigraph_hourly %>% 
  filter(patient_id == 1 & directory_name == 'Baseline')

## single patient
sub %>% 
  ggplot() +
  geom_line(
    aes(
      x = datetime_hour,
      y = total_step,
      group = patient_id
    )
  ) +
  theme_classic() +
  scale_x_datetime(
    labels = date_format("%b %d %H:%M"),
    date_breaks = "1 day",
    name = 'time'
  ) +
  scale_y_continuous(
    name = "total step"
  )

patient_id_date <- df_actigraph_hourly[
  (df_actigraph_hourly$patient_id == 1) & (df_actigraph_hourly$directory_name == 'Baseline'),
  'datetime_hour'
  ]
dummy_patient <- df_actigraph_hourly[
  (df_actigraph_hourly$patient_id == 10) & (df_actigraph_hourly$directory_name == 'Baseline'),
  ]
dummy_patient$datetime_hour <- patient_id_date$datetime_hour
dummy_patient$patient_id <- 99
df_hourly_sample <- rbind(sub, dummy_patient)
df_hourly_sample$patient_id <- as.factor(df_hourly_sample$patient_id)

## 2 patients, same time frame
hourly_plot <- df_hourly_sample %>% 
  ggplot() +
  geom_line(
    aes(
      x = datetime_hour,
      y = total_step,
      group = patient_id,
      color = patient_id
    )
  ) +
  theme_classic() +
  scale_x_datetime(
    labels = date_format("%b %d %H:%M"),
    date_breaks = "1 day",
    name = 'time'
  ) +
  scale_y_continuous(
    name = "total step"
  )
hourly_plot

## 3 patients, different time frame
## add another patient
dummy_patient <- df_actigraph_hourly[
  (df_actigraph_hourly$patient_id == 2) & (df_actigraph_hourly$directory_name == 'Baseline'),
  ]
dummy_patient$patient_id <- as.factor(dummy_patient$patient_id)
dummy_patient$datetime_hour <- dummy_patient$datetime_hour - days(21)
df_hourly_sample <- rbind(df_hourly_sample, dummy_patient)

hourly_plot <- df_hourly_sample %>% 
  ggplot() +
  geom_line(
    aes(
      x = datetime_hour,
      y = total_step,
      group = patient_id,
      color = patient_id
    )
  ) +
  theme_classic() +
  scale_x_datetime(
    labels = date_format("%b %d"),
    date_breaks = "1 day",
    name = 'time'
  ) +
  scale_y_continuous(
    name = "total step"
  )
hourly_plot

########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK
## 02-2. STEP COUNT BASED ON ACTIGRAPH -- SCATTERPLOT BY TIME SEQUENCE
########################################################################

patient_ids <- df_actigraph_hourly[
  (df_actigraph_hourly$directory_name == 'Baseline'),
  'patient_id'
  ] %>% distinct()

set.seed(50)
SAMPLE_PATIENT_IDS <- sample(patient_ids$patient_id, size = 15)

ggplot(data = df_actigraph_hourly %>% 
         filter(
           (directory_name == 'Baseline') &
             (patient_id %in% SAMPLE_PATIENT_IDS)
         )
) + 
  geom_point(
    mapping = aes(
      x = time.seq,
      y = total_step
    ), 
    color = 'orange',
    alpha = 0.5
  ) +
  scale_x_continuous(
    name = '',
    labels = c('Mon\n 12AM','Tue\n 12AM','Wed\n 12AM','Thu\n 12AM','Fri\n 12AM','Sat\n 12AM','Sun\n 12AM','Mon\n 12AM'),
    breaks = seq(0, 168, 24)
  ) +
  scale_y_continuous(
    name = 'total step',
    breaks = seq(0, 5000, 500),
    limits = c(0, 5000)
  ) +
  theme_classic()
# theme(
#   panel.grid = element_blank(),
#   axis.line = element_line(colour = "black")
# )


########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK
## 02-3. PLOT MINUTELY SEQUENCE OF ACTIVITY TYPE
########################################################################

patient_timeframe <- df_actigraph_full_obs %>% 
  group_by(patient_id, visit_info) %>% 
  summarise(
    min.datetime = min(as.Date(datetime_hour)), 
    max.datetime = max(as.Date(datetime_hour))
  )

## create empty dataframe to store the populated time sequence per patient ID
df_patient_obs <- data.frame()

for (pid in unique(patient_timeframe$patient_id)){
  patient.sub <- patient_timeframe %>% filter(patient_id == pid)
  for (period in patient.sub$visit_info){
    patient.sub.period <- patient.sub %>% filter(visit_info == period)
    tmp.seq <- seq(
      as.POSIXct(patient.sub.period$min.datetime), 
      as.POSIXct(patient.sub.period$max.datetime),
      'min')
    tmp.df <- data.frame(
      patient.id = pid,
      visit.info = period,
      datetime.minute = tmp.seq
    )
    df_patient_obs <- rbind(
      df_patient_obs,
      tmp.df
    )
  }
}

df_patient_obs$datetime.minute <- as.POSIXct(df_patient_obs$datetime.minute)
df_patient_obs$hour <- hour(df_patient_obs$datetime.minute)
df_patient_obs$minute <- minute(df_patient_obs$datetime.minute)
df_patient_obs$day.of.week.string <- weekdays(df_patient_obs$datetime.minute)
## day.of.week 0 = Sunday, 6 = Saturday
df_patient_obs$day.of.week <- as.numeric(format(df_patient_obs$datetime.minute,'%w'))
df_patient_obs$day.of.week <- ifelse(
  df_patient_obs$day.of.week.string == 'Sunday', 
  6,
  ifelse(
    df_patient_obs$day.of.week.string == 'Monday',
    0,
    df_patient_obs$day.of.week - 1
  )
)

## create time sequence
df_patient_obs <- df_patient_obs %>% mutate(
  day.hour.minute = paste(day.of.week, hour, minute, sep='')
)

df_patient_obs <- df_patient_obs %>% group_by(day.of.week, hour, minute, visit.info) %>% mutate(
  time.seq = ceiling(group_indices() / 2)
)

df_actigraph_full_obs$behavior_final <- factor(
  df_actigraph_full_obs$behavior_final,
  levels = c('non_wear_time','sedentary_behavior','light_intensity','moderate','vigorous')
)
df_actigraph_full_obs$datetime <- as.POSIXct(df_actigraph_full_obs$datetime, format = "%Y-%m-%d %H:%M:%S")
df_actigraph_full_obs$minute <- minute(df_actigraph_full_obs$datetime)
df_actigraph_minute_behavior <- df_patient_obs %>% left_join(
  df_actigraph_full_obs %>% dplyr::select(patient_id, visit_info, day_of_week_string, hour, minute, behavior_final, day.hour),
  by = c(
    'patient.id' = 'patient_id',
    'visit.info' = 'visit_info',
    'day.of.week.string' = 'day_of_week_string',
    'hour' = 'hour',
    'minute' = 'minute'
  )
)

df_actigraph_minute_behavior$patient.id <- as.factor(df_actigraph_minute_behavior$patient.id)
# df_actigraph_minute_behavior$patient.id <- as.numeric(df_actigraph_minute_behavior$patient.id)
ggplot(data = df_actigraph_minute_behavior %>% 
         filter(
           (visit.info == 'Baseline') &
             (patient.id %in% SAMPLE_PATIENT_IDS)
         )
) +
  geom_point(
    aes(
      x=time.seq,
      y=patient.id,
      col=behavior_final
    )
  ) + scale_color_manual(
    name = 'Behavior Type',
    labels = c('Non-wear','Sitting/Lying', 'Light-intensity PA', 'Moderate-intensity PA', 'Vigorous-intensity PA'),
    values = c('red','green','blue','pink','black')
  ) + scale_x_continuous(
    name = '',
    labels = c('Mon 12AM','Tue 12AM','Wed 12AM','Thu 12AM','Fri 12AM','Sat 12AM','Sun 12AM','Mon 12AM'),
    breaks = seq(0, 10080, 1440)
  ) + scale_y_discrete(
    name = 'patient ID'
  ) + theme_classic(
  ) + theme(
    axis.text.y = element_blank(),
    # panel.background = element_rect(fill = 'black')
  )
# + theme(
#   # get rid of panel grids
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   # Change plot and panel background
#   plot.background=element_rect(fill = "gray"),
#   panel.background = element_rect(fill = 'black'),
#   # Change legend 
#   legend.position = c(0.6, 0.07),
#   legend.direction = "horizontal",
#   legend.background = element_rect(fill = "black", color = NA),
#   legend.key = element_rect(color = "gray", fill = "black"),
#   legend.title = element_text(color = "white"),
#   legend.text = element_text(color = "white")
# )

########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK
## 02-4. PLOT OVERALL ACTIVITY PROPORTION PER PATIENT
########################################################################

df_actigraph_minute_behavior_clean <- df_actigraph_minute_behavior %>% filter(
  (behavior_final != 'non_wear_time') &
    (
      (
        visit.info == 'Baseline' &
          !(day.hour %in% c('110','111','112','113'))
      ) |
        (
          visit.info == 'Follow up' &
            !(day.hour %in% c('310','311','312','313'))
        )
    )
)
df_actigraph_minute_behavior_clean$behavior_final <- factor(
  df_actigraph_minute_behavior_clean$behavior_final,
  levels=c('sedentary_behavior','light_intensity','moderate','vigorous')
)

ggplot(data = df_actigraph_minute_behavior_clean %>% 
         filter(
           (visit.info == 'Baseline') &
             (patient.id %in% SAMPLE_PATIENT_IDS)
         )) +
  geom_bar(
    aes(
      x=patient.id,
      fill=behavior_final
    ),
    position = 'fill'
  ) +
  scale_fill_brewer(
    name = '',
    label = c(
      'Sitting/Lying','Light intensity physical activity','Moderate intensity physical activity','Vigorous intensity physical activity'
    ),
    palette = 'Set2'
  )  +
  coord_flip() +
  scale_x_discrete(
    name = 'patient ID'
  ) +
  scale_y_continuous(
    name = 'proportion of activity',
    breaks = seq(0,1,.2),
    labels = scales::percent
  ) + theme_classic() + 
  theme(
    axis.text.y = element_blank()
  )

########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK / DISSERTATION REPORT
## 02-4x. PLOT OVERALL ACTIVITY PROPORTION PER PATIENT SUBSAMPLE
########################################################################

ggplot(data = df_actigraph_minute_behavior_clean %>% 
         filter(
           (visit.info == 'Baseline') &
             (patient.id %in% head(SAMPLE_PATIENT_IDS, 3))
         )) +
  geom_bar(
    aes(
      x=patient.id,
      fill=behavior_final
    ),
    position = 'fill'
  ) +
  scale_fill_brewer(
    name = '',
    label = c(
      'Sitting/Lying','Standing','Stepping','Vigorous intensity physical activity'
    ),
    palette = 'Set2'
  )  +
  coord_flip() +
  scale_x_discrete(
    name = 'patient ID'
  ) +
  scale_y_continuous(
    name = 'time spent (per day)',
    breaks = seq(0,1,.2),
    labels = scales::percent
  ) + theme_classic() + 
  theme(
    axis.text.y = element_blank()
  )

########################################################################
## 02. DATA VISUALISATION -- FOR RLADIES TALK
## 02-5. PLOT AVERAGE ACTIVITY PROPORTION IN 3-D
########################################################################

library(compositions)
ACTIGRAPH_ACTIVITY_DURATION_COLUMNS <- c('sedentary_behavior','light_intensity','moderate','vigorous')
ACTIGRAPH_PA_DURATION_COLUMNS <- c('light_intensity','moderate','vigorous')
df_actigraph_hourly$non_sedentary <- apply(df_actigraph_hourly[, ACTIGRAPH_PA_DURATION_COLUMNS], 1, sum)

actigraph_hourly_composition <- acomp(
  df_actigraph_hourly[, ACTIGRAPH_ACTIVITY_DURATION_COLUMNS]
)
actigraph_hourly_composition <- as.data.frame(actigraph_hourly_composition)
colnames(actigraph_hourly_composition) <- c('sedentary.proportion','lipa.proportion','moderate.proportion','vigorous.proportion')

df_actigraph_hourly_with_prop <- dplyr::bind_cols(df_actigraph_hourly, actigraph_hourly_composition)

## remove record with NA
df_actigraph_hourly_with_prop <- df_actigraph_hourly_with_prop %>% filter(!is.na(haq) & !is.na(sf36_total))


### USE AVG OF DAILY DATA
df_actigraph_daily_comp_all <- df_actigraph_hourly_with_prop %>% 
  group_by(patient_id, directory_name, date, age, bmi, sf36_total, haq, is.awake.time) %>% 
  summarise(
    sedentary_behavior = sum(sedentary_behavior),
    light_intensity = sum(light_intensity),
    mvpa = sum(moderate + vigorous)
  )
actigraph_comp_temp <- as.data.frame(
  acomp(df_actigraph_daily_comp_all[, c("sedentary_behavior", "light_intensity","mvpa")])
)
colnames(actigraph_comp_temp) <- c('sedentary.proportion','lipa.proportion','mvpa.proportion')
df_actigraph_daily_comp_all <- dplyr::bind_cols(df_actigraph_daily_comp_all, actigraph_comp_temp)

df_actigraph_daily_comp_all <- df_actigraph_daily_comp_all %>% 
  group_by(patient_id, directory_name, age, bmi, sf36_total, haq, is.awake.time) %>% 
  summarise(
    sedentary.proportion = mean(sedentary.proportion),
    lipa.proportion = mean(lipa.proportion),
    mvpa.proportion = mean(mvpa.proportion)
  )
df_actigraph_daily_comp_all <- as.data.frame(df_actigraph_daily_comp_all)
df_actigraph_daily_comp_all$time <- ifelse(df_actigraph_daily_comp_all$is.awake.time == 0, 'Night', 'Day')

library(ggtern)
ggtern(
  data = df_actigraph_daily_comp_all %>% 
    filter(
      (directory_name == 'Baseline') &
        (patient_id %in% SAMPLE_PATIENT_IDS)
    ), 
  aes(
    x = sedentary.proportion, 
    y = lipa.proportion, 
    z = mvpa.proportion, 
    col = time
  )
) + 
  geom_point(alpha = 0.8) +
  scale_color_manual(values=c("#FF69B4", "#7FFF00"),
                     name="Period",
                     breaks=c("Night", "Day"),
                     labels=c("Night", "Day"),
  ) +
  labs(x = 'SB', y = 'LIPA', z = 'MVPA') +
  theme_bluelight()