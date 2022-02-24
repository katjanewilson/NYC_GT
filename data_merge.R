### load libraries
library(tidyverse)
library(readr)
### bring in raw
X2017_2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT <- read_csv("/cloud/project/raw/2017-2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT.csv")
#take out class size because that doesn't include D75
X2018_2019_School_Demographic_Snapshot <- read_csv("/cloud/project/raw/2018-2019_School_Demographic_Snapshot.csv")
X2013_2019_Attendance_Results_School <- read_csv("/cloud/project/raw/2013-2019_Attendance_Results_-_School.csv")

################
### Step I: Create the full_school data file
################

### step 1: from the demographics, get one row per DBN of just the demographics (they stay constant for 2017)
demos<- X2018_2019_School_Demographic_Snapshot %>%
  filter(Year == "2017-18") %>%
  select(DBN, `School Name`, `Total Enrollment`, 
         `Economic Need Index`, '% Male', '% Black', '% White', 
         '% Students with Disabilities', '% Poverty', '% English Language Learners',
         '% Asian', '% Hispanic')
### step 2: merge the Grade + Program Type variables, then spread 
classes <- X2017_2018_SCHOOL_LEVEL_CLASS_SIZE_REPORT %>%
  mutate(program_type_grade = paste(`Program Type`, '-', `Grade Level`)) %>%
  select(DBN, program_type_grade, `Number of Classes`)%>%
  # mutate(grouped_id = row_number()) %>%
  spread(program_type_grade, `Number of Classes`)
#replace NA with 0
classes[is.na(classes)] <- 0
#rename columns
classes <- classes %>%
  rename(GT1 = "G&T - 1", GT2 = "G&T - 2", GT3 = "G&T - 3",
         GT4 = "G&T - 4", GT5 = "G&T - 5", GTK = "G&T - K",
         GE1 = "Gen Ed - 1",  GE2 = "Gen Ed - 2",  GE3 = "Gen Ed - 3", 
         GE4 = "Gen Ed - 4",  GE5 = "Gen Ed - 5",  GE6 = "Gen Ed - 6", 
         GE7 = "Gen Ed - 7",  GE8 = "Gen Ed - 8",  GEK = "Gen Ed - K", 
         ICT1 = "ICT - 1",   ICT2 = "ICT - 2",   ICT3 = "ICT - 3", 
         ICT4 = "ICT - 4",   ICT5 = "ICT - 5",   ICT6 = "ICT - 6",
         ICT7 = "ICT - 7",   ICT8 = "ICT - 8",ICTK = "ICT - K",
         ICTGT1 = "ICT & G&T - 1",  ICTGT2 = `ICT & G&T - 2`,  ICTGT3 = `ICT & G&T - 3`,
         ICTGT4 = "ICT & G&T - 4",  ICTGT5 = `ICT & G&T - 5`,  ICTGTK = `ICT & G&T - K`,
         SC121 = "SC 12:1 - K-8 SC", SC1211 = "SC 12:1:1 - K-8 SC", SC151 = "SC 15:1 - K-8 SC", 
         SC611 = "SC 6:1:1 - K-8 SC", SC811 = "SC 8:1:1 - K-8 SC")
### step 2A: create a binary indicator of whether middle school, K-8, or K-5
classes <- classes %>%
  mutate(middle_levels = ifelse((GE6 | GE7 | GE8 > 1) |(ICT6 | ICT7 | ICT8 > 1) , 1,0),
         elementary_levels = ifelse((GE1 | GE2 | GE3 |GE4| GE5 >1) | (ICT1 |ICT2|ICT3|ICT4|ICT5 > 1), 1,0),
         self_contained = ifelse(SC121 | SC1211 | SC151 | SC611 | SC811 > 1, 1,0),
         gifted = ifelse(GT1 | GT2 | GT3 | GT4 |GT5 |GTK >1, 1,0),
         self_contained_and_gifted = ifelse(self_contained ==1 & gifted ==1,1,0))
classes <- classes %>%
  mutate(school_type = case_when(middle_levels ==1 & elementary_levels ==0 ~ "Middle",
                                 middle_levels ==0 & elementary_levels ==1 ~ "Elementary",
                                 middle_levels == 1 & elementary_levels ==1 ~ "K to 8",
                                 middle_levels ==0 & elementary_levels ==0 ~ "gifted"),
         class_option = case_when(self_contained ==1 & gifted ==0 ~ "SC",
                                  self_contained ==1 & gifted ==1 ~ "SC and GT",
                                  self_contained ==0 & gifted == 0 ~ "no option",
                                  self_contained == 0 & gifted ==1 ~ "GT"))
classes <- classes %>%
  mutate(borough = case_when(str_detect(`DBN`, "X") ~"Bronx",
                             str_detect(`DBN`, "Q") ~ "Queens",
                             str_detect(`DBN`, "K") ~"Brooklyn",
                             str_detect(`DBN`, "M") ~ "Manhattan",
                             str_detect(`DBN`, "R") ~"Staten Island"))
## step 3: merge with the demographics
merged <- merge(demos, classes, by = "DBN") %>%
  distinct()

### step 4: get attendance
attendance<- X2013_2019_Attendance_Results_School %>%
  filter(`Grade` == "All Grades" & `Demographic Variable` == "All Students") %>%
  select(DBN, '% Attendance', '% Chronically Absent', 'Year') %>%
  filter(Year == '2017-18')

### merge the DVs
### merge with attendance
mergedA <- merge(merged, attendance, by = "DBN")

### merge the ela and math scores
mergedB <- merge(mergedA, math_wide_1_, by = "DBN")
mergedC <- merge(mergedB, ela_wide_1_, by = "DBN")
total_data_wide<- mergedC

### saved mergedC
saveRDS(total_data_wide, file = "/cloud/project/data/total_data_wide.RDS")
