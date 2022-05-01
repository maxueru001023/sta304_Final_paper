

# install.packages("knitr")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("kableExtra")
# install.packages("car")
# install.packages("dplyr")
# install.packages("haven")
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("forcats")
# install.packages("tidyr")

# setup libraries 
library(knitr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(kableExtra)
library(car)
library(dplyr)
library(haven)
library(hrbrthemes)
library(viridis)
library(forcats)
library(tidyr)






## load Data

gss2021 <- read_dta("gss2021.dta")





## data cleaning
gss2021_cleaned <- gss2021 %>%
  mutate(`educ_level` = case_when(
    educ >= 1 & educ <= 6 ~ 'elementary school',
    educ >= 7 & educ <=8 ~ 'middle school',
    educ >= 9 & educ <=12 ~ 'high school',
    educ >= 13 ~ 'college or above')) %>%
  mutate(`age_group` = case_when(
    age >= 10 & age <= 19 ~ '10-19',    # the first age group includes respondents with age between 10 and 19 years old
    age >= 20 & age <=29~ '20-29',      # the second age group includes respondents with age between 20 and 29 years old
    age >= 30 & age <= 39 ~ '30-39',    # the third age group includes respondents with age between 30 and 39 years old
    age >= 40 & age <=49~ '40-49',      # the forth age group includes respondents with age between 40 and 49 years old
    age >= 50 & age <=59~ '50-59',      # the fifth age group includes respondents with age between 50 and 59 years old
    age >= 60 & age <=69~ '60-69',      # the sixth age group includes respondents with age between 60 and 69 years old
    age >= 70 & age <=79~ '70-79',      # the seventh age group includes respondents with age between 70 and 79 years old
    age >= 80 ~ '80+')) %>%             # the eighth age group includes respondents with age larger than 80 years old
  filter(!is.na(wwwhr) & !is.na(income)  & !is.na(age) & !is.na(educ) & !is.na(emailhr) & !is.na(quallife) & !is.na(hlthphys) & !is.na(hlthmntl) & !is.na(sex) ) %>% 
  select(wwwhr,income,age,educ,emailhr,quallife,hlthphys,hlthmntl,age_group,educ_level,sex) 

gss2021_data <- gss2021_cleaned

## make sure everything is factor
gss2021_cleaned$wwwhr <- as.numeric(gss2021_cleaned$wwwhr)
gss2021_cleaned$income <- as.factor(gss2021_cleaned$income)
gss2021_cleaned$age <- as.numeric(gss2021_cleaned$age)
gss2021_cleaned$educ <- as.numeric(gss2021_cleaned$educ)
gss2021_cleaned$emailhr <- as.factor(gss2021_cleaned$emailhr)
gss2021_cleaned$quallife <- as.factor(gss2021_cleaned$quallife)
gss2021_cleaned$hlthphys <- as.factor(gss2021_cleaned$hlthphys)
gss2021_cleaned$hlthmntl <- as.factor(gss2021_cleaned$hlthmntl)
gss2021_cleaned$age_group <- as.factor(gss2021_cleaned$age_group)
gss2021_cleaned$educ_level <- as.factor(gss2021_cleaned$educ_level)
gss2021_cleaned$sex <- as.factor(gss2021_cleaned$sex)

# install.packages("pointblank")
library(pointblank)
agent <-
  create_agent(tbl = gss2021_cleaned) %>%
  col_is_factor(columns = vars(income, emailhr,quallife,hlthmntl,hlthphys,age_group,educ_level,sex)) %>%
  col_is_numeric(columns = vars(wwwhr,age, educ)) %>% interrogate()
# agent
