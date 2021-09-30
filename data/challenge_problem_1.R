library(plyr)
require(dplyr)
library(lubridate)
library(ggpubr)
library(data.table)
require(ggplot2)
require(reshape2)
library(tidyr)
library(base)

setwd("C:/Users/casuk/Desktop/RScripts/nics-firearm-background-checks/data")
BC_data <- read.csv("nics-firearm-background-checks.csv", header = TRUE)

# Structure of data #
str(BC_data)

# Summary of handguns #
summary(BC_data)



# General Idea : Take 3 states of differing political leanings - Alabama (R), California (D), Ohio (Swing)
# and Conduct a time series of permits for handguns from Jan. 2018 to Aug. 2021
# Does the political leaning impact the amount of permits per state? What is happening in those
# Time periods contributing to the spike?
data2 = BC_data %>% select(month, state, permit, handgun, totals)

## Didn't need as.Date since the database already had dates as date data objects
data3 = filter(data2, month >= "2018-01")

# --- sin NY ---
data3_small = filter(data3, state=='Alabama' | state=='California' | state=='Ohio')

# Time series past 42 months;
ggplot(data3_small, aes(month, handgun, group = state)) +
  geom_line(aes(color = state), size = 2) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) +
  labs(x= "Year", y = "# of permits for handguns", title = "Handgun permits in AL, OH and CA from 2018-01 to 2021-08")


# --- con NY ---
data3_small = filter(data3, state=='Alabama' | state=='California' | state=='Ohio' | state=='New York')

# Time series past 42 months;
ggplot(data3_small, aes(month, handgun, group = state)) +
  geom_line(aes(color = state), size = 2) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) +
  labs(x= "Year", y = "# of permits for handguns", title = "Handgun permits in AL, OH, NY and CA from 2018-01 to 2021-08")

# ====== Look at 2021 data ======
data4 = filter(data3_small, month >= "2021-01")

# Graph of the past 8 months
ggplot(data4, aes(month, handgun, group = state)) +
  geom_line(aes(color = state), size = 2) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        text=element_text(size=9)) +
  labs(x= "Year", y = "# of permits for handguns", title = "Handgun permits in AL, OH and CA from 2021-01 to 2021-08")


