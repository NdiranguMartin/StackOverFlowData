library(tidyverse)
library(funModeling)

#importing the data
stack.scaled <- read_csv("StackScaled.csv")
stack.scaled <- stack.scaled[-1]

#investigating the structure of the data
str(stack.scaled)

glimpse(stack.scaled)

df_status(stack.scaled)

#Getting frequency tables and plots for categorical variables
freq(stack.scaled)
