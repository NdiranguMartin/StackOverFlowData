library(tidyverse)
the.data <- read_csv("RawData.csv")

the.data <- the.data %>% 
  mutate(ID = 1:as.numeric(count(the.data))) %>% 
  select(ID, everything()) %>% 
  mutate(ID = as.numeric(ID))


sample1 <-  the.data[sample(nrow(the.data), 15), ]
cleaning.function <- function(sample1)
{
  sample1[is.na(sample1)] = "NULL"
  sample1 <- sample1 %>% 
    mutate(dev.environment = str_c(dev.environment.Windows,
                                   dev.environment.macOS,
                                   `dev.environment.Unix / Linux`,
                                   `dev.environment.Other - Write In`,
                                   sep = ".")) %>% 
    
    mutate(app.type = str_c(`app.type.Web Back-end`,
                            `app.type.Web Front-end`,
                            app.type.Mobile, 
                            app.type.Desktop, 
                            `app.type.Enterprise Back-end Service`, 
                            `app.type.Data analysis / BI`,
                            `app.type.Embedded / IoT`,
                            `app.type.Other - Write In`,
                            sep = "."))
  
  sample1$dev.environment <- str_replace_all(sample1$dev.environment, c("NULL." = "", ".NULL" = ""))
  sample1$app.type <- str_replace_all(sample1$app.type, c("NULL." = "", ".NULL" = ""))
  cleaned.data <- select(sample1, country, language, age, channel, dev.environment, app.type, team.distributed, ambidexterity)
  
  return(cleaned.data)
}
new.data <- cleaning.function(the.data)



###########################################################


#################### ALTERNATIVE ###############

################################-------------
# Use regular expressions to extract specific column names you want from the data (google regex)
#In R, grepl function does this for you, returns true if there exists a value matching your expression and FALSE otherwise. 
#Example, to check if there exists a value "programming.language." in our column names, do...

#use grepl
grepl("programming.language.", names(sample1)) #--- observe that every column containig "programming.language." in it regardless of 
#what follows or comes before it is marked as TRUE and false otherwise

#We therefore can use that to pick any variable we want from the data::
#  Use grepl
programming_Language <- sample1[ , grepl( "programming.language." , names( sample1 ) ) ]
names(programming_Language) #Observe that all the "programming.language" columns have been selected. However, we want to narrow down to pure programming language 
#only, without "migrate"-where they plan to migrate to and "primary"- their primary go 2 language...these can be fetched separately and added as diffrent columns 
#later on...so tunazichuja same way...

#For more than one pattern extraction....
#create a variable with your patterns
patterns<-c("migrate","primary") #CASE SENSITIVE

programming_Language_pure <- programming_Language[ , grepl( paste(patterns,collapse="|") , names( programming_Language ) )==FALSE ] #Notice use of FALSE
#to only pick ones that DO NOT have "migrate" and "Primary" in them.
names(programming_Language_pure) #with this we have a reduced data easier to work with for that one variable




########################################
############################################
#Now bind these dataframe with any other column(from original data) you want to make our new dataframe with what we need only...Example;

new_data <- data.frame(ID = sample1$ID, language = sample1$language, country = sample1$country, programming_Language_pure ) 
names(new_data)#we can now reformat this data into long format (columnwise for a variable x) in this case for programming language


#Reshape data
#we can now reformat this data into long format (columnwise for a variable x)
library(reshape2) #data formatting tool

new_data1 <- melt(new_data, id.vars = c("language", "country")) #id vars contains any column that you need and is already in long format...example the two:
new_data2 <- melt(new_data, id.vars = c("language", "country"), na.rm = TRUE) #use this to remove nulls which we dont need...check difference between the two


#rename columns for new data and remove "variable" column 
cleanData <- new_data2 %>% select(language, country, programmingLanguage = value)
head(cleanData)


##NB dplyr also has a filter function to help youfilter anything by any column which is even easier than the above...read on it and try it out 


#creating data exclusive for dev environment

environment.function <- function(sample1)
{
  dev.environment <- sample1[, grepl("dev.environment", names(sample1))]
  names(dev.environment)
  
  dev.environment.data <- data.frame(ID = sample1$ID,language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, dev.environment)
  dev.environment.data <- melt(dev.environment.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  dev.environment.data <- dev.environment.data %>% 
    select(ID, language, channel, country, age, dev.environment = value)
  
  return(dev.environment.data)
}
dev.environment.data <- environment.function(the.data)

#creating data exclusively for app type

app.type.function <- function(sample1)
{
  app.type <- sample1[, grepl("app.type", names(sample1))]
  names(app.type)
  
  app.type.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, app.type)
  app.type.data <- melt(app.type.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  app.type.data <- app.type.data %>% 
    select(ID, language, channel, country, age, app.type = value)
  
  return(app.type.data)
}
app.type.data <- app.type.function(the.data)

#creating data exclusively for mobile OS
mobile.os.function <- function(sample1)
{
  mobile.os <- sample1[, grepl("mobile.os", names(sample1))]
  names(mobile.os)
  
  mobile.os.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, mobile.os)
  mobile.os.data <- melt(mobile.os.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  mobile.os.data <- mobile.os.data %>% 
    select(ID, language, channel, country, age, mobile.os = value)
  
  return(mobile.os.data)
}
mobile.os.data <- mobile.os.function(the.data)

desktop.os.function <- function(sample1)
{
  desktop.os <- sample1[, grepl("desktop.os", names(sample1))]
  
  desktop.os.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, desktop.os)
  desktop.os.data <- melt(desktop.os.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  desktop.os.data <- desktop.os.data %>% 
    select(ID, language, channel, country, age, desktop.os = value)
  
  return(desktop.os.data)
}
desktop.os.data <- desktop.os.function(the.data)

programmming.language.function <- function(sample1)
{
  programming.language <- sample1[, grepl("programming.language", names(sample1))]
  patterns <- c("migrate", "primary")
  programming.language <- programming.language[, grepl( paste(patterns, collapse="|"), names(programming.language)) == FALSE ]# not yet conversant with this step
  
  programming.language.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, programming.language)
  programming.language.data <- melt(programming.language.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  programming.language.data <- programming.language.data %>% 
    select(ID, language, channel, country, age, programming.language = value)
  
  return(programming.language.data)
}
programming.language.data <- programmming.language.function(the.data)

programmming.language.migrate.function <- function(sample1)
{
  programming.language.migrate <- sample1[, grepl("programming.language.migrate", names(sample1))]
  
  programming.language.migrate.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, programming.language.migrate)
  programming.language.migrate.data <- melt(programming.language.migrate.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  programming.language.migrate.data <- programming.language.migrate.data %>% 
    select(ID, language, channel, country, age, programming.language.migrate = value)
  
  return(programming.language.migrate.data)
}
programming.language.migrate.data <- programmming.language.migrate.function(the.data)

programming.language.primary.function <- function(sample1)
{
  programming.language.primary <- sample1[, grepl("programming.language.primary", names(sample1))]
  
  programming.language.primary.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, programming.language.primary)
  programming.language.primary.data <- melt(programming.language.primary.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  programming.language.primary.data <- programming.language.primary.data %>% 
    select(ID, language, channel, country, age, programming.language.primary = value)
  
  return(programming.language.primary.data)
}
programming.language.primary.data <- programming.language.primary.function(the.data)

unit.testing.function <- function(sample1)
{
  unit.testing <- sample1[, grepl("unit.testing", names(sample1))]
  unit.testing <- unit.testing[, 1]
  
  unit.testing.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, unit.testing)
  unit.testing.data <- melt(unit.testing.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  unit.testing.data <- unit.testing.data %>% 
    select(ID, language, channel, country, age, unit.testing = value)
  
  return(unit.testing.data)
}
unit.testing.data <- unit.testing.function(the.data)

database.function <- function(sample1)
{
  database <- sample1[, grepl("database.", names(sample1))]
  patterns <- c("host", "migrate", "how.often")
  database <- database[, grepl( paste(patterns, collapse="|"), names(database)) == FALSE ]
  
  database.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, database)
  database.data <- melt(database.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  database.data <- database.data %>% 
    select(ID, language, channel, country, age, database = value)
  
  return(database.data)
}
database.data <- database.function(the.data)

database.migrate.function <- function(sample1)
{
  database.migrate <- sample1[, grepl("database.migrate", names(sample1))]
  
  database.migrate.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, database.migrate)
  database.migrate.data <- melt(database.migrate.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  database.migrate.data <- database.migrate.data %>% 
    select(ID, language, channel, country, age, database.migrate = value)
  
  return(database.migrate.data)
}
database.migrate.data <- database.migrate.function(the.data)

database.host.function <- function(sample1)
{
  database.host <- sample1[, grepl("database.host", names(sample1))]
  
  database.host.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, database.host)
  database.host.data <- melt(database.host.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  database.host.data <- database.host.data %>% 
    select(ID, language, channel, country, age, database.host = value)
  
  return(database.host.data)
}
database.host.data <- database.host.function(the.data)

tool.usage.function <- function(sample1)
{
  tool.usage <- sample1[, grepl("tool.usage", names(sample1))]
  
  tool.usage.data <- data.frame(ID = sample1$ID, language = sample1$language, channel = sample1$channel, country = sample1$country, age = sample1$age, tool.usage)
  tool.usage.data <- melt(tool.usage.data, id.vars = c("ID","language", "channel", "country", "age"), na.rm = T)
  tool.usage.data <- tool.usage.data %>% 
    select(ID, language, channel, country, age, tool.usage = value)
  
  return(tool.usage.data)
}
tool.usage.data <- tool.usage.function(the.data)

join.by <- c("ID","language", "channel", "country", "age")

final.data <- the.data %>% 
  select(join.by)

final.data <- left_join(final.data, dev.environment.data)
final.data <- left_join(final.data, app.type.data)
final.data <- left_join(final.data, mobile.os.data)
final.data <- left_join(final.data, desktop.os.data)
final.data <- left_join(final.data, programming.language.migrate.data)
final.data <- left_join(final.data, programming.language.primary.data)
final.data <- left_join(final.data, unit.testing.data)
final.data <- left_join(final.data, database.data)
final.data <- left_join(final.data, database.migrate.data)
final.data <- left_join(final.data, database.host.data)
final.data <- left_join(final.data, tool.usage.data)

StackScaled <- final.data %>% 
  drop_na() %>% 
  
  filter(!duplicated(ID))
write.csv(StackScaled, "StackScaled.csv")
