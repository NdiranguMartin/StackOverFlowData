library(tidyverse)

stack.scaled <- read_csv("StackScaled.csv")
stack.scaled <- stack.scaled[-1]

# A bar plot for language
ggplot(stack.scaled, mapping = aes( x = language, fill = language)) +
  geom_bar(show.legend = F) +
  coord_flip() + 
  labs(title = "Distribution of languages")

# A pie chart for channel
ggplot(stack.scaled, mapping = aes(x = "", y = channel, fill = channel)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "A pie showing Channels") 

# The counties are so many, what approach do we give them?

# A bar graph for age
ggplot(stack.scaled, mapping = aes(x = age, fill = age)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users into different age groups")
  
# A bar graph for the develepment environment
ggplot(stack.scaled, mapping = aes(x = dev.environment, fill = dev.environment)) +
  geom_bar(show.legend = F) +
  labs("Users Development Environment")

# A pie graph for app type
ggplot(stack.scaled, mapping = aes(x = "", y = "app.type", fill = app.type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") + 
  labs(title =  "Users' application type")

# A bar plot for mobile OS
ggplot(stack.scaled, mapping = aes(x = mobile.os, fill = mobile.os)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on Mobile OS")

# A bar plot for desktop OS
ggplot(stack.scaled, mapping = aes(x = desktop.os, fill = desktop.os)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on Desktop OS")

# A bar plot for primary programming language
ggplot(stack.scaled, mapping = aes(x = programming.language.primary, fill = programming.language.primary)) +
  geom_bar(show.legend = F) +
  coord_flip() +
  labs(title = "Distribution of users based on their Primary Programming Language")

# A bar plot for migrate programming language
ggplot(stack.scaled, mapping = aes(x = programming.language.migrate, fill = programming.language.migrate)) +
  geom_bar(show.legend = F) +
  coord_flip() +
  labs(title = "Distribution of users based on their migrate Programming Language")

# A bar plot for unit testing
ggplot(stack.scaled, mapping = aes(x = unit.testing, fill = unit.testing)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on unit testing")

# A bar plot for database
ggplot(stack.scaled, mapping = aes(x = database, fill = database)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on the database they use")

# A bar plot for database and host
ggplot(stack.scaled, mapping = aes(x = database.host, fill = database.host)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on the database host they use")

# A bar plot for database migrate
ggplot(stack.scaled, mapping = aes(x = database.migrate, fill = database.migrate)) +
  geom_bar(show.legend = F) +
  coord_flip() +
  labs(title = "Distribution of users based on the database they wish to migrate to")

# A bar graph for tool usage
tool.usage <- c("All day long", "Several times a day", "Several times a week", "Less frequently", "Never")

stack.scaled$tool.usage <- factor(stack.scaled$tool.usage, 
                                  levels = tool.usage, 
                                  labels = tool.usage)

ggplot(stack.scaled, mapping = aes(x = tool.usage, fill = tool.usage)) +
  geom_bar(show.legend = F) +
  labs(title = "Distribution of users based on tool usage")
