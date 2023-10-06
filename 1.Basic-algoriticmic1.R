# Create a function to find the minimum value in a Data Frame
find_minimum <- function(x) {
  min <- x[1]
  for (i in 2:length(x)) {
    if (x[i] < min) {
      min <- x[i]
    }
  }
  return(min)
}
# Checking the Minimum function 
find_minimum(c(1,2,3,4,5,6,7,8,9,10))

#  Create a function to find the maximum value in a Data Frame
find_maximum <- function(x) {
  max <- x[1]
  for (i in 2:length(x)) {
    if (x[i] > max) {
      max <- x[i]
    }
  }
  return(max)
}
# Checking the Maximum function
find_maximum(c(1,2,3,4,5,6,7,8,9,10))

# Create a function to find the Average value in a Data Frame

find_average <- function(x) {
  sum <- 0
  for (i in 1:length(x)) {
    sum <- sum + x[i]
  }
  return(sum/length(x))
}
# Checking the Average Function
find_average(c(1,2,3,4,5,6,7,8,9,10))

# Read the csv file
df <- read.csv("City_temperature_data.csv", sep = ";")
View(df)
# Structure of data frame
str(df)
head(df)


# Now check the Minimum Temperature of all cities 
# By  Using my Created Function 
find_minimum(df$London)
find_minimum(df$New.York)
find_minimum(df$Stockholm)
find_minimum(df$Paris)
find_minimum(df$Berlin)
find_minimum(df$Oslo)
find_minimum(df$Helsinki)
find_minimum(df$Dhaka)
find_minimum(df$Tokyo)
find_minimum(df$Rome)

# Now check the Minimum Temperature of all cities 
# By Manual min()
min(df$London)
min(df$New.York)
min(df$Stockholm)
min(df$Paris)
min(df$Berlin)
min(df$Oslo)
min(df$Helsinki)
min(df$Dhaka)
min(df$Tokyo)
min(df$Rome)

# Now check the Maximum Temperature of all cities 
# By  Using my Created Function

find_maximum(df$London)
find_maximum(df$New.York)
find_maximum(df$Stockholm)
find_maximum(df$Paris)
find_maximum(df$Berlin)
find_maximum(df$Oslo)
find_maximum(df$Helsinki)
find_maximum(df$Dhaka)
find_maximum(df$Tokyo)
find_maximum(df$Rome)

# Now check the Maximum Temperature of all cities 
# By Manual max()
max(df$New.York)
max(df$New.York)
max(df$Stockholm)
max(df$Paris)
max(df$Berlin)
max(df$Oslo)
max(df$Helsinki)
max(df$Dhaka)
max(df$Tokyo)
max(df$Rome)
max(df$London)

# Now check the Average Temperature of all cities 
# By  Using my Created Average function

find_average(df$London)
find_average(df$New.York)
find_average(df$Stockholm)
find_average(df$Paris)
find_average(df$Berlin)
find_average(df$Oslo)
find_average(df$Helsinki)
find_average(df$Dhaka)
find_average(df$Tokyo)
find_average(df$Rome)

# Now check the Average Temperature of all cities 
# By Manual mean()

mean(df$London)
mean(df$New.York)
mean(df$Stockholm)
mean(df$Paris)
mean(df$Berlin)
mean(df$Oslo)
mean(df$Helsinki)
mean(df$Dhaka)
mean(df$Tokyo)
mean(df$Rome)

