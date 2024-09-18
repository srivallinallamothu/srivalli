# Load necessary libraries
library(ggplot2)

# Load your dataset
df <- read.csv("C:/Users/sriva/OneDrive/Desktop/r/data_ted_talks.csv")


# Check the structure of the dataset
head(df)

# Define the independent variable (views) and dependent variable (duration)
X <- df$views  # Independent variable
y <- df$duration  # Dependent variable

# Fit the linear regression model
model <- lm(duration ~ views, data = df)

# Predict the duration based on views
df$predicted_duration <- predict(model, newdata = df)

# Plot the data points and the regression line
ggplot(df, aes(x = views, y = duration)) +
  geom_point(color = 'blue', size = 2) +  # Data points
  geom_line(aes(y = predicted_duration), color = 'red', size = 1) +  # Regression line
  labs(title = "Linear Regression: Views vs. Duration", x = "Views", y = "Duration") +
  theme_minimal()

# Load necessary libraries
library(ggplot2)

# Read the data from a CSV file
df <- read.csv("C:\\Users\\sriva\\OneDrive\\Desktop\\r\\data_ted_talks.csv")

# Display the first few rows of the data
head(df)

# Assuming 'views' and 'duration' are the columns you want to use for linear regression
# Remove any rows with NA values in 'views' or 'duration'
df <- na.omit(df)

# Create a linear regression model
model <- lm(duration ~ views, data = df)

# Predict the duration based on views
df$predicted_duration <- predict(model, newdata = df)

# Plot the data points and the regression line using ggplot2
ggplot(df, aes(x = views, y = duration)) +
  geom_point(color = "blue") +  # Scatter plot of actual data
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
  labs(x = "Views", y = "Duration", title = "Linear Regression: Views vs. Duration") +
  theme_minimal()

# Example data (you can replace this with your actual data)
example_data <- data.frame(
  views = c(100, 200, 300, 400, 500),  # Independent variable (views)
  comments = c(10, 20, 25, 35, 50)    # Dependent variable (comments)
)

# Ensure the columns 'views' and 'comments' exist and are numeric
if ("views" %in% names(example_data) && "comments" %in% names(example_data)) {
  # Create a linear regression model
  example_model <- lm(comments ~ views, data = example_data)
  
  # Predict the comments based on views
  example_data$predicted_comments <- predict(example_model, newdata = example_data)
  
  # Plot the data points and the regression line using ggplot2
  ggplot(example_data, aes(x = views, y = comments)) +
    geom_point(color = "blue") +  # Scatter plot of actual data
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
    labs(x = "Views", y = "Comments", title = "Linear Regression: Views vs. Comments") +
    theme_minimal()
} else {
  print("Error: 'views' or 'comments' columns are missing or incorrectly defined.")
}
