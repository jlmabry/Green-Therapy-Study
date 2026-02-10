install.packages("car")
library(car)

filepath <- "C:/Users/justi/OneDrive/School/Experiments and Models in Cognition/final/responses.csv"
df <- read.csv(filepath)
head(data) # make sure it's right format

df <- data.frame(
  subject = as.factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)),
  stress = as.numeric(c(5, 3, 6, 4, 7, 5, 8, 6, 4, 2)),
  energized = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  anxious = as.numeric(c(3, 2, 6, 4, 7, 5, 8, 7, 4, 2)),
  calm = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  happy = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  optimistic = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  connected = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  motivated = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  feel = as.numeric(c(4, 5, 3, 6, 5, 7, 2, 3, 4, 5)),
  time = as.factor(c("before", "after", "before", "after", "before", "after", "before", "after", "before", "after"))
)

cor_matrix <- cor(df[, c("energized", "anxious", "calm", "happy", "optimistic", "connected", "motivated", "feel")])
print(cor_matrix)

df_reduced <- df[, !colnames(df) %in% c("calm", "happy", "optimistic", "connected", "motivated", "feel")]

model_reduced <- lm(stress ~ energized + anxious + time, data = df_reduced)
anova_result <- Anova(model_reduced, type = 3)
print(anova_result)

# this next part is for t-tests!!!
df <- data.frame(
  subject = as.factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)),
  stress = as.numeric(c(5, 3, 6, 4, 7, 5, 8, 6, 4, 2)),
  time = as.factor(c("before", "after", "before", "after", "before", "after", "before", "after", "before", "after"))
)
install.packages("BSDA")
library(BSDA)
sign_test_result <- SIGN.test(df_wide$before, df_wide$after, alternative = "two.sided", paired = TRUE)
print(sign_test_result)

# box plot for stress before and after
library(ggplot2)

# Create a boxplot
ggplot(df, aes(x = time, y = stress, fill = time)) +
  geom_boxplot() +
  labs(title = "Stress Levels Before and After Going Outside",
       x = "Time",
       y = "Stress Level") +
  theme_minimal()

# bar plot for anova results
# Calculate mean stress levels
means <- aggregate(stress ~ time, data = df, mean)

# Create a bar plot
ggplot(means, aes(x = time, y = stress, fill = time)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Stress Levels Before and After Going Outside",
       x = "Time",
       y = "Mean Stress Level") +
  theme_minimal()


