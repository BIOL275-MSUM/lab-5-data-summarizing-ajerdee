
# Load Packages -----------------------------------------------------------

library(tidyverse)    # load the tidyverse package
iris <- as_tibble(iris) # so it prints a little nicer

# question 1 --------------------------------------------------------------

iris <- rename(iris, sepal_length = Sepal.Length, sepal_width = Sepal.Width, petal_length = Petal.Length, petal_width = Petal.Width, species = Species)
iris


# question 2 --------------------------------------------------------------

iris_q2 <- mutate(iris, sepal_length_cm = sepal_length * 10, sepal_width_cm = sepal_width * 10, petal_length_cm = petal_length * 10, petal_width_cm = petal_width * 10)
iris_q2 <- select(iris_2, species, sepal_length_cm, sepal_width_cm, petal_length_cm, petal_width_cm)
iris_q2

# question 3 --------------------------------------------------------------

iris_q3 <- mutate(iris, sepal_area = sepal_length * sepal_width, petal_area = petal_length * petal_width)
iris_q3 <- select(iris3, sepal_area, petal_area, species)
iris_q3

# question 4 --------------------------------------------------------------

summary_q4 <- summarise(iris, sampl_size = n(), max_value = max(sepal_length), min_value = min(sepal_length), range = max_value - min_value, median = median(sepal_length), q1 = quantile(sepal_length, probs = 0.25), q3 = quantile(sepal_length, probs = 0.75), iqr = q3 - q1)
summary_q4

# question 5 --------------------------------------------------------------

iris_grouped <- group_by(iris, species)
summary_q5 <- summarise(iris_grouped, sampl_size = n(), mean_petal_width = mean(petal_width), sd_petal_width = sd(petal_width), variance = var(petal_width), sem = mean_petal_width / sqrt(sampl_size), ci_lower = mean_petal_width - 2 * sem, ci_upper = mean_petal_width + 2 * sem)
summary_q5

# question 6 --------------------------------------------------------------

ggplot(data = iris) + 
  geom_jitter(mapping = aes(x = species, y = petal_length)) + 
  labs(x = "Species", y = "Petal Length (mm)")

# question 7 --------------------------------------------------------------

iris_summary <- summarise(iris_grouped, mean_petal_length = mean(petal_length), sem = sd(petal_length) / sqrt(n()), upper_limit = mean_petal_length + 1.96 * sem, lower_limit = mean_petal_length - 1.96 * sem)
ggplot(data = iris) + geom_jitter(mapping = aes(x = species, y = petal_length)) + geom_crossbar(data = iris_summary, mapping = aes(x = species, y = mean_petal_length, ymax = upper_limit, ymin = lower_limit), color = "red") + labs(x = "Species", y = "Petal Length (mm)")


# question 8 --------------------------------------------------------------

ggplot(data = iris) + geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.8) + labs(x = "Petal Length (mm)", y = "Petal Width (mm)", color = "Species") 


