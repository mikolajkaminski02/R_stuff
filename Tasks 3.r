# 1. Load VG sales dataset ----
install.packages("readr")
install.packages("dplyr")
library(readr)
library(dplyr)
library
vgsales <- read_csv("vgsales.csv")

#Remember to load the libraries 

# 2. Find how many  missing cases are in the dataset ----
sum(is.na(vgsales))

# 3. Check unique Genres in the data  ----
## 3.1. using base R 
str(vgsales)
## 3.2. using dplyr
glimpse(vgsales)
# 4. Find the total sales for Genres using dplyr -----
#Hint: google how to use group_by() function - used to generate summary statistics from the data frame
vgsales %>% dplyr:: select(Genre, Global_Sales)

vgsales %>%
  group_by(Genre) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# 5. Make a boxplot to check differences between Global_Sales by Genres ----
boxplot(vgsales$Genre) 


# 6. Try to use beanplot from the package "beanplot"  to do the same as in task 5 -----
install.packages("beanplot")
library('beanplot')

beanplot(Global_Sales ~ Genre, data = vgsales)

# Which plot is better for visualization of the variables? ----
#bean plot 


