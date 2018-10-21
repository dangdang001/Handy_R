# http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

# Important dplyr verbs to remember:

# dplyr verbs	Description
# select()	select columns
# filter()	filter rows
# arrange()	re-order or arrange rows
# mutate()	create new columns
# summarise()	summarise values
# group_by()	allows for group operations in the “split-apply-combine” concept


# 1. Selecting columns using select()
Data1 <- select(data, col1,col2,...)
Data2 <- select(data, -colx)
Data3 <- select(data, col1:col10)
Data4 <- select(data, starts_with("sl"))

# ends_with() = Select columns that end with a character string
# contains() = Select columns that contain a character string
# matches() = Select columns that match a regular expression
# one_of() = Select columns names that are from a group of names


# 2. Selecting rows using filter()
filter(data, col1 >= 16)
filter(data, col1 >= 16, col2 >= 1)
filter(data, col3 %in% c("Perissodactyla", "Primates"))

# 3. Pipe operator: %>%

data.out<-data %>%
  select(co1,col2,col3,col4,col5) %>%
  arrange(col4, desc(col5)) %>% 
  group_by(col1,col2) %>%
  summarize(my_ave=mean(col3),my_median=median(col3),total=n())


# 4. Create new columns using mutate()

# The mutate() function will add new columns to the data frame. 
# Create a new column called rem_proportion which is the ratio of rem sleep to total amount of sleep.

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

