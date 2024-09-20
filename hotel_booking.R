library(tidyverse)
library(lubridate)
library(modeest)

data = read_csv("/cloud/project/hotel_booking.csv")
View(data)
nrow(is.na(data))
summary(data)
glimpse(data)
data$children[is.na(data$children)] = mfv(data$children , na_rm = TRUE)

data %>% summarize(across(where(is.character), n_distinct))
nrow(is.na(data$arrival_date_month))
data$arrival_date_day_of_month
ggplot(data = data, aes(x = agent)) + 
  geom_boxplot()

data$agent[is.na(data$agent)] = median(data$agent ,na.rm = TRUE)
ggplot(data = data, aes(x = agent)) + 
  geom_boxplot()

nrow(is.na(data$agent))

data <- data %>%
  mutate(arrival_date = make_date(arrival_date_year, match(arrival_date_month, month.name), arrival_date_day_of_month)





















library(tidyverse)
library(lubridate)
hotel_data <- read.csv("hotel_booking.csv")
glimpse(hotel_data)
hotel_data %>% summarize_all(funs(sum(is.na(.))))
hotel_data <- hotel_data %>%
  mutate(children = ifelse(is.na(children), 0, children),
         agent = replace_na(agent, "Unknown"))

hotel_data <- hotel_data %>%
  mutate(arrival_date = make_date(arrival_date_year, match(arrival_date_month, month.name), arrival_date_day_of_month))
hotel_data %>% summarize(across(where(is.character), n_distinct))
hotel_data %>%
  group_by(arrival_date_month, is_canceled) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = reorder(arrival_date_month, -total_bookings), y = total_bookings, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Number of Bookings", fill = "Cancellation", title = "Monthly Bookings and Cancellations") +
  theme_minimal()

ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(x = "Lead Time (Days)", y = "Frequency", title = "Lead Time Distribution") +
  theme_minimal()

hotel_data %>%
  group_by(reserved_room_type) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = reserved_room_type, y = total_bookings, fill = reserved_room_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Room Type", y = "Number of Bookings", title = "Preferred Room Types") +
  theme_minimal()
hotel_data %>%
  group_by(market_segment) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = market_segment, y = total_bookings, fill = market_segment)) +
  geom_bar(stat = "identity") +
  labs(x = "Market Segment", y = "Number of Bookings", title = "Market Segment Preferences") +
  theme_minimal()

hotel_data %>%
  group_by(deposit_type, is_canceled) %>%
  summarize(total = n()) %>%
  ggplot(aes(x = deposit_type, y = total, fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Deposit Type", y = "Number of Bookings", fill = "Canceled", title = "Cancellations by Deposit Type") +
  theme_minimal()

