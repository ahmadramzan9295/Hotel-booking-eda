# 🏨 Hotel Booking Data Analysis (R Project)

## 📌 Project Overview

This project analyzes hotel booking data using **R** to uncover insights related to:

* Booking trends
* Customer behavior
* Cancellations
* Market segments
* Room preferences

---

## 📂 Dataset

* File: `hotel_booking.csv`
* Place it in your working directory or update the path:

```r
hotel_data <- read.csv("hotel_booking.csv")
```

---

## 🛠️ Libraries Used

```r
library(tidyverse)
library(lubridate)
library(modeest)
```

---

## 📥 Data Loading & Exploration

```r
data = read_csv("hotel_booking.csv")

summary(data)
glimpse(data)

# Count missing values
data %>% summarize_all(~sum(is.na(.)))
```

---

## 🧹 Data Cleaning

### Handle Missing Values

```r
# Replace missing children with mode
data$children[is.na(data$children)] = mfv(data$children , na_rm = TRUE)

# Replace missing agent with median
data$agent[is.na(data$agent)] = median(data$agent ,na.rm = TRUE)
```

### Alternative Cleaning Approach

```r
hotel_data <- hotel_data %>%
  mutate(
    children = ifelse(is.na(children), 0, children),
    agent = replace_na(agent, "Unknown")
  )
```

---

## 📅 Feature Engineering

```r
hotel_data <- hotel_data %>%
  mutate(
    arrival_date = make_date(
      arrival_date_year,
      match(arrival_date_month, month.name),
      arrival_date_day_of_month
    )
  )
```

---

## 📊 Visualizations

### 1. Monthly Bookings & Cancellations

```r
hotel_data %>%
  group_by(arrival_date_month, is_canceled) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = reorder(arrival_date_month, -total_bookings),
             y = total_bookings,
             fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Bookings and Cancellations",
       x = "Month",
       y = "Bookings",
       fill = "Canceled") +
  theme_minimal()
```

---

### 2. Lead Time Distribution

```r
ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
  labs(title = "Lead Time Distribution",
       x = "Lead Time (Days)",
       y = "Frequency") +
  theme_minimal()
```

---

### 3. Preferred Room Types

```r
hotel_data %>%
  group_by(reserved_room_type) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = reserved_room_type,
             y = total_bookings,
             fill = reserved_room_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Preferred Room Types",
       x = "Room Type",
       y = "Bookings") +
  theme_minimal()
```

---

### 4. Market Segment Preferences

```r
hotel_data %>%
  group_by(market_segment) %>%
  summarize(total_bookings = n()) %>%
  ggplot(aes(x = market_segment,
             y = total_bookings,
             fill = market_segment)) +
  geom_bar(stat = "identity") +
  labs(title = "Market Segment Preferences",
       x = "Market Segment",
       y = "Bookings") +
  theme_minimal()
```

---

### 5. Cancellations by Deposit Type

```r
hotel_data %>%
  group_by(deposit_type, is_canceled) %>%
  summarize(total = n()) %>%
  ggplot(aes(x = deposit_type,
             y = total,
             fill = as.factor(is_canceled))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancellations by Deposit Type",
       x = "Deposit Type",
       y = "Bookings",
       fill = "Canceled") +
  theme_minimal()
```

---

## ▶️ How to Run

1. Install required packages:

```r
install.packages(c("tidyverse", "lubridate", "modeest"))
```

2. Add dataset:

```
hotel_booking.csv
```

3. Run script in **RStudio**

---

## 📊 Key Insights You Can Derive

* Which months have highest bookings
* Cancellation patterns
* Customer booking behavior (lead time)
* Popular room types
* Key market segments
* Impact of deposit type on cancellations

---

## 📁 Project Structure

```
hotel-booking-analysis/
│── README.md
│── analysis.R
│── hotel_booking.csv
```

---

## 🚀 Future Improvements

* Predict cancellations using ML models
* Customer segmentation (clustering)
* Revenue forecasting
* Dashboard (Power BI / Tableau)

---

## ⭐ Contribution

Feel free to fork, improve, and extend this analysis!

---
