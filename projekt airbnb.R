#Poprawne załadowanie danych ze źródła internetowego do ramki danych, z uwzględnieniem nagłówków, kodowania zbioru, separatorów itd.
install.packages("ggcorrplot")
install.packages("dplyr")
install.packages("ggplot")
library(ggcorrplot)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggplot)

airb <- read.csv("C:/Users/inami/Documents/montreal.csv", header = TRUE, encoding = "UTF-8", sep = ",", quote = "\"", dec = ".")
print(airb)

head(airb)%>% kable() %>% kable_styling()

#poznanie oszacowanie czasochłonności procesu analizy;
start_time <- system.time({
  # ...
})

end_time <- system.time({
  #...
})

total_time <- end_time - start_time
cat("Czas trwania analizy:", total_time, "\n")

#Poznanie rozmiaru zbioru danych (liczby obserwacji i liczby zmiennych, które je opisują)
nrow(airb)
ncol(airb)

# Struktura danych
str(airb)

names_to_delete <- c('id', 'listing_url', 'scrape_id', 'last_scraped', ' picture_url', 'host_about','neighborhood_overview',
                     'host_thumbnail_url', 'host_picture_url','neighbourhood', 'description', 'amenities', 'calendar_updated',
                     'host_verifications', 'picture_url', 'host_id', 'host_url', 'license','neighbourhood_group_cleansed')
airbnb <- airb[, !(names(airb) %in% names_to_delete)]

str(airbnb)
missing_airbnb <- airbnb %>% summarise_all(~(sum(is.na(.))/n()))

missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")
missing_airbnb <- missing_airbnb %>% arrange(desc(percent_missing))
missing_airbnb <- head(missing_airbnb, 20)
missing_airbnb 

ggplot(missing_airbnb, aes(x = reorder(variables, -percent_missing ), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Missing Values by Variable",
       x = "Variables",
       y = "Percentage Missing") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Weryfikacja typów poszczególnych zmiennych (całkowite, zmiennoprzecinkowe, kategoryczne porządkowe, kategoryczne nominalne, 
#zmienne typu logicznego, daty) i ich ewentualna korekta (zamiana typu string na float,interpretacja zmiennych numerycznych jako kategorii itp.);

#data
airb_cols <- c('host_since', 'calendar_last_scraped', 'first_review', 'last_review')
airbnb[airb_cols] <- lapply(airbnb[airb_cols], as.Date)
str(airbnb[airb_cols])

#factor
unique_values <- unique(airbnb$host_response_time)
print(unique_values)
unique_val <- unique(airbnb$property_type)
unique_v <- unique(airbnb$room_type)
print(unique_v)
airbnb$host_response_time <- as.factor(airbnb$host_response_time)
airbnb$room_type <- as.factor(airbnb$room_type)

#logiczna
airbnb$instant_bookable <- ifelse(airbnb$instant_bookable == 't', TRUE, FALSE)
airbnb$has_availability <- ifelse(airbnb$has_availability == 't', TRUE, FALSE)
airbnb$host_identity_verified <- ifelse(airbnb$host_identity_verified == 't', TRUE, FALSE)
airbnb$host_has_profile_pic<- ifelse(airbnb$host_has_profile_pic == 't', TRUE, FALSE)
airbnb$host_is_superhost<- ifelse(airbnb$ host_is_superhost == 't', TRUE, FALSE)

# Usuńiem znaki specjalne i zamień przecinki na kropki w kolumnie price,host_response_rate, host_acceptance_rate
uniquev <- unique(airbnb$price)
print(uniquev)
airbnb$price <- gsub("\\$", "", airbnb$price)
airbnb$price <- gsub(",", "", airbnb$price)
airbnb$host_response_rate <- gsub("%", "", airbnb$host_response_rate)
airbnb$host_acceptance_rate  <- gsub("%", "", airbnb$host_acceptance_rate)

# Konwertujemy kolumnę price,host_response_rate, host_acceptance_rate na typ liczbowy
airbnb$price <- as.numeric(airbnb$price)
airbnb$host_response_rate <- as.integer(airbnb$host_response_rate)
airbnb$host_acceptance_rate <- as.integer(airbnb$host_acceptance_rate)

#transformacja bathrooms
uniqueb<- unique(airbnb$bathrooms)
print(uniqueb)
airbnb$bathrooms <- as.numeric(airbnb$bathrooms)
airbnb$bathrooms <- ifelse(grepl('half', airbnb$bathrooms_text, ignore.case = TRUE) & !is.na(airbnb$bathrooms_text),
                           0.5,
                           as.numeric(gsub('[^0-9.]', '', airbnb$bathrooms_text)))

#transformacja bedrooms
airbnb1 <- data.frame(
  name = c(print(airbnb$name)),
  bedrooms = c(print(airbnb$bedrooms)),
  stringsAsFactors = FALSE
  )

process_name <- function(name) {
  if (grepl("bedroom|bedrooms", name, ignore.case = TRUE)) {
       match <- regexpr(" · \\d+", name)
    if (match != -1) {
      bedrooms_value <- as.numeric(substr(name, match + 3, match + attr(match, "match.length") - 1))
    } else {
      bedrooms_value <- 1  
    }
  } else {
    bedrooms_value <- NA  
  }
  
  return(bedrooms_value)
}

airbnb$bedrooms <- mapply(process_name, airbnb$name)
str(airbnb)

#Zbudowanie podsumowania zmiennych numerycznych opisujących zbiór

#summary of dataset
summary(airbnb) 

#kompaktowe podsumowanie ramki danych, wyświetlające informacje o typach danych 
glimpse(airbnb)

#Czyszczenia danych
colSums(is.na(airbnb))
sum(colSums(is.na(airbnb)) > 0)

#1 review_scores_location

median_scores_location <- airbnb %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(median_review_scores_location = median(review_scores_location, na.rm = TRUE))
airbnb <- left_join(airbnb, median_scores_location, by = "neighbourhood_cleansed")
airbnb$review_scores_location <- ifelse(is.na(airbnb$review_scores_location),
                                        airbnb$median_review_scores_location,
                                        airbnb$review_scores_location)

rows_with_missing <- airbnb[is.na(airbnb$review_scores_location),
                            c('neighbourhood_cleansed', 'review_scores_location')]
airbnb <- airbnb[, !names(airbnb) %in% c('median_review_scores_location')]
airbnb$review_scores_location[is.na(airbnb$review_scores_location)] <- 4.86
sum(is.na(airbnb$review_scores_location))
unique(airbnb$review_scores_location)

#2.review_scores_communication
median_scores_communication <- airbnb %>%
  group_by(host_response_time, host_is_superhost) %>%
  summarise(median_review_scores_communication = median(review_scores_communication, na.rm = TRUE))
airbnb <- left_join(airbnb, median_scores_communication, by = c('host_response_time', 'host_is_superhost'))
airbnb$review_scores_communication <- ifelse(is.na(airbnb$review_scores_communication),
                                             airbnb$median_review_scores_communication,
                                             airbnb$review_scores_communication)
rows_with_missing <- airbnb[is.na(airbnb$review_scores_communication),
                            c('host_response_time', 'host_is_superhost', 'review_scores_communication')]
airbnb <- airbnb[, !names(airbnb) %in% c('median_review_scores_communication')]

str(airbnb)
#3 bedrooms
airbnb <-airbnb %>%
  group_by(room_type, beds) %>%
  mutate(bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm = TRUE), bedrooms))
missing_bedrooms <- airbnb %>% filter(is.na(bedrooms))

#4 reviews_per_month
airbnb$reviews_per_month <- ifelse(is.na(airbnb$reviews_per_month), 0, airbnb$reviews_per_month)
sum(is.na(airbnb$reviews_per_month))
unique(airbnb$reviews_per_month)

#5 review_scores_value
airbnb$review_scores_value
sum(is.na(airbnb$review_scores_value))
mode_value <- as.numeric(names(sort(table(na.omit(airbnb$review_scores_value)), decreasing = TRUE)[1]))
mode_value
airbnb$review_scores_value[is.na(airbnb$review_scores_value)] <- mode_value
unique(airbnb$review_scores_value)

#6 host_response_rate
sum(is.na(airbnb$host_response_rate))
mode_value_h <- as.numeric(names(sort(table(na.omit(airbnb$host_response_rate)), decreasing = TRUE)[1]))
mode_value_h
airbnb$host_response_rate[is.na(airbnb$host_response_rate)] <- mode_value_h
unique(airbnb$host_response_rate)

#7 beds
median_beds <- airbnb %>%
  group_by(accommodates) %>%
  summarise(median_beds = median(beds, na.rm = TRUE))
airbnb <- left_join(airbnb, median_beds, by = 'accommodates')
airbnb$beds <- ifelse(is.na(airbnb$beds), airbnb$median_beds, airbnb$beds)
airbnb <- select(airbnb, -median_beds)       

#8 host_acceptance_rate
sum(is.na(airbnb$host_acceptance_rate))
mode_value_h1 <- as.numeric(names(sort(table(na.omit(airbnb$host_acceptance_rate)), decreasing = TRUE)[1]))
mode_value_h1
airbnb$host_acceptance_rate[is.na(airbnb$host_acceptance_rate)] <- mode_value_h1
unique(airbnb$host_acceptance_rate)

#9 review_scores_rating 
mode_value_r <- as.numeric(names(sort(table(na.omit(airbnb$review_scores_rating)), decreasing = TRUE)[1]))
mode_value_r
airbnb$review_scores_rating[is.na(airbnb$review_scores_rating )] <- mode_value_r
sum(is.na(airbnb$review_scores_rating))
unique(airbnb$review_scores_rating )

#10 review_scores_cleanliness 
mode_value_c <- as.numeric(names(sort(table(na.omit(airbnb$review_scores_cleanliness)), decreasing = TRUE)[1]))
airbnb$review_scores_cleanliness[is.na(airbnb$review_scores_cleanliness)] <- mode_value_c
sum(is.na(airbnb$review_scores_cleanliness))
unique(airbnb$review_scores_cleanliness)

#11 review_scores_accuracy  
mode_value_ac <- as.numeric(names(sort(table(na.omit(airbnb$review_scores_accuracy)), decreasing = TRUE)[1]))
airbnb$review_scores_accuracy[is.na(airbnb$review_scores_accuracy)] <- mode_value_ac
sum(is.na(airbnb$review_scores_accuracy))
unique(airbnb$review_scores_accuracy)


#12 review_scores_checkin 
mode_value_ch <- as.numeric(names(sort(table(na.omit(airbnb$review_scores_checkin)), decreasing = TRUE)[1]))
mode_value_ch
airbnb$review_scores_checkin[is.na(airbnb$review_scores_checkin)] <- mode_value_r
sum(is.na(airbnb$review_scores_checkin))
unique(airbnb$review_scores_checkin)

#13 bathrooms 
mode_value_b <- as.numeric(names(sort(table(na.omit(airbnb$bathrooms)), decreasing = TRUE)[1]))
mode_value_b
airbnb$bathrooms[is.na(airbnb$bathrooms)] <- mode_value_b
sum(is.na(airbnb$bathrooms))
unique(airbnb$bathrooms)

#14 price 
mode_pr <- as.numeric(names(sort(table(na.omit(airbnb$price)), decreasing = TRUE)[1]))
mode_pr
airbnb$price[is.na(airbnb$price)] <- mode_pr
sum(is.na(airbnb$price))
unique(airbnb$price)

if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

library(e1071)
summary(airbnb$price)
skewness(airbnb$price)
kurtosis(airbnb$price)

#przyjrzyjmy się "price" i transformacji za pomocą wykresów
airbnb <- subset(airbnb, price <= quantile(price, 0.995) & price > 0)
options(repr.plot.width=12, repr.plot.height=5)
# Tworzenie histogramu
ggplot(airbnb, aes(x = price)) +
  geom_histogram(bins = 100, fill = '#008070', color = 'black', alpha = 0.5) +
  geom_density(color = 'black') +
  labs(title = 'Distribution of Price',
       x = 'Price',
       y = 'Frequency') +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

options(repr.plot.width=12, repr.plot.height=5)
# Tworzenie boxplotu
ggplot(airbnb, aes(x = "", y = price)) +
  geom_boxplot(fill = '#008070', color = 'black', alpha = 0.7) +
  labs(title = 'Boxplot of Price',
       x = '',
       y = 'Price') +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10))

ggplot(airbnb, aes(x = price, y = room_type)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price by Room Type",
       x = "Price",
       y = "Room Type")

filtered_airbnb <- subset(airbnb, price > 0 & price < 2000)
filtered_airbnb$log_price <- log(filtered_airbnb$price)
# Utworzenie histogramu logarytmu cen
ggplot(filtered_airbnb, aes(x = log_price)) +
  geom_histogram(bins = 30, fill = '#008070', color = 'black', alpha = 0.7) +
  labs(title = "Histogram of Log-transformed Price (0 < Price < 2000)",
       x = "Log-transformed Price",
       y = "Frequency")

#Wizualizacja rozkładu (wybranych) zmiennych
plot_data <- airbnb %>%
  filter(price >= mean(price)) %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(mean_price = mean(price)) %>%
  arrange(mean_price, decreasing = FALSE) %>%
  top_n(10)

# Plot the mean price for the top 10 neighborhoods
ggplot(plot_data, aes(x = reorder(neighbourhood_cleansed, mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "#008070") +
  labs(title = "Top 10 Neighborhoods by Mean Price",
       x = "Neighborhood",
       y = "Mean Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = airbnb) +
  geom_point(mapping = aes(x = longitude, y = latitude, color=neighbourhood_cleansed)) +
  xlab("") +
  ylab("") +
  labs(color = NULL) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())

install.packages("leaflet")
library(leaflet)

basemap <- addTiles(leaflet())
map <- addCircleMarkers(setView(basemap, lng = -73.56, lat = 45.5, zoom = 12), lng = airbnb$longitude, lat = airbnb$latitude, radius = 1, fillOpacity = 6)
map

#analiza zmiennych kategorycznych
categorical_variables <- sapply(airbnb, is.factor)
print(names(airbnb)[categorical_variables])

airbnb_ad <- airbnb %>%
  group_by(room_type, host_response_time) %>%
  summarise(perc = n() / nrow(airbnb))
# Tworzenie wykresu
ggplot(airbnb_ad, aes(x = host_response_time, y = perc, fill = host_response_time)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~room_type, scales = "free_y") +
  labs(title = "Percentage of host_response_time by Room Type",
       x = "host_response_time",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

##normalizacja danych numerycznych  

numeric_variables <- sapply(airbnb, function(x) is.numeric(x) | is.integer(x))
print(names(airbnb)[numeric_variables])

numeric_data <- airbnb[, numeric_variables]

# Z-score normalization
z_score_normalized_data <- as.data.frame(scale(numeric_data))
print(head(z_score_normalized_data))

numeric_data <- airbnb[, numeric_variables]

# Min-max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

min_max_scaled_data <- as.data.frame(sapply(numeric_data, min_max_scale))
print(head(min_max_scaled_data))
 
##korelacja

# Convert the correlation matrix to a data frame
cor_df <- as.data.frame(as.table(cor_matrix_pearson))

# Wizualizacja macierzy korelacji za pomocą heatmap
ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Pearson Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

selected_variables <- c(
  'host_response_rate',
  'host_acceptance_rate',
  'host_total_listings_count',
  'accommodates',
  'bathrooms',
  'bedrooms',
  'beds',
  'price',
  'minimum_nights',
  'maximum_nights',
  'availability_365',
  'availability_30',
  'number_of_reviews',
  'review_scores_rating',
  'review_scores_cleanliness',
  'review_scores_communication',
  'review_scores_location',
  'reviews_per_month'
)
subset_airbnb <- airbnb[, selected_variables]

cor_matrix_spearman <- cor(subset_airbnb, method = "spearman")

# Convert the correlation matrix to a data frame
cor_df_spearman <- as.data.frame(as.table(cor_matrix_spearman))

# Wizualizacja macierzy korelacji rang Spearmana za pomocą heatmap
ggplot(data = cor_df_spearman, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Freq)), vjust = 1) +  
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Spearman Rank Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) 






