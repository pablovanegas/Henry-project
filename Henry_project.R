#IMPORTING DATA 

library(readxl)
library(janitor)
library(dplyr)
library(ggplot2)
stores <- read_excel("Datawarehouse.xlsx", 
                            sheet = "stores")
sales <- read_excel("Datawarehouse.xlsx", 
                     sheet = "sales")
features <- read_excel("Datawarehouse.xlsx", 
                     sheet = "features")

summary(stores)

stores$Size <- as.numeric(stores$Size)
stores$Type <- as.factor(stores$Type)

#Grafico 1 pretty histogram of the size of the stores

hist(stores$Size, col = "lightblue", main = "Size of the stores", xlab = "Size of the store", ylab = "Frequency")

#Añadir grafico de tamaño de la tienda por tipo de tienda

#KPI 1 

library(dplyr)
library(ggplot2)

# Calculate mean size by type
mean_size_by_type <- stores %>%
  group_by(Type) %>%
  summarise(mean_size = mean(Size))

#  
ggplot(mean_size_by_type, aes(x = Type, y = mean_size)) +
  geom_col(fill = "skyblue") +
  labs(title = "Mean Size of Stores by Type",
       y = "Mean Size",
       x = "Store Type") +
  theme_minimal()


#--------------------------------SALES---------------------------------------#
#summary(sales) 

sales$Weekly_Sales <- as.numeric(sales$Weekly_Sales)
sales$Dept <- as.factor(sales$Dept)
sales$IsHoliday <- as.factor(sales$IsHoliday)
summary(sales)

# Grafico 2
holiday_freq <- table(sales$IsHoliday)

barplot(holiday_freq, 
        main = "Sales on Holidays",
        names.arg = c("Not a holiday", "Holiday"),
        col = "lightblue", 
        xlab = "Is it a holiday?", 
        ylab = "Frequency",
        ylim = c(0, max(holiday_freq) + 50000))  


text(x = 1:2, y = holiday_freq + 100, labels = holiday_freq, pos = 3)

legend("topright", legend = levels(as.factor(sales$IsHoliday)), fill = "lightblue", title = "Is it a holiday?")


#KPI 2
orden <- (sort(table(sales$Dept), decreasing = TRUE))

df1 <- data.frame(Department = names(orden), Sales = as.numeric(orden))

ggplot(df1, aes(x = reorder(Department, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Sales on the departments", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank())  


# KPI 3

top_dept <- head(sort(table(sales$Dept), decreasing = TRUE), 10)

top_dept_df <- data.frame(Department = names(top_dept), Sales = as.numeric(top_dept))

ggplot(top_dept_df, aes(x = reorder(Department, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Sales), vjust = -0.5, size = 3.5) +  # Add text labels for sales values
  labs(title = "Top 10 Sales by Department", x = "Department", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# KPI 4
bottom_dept <- tail(sort(table(sales$Dept)), 10)

bottom_dept_df <- data.frame(Department = names(bottom_dept), Sales = as.numeric(bottom_dept))

ggplot(bottom_dept_df, aes(x = reorder(Department, Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = Sales), vjust = -0.5, size = 3.5, color = "black") +  # Add text labels for sales values
  labs(title = "Bottom 10 Sales by Department", x = "Department", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#-------------------------------features-------------------------------------#
summary(features)

features$Temperature <- as.numeric(features$Temperature)
features$Fuel_Price <- as.numeric(features$Fuel_Price)
features$MarkDown1 <- as.numeric(features$MarkDown1)
features$MarkDown2 <- as.numeric(features$MarkDown2)
features$MarkDown3 <- as.numeric(features$MarkDown3)
features$MarkDown4 <- as.numeric(features$MarkDown4)
features$MarkDown5 <- as.numeric(features$MarkDown5)
features$CPI <- as.numeric(features$CPI)
features$Unemployment <- as.numeric(features$Unemployment)
features$IsHoliday <- as.factor(features$IsHoliday)

#Grafico 3
ggplot(features, aes(x = Temperature)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Temperature Distribution", x = "Temperature", y = "Frequency") +
  theme_minimal()

#Graph 4
ggplot(features, aes(x = Fuel_Price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Fuel Price Distribution", x = "Fuel Price", y = "Frequency") +
  theme_minimal()

"
We can see how there is values = 0 what may reflect an error on the input but there is a
option than this could be disccounts or promotions, repeating the plot  without the 0 values
"

#Graph 4.1
filtered_features <- features %>%
  filter(Fuel_Price >= 3000)

# Plot histogram
ggplot(filtered_features, aes(x = Fuel_Price)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Fuel Price Distribution", x = "Fuel Price", y = "Frequency") +
  theme_minimal()



#Graph 5
ggplot(features, aes(x = Unemployment)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Unemploymen Distribution", x = "Fuel Price", y = "Frequency") +
  theme_minimal()

#Graph 6

holiday_freq <- table(features$IsHoliday)

barplot(holiday_freq, 
        main = "Sales on Holidays",
        names.arg = c("Not a holiday", "Holiday"),
        col = "lightblue", 
        xlab = "Is it a holiday?", 
        ylab = "Frequency",
        ylim = c(0, max(holiday_freq) + 1000))  

text(x = 1:2, y = holiday_freq + 100, labels = holiday_freq, pos = 3)

legend("topright", legend = levels(as.factor(sales$IsHoliday)), fill = "lightblue", title = "Is it a holiday?")

# joining with primary key of store on sales and features and sales and store

sales_features <- inner_join(sales, features, by = c("Store", "Date", "IsHoliday"))

sales_features_stores <- inner_join(sales_features, stores, by = "Store")

# KPI 5
#Seeing if there is a correlation between the size and the sales
cor(sales_features_stores$Size, sales_features_stores$Weekly_Sales)

'the value of correlation is 0.2438289,
what means that there is a positive correlation between the size of the store and the weekly sales'


#Fuel price by store

ggplot(sales_features_stores, aes(x = reorder(Store, Fuel_Price), y = Fuel_Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Fuel Price by Store", x = "Store", y = "Fuel Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 4000)


# KPI 6
ggplot(features, aes(x = Store, y = Fuel_Price)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(title = "Average Fuel Prices by Store Category", x = "Store Category", y = "Average Fuel Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Calculate average fuel price for each store
average_fuel_price <- aggregate(Fuel_Price ~ Store, data = features, FUN = mean)

# Sort stores by average fuel price in descending order
top_10_stores_by_fuel_price <- average_fuel_price[order(average_fuel_price$Fuel_Price, decreasing = TRUE), ]

# Get the top 10 stores
top_10_stores_by_fuel_price <- head(top_10_stores_by_fuel_price, 10)

# ploting top 10 stores by fuel price
ggplot(top_10_stores_by_fuel_price, aes(x = reorder(Store, -Fuel_Price), y = Fuel_Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Stores by Fuel Price", x = "Store", y = "Fuel Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 4000)
#Now the top 10 bottom stores by fuel price
bottom_10_stores_by_fuel_price <- average_fuel_price[order(average_fuel_price$Fuel_Price, decreasing = FALSE), ]

# Get the bottom 10 stores
bottom_10_stores_by_fuel_price <- head(bottom_10_stores_by_fuel_price, 10)

#plot
ggplot(bottom_10_stores_by_fuel_price, aes(x = reorder(Store, Fuel_Price), y = Fuel_Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Bottom 10 Stores by Fuel Price", x = "Store", y = "Fuel Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 4000)
