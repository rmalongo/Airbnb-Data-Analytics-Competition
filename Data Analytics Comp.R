# Set digits
set.seed(1234) 

library("tidyverse")
library("ggplot2")
library("data.table")
library("stringr")
library("readxl")
library("leaflet")
library("htmltools")
library("stargazer")
library("caTools")

df_det <- read_csv("listings_details.csv")

# Set working directory
setwd("~/Desktop/Data Analytics Competition")

# Data cleaning -----------------------------------------------------------
df_bost_list<- read_csv("listings_details.csv")

# Space, summary (Already in Description), Notes,Access, thumbnail_url, hostname, host_url, host_id,
# host_about,
df_bost_list_1 <- df_bost_list %>% 
  select(id, room_type, price,host_listings_count, host_neighbourhood,neighbourhood, street,
         neighbourhood_group_cleansed, city, state, zipcode,smart_location,latitude,longitude,cleaning_fee) %>% 
  mutate(price = as.numeric(str_sub(price,2)),
         cleaning_fee = as.numeric(str_sub(cleaning_fee,2))) %>% 
  filter(!is.na(price))

# Decode price of Entire homes (above 1,000 whiche were coerced as NAs)
p2 <- df_bost_list %>% 
  mutate(price_1= as.numeric(str_sub(price,2))) %>% 
  filter(is.na(price_1)) %>% 
  separate(price, into = c("p3","p4"), sep = ",") %>% 
  unite(p5, p3:p4, remove = FALSE, sep = "") %>% 
  mutate(price=as.numeric(str_sub(p5,2))) %>% 
  select(id, room_type, price,host_listings_count, host_neighbourhood,neighbourhood, street,
         neighbourhood_group_cleansed, city, state, zipcode,smart_location,latitude,longitude, cleaning_fee)

# Combine datasets: price removed on NAs & encode neighbourhood
df_bost_list_2 <- df_bost_list_1 %>% 
  rbind(p2) %>% 
  mutate(neighbourhood = ifelse(is.na(neighbourhood),host_neighbourhood,neighbourhood))

# remove used datasets
rm(df_bost_list_1, p2)

# Summary statistics ------------------------------------------------------
df_sum <-df_bost_list_2 %>% 
  group_by(neighbourhood,room_type) %>% 
  summarise(Count = n(),
           avg_price = round(mean(price),0),
           min_price = round(min(price),0)) %>% 
  mutate(percent =round((Count/54869)*100,1)) %>% 
  na.omit()

df_sum_price <- df_bost_list_2 %>% 
  group_by(room_type) %>% 
  summarise(Min = round(min(price),0),
            Max = round(max(price),0),
            Mean = round(mean(price),0),
            SD = round(sd(price),0),
            N = n())

write.csv(df_sum_price, "df_sum_price.csv")

         
            
# Plot 1: Number of rooms
plot_1 <- ggplot(data = df_sum,aes(x=neighbourhood, y= percent)) + 
  geom_col(fill = "dark green") + 
  facet_wrap(~room_type) +
  coord_flip() +
  geom_text(aes(label=percent,hjust=0, vjust=0)) +
  ylab("Market share(%)") +
  xlab("Neighbourhood") +
  ggtitle("Airbnb Market in Boston") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(expand = c(0, 0.5))
 plot_1

# Avg price: Entire home/apt
 df_sum_home <- df_sum %>% 
   filter(room_type %in% c("Entire home/apt"))
 
# plot (maybe use lowest)
plot_2  <- ggplot(data=df_sum_home,aes(x = reorder(neighbourhood, avg_price),  y = avg_price, fill = avg_price)) +
  geom_col(fill = "dark blue") + 
  coord_flip() +
  geom_text(aes(label=avg_price,hjust=0, vjust=0)) +
  ylab("Average per unit") +
  xlab("Neighbourhood") +
  ggtitle("Airbnb Entire home/apt prices in Boston") +
  
plot_2

# Private room types
df_sum_pvt_room <- df_sum %>% 
  filter(room_type %in% c("Private room"))

plot_3 <-ggplot(data=df_sum_pvt_room,aes(x = reorder(neighbourhood, avg_price),  y = avg_price, fill = avg_price)) +
  geom_col() +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  coord_flip() +
  geom_text(aes(label=avg_price,hjust=0, vjust= 0.2)) +
  ylab("Average price per room") +
  xlab("Neighbourhood") +
  ggtitle("Airbnb room prices in Boston: Private room") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(expand = c(0, 0))
plot_3


# Shared room
df_sum_shared_room <- df_sum %>% 
  filter(room_type %in% c("Shared room"))

plot_4 <-ggplot(data=df_sum_shared_room,aes(x = reorder(neighbourhood, avg_price),  y = avg_price, fill = avg_price)) +
  geom_col(fill = "dark blue") +
  coord_flip() +
  geom_text(aes(label=avg_price,hjust=0, vjust=0)) +
  ylab("Average price per room") +
  xlab("Neighbourhood") +
  ggtitle("Airbnb room prices in Boston:Shared room")
plot_4

# Spatial data: 2 ---------------------------------------------------------

# Spatial data file
df_spatial <- df_bost_list_2 %>% 
  select(id,neighbourhood, street,city, state, zipcode, latitude,longitude) %>% 
  separate(zipcode, into = c("col_1", "col_2"), sep = " ") %>% 
  separate(col_1, into = c("col_3", "col_4"), sep = "-") %>% 
  mutate(zip_code = ifelse(col_3 %in% "MA", col_4, col_3)) %>% 
  mutate(zip_code = str_pad(zip_code, width = 5, pad = "0")) %>% 
  select(id, neighbourhood, street,city, state, zip_code, latitude,longitude) %>% 
  distinct(neighbourhood,  zip_code)


df_spatial_2 <- df_bost_list_2 %>% 
  select(id,neighbourhood, street,city, state, zipcode, latitude,longitude) %>% 
  separate(zipcode, into = c("col_1", "col_2"), sep = " ") %>% 
  separate(col_1, into = c("col_3", "col_4"), sep = "-") %>% 
  mutate(zip_code = ifelse(col_3 %in% "MA", col_4, col_3)) %>% 
  mutate(zip_code = str_pad(zip_code, width = 5, pad = "0")) %>% 
  select(id, neighbourhood, street,city, state, zip_code, latitude,longitude) %>% 
  filter( neighbourhood %in% c("Financial District", "Downtown", 
                               "Downtown Crossing", "Charlestown", "Theater District")) %>% 
  distinct(neighbourhood,  zip_code)



# Income data
df_income <- read_csv("ACS_17_5YR_B19001_with_ann_edited.csv")

df1_income <- df_income %>% 
  separate(Geography, into = c("col_1", "zip_code"), " ") %>% 
  select(-col_1) %>% 
  mutate( 
        low_income = round((low_income/Total_pop)*100,1),
         low_middle = round((low_middle/Total_pop)*100,1),
         upper_middle = round((upper_middle/Total_pop)*100,1),
         upper_class = round((upper_class/Total_pop)*100,1))

df_bost_list_5 <- df_bost_list_2 %>% 
  select(id, zipcode)
df_pvt_zip <- df

df_income_spatial_2 <- df_spatial_2 %>% 
  right_join(df1_income) %>% 
  filter(!is.na(neighbourhood)) %>% 
  pivot_longer(low_income:upper_class, names_to = "income_category", values_to ="income_dist" )
  


df_income_spatial_pvt_rm <-  df_income_spatial %>% 
  filter(neighbourhood %in% c ("Financial District", "Downtown", "Downtown Crossing",
                               "Charlestown","Theater District"))

ggplot(df_income_spatial_pvt_rm, aes(x=neighbourhood, y=income_dist)) + geom_col() + 
  facet_wrap(~income_category)

# output to .csv
write.csv(df_income_spatial, "Income_by_zipcode.csv")
# Ner to investigate 


# Leaflet map -------------------------------------------------------------

#  slice location
 df_loc <- df_bost_list_2  %>% 
   select(neighbourhood, latitude, longitude) %>% 
   group_by(neighbourhood) %>% 
   slice(1) %>% 
   ungroup()
 
df_tot <- df_bost_list %>% 
  group_by(neighbourhood) %>% 
  summarise(Count = n(),
            Share = round((Count/55501)*100,0))

 df_sum_loc <- df_tot %>% 
   left_join(df_loc, by ="neighbourhood")


 # Customize Marker Colors
 get_color <- function(df_sum_loc) {
   sapply(df_sum_loc$Count, function(Count) {
     if(Count <= 1000) {
       "red"
     } else if(Count <= 3000) {
       "orange"
     } else if(Count <= 5000) {
       "blue"
     } else {
       "green"
     } })
 }
 
 
 # Customize icon colors
 icons <- awesomeIcons(
   icon = 'ios-close',
   iconColor = 'black',
   library = 'ion',
   markerColor = get_color(df_sum_loc)
 )
 
 # Create bin formats
 bins <- c(0, 1000,3000,5000, 7000)
 pal <- colorBin(c("red", "orange", "blue", "green"), domain = df_sum_loc$Count, bins = bins)
 
 # Title
 title <- "Airbnb market in Boston"
 # Plot Leaftet Map 
 leaflet(data = df_sum_loc)%>% 
   setView(lat = 42.35210, lng =  -71.06168, zoom = 12) %>% 
   addTiles() %>% 
   addAwesomeMarkers(lat = ~latitude, lng = ~longitude, label = ~neighbourhood, 
                     labelOptions= labelOptions(noHide = TRUE, direction = "bottom"),
                     icon = icons) %>% 
   addLegend(
     pal = pal,
     values = ~Count,
     opacity = 1,
     title = "Number of Listings (2009)",
     position = "bottomright")
 
 
# Regression Modellng variables
# Price: income level (four income levels (low_income =1 , upper_come)), room_type (dummy variable), 

# Regression modelling ----------------------------------------------------

# 70% of the entire Dataset for training (Training data)
# 15% of the entire Dataset for validation (Validation data)
# 15% of the entire Dataset for testing (Testing data)
 
 # Sample of 500 listings
 df_samp_lm <- read_csv("sample_new.csv")
   
   df_reg <-  df_samp_lm  %>% 
   mutate(log_price   = log(price),
          incomelevel = (low_income*1 + low_middle*2+ upper_middle*3 
                         + upper_class*4)/100,
          room_type_shared_room = ifelse(room_type == "Shared room", 1,0),
          room_type_private_room = ifelse(room_type == "Private room", 1,0),
          room_type_entire_home_apt = ifelse(room_type == "Entire home/apt", 1,0),
          season_autumn = ifelse(season == "Autumn", 1,0),
          season_winter = ifelse(season == "Winter", 1,0),
          season_spring = ifelse(season == "Spring", 1,0))
       
  # Split data into train and test datasets    
 df_reg_sample = sample.split(df_reg$neighbourhood, SplitRatio = .75)

 # Train data set
 df_reg_train <- subset(df_reg,  df_reg_sample == TRUE)
 
 # Test dataset
df_reg_test <-  subset(df_reg,  df_reg_sample== FALSE)

# Train data --------------------------------------------------------------

 # Regression: Train data
reg_1 <- lm(data = df_reg_train,
            log_price ~ cleaning_fee+host_is_superhost+host_has_profile_pic +
            host_identity_verified+accommodates +
            security_deposit+review_scores_accuracy+
            review_scores_value+ n_reviews + avg_len_review + review_dummy +
            incomelevel + desc_length +listing_desc + location + room_type_entire_home_apt +
            room_type_private_room + room_type_shared_room +is_Fri_Sat + is_holiday +
            season_autumn + season_winter+ season_spring)
summary(reg_1)

# Regression: Test data

# parameters to report
reported <- "vcsp*"
                 
lm_results <- stargazer(reg_1, type = "text",title="Regression Model Results", report = reported, 
                        digits=4, out="lm_results.txt")
lm_results <- stargazer(reg_1, type ="html", title="Regression Model Results", report = reported, 
                        digits=4, out="lm_results.html")

df_reg_train_lm <- df_reg_train %>% 
  mutate(residuals = resid(reg_1),
         std_residuals = rstandard(reg_1),
         predicted = fitted.values(reg_1))
# qq plot: Normal probabillity plot of residuals
 qqnorm(df_reg_train_lm$std_residuals, 
        ylab="Standardized Residuals", 
        xlab="Normal Scores", 
        main="Normal probabillity plot of residuals" ) 
qqline(df_reg_train_lm$std_residuals)

# log_price and predicted values
ggplot(df_reg_train_lm, aes(x=log_price, y=predicted)) + geom_point(size=2,pch = 1) +
 geom_abline(intercept = 0, slope = 1, color ="blue") +
  ylab("Predicted log price") +
  xlab("log price") +
  ggtitle("Airbnb prices in Boston")
  
# residauls vs predicted values
ggplot(df_reg_train_lm, aes(x=predicted, y=std_residuals)) + geom_point(size=2, pch =1) +
  geom_abline(intercept = 0, slope =0, color ="blue")  +
  ylab("Standardized Residuals") +
  xlab("Predicted log price") +
  ggtitle("Airbnb prices in Boston") 

hist(df_reg_train_lm$std_residuals, col = "gray", xlab = "Residuals",
     main = "Airbnb prices in Boston")

# Test data ---------------------------------------------------------------
# Regression: Train data
reg_test <- lm(data = df_reg_test,
            log_price ~ cleaning_fee+host_is_superhost+host_has_profile_pic +
              host_identity_verified+accommodates +
              security_deposit+review_scores_accuracy+
              review_scores_value+ n_reviews + avg_len_review + review_dummy +
              incomelevel + desc_length +listing_desc + location + room_type_entire_home_apt +
              room_type_private_room + room_type_shared_room +is_Fri_Sat + is_holiday +
              season_autumn + season_winter+ season_spring)
summary(reg_test)

# Regression: Test data

# parameters to report
reported <- "vcstp*"

lm_results_reg_test <- stargazer(reg_1, type = "text",title="Regression Model Results", report = reported, 
                        digits=4, out="lm_results.txt")
lm_results_reg_test <- stargazer(reg_1, type ="html", title="Regression Model Results", report = reported, 
                        digits=4, out="lm_results.html")

df_reg_test_lm <- df_reg_test %>% 
  mutate(residuals = resid(reg_test),
         std_residuals = rstandard(reg_test),
         predicted = fitted.values(reg_test))
# qq plot: Normal probabillity plot of residuals
qqnorm(df_reg_test_lm$std_residuals, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Normal probabillity plot of residuals" ) 
qqline(df_reg_test_lm$std_residuals)

# Standardized residuals distribution
hist(df_reg_test_lm$std_residuals, col = "navy", xlab = "Residuals",
     main = "Standardized residuals distribution")

# log_price and predicted values
ggplot(df_reg_test_lm, aes(x=log_price, y=predicted)) + geom_point(size=2,pch = 1) +
  geom_abline(intercept = 0, slope = 1, color ="blue", size =1.5) +
  ylab("Predicted log price") +
  xlab("log price") +
  ggtitle("Observed values against predicted values") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(expand = c(0, 0))

# residauls vs predicted values
ggplot(df_reg_test_lm, aes(x=predicted, y=std_residuals)) + geom_point(size=2, pch =1) +
  geom_abline(intercept = 0, slope =0, color ="blue", size =1.5)  +
  ylab("Standardized Residuals") +
  xlab("Predicted log price") +
  ggtitle("Predicted values vs Standardized residuals") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  ) +
  scale_y_continuous(expand = c(0, 0))


