#================================================================================
rm(list = ls())
library(tidyr)
library(ggplot2)     # Used for creating plots.
library(gganimate)   # Extends the ggplot2 package with the frame aesthetic.
library(tweenr)      # Makes the animation smooth.
library(magick)      # Takes a set of images and coverts them into a .gif format.
library(gifski)
library(dplyr)
library(magrittr)
library(countrycode)
library(grid)
library(ggflags)
#=================================================================================
gdp = read.csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_2.csv")
gdp = gdp[, c(-2 , -3 , -4)]
colnames(gdp)[1] = "Country"
gdp_1 = gdp %>%
  pivot_longer(!Country,
               names_to = "year",
               values_to = "GDP",
               values_drop_na = TRUE)
gdp_1$year = as.integer(gsub("X", "", gdp_1$year))
# Vector of G20 countries
g20_countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", 
                   "Germany", "India", "Indonesia", "Italy", "Japan", "Mexico", "Russia", 
                   "Saudi Arabia", "South Africa", "South Korea", "Turkey", 
                   "United Kingdom", "United States", "Spain", "Portugal", "Netharlands",
                   "Switzerland", "Sweden", "Taiwan" , "UAE" , "Norway" , "Belgium" , "Ireland",
                   "Malaysia" , "Poland" , "Singapore")
#Filter the dataset to include only G20 countries
gdp_g20 <- subset(gdp_1, Country %in% g20_countries)
gdp_g20 <- gdp_g20 %>%
  group_by(year) %>%
   mutate(rank = rank(-GDP))  # Using rank with descending GDP

gdp_g20 <- gdp_g20 %>%   # considering years from 1985
  filter(year >= 1985) %>%
  group_by(year) %>%
  mutate(rank = rank(-GDP))


D1 <- gdp_g20 %>%         # Multiplying years with 120 
  group_by(year) %>%
  filter() %>%
  top_n(n = 15, wt = GDP) %>%
  mutate(Rank = rank * 120) %>%
  ungroup()
D1 <- D1[with(D1, order(year, -GDP)),]

D1 = na.omit(D1)  # Removing the NA values
colnames(D1) = c("Country" , "Year" , "Total" , "rank" , "Rank" , "Code")

D1$Total = D1$Total/1e9    # Converting the GDP from $ to Billion $
D1$Total = as.integer(D1$Total)    #Rounding off the GDP


#Adding the GDP's of 2023
Country = c("United States", "China" , "Germany", "Japan","India", "United Kingdom",
            "France", "Italy", "Brazil", "Canada", "Russia", "Mexico", "Australia",
            "Spain", "Indonesia")
Year = rep(2023 , time = 15)
Total = as.integer(c(25462.73, 17886.33, 4085.68, 4237.53, 3389.69, 3081.87, 2780.14, 
                     2012.01, 1920.02, 2137.94, 2244.25, 1465.85, 1702.55, 
                     1418.92, 1318.81))
rank  = 1:15
Rank = 120*(1:15)
D1_23 = data.frame(Country , Year , Total , rank , Rank)
D1_new = rbind(D1 , D1_23)

#Adding country codes
D1_new$Code <- tolower(countrycode(D1_new$Country, "country.name", "iso2c"))

#===============================================================================
#===============================================================================

# PLOT
p <- ggplot(D1_new) + #, aes(x = Rank, group = Country, country = as.factor(Code)
  geom_col(aes(x = Rank, y = Total, fill = ifelse(Country == "India", "#EC7063", "#AED6F1")), 
           width = 100, color = "white") + # Columns
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title='GDP of the Year: {closest_state}', x = "", y = "GDP in ($ billion)") + # Labels
  theme_minimal() + # Theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    legend.position = "none"              # Hide legend
  ) +
  geom_text(aes(x = Rank, y = -2000, label = Country), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Total + 200, label = as.character(Total)), hjust = 0, color = "black") + # Values  
  geom_flag(aes(x = Rank, y = -1000,  country = Code), size = 10) + # Flags
  scale_fill_identity() +  # Use manual fill colors
  scale_y_continuous(labels = scales::comma) + # Format y-axis values
  scale_x_reverse() + # Highest values on top
  transition_states(Year, transition_length = 4, state_length = 1) + # Animate
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.margin = margin(0,2,0,3,"cm"),
    axis.text.y  = element_blank()
  )
# ANIMATION
anime = animate(p, fps = 50, duration = 30, width = 800, height = 600)
anim_save("animation.gif", anime)
