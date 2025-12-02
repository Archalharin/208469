install.packages('moderndive')
install.packages("skimr")
install.packages("ggplot2")
install.packages("tidyverse")

library(moderndive)
library(skimr)
library(ggplot2)
library(dplyr)
library(tidyverse)

data(house_prices)
help(house_prices)
View(house_prices)

table(house_prices$price)

data1 = house_prices %>%
  select(price, sqft_living, condition)%>%
  skim()

house_prices %>% skim()

#data visualizations
p1 <- ggplot(house_prices, aes(x = price)) +
  geom_histogram(binwidth=100000, color = "white") +
  labs(x = "price (USD)", title = "House price")

p2 <- ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(binwidth=500, color = "red") +
  labs(x = "sqft_living", title = "House price")

p3 <- ggplot(house_prices, aes(x = condition)) +
  geom_bar() +
  labs(x = "condition", title = "House condition")

library(patchwork)
p1 + p2 + p3+ plot_layout(ncol = 2)

#11
house_prices$log10_prices = log10(house_prices$price)
#log tranform
house_prices <- house_prices %>%
  mutate(
    log10_price = log10(price),
    log10_size = log10(sqft_living)
  )
# 
g1 <- ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price")

g2 <- ggplot(house_prices, aes(x = log10_size)) +
  geom_histogram(color = "red") +
  labs(x = "sqft_living", title = "House price")

g3 <- ggplot(house_prices, aes(x = condition)) +
  geom_bar() +
  labs(x = "condition", title = "House condition")

p4 <- ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price: Before")
p5 <- ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram(color = "white") +
  labs(x = "log10 price (USD)", title = "House price: After")
p4 + p5

p6 <- ggplot(house_prices,
       aes(x = log10_size, y = log10_price)) +
  geom_point() +
  labs(title = "House prices in Seattle",
       x = "log10 size",
       y = "log10 price")

p7 <- ggplot(house_prices,
       aes(x =sqft_living , y = price)) +
  geom_point() +
  labs(title = "House prices in Seattle",
       x = "size",
       y = "price")

p6 + p7

#14
g8 <- ggplot(house_prices, aes(x = log10_size, y = log10_price, col = condition)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se= FALSE) +
  labs(y = "log10 price", x = "log10 size", title = "House prices in Seattle") +
  facet_wrap(~condition)
  
ggplot(house_prices, aes(x="",fill= condition)) +
  geom_bar(position ="fill") +
  coord_polar(theta = "y", start = 0)

#cum
ggplot(house_prices, aes(x = grade)) +
  geom_bar() +
  labs(x = "grade", title = "grade level")

top_zip <- house_prices %>%
  count(zipcode, sort = TRUE) %>%
  slice(1:10) %>%
  pull(zipcode)

ggplot(house_prices %>% filter(zipcode %in% top_zip), aes(x = zipcode)) + 
         geom_bar() +
         labs(title="Price by Top 10 Zipcode",
              x = "Zipcode",
              )
       
    