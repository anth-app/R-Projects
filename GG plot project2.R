library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)

books <- read_csv("Books_Data_Clean.csv")
View(books)

# DATA CLEANING AND MANIPULATION

books$genre[books$genre == 'genre fiction']<-'fiction' 

authors <- books %>% group_by(Author) %>% 
  summarise(total_units_sold = sum(`units sold`),
            gross_sales = sum(`gross sales`),
            avg_price = mean(`sale price`),
            rating = mean(Book_average_rating))
books1800s <- books[books$`Publishing Year` >1800,]

books1900s <- books[books$`Publishing Year` >1900,]

books_year1800s <- books1800s %>% group_by(`Publishing Year`) %>% 
  summarise(avg_price = mean(`sale price`),
            avg_rating = mean(`Book_average_rating`),
            ratings = sum(Book_ratings_count),
            total_sales = sum(`units sold`))

books_year1900s <- books1900s %>% group_by(`Publishing Year`) %>% 
  summarise(avg_price = mean(`sale price`),
            avg_rating = mean(`Book_average_rating`),
            ratings = sum(Book_ratings_count),
            total_sales = sum(`units sold`))


# PLOTS

# First two visuals show similar data 
ggplot(data = books, mapping = aes(x = Book_ratings_count, y = Book_average_rating, color = Author_Rating))+ 
  geom_point() + labs(x = 'Number of Ratings',y= ' Average Rating', title = 'Average Book Rating by Number of Ratings' ) +
  theme_minimal()
# Stacked bar chart showing ratings by prestige
ggplot(data = books, aes(Book_average_rating))+
  geom_bar(aes(fill = Author_Rating))
# Box plot 
ggplot(data = books, mapping = aes(x = `Author_Rating`, y = `Book_average_rating`))+
  geom_boxplot(aes(fill = `Author_Rating`)) +
  labs(x = 'Author Rating', y = 'Average Book Rating', title = ' Average Book Ratings by Author Prestige')+
  theme_grey()

# fourth visual 
authors %>%
  top_n(10, total_units_sold) %>%
  ggplot()+geom_bar(mapping=aes(x=fct_reorder(Author,total_units_sold),y= total_units_sold),stat = "identity", fill = 'lightblue')+
  labs(x = 'Author', y = 'Total Books Sold', title = 'Top 10 Selling Authors') +
  coord_flip()

books %>% 
  top_n(25, `units sold`) %>% 
  ggplot() + geom_bar(mapping = aes(x = fct_reorder(`Book Name`,`units sold`), y = `units sold`),stat = 'identity', fill = 'orange')+
  labs(x = 'Book Title', y = 'Units Sold', title = 'Top 25 Books based on Units Sold')+
  coord_flip()+
  theme_minimal()


# Revenue by Publisher
ggplot(data = books, mapping = aes(x = Publisher, y = `publisher revenue`))+
  geom_bar(stat = 'identity', fill = 'navy')+
  labs(title = 'Revenue by Publisher')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# units sold by publisher
ggplot(data = books, mapping = aes(x = Publisher, y = `units sold`))+
  geom_bar(stat = 'identity', fill = 'blue')+
  labs(title = 'Number of Units Sold by Publisher')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Extra Plots
ggplot(data = books1900s , mapping = aes(x =`Publishing Year`, y = `units sold`))+
  geom_line() + facet_wrap(~Publisher)

ggplot(data = books, mapping= aes(x = genre, y = `gross sales`)) + 
  geom_bar(aes(fill = `Publisher`), stat = 'identity') 

ggplot(data = books, mapping= aes(x = genre, y = `publisher revenue`)) + 
  geom_bar(aes(fill = `Publisher`), stat = 'identity')

# geom denisty of authors average rating
ggplot(data = authors, mapping = aes(rating))+  geom_density()

ggplot(data = books_year2, aes(x = `Publishing Year`, y = avg_price)) + 
  geom_line(color = 'green') + 
  labs(x = 'Publishing Year', y = 'Average Price', title = "Average Price of Books from 1900 to Present")+
  theme_dark()






