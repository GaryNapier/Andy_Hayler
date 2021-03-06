
# TO DO


# Summary (and plot) of all data
  # Tables/ranges of each column

# Clean locations data

# Label interesting points on scatterplots
  # Leading to breakdown of top/most interesting restaurants

# --------------------------------------------------------------------------------

# Andy Hayler 

# setwd("~/Documents/Github/Andy_Hayler")

heaD <- function(x, ...){
  head(x, ...)
}
Round_up <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}

Remove_rows <- function(Data, Col, Value, N){
  Data[-(sample(as.numeric(row.names(subset(Data, Col == Value) )), N)), ]
}

fmt <- function(x, ...){format(x, big.mark=",",scientific=FALSE, ...)}

max_hist <- function(ggplot_hist){
  max(ggplot_build(ggplot_hist)$data[[1]]$count)
}

library(XML)
library(plyr)
library(dplyr)
library(shiny)
library(ggplot2)
# library(kimisc)


# Read in data 

URL <- "https://www.andyhayler.com/restaurant-guide?size=0"

Lines <- readLines(URL)

# ------
# Clean
# ------

# Lines <- readLines(x)

# load("Andy_Hayler_data.RData")

Lines <- gsub("<span></span>", 1, Lines)

Tab <- cbind(readHTMLTable(Lines, header=T, which=2,stringsAsFactors=F),
             readHTMLTable(Lines, header=T, which=1,stringsAsFactors=F))

# Price
Tab$Price <- as.numeric(gsub(".([0-9]+).*$", "\\1", Tab$Price))
Tab$Price <- Round_up(Tab$Price) # round up 

# Rating
Tab$Rating <- as.numeric(substr(Tab$Rating, 1, 2)) - 10

# Stars
x <- Tab$Stars
x <- gsub(" ", "", x)
x <- gsub("\n", "", x)
x <- strsplit(x, "")

Tab$Stars <- sapply(x, function(x){sum(as.numeric(x))})

Tab[is.na(Tab["Stars"]), "Stars"] <- 0

# Value
Tab$Value <- Tab$Rating / Tab$Price
Tab$Value <- signif((Tab$Value / max(Tab$Value[!is.na(Tab$Value)]) ) * 100, 2)

# Location
x <- Tab$`Name of restaurant`

x <- strsplit(x, ",")

Tab$Name <- unlist(sapply(x, function(x){x[1]}))
Tab$City <- unlist(sapply(x, function(x){x[2]}))
Tab$Country <- unlist(sapply(x, function(x){x[3]}))

Tab$`Name of restaurant` <- NULL

# # hist(Tab$Price)
# # hist(Tab$Rating)
# # hist(Tab$Value)
# # hist(Tab$Stars)
# # barplot(Tab$City)
# # 
# # with(Tab, plot(Price, Rating))
# # with(Tab, plot(Price, log(Value)))
# # with(Tab, plot(Rating, Value))

Tab <- Tab[complete.cases(Tab), ]

# Clean 'Cusine' col
Tab$Cuisine <- ifelse(Tab$Cuisine == "Fish & Chips", "British", Tab$Cuisine)
European <- c("Austrian", "Czech", "Danish", "Greek", "Hungarian", "Norwegian", "Russian", 
              "Portuguese", "Swedish")
Tab$Cuisine <- ifelse(Tab$Cuisine %in% European, "European", Tab$Cuisine)
Asian <- c("Burmese", "Indonesian", "Korean", "Malaysian", "Singaporean", "Vietnamese")
Tab$Cuisine <- ifelse(Tab$Cuisine %in% Asian, "Asian", Tab$Cuisine)
Middle_eastern <- c("Lebanese", "Persian", "Tunisian", "Turkish")
Tab$Cuisine <- ifelse(Tab$Cuisine %in% Middle_eastern, "Middle Eastern", Tab$Cuisine)
Tab$Cuisine <- ifelse(Tab$Cuisine == "Peruvian", "South American", Tab$Cuisine)
Tab$Cuisine <- ifelse(Tab$Cuisine == "South African", "African", Tab$Cuisine)

# Clean 'Country' col
UK <- c(" Channel Islands", " Jersey", " Man", " United Kingdom")
Tab$Country <- ifelse(Tab$Country %in% UK, "United Kingdom", Tab$Country)
USA <- c(" Colorado", " Colorado ", " Florida ", " Hawaii ", " Texas ", " United States")
Tab$Country <- ifelse(Tab$Country %in% USA, "United States", Tab$Country)
France <- c(" Loire", " France")
Tab$Country <- ifelse(Tab$Country %in% France, "France", Tab$Country)
# Remove trailing spaces
Tab$Country <- trimws(Tab$Country)


# ----------------
# Data summary
# ----------------

n_reviews <- nrow(Tab)
Alpha <- 0.75
Text_sz <- 6

# Overall 

# Barplot of reviews by star
Star_plot <- ggplot(Tab, aes(Stars))+
  geom_bar(alpha=Alpha, colour = "black", fill = "red")+
  xlab("Number Michelin stars")+ylab("Number of reviews")+
  scale_y_continuous(breaks = seq(0, nrow(Tab), 100), limits = c(0, nrow(Tab)+10))+
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1, hjust=1)+
  stat_count(aes(y=..count..,label = paste0("(", scales::percent(..prop..), ")")),
             geom="text", vjust=-1, hjust=-0.25)+
  theme_bw()


# Rating

# Barplot of reviews by rating 
Ratings <- sort(unique(Tab$Rating))
Med_rate <- median(Tab$Rating)
Rating_plot <- ggplot(Tab, aes(Rating))+
  geom_bar(alpha=Alpha, colour = "black", fill = "blue")+
  xlab("Rating")+ylab("Number of reviews")+
  scale_y_continuous(breaks = seq(0, nrow(Tab), 100), limits = c(0, nrow(Tab)+10))+
  scale_x_continuous(breaks = Ratings)+
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1, hjust=1.25)+
  stat_count(aes(y=..count..,label = paste0("(", scales::percent(round(..prop.., 1)), ")")),
             geom="text", vjust=-1, hjust=-0.05)+
  annotate(x = Med_rate, y = nrow(Tab) - 50, geom = "text", size = Text_sz, 
           label = sprintf("Median rating = %s", Med_rate))+
  geom_vline(xintercept = Med_rate, linetype = "dotted")+
  theme_bw()

# Split rating by star
Med_rate_star <- aggregate(Tab$Rating, list(Tab$Stars), median)
names(Med_rate_star) <- c("Stars", "Med_rating")
Med_rate_star$Med_rating <- round(Med_rate_star$Med_rating, 2)
Rate_star_max <- Round_up(max(table(Tab$Rating, Tab$Stars)), 100)
Rate_star_max_pc_inc <- Rate_star_max*0.1

Rate_star_hist <- ggplot()+
  geom_bar(data = Tab, aes(x = Rating, fill=factor(Stars)), alpha = Alpha, colour = "black")+
  scale_x_continuous(breaks = Ratings, labels = Ratings)+
  scale_y_continuous(breaks = seq(0, Rate_star_max, 50), 
                     limits = c(0, Rate_star_max+Rate_star_max_pc_inc))+
  geom_segment(data = Med_rate_star, 
               aes(x = Med_rating , y = 0, xend = Med_rating, yend = Rate_star_max-Rate_star_max_pc_inc), 
               linetype = "dotted")+
  geom_text(data = Med_rate_star, 
            mapping = aes(x = Med_rating, y = Inf,
                          label = sprintf("Median rating = %s", Med_rating)), vjust = 2)+
  facet_wrap( ~ factor(Stars) )+
  labs(fill = "Stars")+
  theme_bw()



# Price

# Hist of reviews by price 
Med_price <- round(median(Tab$Price), 0)
n_price <- 50
Max_price <- Round_up(max(Tab$Price), to = 100)

Price_plot <- ggplot(Tab, aes(Price))+
  geom_histogram(binwidth = n_price, alpha=Alpha, colour = "black", fill = "green",  boundary = 0)+
  xlab("Price")+ylab("Number of reviews")+
  scale_y_continuous(breaks = seq(0, nrow(Tab), 100), limits = c(0, nrow(Tab)+10))+
  scale_x_continuous(breaks = seq(0, Max_price, n_price), 
                     labels = seq(0, Max_price, n_price))+
  annotate(x = Med_price, y = nrow(Tab) - 50, geom = "text", size = Text_sz, 
           label = sprintf("Median price = £%s", Med_price))+
  geom_vline(xintercept = Med_price, linetype="dotted")+
  theme_bw()

# Split price by star
Med_price_star <- aggregate(Tab$Price, list(Tab$Stars), median)
names(Med_price_star) <- c("Stars", "Med_price")
Med_price_star$Med_price <- round(Med_price_star$Med_price, 2)
Price_star_max <- Round_up(max(table(Tab$Price, Tab$Stars)), 100)
Price_star_max_pc_inc <- Price_star_max*0.1

Price_star_hist <- ggplot()+
  geom_histogram(data = Tab, aes(x = Price, fill=factor(Stars)), 
                 bins = n_price, alpha = Alpha, colour = "black", boundary = 0)+
  scale_x_continuous(breaks = seq(0, Max_price, n_price), 
                     labels = seq(0, Max_price, n_price))+
  scale_y_continuous(breaks = seq(0, Price_star_max, 50), 
                     limits = c(0, Price_star_max+Price_star_max_pc_inc))+
  geom_segment(data = Med_price_star, 
               aes(x = Med_price , y = 0, xend = Med_price, yend = Price_star_max-Price_star_max_pc_inc), 
               linetype = "dotted")+
  geom_text(data = Med_price_star, 
            mapping = aes(x = Med_price, y = Inf, label = sprintf("Median price = £%s", Med_price)), vjust = 2)+
  facet_wrap( ~ factor(Stars) )+
  labs(fill = "Stars")+
  theme_bw()


# Value

# Hist of reviews by value 
Med_value <- round(median(Tab$Value), 0)
Max_val <- max(Tab$Value)
x_scale <- seq(0, Max_val, 10)
n_val <- 10


Value_plot <- ggplot(Tab, aes(Value))+
  geom_histogram(bins = n_val+1, alpha=Alpha, colour = "black", fill = "orange",  boundary = 0)+
  xlab("Value")+ylab("Number of reviews")+
  scale_y_continuous(breaks = seq(0, nrow(Tab), 100), limits = c(0, nrow(Tab)+10))+
  scale_x_continuous(breaks = x_scale, labels = x_scale)+
  annotate(x = Med_value, y = nrow(Tab) - 50, geom = "text", size = 6, 
           label = sprintf("Median value = %s", Med_value))+
  # geom_vline(xintercept = Med_value, linetype="dotted")+
  geom_segment(aes(x = Med_value, y = 0, xend = Med_value, yend = nrow(Tab)-nrow(Tab)*0.1),
               linetype = "dotted")+
  theme_bw()

# Split val by star
Med_val_star <- aggregate(Tab$Value, list(Tab$Stars), median)
names(Med_val_star) <- c("Stars", "Med_value")
Med_val_star$Med_value <- round(Med_val_star$Med_value, 2)
# Val_star_max <- Round_up(max(table(Tab$Value, Tab$Stars)), 100)
# Val_star_max_pc_inc <- Val_star_max*0.1



Val_star_hist <- ggplot()+
  geom_histogram(data = Tab, aes(x = Value, fill=factor(Stars)), 
                 bins = n_val+1, alpha = Alpha, colour = "black",
                 boundary = 0, closed = "left")+
  scale_x_continuous(breaks = seq(0, Max_val, n_val), 
                     labels = seq(0, Max_val, n_val))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  geom_text(data = Med_val_star, 
            mapping = aes(x = Med_value, y = Inf, label = sprintf("Median value = %s", Med_value)), vjust = 2)+
  facet_wrap( ~ factor(Stars) )+
  labs(fill = "Stars")+
  theme_bw()
yend <- max_hist(Val_star_hist)
Val_star_hist <- Val_star_hist + 
  geom_segment(data = Med_val_star,
             aes(x = Med_value, y = 0, xend = Med_value, yend = yend + yend*0.1),
             linetype = "dotted")



# Country

# Rating by country
N_country <- data.frame(table(Tab$Country))
Meds_country <- c(by(Tab$Rating, Tab$Country, median, na.rm = T))
Rating_country <- ggplot(Tab, aes(Country, Rating))+
  geom_boxplot(fill = "yellow", alpha = Alpha)+
  geom_text(data=data.frame(),
            aes(x=names(Meds_country), y=Meds_country+0.5, label=N_country$Freq), size=Text_sz-2)+
  # ggtitle("Rating by country (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Barplot of stars by country
Stars_country <- data.frame(table(Tab$Country, Tab$Stars))
names(Stars_country) <- c("Country", "Stars", "Number")

Stars_country_plot <- ggplot()+
  geom_bar(data = Stars_country, aes(x = Country, y = Number, fill = Stars), 
          position="dodge", stat="identity")+
  # scale_y_continuous(breaks = seq(0, Round_up(max(Stars_country$Number), 100), 50))+
  scale_y_continuous(breaks = seq(0, Round_up(max(Stars_country$Number), 100), 50), 
                     limits = c(0, Round_up(max(Stars_country$Number), 100)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cuisine

# Rating by cuisine type
N_cuis <- data.frame(table(Tab$Cuisine))[,2]
Meds <- c(by( Tab$Rating, Tab$Cuisine, median, na.rm = T))
Rating_cuisine <- ggplot(Tab, aes(Cuisine, Rating))+
  geom_boxplot(fill = "cyan", alpha = Alpha)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data=data.frame(),
            aes(x=names(Meds), y=Meds+0.5, label=N_cuis), size=4)+
  # ggtitle("Rating by cuisine type (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )

# ---------------
# Analysis plots
# ---------------

# Price & rating

# Colours
#cbPalette <- c("#00C5D3", "BF003C", "#F7B100", "#34C400") # red "#FF0000"
#cbPalette <- c("#FF0000", "#0048ff", "#28c128", "#c128b1") 
cbPalette <- c("#00AFBB", "#E7B800", "#FC4E07", "#c128b1")
Size <- 2
Circle_size <- 1
Alpha <- 0.7
Text_size <- 14
Size_0 <- 1
Size_1 <- 3
Size_2 <- 5
Size_3 <- 7
Stroke <- 1


Star_0 <- subset(Tab, Stars == 0)
Star_1 <- subset(Tab, Stars == 1)
Star_2 <- subset(Tab, Stars == 2)
Star_3 <- subset(Tab, Stars == 3)


# Price_rate <- ggplot()
# Price_rate <- Price_rate + geom_point(data = Star_0, aes(Price, Rating, colour = factor(Stars),
#                                                          shape = factor(Stars)),
#                                       size = Size_0, alpha = Alpha, stroke = Stroke)
# Price_rate <- Price_rate + geom_point(data = Star_1, aes(Price, Rating, colour = factor(Stars),
#                                                          shape = factor(Stars)),
#                                       size = Size_1, alpha = Alpha, stroke = Stroke)
# Price_rate <- Price_rate + geom_point(data = Star_2, aes(Price, Rating, colour = factor(Stars),
#                                                          shape = factor(Stars)),
#                                       size = Size_2, alpha = Alpha, stroke = Stroke)
# Price_rate <- Price_rate + geom_point(data = Star_3, aes(Price, Rating, colour = factor(Stars),
#                                                          shape = factor(Stars)),
#                                       size = Size_3, alpha = Alpha, stroke = Stroke)+
#   stat_ellipse(data=Tab, aes(x = Price, y = Rating, colour = factor(Stars)), 
#                size = Circle_size, type = "t", show.legend = F, alpha = Alpha)+
#   scale_shape_manual(values=c(1, 1, 1, 1))+
#   theme_classic()+
#   ggtitle("Price vs Rating, grouped by star status")+
#   stat_smooth(data = Tab, se = T, formula = y~poly(x, 2), method = "lm", aes(x=Price, y=Rating),
#               show.legend = F)+
#   scale_colour_manual(values=cbPalette)+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   scale_x_continuous(breaks = seq(0, max(Tab$Price), 50))+
#   theme(axis.title = element_text(size=Text_size),axis.text = element_text(size = Text_size),
#         legend.justification=c(1,0), legend.position=c(1,0))+
#   labs(shape = "Stars", colour = "Stars")+
#   guides(colour = guide_legend(override.aes = list(size=c(Size_0, Size_1, Size_2, Size_3))))


# # Change colors and shapes manually
# ggplot(Tab, aes(Price, y=Rating, group=factor(Stars))) +
#   geom_point(aes(shape=factor(Stars), color=factor(Stars)), size=2) +
#   scale_shape_manual(values=c(3, 16, 17))+
#   scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
#   theme(legend.position="top")


# Price vs Rating (all)
Price_rate <- ggplot(Tab, aes(Price, Rating, colour = factor(Stars)))
Price_rate <- Price_rate + geom_point(size = Size, alpha = Alpha, 
                                      position = position_jitter(w = 5, h = 0))
# Price_rate <- Price_rate + ggtitle("Price vs Rating, grouped by star status")
Price_rate <- Price_rate + stat_smooth(se = T,  
                                       formula = y~poly(x, 2),
                                       method = "lm", aes(group = 1), 
                                       show.legend = F)
Price_rate <- Price_rate + scale_colour_manual(values=cbPalette)
Price_rate <- Price_rate + scale_y_continuous(breaks = seq(0, 10, 1))
Price_rate <- Price_rate + scale_x_continuous(breaks = seq(0, max(Tab$Price), 50))
Price_rate <- Price_rate + theme(axis.title = element_text(size=Text_size),
                                 axis.text = element_text(size = Text_size)) 
Price_rate <- Price_rate + labs(colour = "Stars")
Price_rate <- Price_rate + stat_ellipse(size = Circle_size, type = "t", show.legend = F,
                                        alpha = Alpha)
Price_rate <- Price_rate + theme_bw()

# Anova analysis of stars and rating
Star_lm <- glm(Rating~factor(Stars)+I(Price/10) -1, data = Tab)
Star_lm <- glm(Rating~factor(Stars) -1, data = Tab)
Star_lm <- lm(Rating~I(Price/10), data = Tab)
summary(Star_lm)

Stars_anova <- aov(Rating~factor(Stars)+Price, data = Tab)
summary(Stars_anova)
# TukeyHSD(Stars_anova, "Price", ordered = TRUE)


# Price vs rating (split by star)
Price_rate_split <- ggplot(Tab, aes(Price, Rating, colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  # ggtitle("Price vs Rating by stars, split out")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+ # aes(group = 1)
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme_bw()+
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size), 
        legend.position = "none") 

# ---

# Price vs value (all)
log_val_max <- Round_up(log(max(Tab$Value+1, na.rm = T)), 1)
Price_val <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha, position = position_jitter(w = 5, h = 0))+
  # ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(0, log_val_max))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Price vs value (split by star)
Price_val_split <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(0, log_val_max))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme_bw()+
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size), 
        legend.position = "none") 

# ---

# Rating v log(value) (all)
Rating_val <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha, position = position_jitter(w = 0.1, h = 0))+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(-0.5, log_val_max))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating), 1), 
                     limits = c(-0.5, max(Tab$Rating)+0.5))+
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Rating vs log value (split by star)
Rating_val_split <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  # ggtitle("Rating vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating, na.rm = T), 1)) +
  theme_bw()+
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size), 
        legend.position = "none") 
  


# # Rating by star
# N_star <- data.frame(table(Tab$Stars))[,2]
# Med_star <- c(by( Tab$Rating, Tab$Stars, median, na.rm = T))
# 
# ggplot(Tab, aes(factor(Stars), Rating))+
#   geom_boxplot(fill = cbPalette[1], alpha = Alpha)+theme_classic()+
#   ggtitle("Rating by stars (and # of reviews)")+
#   geom_text(data=data.frame(),
#             aes(x=names(Med_star), y=Med_star+0.5, label=N_star), 
#             col='blue', size=4)+
#   scale_y_continuous(breaks = 1:10)
# 
# 
# # Predict rating from price and stars
# # http://topepo.github.io
# 
# # Scatterplots
# library(AppliedPredictiveModeling)
# library(caret)
# 
# head(Tab)
# 
# head(model.matrix(Rating ~ ., data = Tab))
# 
# For_model <- Tab[, c("Cuisine", "Price", "Value", "Rating", "Stars", "Country")]
# 
# For_model <- model.matrix(Rating ~ ., data = For_model)
# 
# # Dummies <- dummyVars(Rating ~ ., data = For_model)
# # For_model <- predict(Dummies, newdata = For_model)

# ------------------------------------------------------------------------------------
#                             RUN SHINY
# ------------------------------------------------------------------------------------

# # Define UI for application that draws a plot
ui <- fluidPage(
  
  # Application title
  titlePanel("Restaurant review data from AndyHayler.com"),
  
  # Sidebar with a slider input for number of bins
  #    sidebarLayout(
  #       sidebarPanel(
  #          sliderInput("bins",
  #                      "Number of bins:",
  #                      min = 1,
  #                      max = 50,
  #                      value = 30)
  #       ),
  #
  # Show a plot of the generated distribution
  
  mainPanel(
    
    img(src="andy-hayler-logo.png"),
    img(src="Andy_Hayler_photo.png",width='200px'),
    
    h4(tags$div(
      sprintf("Andy Hayler is an independent restaurant critic who has reviewed around %s restaurants
      around the world.", fmt(Round_up(n_reviews))),
      tags$br(),
      tags$br(),
      "In five different years he ate in all three-Michelin star restaurants in the world.
      This is an exploration of the basic data from his reviews, including subjective rating, price,
      Michelin star status, location and cuisine type.",
      tags$br(),
      tags$br(), 
      "To view the raw data and the reviews, go to ",
      tags$a(href="https://www.andyhayler.com/restaurant-guide", 
             "AndyHayler.com > Restaurant guide."), 
      tags$br(), 
      tags$br(), 
      "First there are plots summarising all the reviews by rating, price, value, country and cusine, 
      which is broken down by Michelin star.", 
      tags$br(),
      tags$br(),
      "Then there are plots which go into analyes, for example how price is related to Hayler's rating, 
      again relating to Michelin star.", 
      tags$br(),
      tags$br(),
      "Finally there is a section on interesting individial restaurants, for example best and worst value restaurants."
      
    )),
    
    tags$br(),

    h5(tags$div(
      "A note on the ratings:", 
      tags$br(), 
      tags$br(),
      "These are the subjective ratings of Andy Hayler on his most recent
      visit to each restaurant.",
      tags$br(),
      tags$br(),
      "He scores them on a scale from 1 to 20. Previously, he rated restaurants from 1 to 10,
      however this hasn't seemed to increased the range of scores because there are very few 
      reviews with less than 10/20. Therefore ten was subtracted from all scores and any
      scores less than zero are rounded up to just zero. 
      So the scores here have been adjusted back to a just a simple 1 to 10 scale.",
      tags$br(),
      tags$br(),
      "The range of restaurants is quite varied. There are reviews of
      pizzerias and fish & chips alongside, say, a three-Michelin star restaurant in the centre
      of Paris. But the criteria for review is generally that it's an independent place
      with some interesting reason to visit such as a prior recommendation or review. 
      However, since Hayler is from London, there are many more reviews of places in England.",
      tags$br(),
      tags$br(),
      "It's quite rare that he marks a zero, and a score of more than 2 or 3 means the food is actually pretty good
      because the score is relative to the extreme and rare values of 10/10 (see 'Number of reviews by rating' plot below).",
      tags$br(),
      tags$br(),
      "Hayler pays for the restaurants himself so there should be little bias in the subjective score ('rating' from hereon).")),
    tags$br(),
    h5(tags$div(
      "A note on prices:", 
      tags$br(),
      tags$br(),
      "The prices here are what he calls an 'average price', 
      which is described as 'typical price for three courses and modest wine', 
      however it's unclear how this is calculated exactly.",
      tags$br(),
      tags$br(),
      "There can also be a big difference between this 'average price' and what Hayler himself pays,
      especially at the top restaurants.
      For example at Troisgros (10/10, 3 stars), 'average price' is 200, but the price he paid was 519, 
      and at Les Pres Eugenie (also 10/10, 3 stars) 'average price' is 180 whereas Hayler paid ~600 (!).
      Generally he either pays the average or more than the average. 
      Details of what he pays are on each restaurant's review page."
    )), 
    
    tags$br(),
    tags$br(), 
    
    h2("Summary of data"),
    
    tags$br(),
    
    h3("Number of reviews by star"),
    plotOutput("Star_plot"),
    
    tags$br(),
    
    h3("Number of reviews by rating"),
    plotOutput("Rating_plot"),
    
    tags$br(),
    
    h3("Number of reviews by rating, split by star"),
    plotOutput("Rate_star_hist"),
    
    tags$br(),
    
    h3("Number of reviews by price"),
    plotOutput("Price_plot"),
    
    tags$br(),
    
    h3("Number of reviews by price, split by star"),
    plotOutput("Price_star_hist"),
    
    tags$br(),
    
    h3("Number of reviews by value"),
    plotOutput("Value_plot"),
    
    tags$br(),
    
    h3("Number of reviews by value, split by star"),
    plotOutput("Val_star_hist"),
    
    tags$br(),
    
    h3("Rating by country (and number of reviews)"),
    plotOutput("Rating_country"),
    
    
    h4(sprintf("Comments: The UK scores about %s (median), while other European countries such as France, Germany, 
    Swizerland and the Netherlands score top. However, since Hayler lives in London there are far more UK reviews
    which include a lot more casual / cheaper places 
    (see Michelin stars by country plot below - the review overwhelmingly cover 0-star places in the UK). 
               Also, the choice of overseas restaurants is probably much more selective.", 
               Meds_country["United Kingdom"])),
    
    h3("Number of stars by country"),
    plotOutput("Stars_country_plot"),
    
    h3("Rating by cuisine (and number of reviews)"),
    plotOutput("Rating_cuisine"), 
    
    # output$Rating_country <- renderPlot({Rating_country})
    # output$Rating_cuisine <- renderPlot({Rating_cuisine})
    
    tags$br(),
    
    h2("Analyses"),
    
    tags$br(),
    
    h3("Price vs rating, all"),
    plotOutput("Price_rate"),
    h3("Price vs rating, split out by star status"),
    plotOutput("Price_rate_split"),
    h5(tags$div("The circles in the first plot highlight the star rating groups (0, 1, 2 or 3 Michelin stars),
                centering on the averages",
                tags$br(),
                tags$br(),
                "Comments: Here there seems to be an interesting 'diminishing returns' effect overall (top plot)
                between rating, price and Michelin star level: Hayler's rating flattens out as the price 
                and star status increase, suggesting price or star status only predict quality to a certain level.",
                tags$br(),
                tags$br(),
                "This is particularly the case with three Michelin star status - the
                subjective rating flattens out between two and three stars, while the
                price continues to increase.",
                tags$br(),
                tags$br(),
                "Indeed it looks as though from about £250 onwards, quality varies considerably,
                ranging from about 4 to maximum of 10.", 
                tags$br(),
                tags$br(),
                "However it should be noted that there is more uncertainty a the two and three-star levels
                (as indicated by the widening confidence intervals - grey shading) 
                since they have the fewest reviews."
    )),

    h3("Price vs value, all"),
    plotOutput("Price_val"),
    h3("Price vs value, split by star status"),
    plotOutput("Price_val_split"),
    h5(tags$div("Value has been shown here on a log scale just to tidy up the data and to show a better line of best fit 
                (original value scale 0-100)", 
                tags$br(), 
                tags$br(), 
                "Value quickly decreases the higher the price both overall and among all the star classes. 
                The best value places seem to be below about £100 in all three star classes, 
                the best of these being in the 0-star category, though it looks like there are many good-value 
                1-star places."
                )), 

    h3("Rating vs value"),
    plotOutput("Rating_val"),
    plotOutput("Rating_val_split")
    
    ) # mainPanel
  #    )
    ) # fluidPage

# Server output
server <- function(input, output) {
  
  # Basics
  output$Star_plot <- renderPlot({Star_plot})
  
  output$Rating_plot <- renderPlot({Rating_plot})
  output$Rate_star_hist <- renderPlot({Rate_star_hist})
  
  output$Price_plot <- renderPlot({Price_plot})
  output$Price_star_hist <- renderPlot({Price_star_hist})
  
  output$Value_plot <- renderPlot({Value_plot})
  output$Val_star_hist <- renderPlot({Val_star_hist})
  
  output$Rating_country <- renderPlot({Rating_country})
  output$Stars_country_plot <- renderPlot({Stars_country_plot})
  output$Rating_cuisine <- renderPlot({Rating_cuisine})
  
  # Analysis
  output$Price_rate <- renderPlot({Price_rate})
  output$Price_rate_split <- renderPlot({Price_rate_split})

  output$Price_val <- renderPlot({Price_val})
  output$Price_val_split <- renderPlot({Price_val_split})

  output$Rating_val <- renderPlot({Rating_val})
  output$Rating_val_split <- renderPlot({Rating_val_split})

  # output$Price_country_hist <- renderPlot({Price_country_hist})

}

# Run the application 
shinyApp(ui = ui, server = server)

# ------------------------------------------------------------------------------------ 
#                                         OTHER
# ------------------------------------------------------------------------------------ 

# ------------------------------------------------
# Make size of points reflect freq of data points
# ------------------------------------------------

# Freq_data <- as.data.frame(table(Tab$Price, Tab$Rating, Tab$Stars))
# names(Freq_data) <- c("Price", "Rating", "Stars", "Freq")
# Freq_data <- filter(Freq_data, Freq > 0)

# Sub_tab <- Tab[-sample(which(Tab$Stars == 0), 900), ]

# freqData <- as.data.frame(table(galton$child, galton$parent))
# names(freqData) <- c("child", "parent", "freq")
# freqData$child <- as.numeric(as.character(freqData$child))
# freqData$parent <- as.numeric(as.character(freqData$parent))
# 
# g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
# g <- g  + scale_size(range = c(2, 20), guide = "none" )
# g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
# g <- g + geom_point(aes(colour=freq, size = freq))
# g <- g + scale_colour_gradient(low = "lightblue", high="white")  
# g

# ---------------------
# Puts one star on top
# ---------------------
# ggplot(Tab)+
#   geom_point(aes(Price, Rating, color = factor(Stars), size = Size, alpha = Alpha))+
#   geom_point(data = subset(Tab, factor(Stars) == '1'),
#              aes(x = Price, y = Rating, color = factor(Stars), size = Size, alpha = Alpha))+
#   theme_classic()+
#   ggtitle("Price vs Rating by stars")+
#   stat_smooth(data = Tab,
#               aes(x = Price, y = Rating),
#               se = T,  formula = y~poly(x, 2), method = "lm")+ #aes(group = 1)
#   scale_colour_manual(values=cbPalette)+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
#   theme(axis.title = element_text(size=Text_size),
#         axis.text = element_text(size = Text_size)) +
#   labs(colour = "Stars")

# ----
# PCA
# ----
# library("FactoMineR")
# library("factoextra")
# 
# AH_pca <- PCA(Tab[, 3:5], graph = FALSE)
# 
# fviz_eig(AH_pca, addlabels = TRUE, ylim = c(0, 100))
# 
# fviz_pca_biplot(AH_pca,
#                 col.ind = factor(Tab$Stars), palette = cbPalette,
#                 addEllipses = TRUE, label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "Stars")
# 
# 
# 
# x <- 700
# Disc <- 0.30
# 
# Res <- x - (x*Disc)
# Res
# Res/(1-Disc)
# 
# x <- sample(c(0,1), 100, replace = T)
# sample(x, 100)
# mean(x)
# sd(x)


# date()
# "Sun May 27 16:40:44 2018"





