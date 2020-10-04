
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
# 
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

# ----------------
# Data summary
# ----------------

n_reviews <- nrow(Tab)



# Table and barplot of reviews by star

# Star_tab <- table(Tab$Stars)
Star_plot <- ggplot(Tab, aes(Stars))+
  geom_histogram(bins = 4, alpha= 0.75, colour = "black", fill = "red")+
  xlab("Number Michelin stars")+ylab("Number of reviews")+
  # ylim(c(0, nrow(Tab)+100))+
  scale_y_continuous( breaks = seq(0, nrow(Tab), 100), limits = c(0, nrow(Tab)+10))+
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1)+
  theme_bw()


# ------
# Plots
# ------

# Price & rating

# Colours
#cbPalette <- c("#00C5D3", "BF003C", "#F7B100", "#34C400") # red "#FF0000"
#cbPalette <- c("#FF0000", "#0048ff", "#28c128", "#c128b1") 
cbPalette <- c("#00AFBB", "#E7B800", "#FC4E07", "#c128b1")
Size <- 2
Circle_size <- 2
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
Price_rate <- Price_rate + geom_point(size = Size, alpha = Alpha)
Price_rate <- Price_rate + theme_classic()
Price_rate <- Price_rate + ggtitle("Price vs Rating, grouped by star status")
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
  ggtitle("Price vs Rating by stars, split out")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+ # aes(group = 1)
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Price vs value (all)
Price_val <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  # geom_point(size = Size, alpha = Alpha, position = "jitter")+
  geom_point(size = Size, alpha = Alpha)+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Price vs value (split by star)
Price_val_split <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Rating v log(value) (all)
Rating_val <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha, position = "jitter")+
  # geom_point(size = Size, alpha = Alpha)+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating, na.rm = T), 1)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Rating vs log value (split by star)
Rating_val_split <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating, na.rm = T), 1)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+
  theme_bw()

# Price frequency by country
# Price_country_hist <- ggplot(Tab[Tab["Country"] == " United Kingdom", ], aes(Price))+
#   geom_histogram()+
#   ggtitle("Histogram of price by country")

Price_country_hist <- ggplot(Tab, aes(Price, fill= factor(Stars)))+
  geom_histogram(bins = 50, alpha = Alpha)+
  ggtitle("Histogram of price by stars")+
  scale_y_continuous(limits = c(0, 250))+
  labs(fill = "Stars")+
  theme_bw()

# ggplot(Tab[Tab["Country"] == " United Kingdom", ], aes(Price, fill= factor(Stars)))+
#   geom_histogram(bins = 50, alpha = Alpha)+theme_classic()

# Rating by country
N_country <- data.frame(table(Tab$Country))
Meds_country <- c(by(Tab$Rating, Tab$Country, median, na.rm = T))

Rating_country <- ggplot(Tab, aes(Country, Rating))+
  geom_boxplot(fill = cbPalette[1], alpha = Alpha)+
  geom_text(data=data.frame(),
            aes(x=names(Meds_country), y=Meds_country+0.5, label=N_country$Freq),
            col='blue', size=4)+
  ggtitle("Rating by country (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Rating by cuisine type
N_cuis <- data.frame(table(Tab$Cuisine))[,2]
Meds <- c(by( Tab$Rating, Tab$Cuisine, median, na.rm = T))

Rating_cuisine <- ggplot(Tab, aes(Cuisine, Rating))+
  geom_boxplot(fill = cbPalette[1], alpha = Alpha)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data=data.frame(),
            aes(x=names(Meds), y=Meds+0.5, label=N_cuis),
            col='blue', size=4)+
  ggtitle("Rating by cuisine type (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )


# 

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
# 
# 
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
      around the world.", fmt(n_reviews)),
      tags$br(),
      tags$br(),
      "In five different years he ate in all three Michelin star restaurants in the world.
      This is an exploration of the basic data from his reviews, including subjective rating, price,
      Michelin star status, location and cuisine type."
    )),
    
    tags$br(),
    tags$br(),

    h5(tags$div(
      "A note on the ratings: These are the subjective ratings of Andy Hayler on his most recent
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
      pizzerias and fish & chips alongside, say, a three michelin star restaurant in the centre
      of Paris. But the criteria for review is generally that it's an independent place
      with some interesting reason to visit such as a prior recommendation or review",
      tags$br(),
      tags$br(),
      "However, since Hayler is from London, there are many more reviews of places in England.
      It's quite rare that he marks a zero, and a score of more than 2 or 3 means the food is actually pretty good
      because the score is relative to the extreme and rare values of 10/10 (see table below).",
      tags$br(),
      tags$br(),
      "Hayler pays for the restaurants himself so there should be little bias in the subjective score.")),
    
    h5(tags$div(
      "A note on prices: The prices here are what he calls an 'average price', 
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
    
    
    h3("Number of reviews by star"),
    plotOutput("Star_plot"),
    
    h3("Price vs rating"),
    plotOutput("Price_rate"),
    plotOutput("Price_rate_split"),
    h6(tags$div("Note: Many of the one-star restaurants are obscured by the zero-star restaurants.
                The circles highlight the star rating groups and are calculated using a
                '95% confidence interval'",
                tags$br(),
                tags$br(),
                "Comments: Here we see an interesting 'diminishing returns' affect with
                Michelin star status. The subjective rating flattens out as the star status
                increases.",
                tags$br(),
                tags$br(),
                "This is particularly the case with three Michelin star status - the
                subjective rating flattens out between two and three stars, while the
                price continues to increase (x-axis)",
                tags$br(),
                tags$br(),
                "However it should be noted that there is more 'uncertainty' a the three-star level
                (as indicated by the widening confidence interval) since they have the fewest reviews."
    )),

    h3("Price vs value"),
    plotOutput("Price_val"),
    plotOutput("Price_val_split"),

    h3("Rating vs value"),
    plotOutput("Rating_val"),
    plotOutput("Rating_val_split"),

    h3("Data by country / cuisine"),
    plotOutput("Price_country_hist"),
    plotOutput("Rating_country"),

    h4("Here we can see that the UK scores about 3 (median), while France and Germany
       are score top. I think the UK's low score can be explained by the fact that Andy reviews
       nearly everywhere he'll eat out, and being a native to the UK this will include
       a lot more casual / cheaper places.
       It's a bit surprising that Germany is higher than France,
       though there are a lot fewer reviews and a bit more spread."),

    plotOutput("Rating_cuisine")
    
    ) # mainPanel
  #    )
    ) # fluidPage

# Server output
server <- function(input, output) {
  
  output$Star_plot <- renderPlot({Star_plot})
  
  output$Price_rate <- renderPlot({Price_rate})
  output$Price_rate_split <- renderPlot({Price_rate_split})

  output$Price_val <- renderPlot({Price_val})
  output$Price_val_split <- renderPlot({Price_val_split})

  output$Rating_val <- renderPlot({Rating_val})
  output$Rating_val_split <- renderPlot({Rating_val_split})

  output$Price_country_hist <- renderPlot({Price_country_hist})
  output$Rating_country <- renderPlot({Rating_country})
  output$Rating_cuisine <- renderPlot({Rating_cuisine})
  
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





