
# TO DO
# Change scatterplots to either 
# pointsize = n data points
# sample < 1 star

# Clean locations data

# Add notes

# Label interesting points on scatterplot

# --------------------------------------------------------------------------------

# Andy Hayler 

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

library(XML)
library(plyr)
library(dplyr)
library(shiny)
library(kimisc)

# x <- "https://www.andyhayler.com/restaurant-guide?size=0"

# ------
# Clean
# ------

# Lines <- readLines(x)

load("Andy_Hayler_data.RData")

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
# 
Tab <- Tab[complete.cases(Tab), ]

# ------
# Plots
# ------
# 
# Price & rating
library(ggplot2)
# Colours
cbPalette <- c("#F39C12","#0000FF", "#FF00FF", "#000000") # red "#FF0000"
Size <- 2
Circle_size <- 1
Alpha <- 0.7
Text_size <- 14

# Puts one star on top
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

# Price vs Rating (all)

# Freq_data <- as.data.frame(table(Tab$Price, Tab$Rating))
# names(Freq_data) <- c("Price", "Rating", "Freq")
# Freq_data <- filter(Freq_data, Freq > 0)

as.numeric(which(subset(Tab, Stars == 0) ))

Sub_tab <- Tab[-sample(which(Tab$Stars == 0), 900), ]

Price_rate <- ggplot(Sub_tab, aes(Price, Rating, colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+#, position = "jitter")+
  theme_classic()+
  ggtitle("Price vs Rating by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")+stat_ellipse(size = Circle_size, type = "t")

# g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
# g <- g  + scale_size(range = c(2, 20), guide = "none" )
# g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
# g <- g + geom_point(aes(colour=freq, size = freq))
# g <- g + scale_colour_gradient(low = "lightblue", high="white") 

# Price vs rating (split by star)
Price_rate_split <- ggplot(Tab, aes(Price, Rating, colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  theme_classic()+
  ggtitle("Price vs Rating by stars, split out")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+ # aes(group = 1)
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, 10, 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")

# Price vs value (all)
Price_val <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha, position = "jitter")+
  theme_classic()+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")

# Price vs value (split by star)
Price_val_split <- ggplot(Tab, aes(Price, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  theme_classic()+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Price), 50)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")

# Rating v log(value) (all)
Rating_val <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha, position = "jitter")+
  theme_classic()+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating, na.rm = T), 1)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")

# Rating vs log value (split by star)
Rating_val_split <- ggplot(Tab, aes(Rating, log(Value+1), colour = factor(Stars)))+
  geom_point(size = Size, alpha = Alpha)+
  facet_wrap( ~ factor(Stars) )+
  theme_classic()+
  ggtitle("Price vs log(Value) by stars")+
  stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
  scale_colour_manual(values=cbPalette)+
  scale_y_continuous(breaks = seq(0, log(max(Tab$Value+1, na.rm = T)), 1))+
  scale_x_continuous(breaks = seq(0, max(Tab$Rating, na.rm = T), 1)) +
  theme(axis.title = element_text(size=Text_size),
        axis.text = element_text(size = Text_size)) +
  labs(colour = "Stars")

# Price frequency by country
# Price_country_hist <- ggplot(Tab[Tab["Country"] == " United Kingdom", ], aes(Price))+
#   geom_histogram()+
#   ggtitle("Histogram of price by country")

Price_country_hist <- ggplot(Tab, aes(Price, fill= factor(Stars)))+
  geom_histogram(bins = 50, alpha = Alpha)+theme_classic()+ggtitle("Histogram of price by country")

# ggplot(Tab[Tab["Country"] == " United Kingdom", ], aes(Price, fill= factor(Stars)))+
#   geom_histogram(bins = 50, alpha = Alpha)+theme_classic()

# Rating by country
N_country <- data.frame(table(Tab$Country))
Meds_country <- c(by(Tab$Rating, Tab$Country, median, na.rm = T))

Rating_country <- ggplot(Tab, aes(Country, Rating))+
  geom_boxplot(fill = cbPalette[1], alpha = Alpha)+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data=data.frame(),
            aes(x=names(Meds_country), y=Meds_country+0.5, label=N_country$Freq),
            col='blue', size=4)+
  ggtitle("Rating by country (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )

# Rating by cuisine type
N_cuis <- data.frame(table(Tab$Cuisine))[,2]
Meds <- c(by( Tab$Rating, Tab$Cuisine, median, na.rm = T))

Rating_cuisine <- ggplot(Tab, aes(Cuisine, Rating))+
  geom_boxplot(fill = cbPalette[1], alpha = Alpha)+theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data=data.frame(),
            aes(x=names(Meds), y=Meds+0.5, label=N_cuis),
            col='blue', size=4)+
  ggtitle("Rating by cuisine type (with # of reviews)")+
  scale_y_continuous(breaks = 0:10)+
  geom_hline(yintercept = median(Tab$Rating, na.rm = T),
             linetype = "dashed", colour = "red" )

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
  titlePanel("Michelin star data from AndyHayler.com"),
  
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
    h2("Andy Hayler introduction text here... "),
    
    h4("A note on the ratings: these are the subjective ratings of Andy Hayler. 
       He scores restaurants on a scale from 1 to 20. 
       For some reason he changed this from 1 to 10 a while ago. This hasn't seemed to 
       have increased the range of scores however because there are only about two 
       reviews with less than 10/20. So I just subtracted ten from all scores and made any 
       scores less than zero to zero.
       The range of restaurants reviewed is generally quite varied. There are reviews of 
       pizzerias and fish & chips alongside, say, a three-michelin star restaurant in the centre 
       of Paris. But the criteria for review is gerally that it's an independent place 
       with some interesting reason to 
       It's quite rare to get a zero, and a score of 1-3 or 4 is actually pretty good because it's
       relative to the extreme values of 
       
       Andy pays for the restaurants himself so there should be little bias here."),
    
    h3("Price vs rating"),
    plotOutput("Price_rate"),
    plotOutput("Price_rate_split"),
    
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
    
    )
  #    )
    )

# Server output
server <- function(input, output) {
  
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

# # Run the application 
shinyApp(ui = ui, server = server)



# ------------------------------------------------------------------------------------ 
# # PCA
# 
# # library("FactoMineR")
# # library("factoextra")
# # 
# # AH_pca <- PCA(Tab[, 3:5], graph = FALSE)
# # 
# # fviz_eig(AH_pca, addlabels = TRUE, ylim = c(0, 100))
# # 
# # fviz_pca_biplot(AH_pca, 
# #                 col.ind = factor(Tab$Stars), palette = cbPalette, 
# #                 addEllipses = TRUE, label = "var",
# #                 col.var = "black", repel = TRUE,
# #                 legend.title = "Stars")
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





