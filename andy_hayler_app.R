
# TODO

# star_plot y-axis lims

# Summary (and plot) of all data
  # tables/ranges of each column

# Clean locations data

# Label interesting points on scatterplots
  # Leading to breakdown of top/most interesting restaurants

# --------------------------------------------------------------------------------

# Andy Hayler 

# Deps ----

library(shiny)

# library(kimisc)

# Read in data ---

URL <- "https://www.andyhayler.com/restaurant-guide?size=0"
tab <- parse_data(URL)

# Clean ----

tab <- clean_data(tab)

# save(tab, file = "tab.RData")

# Plots - data summary ----

n_reviews <- nrow(tab)
alpha <- 0.75
text_sz <- 6

# Barplot - number of reviews by star
star_plot <- plot_stars(tab)

# Barplot - n reviews by rating
rating_plot <- plot_rating(tab)

# Hist - split rating by star
rating_by_star_plot <- plot_rating_by_star(tab)

# Price 

med_price <- round(median(tab$price), 0)
n_price <- 50

# Hist - reviews by price 
price_plot <- plot_price(tab, med_price)

# Hist - price by star
price_star_plot <- plot_price_star(tab, med_price)

# Value

# Hist of reviews by value 
value_plot <- plot_value(tab, med_value)

# Value by star
val_star_plot <- plot_val_star(tab)

# Country

# Rating by country
rating_country_plot <- plot_rating_country(tab)

# Barplot of stars by country
stars_country_plot <- plot_stars_country(tab)

# Cuisine
rating_cuisine_plot <- plot_rating_cuisine(tab)


# Analysis plots ----

# price & rating

# Colours
#cbPalette <- c("#00C5D3", "BF003C", "#F7B100", "#34C400") # red "#FF0000"
#cbPalette <- c("#FF0000", "#0048ff", "#28c128", "#c128b1") 
cbPalette <- c("#00AFBB", "#E7B800", "#FC4E07", "#c128b1")
size <- 2
circle_size <- 1
alpha <- 0.7
text_size <- 14
size_0 <- 1
size_1 <- 3
size_2 <- 5
size_3 <- 7
stroke <- 1

star_0 <- subset(tab, stars == 0)
star_1 <- subset(tab, stars == 1)
star_2 <- subset(tab, stars == 2)
star_3 <- subset(tab, stars == 3)

# Anova analysis of stars and rating
star_lm <- glm(rating~factor(stars)+I(price/10) -1, data = tab)
star_lm <- glm(rating~factor(stars) -1, data = tab)
star_lm <- lm(rating~I(price/10), data = tab)
summary(star_lm)

stars_anova <- aov(rating~factor(stars)+price, data = tab)
summary(stars_anova)
# TukeyHSD(stars_anova, "price", ordered = TRUE)

# price vs rating
price_rating_plot <- plot_price_rating(tab, cbPalette)

# Price vs rating (split by star)
price_rate_star_plot <- plot_price_rate_star(tab, cbPalette)

# price vs value 
price_value_plot <- plot_price_value(tab)

# price vs value (split by star)
price_value_star_plot <- plot_price_value_star(tab)

# rating v log(value) (all)
rating_value_plot <- plot_rating_value(tab)

# rating vs log value (split by star)
rating_val_star_plot <- plot_rating_value_star(tab)




# UI ----

# # Define UI for application that draws a plot
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Restaurant review data from AndyHayler.com"),
#   
#   mainPanel(
#     
#     img(src="andy-hayler-logo.png"),
#     img(src="Andy_Hayler_photo.png",width='200px'),
#     
#     h4(tags$div(
#       sprintf("Andy Hayler is an independent restaurant critic who has reviewed around %s restaurants
#       around the world.", fmt(Round_up(n_reviews))),
#       tags$br(),
#       tags$br(),
#       "In five different years he ate in all three-Michelin star restaurants in the world.
#       This is an exploration of the basic data from his reviews, including subjective rating, price,
#       Michelin star status, location and cuisine type.",
#       tags$br(),
#       tags$br(), 
#       "To view the raw data and the reviews, go to ",
#       tags$a(href="https://www.andyhayler.com/restaurant-guide", 
#              "AndyHayler.com > Restaurant guide."), 
#       tags$br(), 
#       tags$br(), 
#       "First there are plots summarising all the reviews by rating, price, value, country and cusine, 
#       which is broken down by Michelin star.", 
#       tags$br(),
#       tags$br(),
#       "Then there are plots which go into analyes, for example how price is related to Hayler's rating, 
#       again relating to Michelin star.", 
#       tags$br(),
#       tags$br(),
#       "Finally there is a section on interesting individial restaurants, for example best and worst value restaurants."
#       
#     )),
#     
#     tags$br(),
# 
#     h5(tags$div(
#       "A note on the ratings:", 
#       tags$br(), 
#       tags$br(),
#       "These are the subjective ratings of Andy Hayler on his most recent
#       visit to each restaurant.",
#       tags$br(),
#       tags$br(),
#       "He scores them on a scale from 1 to 20. Previously, he rated restaurants from 1 to 10,
#       however this hasn't seemed to increased the range of scores because there are very few 
#       reviews with less than 10/20. Therefore ten was subtracted from all scores and any
#       scores less than zero are rounded up to just zero. 
#       So the scores here have been adjusted back to a just a simple 1 to 10 scale.",
#       tags$br(),
#       tags$br(),
#       "The range of restaurants is quite varied. There are reviews of
#       pizzerias and fish & chips alongside, say, a three-Michelin star restaurant in the centre
#       of Paris. But the criteria for review is generally that it's an independent place
#       with some interesting reason to visit such as a prior recommendation or review. 
#       However, since Hayler is from London, there are many more reviews of places in England.",
#       tags$br(),
#       tags$br(),
#       "It's quite rare that he marks a zero, and a score of more than 2 or 3 means the food is actually pretty good
#       because the score is relative to the extreme and rare values of 10/10 (see 'Number of reviews by rating' plot below).",
#       tags$br(),
#       tags$br(),
#       "Hayler pays for the restaurants himself so there should be little bias in the subjective score ('rating' from hereon).")),
#     tags$br(),
#     h5(tags$div(
#       "A note on prices:", 
#       tags$br(),
#       tags$br(),
#       "The prices here are what he calls an 'average price', 
#       which is described as 'typical price for three courses and modest wine', 
#       however it's unclear how this is calculated exactly.",
#       tags$br(),
#       tags$br(),
#       "There can also be a big difference between this 'average price' and what Hayler himself pays,
#       especially at the top restaurants.
#       For example at Troisgros (10/10, 3 stars), 'average price' is 200, but the price he paid was 519, 
#       and at Les Pres Eugenie (also 10/10, 3 stars) 'average price' is 180 whereas Hayler paid ~600 (!).
#       Generally he either pays the average or more than the average. 
#       Details of what he pays are on each restaurant's review page."
#     )), 
#     
#     tags$br(),
#     tags$br(), 
#     
#     h2("Summary of data"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by star"),
#     plotOutput("Star_plot"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by rating"),
#     plotOutput("rating_plot"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by rating, split by star"),
#     plotOutput("Rate_star_hist"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by price"),
#     plotOutput("price_plot"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by price, split by star"),
#     plotOutput("price_star_hist"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by value"),
#     plotOutput("Value_plot"),
#     
#     tags$br(),
#     
#     h3("Number of reviews by value, split by star"),
#     plotOutput("Val_star_hist"),
#     
#     tags$br(),
#     
#     h3("rating by country (and number of reviews)"),
#     plotOutput("rating_country"),
#     
#     
#     h4(sprintf("Comments: The UK scores about %s (median), while other European countries such as France, Germany, 
#     Swizerland and the Netherlands score top. However, since Hayler lives in London there are far more UK reviews
#     which include a lot more casual / cheaper places 
#     (see Michelin stars by country plot below - the review overwhelmingly cover 0-star places in the UK). 
#                Also, the choice of overseas restaurants is probably much more selective.", 
#                Meds_country["United Kingdom"])),
#     
#     h3("Number of stars by country"),
#     plotOutput("stars_country_plot"),
#     
#     h3("rating by cuisine (and number of reviews)"),
#     plotOutput("rating_cuisine"), 
#     
#     # output$rating_country <- renderPlot({rating_country})
#     # output$rating_cuisine <- renderPlot({rating_cuisine})
#     
#     tags$br(),
#     
#     h2("Analyses"),
#     
#     tags$br(),
#     
#     h3("price vs rating, all"),
#     plotOutput("price_rate"),
#     h3("price vs rating, split out by star status"),
#     plotOutput("price_rate_split"),
#     h5(tags$div("The circles in the first plot highlight the star rating groups (0, 1, 2 or 3 Michelin stars),
#                 centering on the averages",
#                 tags$br(),
#                 tags$br(),
#                 "Comments: Here there seems to be an interesting 'diminishing returns' effect overall (top plot)
#                 between rating, price and Michelin star level: Hayler's rating flattens out as the price 
#                 and star status increase, suggesting price or star status only predict quality to a certain level.",
#                 tags$br(),
#                 tags$br(),
#                 "This is particularly the case with three Michelin star status - the
#                 subjective rating flattens out between two and three stars, while the
#                 price continues to increase.",
#                 tags$br(),
#                 tags$br(),
#                 "Indeed it looks as though from about £250 onwards, quality varies considerably,
#                 ranging from about 4 to maximum of 10.", 
#                 tags$br(),
#                 tags$br(),
#                 "However it should be noted that there is more uncertainty a the two and three-star levels
#                 (as indicated by the widening confidence intervals - grey shading) 
#                 since they have the fewest reviews."
#     )),
# 
#     h3("price vs value, all"),
#     plotOutput("price_val"),
#     h3("price vs value, split by star status"),
#     plotOutput("price_val_split"),
#     h5(tags$div("Value has been shown here on a log scale just to tidy up the data and to show a better line of best fit 
#                 (original value scale 0-100)", 
#                 tags$br(), 
#                 tags$br(), 
#                 "Value quickly decreases the higher the price both overall and among all the star classes. 
#                 The best value places seem to be below about £100 in all three star classes, 
#                 the best of these being in the 0-star category, though it looks like there are many good-value 
#                 1-star places."
#                 )), 
# 
#     h3("rating vs value"),
#     plotOutput("rating_val"),
#     plotOutput("rating_val_split")
#     
#     ) # mainPanel
#   #    )
#     ) # fluidPage
# 
# 
# # Server ----
# 
# server <- function(input, output) {
#   
#   # Basics
#   output$Star_plot <- renderPlot({star_plot})
#   
#   output$rating_plot <- renderPlot({rating_plot})
#   output$Rate_star_hist <- renderPlot({sate_star_hist})
#   
#   output$price_plot <- renderPlot({price_plot})
#   output$price_star_hist <- renderPlot({price_star_hist})
#   
#   output$Value_plot <- renderPlot({salue_plot})
#   output$Val_star_hist <- renderPlot({sal_star_hist})
#   
#   output$rating_country <- renderPlot({rating_country})
#   output$stars_country_plot <- renderPlot({stars_country_plot})
#   output$rating_cuisine <- renderPlot({rating_cuisine})
#   
#   # Analysis
#   output$price_rate <- renderPlot({price_rate})
#   output$price_rate_split <- renderPlot({price_rate_split})
# 
#   output$price_val <- renderPlot({price_val})
#   output$price_val_split <- renderPlot({price_val_split})
# 
#   output$rating_val <- renderPlot({rating_val})
#   output$rating_val_split <- renderPlot({rating_val_split})
# 
#   # output$price_country_hist <- renderPlot({price_country_hist})
# 
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)


# Other ----

# ------------------------------------------------
# Make size of points reflect freq of data points
# ------------------------------------------------

# Freq_data <- as.data.frame(table(tab$price, tab$rating, tab$stars))
# names(Freq_data) <- c("price", "rating", "stars", "Freq")
# Freq_data <- filter(Freq_data, Freq > 0)

# Sub_tab <- tab[-sample(which(tab$stars == 0), 900), ]

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
# ggplot(tab)+
#   geom_point(aes(price, rating, color = factor(stars), size = Size, alpha = alpha))+
#   geom_point(data = subset(tab, factor(stars) == '1'),
#              aes(x = price, y = rating, color = factor(stars), size = Size, alpha = alpha))+
#   theme_classic()+
#   ggtitle("price vs rating by stars")+
#   stat_smooth(data = tab,
#               aes(x = price, y = rating),
#               se = T,  formula = y~poly(x, 2), method = "lm")+ #aes(group = 1)
#   scale_colour_manual(values=cbPalette)+
#   scale_y_continuous(breaks = seq(0, 10, 1))+
#   scale_x_continuous(breaks = seq(0, max(tab$price), 50)) +
#   theme(axis.title = element_text(size=Text_size),
#         axis.text = element_text(size = Text_size)) +
#   labs(colour = "stars")

# ----
# PCA
# ----
# library("FactoMineR")
# library("factoextra")
# 
# AH_pca <- PCA(tab[, 3:5], graph = FALSE)
# 
# fviz_eig(AH_pca, addlabels = TRUE, ylim = c(0, 100))
# 
# fviz_pca_biplot(AH_pca,
#                 col.ind = factor(tab$stars), palette = cbPalette,
#                 addEllipses = TRUE, label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "stars")
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





