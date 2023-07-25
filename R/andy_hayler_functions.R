# Andy Hayler app functions


# Deps ----

library(XML)
library(plyr)
library(dplyr)
library(ggplot2)

# Misc ----

# Ignore
heaD <- function(x, ...){
  head(x, ...)
}
# Round up to nearest 10
round_up <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}
# Remove random n of rows
remove_rows <- function(data, col, value, n){
  data[-(sample(as.numeric(row.names(subset(data, col == value) )), n)), ]
}
# Add comma to numbers
fmt <- function(x, ...){format(x, big.mark=",",scientific=FALSE, ...)}

max_hist <- function(ggplot_hist){
  max(ggplot_build(ggplot_hist)$data[[1]]$count)
}

# Parse data ----

# Parse data from site and convert to table
# Not sure exactly how this code works, but returns correct table
# Uses XML package
parse_data <- function(URL){
  lines <- readLines(URL)
  
  lines <- gsub("<span></span>", 1, lines)
  
  cbind(readHTMLTable(lines, header=T, which=2,stringsAsFactors=F),
        readHTMLTable(lines, header=T, which=1,stringsAsFactors=F))
}

# Clean data ----

clean_headers <- function(tab){
  names(tab) <- c("Name of restaurant", "cuisine", "review", "price", "value", "rating", "stars")
  tab
}

# Price
clean_price <- function(tab){
  # Price
  tab$price <- as.numeric(gsub(".([0-9]+).*$", "\\1", tab$price))
  tab$price <- round_up(tab$price) # round up 
  tab
}

# Rating
# Subtract 10 from rating because Hayler changed scores from /10 to /20 for 
# some reason (after Brillat Savarin?), but hardly any score are below 10, 
# so effectively 10 is 0 and 20 is 10.
clean_rating <- function(tab){
  tab$rating <- as.numeric(substr(tab$rating, 1, 2)) - 10
  tab 
}

# Stars
clean_stars <- function(tab){
  x <- tab$stars
  x <- gsub(" ", "", x)
  x <- gsub("\n", "", x)
  x <- strsplit(x, "")
  tab$stars <- sapply(x, function(x){sum(as.numeric(x))})
  tab[is.na(tab["stars"]), "stars"] <- 0
  tab
}

# Value
# Calculate as rating / price
clean_value <- function(tab){
  tab$value <- tab$rating / tab$price
  tab$value <- signif((tab$value / max(tab$value[!is.na(tab$value)]) ) * 100, 2)
  tab
}

# Location
# Parse name of restaurant, city and country 
clean_location <- function(tab){
  x <- tab$`Name of restaurant`
  x <- strsplit(x, ",")
  
  tab$name <- unlist(sapply(x, function(x){x[1]}))
  tab$city <- unlist(sapply(x, function(x){x[2]}))
  tab$country <- unlist(sapply(x, function(x){x[3]}))
  
  # Remove col
  tab$`Name of restaurant` <- NULL
  # Remove dead reviews
  tab <- tab[complete.cases(tab), ]
  tab
}

# Cuisine
# Group together cuisines
clean_cuisine <- function(tab){
  tab$cuisine <- ifelse(tab$cuisine == "Fish & Chips", "British", tab$cuisine)
  european <- c("Austrian", "Czech", "Danish", "Greek", "Hungarian", "Norwegian", "Russian", 
                "Portuguese", "Swedish")
  tab$cuisine <- ifelse(tab$cuisine %in% european, "European", tab$cuisine)
  asian <- c("Burmese", "Indonesian", "Korean", "Malaysian", "Singaporean", "Vietnamese")
  tab$cuisine <- ifelse(tab$cuisine %in% asian, "Asian", tab$cuisine)
  middle_eastern <- c("Lebanese", "Persian", "Tunisian", "Turkish")
  tab$cuisine <- ifelse(tab$cuisine %in% middle_eastern, "Middle Eastern", tab$cuisine)
  tab$cuisine <- ifelse(tab$cuisine == "Peruvian", "South American", tab$cuisine)
  tab$cuisine <- ifelse(tab$cuisine == "South African", "African", tab$cuisine)
  tab
}

# Country
clean_country <- function(tab){
  uk <- c(" Channel Islands", " Jersey", " Man", " United Kingdom")
  tab$country <- ifelse(tab$country %in% uk, "United Kingdom", tab$country)
  usa <- c(" Colorado", " Colorado ", " Florida ", " Hawaii ", " Texas ", " United States")
  tab$country <- ifelse(tab$country %in% usa, "United States", tab$country)
  france <- c(" Loire", " France")
  tab$country <- ifelse(tab$country %in% france, "France", tab$country)
  # Remove trailing spaces
  tab$country <- trimws(tab$country)
  tab
}


# Run all clean functions
clean_data <- function(tab){
  tab %>% clean_headers() %>%
    clean_price() %>%
    clean_rating() %>%
    clean_stars() %>% 
    clean_value() %>%
    clean_location() %>%
    clean_cuisine() %>%
    clean_country()
}

# Summary plots ----

# Barplot - n reviews by star
plot_stars <- function(tab, alpha = 0.75){
  ggplot(tab, aes(stars))+
  geom_bar(alpha=alpha, colour = "black", fill = "red")+
  xlab("Michelin stars")+ylab("Number of reviews")+
  scale_y_continuous(breaks = seq(0, nrow(tab), 100), limits = c(0, nrow(tab)+10))+
  # stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1, hjust=1)+
    stat_count(aes(y=after_stat(count),
                   label=after_stat(count)),
               geom="text",vjust=-1, hjust=1)+
  # stat_count(aes(y=..count..,label = paste0("(", scales::percent(..prop..), ")")),
    stat_count(aes(y=after_stat(count),
                   label = paste0("(", scales::percent(after_stat(prop)), ")")),
             geom="text", vjust=-1, hjust=-0.25)+
  theme_bw()
}


# Rating

plot_rating <- function(tab){
  ratings <- sort(unique(tab$rating))
  med_rate <- median(tab$rating)
  ggplot(tab, aes(rating))+
    geom_bar(alpha=alpha, colour = "black", fill = "blue")+
    xlab("Rating")+ylab("Number of reviews")+
    scale_y_continuous(breaks = seq(0, nrow(tab), 100), limits = c(0, nrow(tab)+10))+
    scale_x_continuous(breaks = ratings)+
    stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1, hjust=1.25)+
    stat_count(aes(y=..count..,label = paste0("(", scales::percent(round(..prop.., 1)), ")")),
               geom="text", vjust=-1, hjust=-0.05)+
    annotate(x = med_rate, y = nrow(tab) - 50, geom = "text", size = text_sz, 
             label = paste("Median rating =", med_rate))+
    geom_vline(xintercept = med_rate, linetype = "dotted")+
    theme_bw()
}

plot_rating_by_star <- function(tab){
  med_rate_star <- aggregate(tab$rating, list(tab$Stars), median)
  names(med_rate_star) <- c("Stars", "Med_rating")
  med_rate_star$med_rating <- round(med_rate_star$med_rating, 2)
  rate_star_max <- round_up(max(table(tab$rating, tab$stars)), 100)
  rate_star_max_pc_inc <- rate_star_max*0.1
  
  ggplot()+
    geom_bar(data = tab, aes(x = rating, fill=factor(stars)), alpha = alpha, colour = "black")+
    scale_x_continuous(breaks = ratings, labels = ratings)+
    scale_y_continuous(breaks = seq(0, rate_star_max, 50), 
                       limits = c(0, rate_star_max+rate_star_max_pc_inc))+
    geom_segment(data = med_rate_star, 
                 aes(x = Med_rating , y = 0, xend = Med_rating, yend = rate_star_max-rate_star_max_pc_inc), 
                 linetype = "dotted")+
    geom_text(data = med_rate_star, 
              mapping = aes(x = Med_rating, y = Inf,
                            label = sprintf("Median rating = %s", Med_rating)), vjust = 2)+
    facet_wrap( ~ factor(Stars) )+
    labs(fill = "Stars")+
    theme_bw()
}

# Price

plot_price <- function(tab, med_price, n_price = 50){
  ggplot(tab, aes(price))+
    geom_histogram(binwidth = n_price, alpha=alpha, colour = "black", fill = "green",  boundary = 0)+
    xlab("price")+ylab("Number of reviews")+
    scale_y_continuous(breaks = seq(0, nrow(tab), 100), limits = c(0, nrow(tab)+10))+
    scale_x_continuous(breaks = seq(0, Max_price, n_price), 
                       labels = seq(0, Max_price, n_price))+
    annotate(x = med_price, y = nrow(tab) - 50, geom = "text", size = text_sz, 
             label = sprintf("Median price = £%s", med_price))+
    geom_vline(xintercept = med_price, linetype="dotted")+
    theme_bw()
}

plot_price_star <- function(tab, med_price, n_price = 50){
  max_price <- round_up(max(tab$price), to = 100)
  med_price_star <- aggregate(tab$price, list(tab$stars), median)
  names(med_price_star) <- c("stars", "med_price")
  med_price_star$med_price <- round(med_price_star$med_price, 2)
  price_star_max <- round_up(max(table(tab$price, tab$stars)), 100)
  price_star_max_pc_inc <- price_star_max*0.1
  
  ggplot()+
    geom_histogram(data = tab, aes(x = price, fill=factor(stars)), 
                   bins = n_price, alpha = alpha, colour = "black", boundary = 0)+
    scale_x_continuous(breaks = seq(0, max_price, n_price), 
                       labels = seq(0, max_price, n_price))+
    scale_y_continuous(breaks = seq(0, price_star_max, 50), 
                       limits = c(0, price_star_max+price_star_max_pc_inc))+
    geom_segment(data = med_price_star, 
                 aes(x = med_price, 
                     y = 0, 
                     xend = med_price, 
                     yend = price_star_max-price_star_max_pc_inc), 
                 linetype = "dotted")+
    geom_text(data = med_price_star, 
              mapping = aes(x = med_price, 
                            y = Inf, 
                            label = paste0("Median price = £", med_price)), 
              vjust = 2)+
    facet_wrap( ~ factor(stars) )+
    labs(fill = "stars")+
    theme_bw()
}

# Value

plot_value <- function(tab, n_val = 10){
  med_value <- round(median(tab$value), 0)
  max_val <- max(tab$value)
  x_scale <- seq(0, max_val, 10)
  
  ggplot(tab, aes(Value))+
    geom_histogram(bins = n_val+1, alpha=alpha, colour = "black", fill = "orange",  boundary = 0)+
    xlab("Value")+ylab("Number of reviews")+
    scale_y_continuous(breaks = seq(0, nrow(tab), 100), limits = c(0, nrow(tab)+10))+
    scale_x_continuous(breaks = x_scale, labels = x_scale)+
    annotate(x = med_value, y = nrow(tab) - 50, geom = "text", size = 6, 
             label = sprintf("Median value = %s", med_value))+
    # geom_vline(xintercept = med_value, linetype="dotted")+
    geom_segment(aes(x = med_value, y = 0, xend = med_value, yend = nrow(tab)-nrow(tab)*0.1),
                 linetype = "dotted")+
    theme_bw()
}

plot_val_star <- function(tab, n_val = 10){
  max_val <- max(tab$value)
  med_val_star <- aggregate(tab$value, list(tab$stars), median)
  names(med_val_star) <- c("stars", "med_value")
  med_val_star$med_value <- round(med_val_star$med_value, 2)
  
  p <- ggplot()+
    geom_histogram(data = tab, aes(x = Value, fill=factor(stars)), 
                   bins = n_val+1, alpha = alpha, colour = "black",
                   boundary = 0, closed = "left")+
    scale_x_continuous(breaks = seq(0, max_val, n_val), 
                       labels = seq(0, max_val, n_val))+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
    geom_text(data = med_val_star, 
              mapping = aes(x = med_value, y = Inf, label = sprintf("Median value = %s", med_value)), vjust = 2)+
    facet_wrap( ~ factor(stars) )+
    labs(fill = "stars")+
    theme_bw()
  yend <- max_hist(p)
  p + geom_segment(data = med_val_star,
                   aes(x = med_value, y = 0, xend = med_value, yend = yend + yend*0.1),
                   linetype = "dotted")
}

# Country

plot_rating_country <- function(tab){
  n_country <- data.frame(table(tab$country))
  meds_country <- c(by(tab$rating, tab$country, median, na.rm = T))
  
  ggplot(tab, aes(country, rating))+
    geom_boxplot(fill = "yellow", alpha = alpha)+
    geom_text(data=data.frame(),
              aes(x=names(meds_country), 
                  y=meds_country+0.5, 
                  label=n_country$Freq), 
              size=text_sz-2)+
    # ggtitle("rating by country (with # of reviews)")+
    scale_y_continuous(breaks = 0:10)+
    geom_hline(yintercept = median(tab$rating, na.rm = T),
               linetype = "dashed", colour = "red" )+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_stars_country <- function(tab){
  stars_country <- data.frame(table(tab$country, tab$stars))
  names(stars_country) <- c("country", "stars", "Number")
  
  ggplot()+
    geom_bar(data = stars_country, aes(x = country, y = Number, fill = stars), 
             position="dodge", stat="identity")+
    # scale_y_continuous(breaks = seq(0, Round_up(max(stars_country$Number), 100), 50))+
    scale_y_continuous(breaks = seq(0, Round_up(max(stars_country$Number), 100), 50), 
                       limits = c(0, Round_up(max(stars_country$Number), 100)))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Cuisine

plot_rating_cuisine <- function(tab){
  n_cuis <- data.frame(table(tab$cuisine))[,2]
  meds <- c(by(tab$rating, tab$cuisine, median, na.rm = T))
  ggplot(tab, aes(cuisine, rating))+
    geom_boxplot(fill = "cyan", alpha = alpha)+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_text(data=data.frame(),
              aes(x=names(meds), y=meds+0.5, label=n_cuis), size=4)+
    # ggtitle("rating by cuisine type (with # of reviews)")+
    scale_y_continuous(breaks = 0:10)+
    geom_hline(yintercept = median(tab$rating, na.rm = T),
               linetype = "dashed", colour = "red" )
}

# Analysis plots ----

# Price-rating

plot_price_rating <- function(tab, cbPalette){
  price_rate <- ggplot(tab, aes(price, rating, colour = factor(stars)))
  price_rate <- price_rate + geom_point(size = Size, alpha = alpha, 
                                        position = position_jitter(w = 5, h = 0))
  # price_rate <- price_rate + ggtitle("price vs rating, grouped by star status")
  price_rate <- price_rate + stat_smooth(se = T,  
                                         formula = y~poly(x, 2),
                                         method = "lm", aes(group = 1), 
                                         show.legend = F)
  price_rate <- price_rate + scale_colour_manual(values=cbPalette)
  price_rate <- price_rate + scale_y_continuous(breaks = seq(0, 10, 1))
  price_rate <- price_rate + scale_x_continuous(breaks = seq(0, max(tab$price), 50))
  price_rate <- price_rate + theme(axis.title = element_text(size=Text_size),
                                   axis.text = element_text(size = Text_size)) 
  price_rate <- price_rate + labs(colour = "stars")
  price_rate <- price_rate + stat_ellipse(size = Circle_size, type = "t", show.legend = F,
                                          alpha = alpha)
  price_rate + theme_bw()
}

plot_price_rate_star <- function(tab, cbPalette){
  ggplot(tab, aes(price, rating, colour = factor(stars)))+
    geom_point(size = Size, alpha = alpha)+
    facet_wrap( ~ factor(stars) )+
    # ggtitle("price vs rating by stars, split out")+
    stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+ # aes(group = 1)
    scale_colour_manual(values=cbPalette)+
    scale_y_continuous(breaks = seq(0, 10, 1))+
    scale_x_continuous(breaks = seq(0, max(tab$price), 50)) +
    theme_bw()+
    theme(axis.title = element_text(size=Text_size),
          axis.text = element_text(size = Text_size), 
          legend.position = "none") 
}

plot_price_value <- function(tab){
  log_val_max <- Round_up(log(max(tab$Value+1, na.rm = T)), 1)
  ggplot(tab, aes(price, log(Value+1), colour = factor(stars)))+
    geom_point(size = Size, alpha = alpha, position = position_jitter(w = 5, h = 0))+
    # ggtitle("price vs log(Value) by stars")+
    stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
    scale_colour_manual(values=cbPalette)+
    scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(0, log_val_max))+
    scale_x_continuous(breaks = seq(0, max(tab$price), 50)) +
    theme(axis.title = element_text(size=Text_size),
          axis.text = element_text(size = Text_size)) +
    labs(colour = "stars")+
    theme_bw()
}


plot_price_value_star <- function(tab){
  ggplot(tab, aes(price, log(Value+1), colour = factor(stars)))+
    geom_point(size = size, alpha = alpha)+
    facet_wrap( ~ factor(stars) )+
    stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
    scale_colour_manual(values=cbPalette)+
    scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(0, log_val_max))+
    scale_x_continuous(breaks = seq(0, max(tab$price), 50)) +
    theme_bw()+
    theme(axis.title = element_text(size=Text_size),
          axis.text = element_text(size = Text_size), 
          legend.position = "none") 
}


plot_rating_value <- function(tab){
  ggplot(tab, aes(rating, log(value+1), colour = factor(stars)))+
    geom_point(size = size, alpha = alpha, position = position_jitter(w = 0.1, h = 0))+
    stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm", aes(group = 1))+
    scale_colour_manual(values=cbPalette)+
    scale_y_continuous(breaks = seq(0, log_val_max, 1), limits = c(-0.5, log_val_max))+
    scale_x_continuous(breaks = seq(0, max(tab$rating), 1), 
                       limits = c(-0.5, max(tab$rating)+0.5))+
    theme(axis.title = element_text(size=Text_size),
          axis.text = element_text(size = Text_size)) +
    labs(colour = "stars")+
    theme_bw()
}


plot_rating_value_star <- function(tab){
  ggplot(tab, aes(rating, log(value+1), colour = factor(stars)))+
    geom_point(size = size, alpha = alpha)+
    facet_wrap( ~ factor(stars) )+
    # ggtitle("rating vs log(Value) by stars")+
    stat_smooth(se = T,  formula = y~poly(x, 2), method = "lm")+
    scale_colour_manual(values=cbPalette)+
    scale_y_continuous(breaks = seq(0, log(max(tab$Value+1, na.rm = T)), 1))+
    scale_x_continuous(breaks = seq(0, max(tab$rating, na.rm = T), 1)) +
    theme_bw()+
    theme(axis.title = element_text(size=Text_size),
          axis.text = element_text(size = Text_size), 
          legend.position = "none") 
}
