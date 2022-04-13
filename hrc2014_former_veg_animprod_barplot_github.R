setwd("C:/Users/a.stuartedwards/Dropbox/OnePlotAtATime/Faunalytics")

#Former Veg Animal Product Consumption
library(dplyr)
library(tidyr)
library(ggplot2)
library(MetBrewer)
library(showtext)

data <- read.csv("hrc_animal_produc_consumption_former_veg.csv", stringsAsFactors=T)

data <- data %>%
  pivot_longer(cols = c(X1:X7)) %>%
  rename(frequency = name) %>%
  mutate(frequency = as.factor(recode(frequency,
                            "X1" = "1",
                            "X2" = "2",
                            "X3" = "3",
                            "X4" = "4",
                            "X5" = "5",
                            "X6" = "6",
                            "X7" = "7")),
    frequency2 = factor(frequency),
         Type = factor(Type, levels = c("Dairy", "Eggs", "Chicken", "Beef", "Turkey",
                                        "Fish", "Pork", "Seafood", "Other meats")))

levels(data$frequency) <- c("Never","Less than 1 time per month", "1-3 times per month",
                             "1 time per week", "2-6 times per week","1 time per day",
                             "2 or more times per day")

font_add("Century Regular", "century.ttf")
showtext_auto()

ggplot(data, aes(x = Type, y = value, fill = frequency))+
  geom_bar(position="stack", stat="identity", width = 0.8) +
  scale_fill_met_d("Paquin") +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(title = "HOW OFTEN DO FORMER VEGETARIANS/VEGANS EAT ANIMAL PRODUCTS?",
       subtitle = "Dairy is among most consumed animal products among former vegetarians/vegans while meats such as
duck, lamb, rabbit, deer, and goat, are among least consumed. Former vegetarians/vegans also
do not consume much turkey, pork, fish, and seafood.",
       caption = "Data source: Faunalytics 2014
Created by Anastasia Stuart-Edwards (@OnePlotAtATime)",
       fill = "Frequency")+
  ylab("Percentage")+
   theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, color = "#0B4D19", face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey45"),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50"),
    legend.position = "right",
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(c(20, 20, 20, 20)),
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title = element_blank(),
    text = element_text("Century Regular")
  ) 

ggsave(filename = "former_veg_animprod_barplot.pdf")

