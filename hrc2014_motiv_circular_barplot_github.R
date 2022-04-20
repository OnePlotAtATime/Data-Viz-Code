library(dplyr)
library(tidyr)
library(ggplot2)
library(MetBrewer)
library(showtext)

data <- read.csv("hrc_motivations_2014.csv", stringsAsFactors=T)

data <- data %>%
  filter(status=="current")%>%
  arrange(desc(percentage))

data$id <- seq(1, nrow(data)) #useful blog: https://r-graph-gallery.com/296-add-labels-to-circular-barplot.html
number_of_bar <- nrow(data)
angle <- 90 - 360 * (data$id-0.5) /number_of_bar
angle2 <- 180-360*(data$id)/number_of_bar
data$hjust <- ifelse(angle < -90, 1.1, -0.25)
data$vjust <- ifelse(abs(angle2) < 100, 1, -0.25)

font_add("Century Regular", "century.ttf")
showtext_auto()

ggplot(data, aes(x=as.factor(id), y=percentage, fill = reorder(motivation, -percentage))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", width = 0.8) +
  scale_fill_met_d("Signac")+
  scale_x_discrete(expand = expansion(mult = 0.06))+
  ylim(-25,75) +
  labs(title = "MOTIVATIONS FOR EATING A VEGETARIAN/VEGAN DIET",
       subtitle = "Most vegetarians/vegans report health, animal protection, and negative feelings about 
meat/animal products as reasons for their diets. Many also mention concern for the 
environment and taste preference as factors. Motivations such as social justice, religion, 
cost, social influence, and trendiness are less common.",
       caption = "Data source: Faunalytics 2014 | Created by: Anastasia Stuart-Edwards (@OnePlotAtATime)",
       fill = "Motivations")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, hjust = 0, color = "#82C236", face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust=0, color = "grey50"),
    legend.position="right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(c(t = 20, r = 1, b = 10, l = 1)), 
    text = element_text("Century Regular")
  ) +
  coord_polar(start = 0) + 
  geom_text(data=data, 
            aes(label = paste0(percentage,"%"), hjust = hjust, vjust = vjust), alpha = 0.9)
