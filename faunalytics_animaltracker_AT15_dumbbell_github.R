# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(RColorBrewer)
library(Cairo)
library(ggtext)
library(stringr)
library(showtext)

#CLEAN CODE
#Load data from Faunalytics Animal Tracker Survey: https://faunalytics.org/datasets/

#Prepare data for plotting
data <- data_raw %>%
  filter(ATWAVESCOMBINED==9)%>% #select the most recent year
  dplyr::select(PET_ALL, AT15_1, AT15_2, AT15_3, AT15_4, AT15_5, AT15_6, AT15_7, AT15_8) %>% #select only variables of interest
  mutate(AT15_1 = recode(AT15_1, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_2 = recode(AT15_2, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_3 = recode(AT15_3, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_4 = recode(AT15_4, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_5 = recode(AT15_5, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_6 = recode(AT15_6, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_7 = recode(AT15_7, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA"),
         AT15_8 = recode(AT15_8, "1" = "1", "2" = "0", "3" = "NA", "4" = "NA", "999" = "NA")) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>% #everything as numeric
  filter(across(starts_with('AT15_'), function(x) x < 2))%>% #filter out NAs
  pivot_longer(cols = starts_with("AT15_"))%>% 
  group_by(name, PET_ALL)%>%
  summarize(mean = mean(value, na.rm = T))  %>%
  arrange(mean) %>%
  mutate(name = recode(name,
                       "AT15_1" = "Some animals are capable of thinking and feeling emotions", 
                       "AT15_2" = "Buying clothes made of real animal fur is ethically acceptable", 
                       "AT15_3" = "Dissecting animals is a vital way for students to learn about anatomy", 
                       "AT15_4" = "Farm animals deserve the same consideration as pets and other animals", 
                       "AT15_5" = "People have an obligation to avoid harming all animals", 
                       "AT15_6" = "Protecting endangered or threatened species should be a global priority", 
                       "AT15_7" = "Research on animals is necessary for medical advancement", 
                       "AT15_8" = "Using animals for food is necessary for human survival" )) %>% 
  select(name, PET_ALL, mean)%>% #the following 6 lines calculate the difference between scores to arrange them in descending order
  group_by(name)%>%
  pivot_wider(names_from = PET_ALL, values_from = mean)%>%
  ungroup()%>%
  plyr::rename(c("1" = "pets", "2" = "nopets"))%>%
  mutate(diff = (pets - nopets),
         pos = ifelse(diff > 0, "TRUE", "FALSE")) %>%
  arrange(desc(diff))%>%
  pivot_longer(cols = c(nopets, pets), names_to = "pets") #pivot longer again for plotting

#labels are created separately for (1)big differences, (2) small differences - no ptes, and (3) small differences - pets to align them independently on the plot 
smalldifference_nopets <- data %>%
  group_by(name)%>%
  filter(pets == "nopets")%>%
  filter(name == "Farm animals deserve the same consideration as pets and other animals" |
           name == "Using animals for food is necessary for human survival") %>%
  arrange(desc(diff)) #creates data for labels for those with

smalldifference_pets <- data %>%
  group_by(name)%>%
  filter(pets == "pets")%>%
  filter(name == "Farm animals deserve the same consideration as pets and other animals" |
           name == "Using animals for food is necessary for human survival") %>%
  arrange(desc(diff))

bigdifference <- data %>%
  group_by(name)%>%
  filter(name != "Farm animals deserve the same consideration as pets and other animals" &
           name != "Using animals for food is necessary for human survival") %>%
  arrange(desc(diff))

#Load fonts
font_add_google(name = "Quicksand", family = "Quicksand", regular.wt = 400, bold.wt = 700)
showtext_auto()

#Make the plot
ggplot(data, aes(x = value, y = reorder(name, diff))) +
  geom_line(aes(group = name)) +
  geom_point(aes(color = pets), size = 5) + 
  theme_minimal() + 
  labs(title = "OPINIONS ON ANIMAL WELFARE",
       subtitle = "Differences Between People Living With Pets and Those Living Without Pets",
       caption = "Data Source: Faunalytics Animal Tracker Survey (2016)") +
  xlab ("Mean Score \n(0 = Disagree, 1 = Agree)") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_markdown(size = 14),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 14, vjust = -2),
        plot.title = element_markdown(size = 18, hjust = 0.5),
        plot.caption = element_text(hjust = -30, size = 12, color = "#666666"),
        plot.subtitle = element_text(size = 14, vjust = -1, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 12),
        plot.background = element_rect(color = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family = "Quicksand"))+
  scale_color_brewer(palette = "Dark2", direction = -1,
                     labels = c("No pets in the household", "Some pets in the household"))+
  geom_text(data = smalldifference_nopets, aes(label = round(value, 2)), vjust = 2, hjust = 1.2)+
  geom_text(data = smalldifference_pets, aes(label = round(value, 2)), vjust = 2, hjust = -0.2)+
  geom_text(data = bigdifference, aes(label = round(value, 2)), vjust = 2)
