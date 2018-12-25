require(ggplot)
require(dplyr)

df <-read.table("test_data/bump_data.txt",stringsAsFactors = FALSE,header = TRUE)
df.rankings <- df %>% 
  group_by(date) %>% 
  arrange(date, desc(gold), desc(silver), desc(bronze), country) %>% 
  mutate(ranking = row_number(),
         day = as.numeric(as.Date(date)) - 17571) %>% 
  as.data.frame()

#basci plot
ggplot(data = df.rankings, aes(x = day, y = ranking, group = country)) +
  geom_line(aes(color = country, alpha = 1), size = 2) +
  geom_point(aes(color = country, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))


my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    #theme(panel.grid.major.y = element_blank()) +
    #theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    theme(panel.grid  = element_blank())+
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
}


show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = country)) +
  geom_line(aes(color = country, alpha = 1), size = 2) +
  geom_point(aes(color = country, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:16, minor_breaks = 1:16, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = country, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "16"),
            aes(label = country, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(x = "Competition days with medals",
       y = "Rank",
       title = "PyeongChang 2018 Olympic Winter Games",
       subtitle = "Countries ranked by overall medals after each competition day") +
  my_theme() 


## top 10 and highlight
show.top.n <- 10
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(country %in% c("NOR","GER","CAN","USA","NED"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, country, "zzz"))

ggplot(data = df.rankings, aes(x = day, y = ranking, group = country)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:16, minor_breaks = 1:16, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = country, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "16"),
            aes(label = country, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(x = "Competition days with medals",y = "Rank",
       title = "PyeongChang 2018 Olympic Winter Games",
       subtitle = "Countries ranked by overall medals after each competition day") +
  my_theme() +
  scale_color_manual(values = c("#F70020","#191A1A","#FB9701","#1A7D00","#072C8F","grey"))





#### mytest
require(ggplot2)
require(dplyr)

df <-read.table("test_data/test.txt",stringsAsFactors = FALSE,header = TRUE,sep="\t")    
df.rankings <- df %>% 
  group_by(Method) %>% 
  arrange(Method, desc(Rank), Modification) %>% 
  mutate(ranking = row_number(),
         day = ifelse(Method=="Linear regression",1,2)) %>% 
  as.data.frame()

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Modification)) +
  geom_line(aes(color = Modification, alpha = 1), size = 2) +
  geom_point(aes(color = Modification, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(df.rankings))

## add theme
show.top.n <- 10

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Modification)) +
  geom_line(aes(color = Modification, alpha = 1), size = 2) +
  geom_point(aes(color = Modification, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = 1:2, minor_breaks = 1:2, expand = c(.05, .05)) +
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Modification, x = 0.5) , hjust = .85, fontface = "bold", color = "#888888", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "2"),
            aes(label = Modification, x = 2.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(x = "Models",
       y = "Rank") +
  my_theme() 


###
show.top.n <- 10
df.rankings <- df.rankings %>%
  mutate(flag = ifelse(Modification %in% c("H3K27me3","H3K9me2","H3K27ac"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Modification, "zzz"))

ggplot(data = df.rankings, aes(x = day, y = ranking, group = Modification)) +
  geom_line(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks =0.9:2.1, minor_breaks = 0.9:2.1, expand = c(.1, .1),labels = c("Linear regression","Random forest")) + # expand 
  geom_text(data = df.rankings %>% filter(day == "1"),
            aes(label = Modification, x = 0.9) , hjust = .85, fontface = "bold", color = "black", size = 4) +
  geom_text(data = df.rankings %>% filter(day == "2"),
            aes(label = Modification, x = 2.1) , hjust = 0.15, fontface = "bold", color = "black", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(x = "Models",y = "Rank") +
  my_theme() +
  scale_color_manual(values = c("orange", "steelblue","darkgreen","grey"))


