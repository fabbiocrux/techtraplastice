library(readxl)
library(tidyverse)
library(ggplot2)


TT.col <-  
  tibble(
  "Naranja" = "#F69223",     
  "Celeste" = "#3299CC",     
  "Verde" = "#98C601",   
  "Gris" = "#5E6F75",     
  "Gris_c" = "#F1F1F1"  
)


  

# Data
#datos <- read_excel("delivrables/figures/1.3/Data-USACH.xlsx", sheet = "total")
#datos <- read_excel("Data-USACH.xlsx", sheet = "total")
datos <- read_excel("figures/1.3/Data-USACH.xlsx", sheet = "total")
#View(datos)

datos <- 
  datos %>% 
    mutate(
      Size = factor(Size, levels = c("Large", "Medium", "Small", "Micro"))
    )


# === 2) MAPEO DE FACTORES ===
factor_map <- tibble(
  Variable = c("A.E","I.N","E.S","R.P","R.S",
               "I.E.C","H.C","M.I","S.M","C.K","LEA",
               "I.P","E.P.P","S.T","D.M","T.A","I.P.M","CIRC","C.I","CREA",
               "MARK","F.M","I.P2","C.I3","R.D","T.S","S.P.S.P"),
  Factor = c(rep("External Factors",5),
             rep("Team",6),
             rep("Innovation Process",9),
             rep("Organization",7))
)

#View(factor_map)


data <- 
  datos  %>%
  pivot_longer(
    cols = all_of(factor_map$Variable),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  left_join(factor_map, by = "Variable")
#View(data)



# Grafico: Proporcio de empresas por Tamana y 
by.size <- datos %>%
  group_by(Country,Size) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = round((Count / sum(Count) * 100), 2)
  )



#View(by.size)

# Grafico: Large


by.size %>% 
  group_by(Country, Size) %>%
  summarise(total = sum(Count))  %>% 
  ggplot() +  
  aes(x=Size, y=total, fill=Country) +
  geom_bar(stat = "identity") + 
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c(TT.col$Naranja, TT.col$Verde, TT.col$Celeste)) +
  #scale_fill_manual(values = c( "TT.col[1]","TT.col[2]","TT.col[3]" ) ) +
  coord_flip() + 
  labs(
  title = "Participation of Enterprise at Argentina, Colombia and Chile",
  subtitle = paste("Total of Enterprises: ", sum(by.size$Count) ),
  x = "Type of Enterprises consulted",
  y = "# of Enterprises",
  fill = "Countries"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
    legend.box = "vertical"
  )
  
  
ggsave("Participation.png", width = 10, height = 6)


tamano <- "Large"
rm(tamano)

#plot_por_tamano("Micro", pos=4.5) 
plot_por_tamano <- function(tamano, pos=2, ...) {
  
#  datos_filtrados <- datos_long %>% filter(Size == tamano)
  
# Cmparison of the Global among enterprises
data %>% 
  filter(Size == tamano) %>% 
  group_by(Country, Variable) %>% 
  summarise(Average = mean(Value)) %>% 
  left_join(factor_map, by = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = factor_map$Variable)) %>% 
  ggplot() + 
  #geom_line(data = Climatelabs_bg,  aes(x = name, y = value , group = Perfil), color = "grey75",size = .7) +
  #geom_line( aes(x = name, y = value , group = Profile), color = "#0b53c1",size = .7) +
  #geom_point(aes(x = name, y = value), shape = 21, fill = "white", color = "#0b53c1", stroke = 1.3,  size = 2.8) +
  geom_line( aes(x = Variable, y = Average , color = Country, group = Country), size = .7) +
  geom_point(aes(x = Variable, y = Average , color = Country, group = Country), shape = 21, 
             fill = "white", stroke = 1.3,  size = 2.8) +
  #facet_grid( rows = vars(Country) , scales ="free_y")  +
  coord_flip(ylim = c(1,5)) + 
  #facet_wrap(vars(Country) ~ vars(Profile)) + 
  #facet_grid( cols = vars(Profile) , scales ="free_y")  +
  theme_minimal() +
  ggplot2::annotate("rect", xmin=c(1,6,12,21), xmax=c(5,11,20,27), 
                    ymin=rep(1,4), ymax=rep(5, 4), 
                    alpha = .1 , fill = c("green", "orange", "purple","blue")) +
  ggplot2::annotate("text",
                    x = c(3, 8, 16, 25),
                    y = rep(pos, 4),
                    label = c("External Factors","Team", "Innovation Process", "Organization"),
                    family = "Palatino", fontface = 3, size=4) +
  #scale_y_continuous(breaks=c(1,2,3,4,5), labels=c("(Novice) 1", "2","(Competent) 3", "4", "(Expert) 5")) +
  scale_color_manual(values=c('#307e12','#2a5fc0', '#f47834', 'black')) +
  theme(
    plot.background = element_rect(fill = "#f9fbfc"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1)
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #axis.text.y = element_blank(),
    #axis.text =  element_text(color = "black"),
    #plot.title = element_markdown(face = "bold", hjust = 0.5, size = rel(1.8), margin = margin(t=10, b = 15)),
    #plot.subtitle = element_markdown(size = rel(0.9)),
    #plot.caption = element_markdown(family = "Source Sans Pro",size = rel(.8), margin = margin(t = 10)),
    #strip.text = element_text( face = "bold", size = rel(1.1)),
    #plot.margin = margin(t= 15, r = 10,b = 20, l = 10)
  ) +
  labs(
    title = paste("Sustainable Innovation Index among the", tamano, "Enterprises"),
    subtitle = paste("# Enterprises = ", datos %>% filter(Size == tamano) %>% tally ()),
    x = "", 
    y = "Average Value")


ggsave(paste0("Comparison-",tamano,".png"), width = 6, height = 10)

}









pais <- "Colombia"
plot_por_pais("Chile", pos=4.5)
plot_por_pais <- function(pais, pos=2, ...) {
  
  #  datos_filtrados <- datos_long %>% filter(Size == tamano)
  
  # Cmparison of the Global among enterprises
  data %>% 
    filter(Country == pais) %>% 
    group_by(Size, Variable) %>% 
    summarise(Average = mean(Value)) %>% 
    left_join(factor_map, by = "Variable") %>% 
    mutate(Variable = factor(Variable, levels = factor_map$Variable)) %>% 
    ggplot() + 
    #geom_line(data = Climatelabs_bg,  aes(x = name, y = value , group = Perfil), color = "grey75",size = .7) +
    #geom_line( aes(x = name, y = value , group = Profile), color = "#0b53c1",size = .7) +
    #geom_point(aes(x = name, y = value), shape = 21, fill = "white", color = "#0b53c1", stroke = 1.3,  size = 2.8) +
    geom_line( aes(x = Variable, y = Average , color = Size, group = Size), size = .7) +
    geom_point(aes(x = Variable, y = Average , color = Size, group = Size), shape = 21, 
               fill = "white", stroke = 1.3,  size = 2.8) +
    #facet_grid( rows = vars(Country) , scales ="free_y")  +
    #coord_flip(ylim = c(1,5)) + 
    #facet_wrap(vars(Country) ~ vars(Profile)) + 
    #facet_grid( cols = vars(Profile) , scales ="free_y")  +
    theme_minimal() +
    ggplot2::annotate("rect", xmin=c(1,6,12,21), xmax=c(5,11,20,27), 
                      ymin=rep(1,4), ymax=rep(5, 4), 
                      alpha = .1 , fill = c("green", "orange", "purple","blue")) +
    ggplot2::annotate("text",
                      x = c(3, 8, 16, 25),
                      y = rep(pos, 4),
                      label = c("External Factors","Team", "Innovation Process", "Organization"),
                      family = "Palatino", fontface = 3, size=4) +
    #scale_y_continuous(breaks=c(1,2,3,4,5), labels=c("(Novice) 1", "2","(Competent) 3", "4", "(Expert) 5")) +
    scale_color_manual(values=c('#307e12','#2a5fc0', '#f47834', 'black')) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "#f9fbfc"),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      #axis.text.y = element_blank(),
      #axis.text =  element_text(color = "black"),
      #plot.title = element_markdown(face = "bold", hjust = 0.5, size = rel(1.8), margin = margin(t=10, b = 15)),
      #plot.subtitle = element_markdown(size = rel(0.9)),
      #plot.caption = element_markdown(family = "Source Sans Pro",size = rel(.8), margin = margin(t = 10)),
      #strip.text = element_text( face = "bold", size = rel(1.1)),
      #plot.margin = margin(t= 15, r = 10,b = 20, l = 10)
    ) +
    labs(
      title = paste("Comparison of Sustainable Innovation Index In", pais),
      subtitle = paste("# Enterprises = ", datos %>% filter(Country == pais) %>% tally ()),
      x = "", 
      y = "Average Value")
  
  
  ggsave(paste0("Comparison-",pais,".png"), width = 10, height = 6)
  
}





# Create dataset


# Create dataset
data2 <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)


data %>% 
  filter(Country == "Colombia") %>% 
  group_by(Size, Variable) %>% 
  summarise(Average = mean(Value)) %>% 
  left_join(factor_map, by = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = factor_map$Variable)) %>% 
  ggplot() + 
  
  

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p



