my_data <- read.csv(file.choose())
View(my_data)
attach(my_data)
# One way Anova Model without interaction

model1 <- lm(Time~ Drink, data=my_data)
anova(model1)


#  two way Anova ModelModel with interaction
model2 <- lm(Time~Drink*Method)
summary(model2)
anova(model2)
# Model with aditve effect
model3 <- lm(Time~Drink + Method)
summary(model2)
anova(model3)

## Interaction Plot

with(my_data,interaction.plot(Drink, Method, Time,col = c("#00AFBB", "#E7B800"),
                              main="Interaction Plot",xlab="Drinks", ylab="Time"))


# Color box plot by a second group: "supp"
library("ggpubr")
ggboxplot(my_data, x = "Drink", y = "Time", color = "Method")

# Stacked bar plots as x = Drink, y = Time by method

ggplot(my_data, aes(x = Drink, y = Time)) +
  geom_bar(aes(color = Method, fill = Method),
           stat = "identity", position = position_stack()) 

########dot chart

ggdotchart(my_data, x = "Drink", y ="Time",
           color = "Method", palette = "jco", size = 3, 
           add = "segment", 
           add.params = list(color = "lightgray", size = 1.5),
           position = position_dodge(0.3),
           ggtheme = theme_pubclean())



# Perorm pairwise comparisons
compare_means(len ~ dose,  data = ToothGrowth)


# Visualize: Specify the comparisons you want
compare_means(Time ~ Drink,  data = my_data)

my_comparisons <- list( c("Coffee", "Milk"), c("Milk", "Tea"), c("Coffee", "Tea") )
ggboxplot(my_data, x = "Drink", y = "Time",
          color = "Drink", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 50)
# Use only p.format as label. Remove method name.
ggplot(my_data, aes(Method, Time)) +
  geom_boxplot(aes(color = Method))+
  facet_wrap(~Drink) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  stat_compare_means(label = "p.format")


# Box plot facetted by "dose"
p <- ggpaired(my_data, x = "Method", y = "Time",
              color = "Method", palette = "jco", 
              line.color = "gray", line.size = 0.4,
              facet.by = "Drink", short.panel.labs = FALSE)
p

# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format", paired = TRUE)
