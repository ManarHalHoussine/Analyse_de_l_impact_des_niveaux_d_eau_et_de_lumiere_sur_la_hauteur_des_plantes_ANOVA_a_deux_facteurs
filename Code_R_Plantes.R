# Charger les packages nécessaires
library(ggplot2)
library(car)

# Lire les données à partir du fichier CSV
plant_growth <- read.csv("D:\\plantes\\data_plantes.csv")

# Supprimer les lignes avec des valeurs manquantes
plant_growth <- na.omit(plant_growth)

# Convertir les variables en facteurs
plant_growth$water_level <- as.factor(plant_growth$water_level)
plant_growth$light_level <- as.factor(plant_growth$light_level)

# Afficher les premières lignes des données et un résumé
print(head(plant_growth))
print(summary(plant_growth))

# Boxplot de la hauteur par niveau d'eau et de lumière
ggplot(plant_growth, aes(x=factor(water_level), y=plant_height)) + 
  geom_boxplot(aes(fill=factor(light_level))) + 
  labs(title="Plant Height by Water and Light Levels", x="Water Level", y="Plant Height") +
  theme_minimal()

# Scatter plot de la hauteur par niveau d'eau et de lumière
ggplot(plant_growth, aes(x=plant_height, y=factor(water_level))) + 
  geom_point(aes(color=factor(light_level))) + 
  labs(title="Plant Height by Water and Light Levels", x="Plant Height", y="Water Level") +
  theme_minimal()




#This will give us the correlation coefficient between job satisfaction scores and education level.
cor(plant_growth$plant_height, as.numeric(plant_growth$water_level))
#This will give us the correlation coefficient between job satisfaction scores and gender:
cor(plant_growth$plant_height, as.numeric(plant_growth$light_level))
# Tests de Levene pour l'homogénéité des variances
leveneTest(plant_height ~ water_level * light_level, data = plant_growth)
leveneTest(plant_height ~ water_level, data = plant_growth)
leveneTest(plant_height ~ light_level, data = plant_growth)

# Interaction plot pour visualiser les interactions entre les niveaux d'eau et de lumière
interaction.plot(plant_growth$water_level, plant_growth$light_level, plant_growth$plant_height,
                 xlab = "Water Level", ylab = "Plant Height", 
                 main = "Interaction Plot of Plant Height by Water and Light Levels",
                 col=c("red", "green", "blue"), legend=TRUE)

# ANOVA à un facteur (niveau d'eau)
one.way <- aov(plant_height ~ water_level, data = plant_growth)
summary(one.way)

# ANOVA à deux facteurs (niveau d'eau et niveau de lumière)
two.way <- aov(plant_height ~ water_level + light_level, data = plant_growth)
summary(two.way)

# ANOVA avec interaction entre le niveau d'eau et le niveau de lumière
interaction <- aov(plant_height ~ water_level * light_level, data = plant_growth)
summary(interaction)

estimates <- coef(interaction)
estimates

confint(interaction)

