dataoneway <- read.table("onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

#Assumption 1: All samples are independent9                                                                                           
#Label groups and set as categorical factors

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("Wall lizard", "Viviparous lizard", "Snake-eyed lizard"))

#Assumption 2: Normal distributions of each group, no major outliers

Group1 <- subset(dataoneway, Group == "Wall lizard")
Group2 <- subset(dataoneway, Group == "Viviparous lizard")
Group3 <- subset(dataoneway, Group == "Snake-eyed lizard")

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

#Assumption 3: Homogeneneity of variances
bartlett.test(Length ~ Group, data = dataoneway)




#One Way ANOVA - Test if the means of the k populations are equal

model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)

#Post-hoc test TukeyHSD - Test which of the groups have different means
TukeyHSD(aov(model1))







