# Luciana Franci
# Multinomial logistic model

# Some variables have already been removed to avoid collinearity. 
# If two variabels had r > 0.7 only the most biologically meaningful one was kept

# We want to find out what kinds of variables are needed to produce the most parsimoneous model
# The different types of variables could be 
#  a) the one including nutrients: P + K + Ca
#  b) soil variables (affecting water conditions) -- including river distance + clay + silt + org.matter + fine.sand + 
#       available.water + river.dist + soil.depth 
#  c) biotic, dynamic environment (light): canopy openness
#  d) biotic, tree community (number of trees, tree biomass, canopy height): tree.biomass + n.trees.plot + max.canopy.h

mdata <- read.csv("Complete_data_Mansoa_150219.csv", header = T, sep = ";")

#Mergin forest data and Mansoa data
treedata <- read.csv("tree_community.csv", header = T, sep = ";")
alldata <- merge(mdata, treedata, by = "plot")


#Package to run multinomial logistic regression
require(nnet)


# 2012-2013
up <- subset(alldata, stages12 == "upstanding")
ly <- subset(alldata, stages12 == "lying") # equal to searcher in the ms
c1 <- subset(alldata, stages12 == "climber1") # equal to small climber in the ms
c2 <- subset(alldata, stages12 == "climber2") # equal to large climber in the ms

#Removing levels that don't exist
up$fate12.13 <- factor(up$fate12.13)
levels(up$fate12.13)

ly$fate12.13 <- factor(ly$fate12.13)
levels(ly$fate12.13)

c1$fate12.13 <- factor(c1$fate12.13)
levels(c1$fate12.13)

c2$fate12.13 <- factor(c2$fate12.13)
levels(c2$fate12.13)

#############################################################################################################
# Upstanding
# Choosing the level to used as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
up$fate12.13.2 <- relevel(up$fate12.13, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
up.multi1 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 2
up.multi2 <- multinom(fate12.13.2 ~ P + K + Ca, data = up)
#Model 3
up.multi3 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                      data = up)
#Model 4
up.multi4 <- multinom(fate12.13.2 ~ canopy.12, data = up)
#Model 5
up.multi5 <- multinom(fate12.13.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 6
up.multi6 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12, data = up)
#Model 7
up.multi7 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 8
up.multi8 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = up)
#Model 9
up.multi9 <- multinom(fate12.13.2 ~ P + K + Ca + canopy.12, data = up)
#Model 10
up.multi10 <- multinom(fate12.13.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 11
up.multi11 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 12
up.multi12 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12, data = up)
#Model 13
up.multi13 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = up)
#Model 14
up.multi14 <- multinom(fate12.13.2 ~ canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = up)

#Selecting the model
AIC(up.multi1, up.multi2, up.multi3, up.multi4, up.multi5, up.multi6, up.multi7, up.multi8, up.multi9, up.multi10,
    up.multi11, up.multi12, up.multi13, up.multi14)
#Best model: up.multi10
summary(up.multi10)

# p-values (Wald test)
dfUpCoef10 <- data.frame(t(summary(up.multi10)$coefficients))
round(dfUpCoef10, digits = 3)
dfUpSE10 <- data.frame(t(summary(up.multi10)$standard.errors))
round(dfUpSE10, digits = 3)
# Calculating the p-value using wald statistic
dfUpPValues10 <- data.frame(apply((dfUpCoef10/dfUpSE10)^2, 2, 
                                 pchisq, df = 1, lower.tail = FALSE))
round(dfUpPValues10, digits = 3)


#############################################################################################################
# Lying
# Choosing the level to used as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
ly$fate12.13.2 <- relevel(ly$fate12.13, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
ly.multi1 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 2
ly.multi2 <- multinom(fate12.13.2 ~ P + K + Ca, data = ly)
#Model 3
ly.multi3 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                      data = ly)
#Model 4
ly.multi4 <- multinom(fate12.13.2 ~ canopy.12, data = ly)
#Model 5
ly.multi5 <- multinom(fate12.13.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 6
ly.multi6 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12, data = ly)
#Model 7
ly.multi7 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 8
ly.multi8 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = ly)
#Model 9
ly.multi9 <- multinom(fate12.13.2 ~ P + K + Ca + canopy.12, data = ly)
#Model 10
ly.multi10 <- multinom(fate12.13.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 11
ly.multi11 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 12
ly.multi12 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12, data = ly)
#Model 13
ly.multi13 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = ly)
#Model 14
ly.multi14 <- multinom(fate12.13.2 ~ canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = ly)

#Selecting the model
AIC(ly.multi1, ly.multi2, ly.multi3, ly.multi4, ly.multi5, ly.multi6, ly.multi7, ly.multi8, ly.multi9, ly.multi10,
    ly.multi11, ly.multi12, ly.multi13, ly.multi14)
#Best model: ly.multi2
summary(ly.multi2)

# p-values (Wald test)
dflyCoef2 <- data.frame(t(summary(ly.multi2)$coefficients))
round(dflyCoef2, digits = 3)
dflySE2 <- data.frame(t(summary(ly.multi2)$standard.errors))
round(dflySE2, digits = 3)
# Calculating the p-value using wald statistic
dflyPValues2 <- data.frame(apply((dflyCoef2/dflySE2)^2, 2, 
                                  pchisq, df = 1, lower.tail = FALSE))
round(dflyPValues2, digits = 3)


#############################################################################################################
# Climber 1
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
c1$fate12.13.2 <- relevel(c1$fate12.13, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
c1.multi1 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = c1)

#Model 2
c1.multi2 <- multinom(fate12.13.2 ~ P + K + Ca, data = c1)
#Model 3
c1.multi3 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                      data = c1)
#Model 4
c1.multi4 <- multinom(fate12.13.2 ~ canopy.12, data = c1)
#Model 5
c1.multi5 <- multinom(fate12.13.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = c1)
#Model 6
c1.multi6 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12, data = c1)
#Model 7
c1.multi7 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = c1)
#Model 8
c1.multi8 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = c1)
#Model 9
c1.multi9 <- multinom(fate12.13.2 ~ P + K + Ca + canopy.12, data = c1)
#Model 10
c1.multi10 <- multinom(fate12.13.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = c1)
#Model 11
c1.multi11 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c1)
#Model 12
c1.multi12 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12, data = c1)
#Model 13
c1.multi13 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c1)
#Model 14
c1.multi14 <- multinom(fate12.13.2 ~ canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c1)

#Selecting the model
AIC(c1.multi1, c1.multi2, c1.multi3, c1.multi4, c1.multi5, c1.multi6, c1.multi7, c1.multi8, c1.multi9, c1.multi10,
    c1.multi11, c1.multi12, c1.multi13, c1.multi14)
#Best model: c1.multi14
summary(c1.multi14)

# p-values (Wald test)
dfc1Coef14 <- data.frame(t(summary(c1.multi14)$coefficients))
round(dfc1Coef14, digits = 3)
dfc1SE14 <- data.frame(t(summary(c1.multi14)$standard.errors))
round(dfc1SE14, digits = 3)
# Calculating the p-value using wald statistic
dfc1PValues14 <- data.frame(apply((dfc1Coef14/dfc1SE14)^2, 2, 
                                  pchisq, df = 1, lower.tail = FALSE))
round(dfc1PValues14, digits = 3)


#############################################################################################################
# Climber 2
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
c2$fate12.13.2 <- relevel(c2$fate12.13, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
c2.multi1 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12 + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = c2)

#Model 2
c2.multi2 <- multinom(fate12.13.2 ~ P + K + Ca, data = c2)
#Model 3
c2.multi3 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                      data = c2)
#Model 4
c2.multi4 <- multinom(fate12.13.2 ~ canopy.12, data = c2)
#Model 5
c2.multi5 <- multinom(fate12.13.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = c2)
#Model 6
c2.multi6 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        canopy.12, data = c2)
#Model 7
c2.multi7 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                        tree.biomass + n.trees.plot + max.canopy.h, data = c2)
#Model 8
c2.multi8 <- multinom(fate12.13.2 ~ P + K + Ca + 
                        clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = c2)
#Model 9
c2.multi9 <- multinom(fate12.13.2 ~ P + K + Ca + canopy.12, data = c2)
#Model 10
c2.multi10 <- multinom(fate12.13.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = c2)
#Model 11
c2.multi11 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c2)
#Model 12
c2.multi12 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.12, data = c2)
#Model 13
c2.multi13 <- multinom(fate12.13.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c2)
#Model 14
c2.multi14 <- multinom(fate12.13.2 ~ canopy.12 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = c2)

#Selecting the model
AIC(c2.multi1, c2.multi2, c2.multi3, c2.multi4, c2.multi5, c2.multi6, c2.multi7, c2.multi8, c2.multi9, c2.multi10,
    c2.multi11, c2.multi12, c2.multi13, c2.multi14)
#Best model: c2.multi5
summary(c2.multi5)

# p-values (Wald test)
dfc2Coef5 <- data.frame(t(summary(c2.multi5)$coefficients))
round(dfc2Coef5, digits = 3)
dfc2SE5 <- data.frame(t(summary(c2.multi5)$standard.errors))
round(dfc2SE5, digits = 3)
# Calculating the p-value using wald statistic
dfc2PValues5 <- data.frame(apply((dfc2Coef5/dfc2SE5)^2, 2, 
                                  pchisq, df = 1, lower.tail = FALSE))
round(dfc2PValues5, digits = 3)


###########################################################################################################
# 2013-2014
up1 <- subset(alldata, stages13 == "upstanding")
ly1 <- subset(alldata, stages13 == "lying")
c1.1 <- subset(alldata, stages13 == "climber1")
c2.1 <- subset(alldata, stages13 == "climber2")

#Removing levels that don't exist

up1$fate13.14 <- factor(up1$fate13.14)
levels(up1$fate13.14)

ly1$fate13.14 <- factor(ly1$fate13.14)
levels(ly1$fate13.14)

c1.1$fate13.14 <- factor(c1.1$fate13.14)
levels(c1.1$fate13.14)

c2.1$fate13.14 <- factor(c2.1$fate13.14)
levels(c2.1$fate13.14)

#############################################################################################################
# Upstanding
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
up1$fate13.14.2 <- relevel(up1$fate13.14, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
up1.multi1 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.13 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 2
up1.multi2 <- multinom(fate13.14.2 ~ P + K + Ca, data = up1)
#Model 3
up1.multi3 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                       data = up1)
#Model 4
up1.multi4 <- multinom(fate13.14.2 ~ canopy.13, data = up1)
#Model 5
up1.multi5 <- multinom(fate13.14.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 6
up1.multi6 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.13, data = up1)
#Model 7
up1.multi7 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 8
up1.multi8 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = up1)
#Model 9
up1.multi9 <- multinom(fate13.14.2 ~ P + K + Ca + canopy.13, data = up1)
#Model 10
up1.multi10 <- multinom(fate13.14.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 11
up1.multi11 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 12
up1.multi12 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13, data = up1)
#Model 13
up1.multi13 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = up1)
#Model 14
up1.multi14 <- multinom(fate13.14.2 ~ canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = up1)

#Selecting the model
AIC(up1.multi1, up1.multi2, up1.multi3, up1.multi4, up1.multi5, up1.multi6, up1.multi7, up1.multi8, up1.multi9, up1.multi10,
    up1.multi11, up1.multi12, up1.multi13, up1.multi14)
#Best model: up1.multi5
summary(up1.multi5)

# p-values (Wald test)
dfup1Coef5 <- data.frame(t(summary(up1.multi5)$coefficients))
round(dfup1Coef5, digits = 3)
dfup1SE5 <- data.frame(t(summary(up1.multi5)$standard.errors))
round(dfup1SE5, digits = 3)
# Calculating the p-value using wald statistic
dfup1PValues5 <- data.frame(apply((dfup1Coef5/dfup1SE5)^2, 2, 
                                   pchisq, df = 1, lower.tail = FALSE))
round(dfup1PValues5, digits = 3)

#############################################################################################################
# Lying
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
ly1$fate13.14.2 <- relevel(ly1$fate13.14, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
ly1.multi1 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.13 + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 2
ly1.multi2 <- multinom(fate13.14.2 ~ P + K + Ca, data = ly1)
#Model 3
ly1.multi3 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                       data = ly1)
#Model 4
ly1.multi4 <- multinom(fate13.14.2 ~ canopy.13, data = ly1)
#Model 5
ly1.multi5 <- multinom(fate13.14.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 6
ly1.multi6 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         canopy.13, data = ly1)
#Model 7
ly1.multi7 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                         tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 8
ly1.multi8 <- multinom(fate13.14.2 ~ P + K + Ca + 
                         clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = ly1)
#Model 9
ly1.multi9 <- multinom(fate13.14.2 ~ P + K + Ca + canopy.13, data = ly1)
#Model 10
ly1.multi10 <- multinom(fate13.14.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 11
ly1.multi11 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 12
ly1.multi12 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13, data = ly1)
#Model 13
ly1.multi13 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = ly1)
#Model 14
ly1.multi14 <- multinom(fate13.14.2 ~ canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = ly1)

#Selecting the model
AIC(ly1.multi1, ly1.multi2, ly1.multi3, ly1.multi4, ly1.multi5, ly1.multi6, ly1.multi7, ly1.multi8, ly1.multi9, ly1.multi10,
    ly1.multi11, ly1.multi12, ly1.multi13, ly1.multi14)
#Best model: ly1.multi3
summary(ly1.multi3)

# p-values (Wald test)
dfly1Coef3 <- data.frame(t(summary(ly1.multi3)$coefficients))
round(dfly1Coef3, digits = 3)
dfly1SE3 <- data.frame(t(summary(ly1.multi3)$standard.errors))
round(dfly1SE3, digits = 3)
# Calculating the p-value using wald statistic
dfly1PValues3 <- data.frame(apply((dfly1Coef3/dfly1SE3)^2, 2, 
                                  pchisq, df = 1, lower.tail = FALSE))
round(dfly1PValues3, digits = 3)

#############################################################################################################
# Climber 1
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
c1.1$fate13.14.2 <- relevel(c1.1$fate13.14, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
c1.1.multi1 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 2
c1.1.multi2 <- multinom(fate13.14.2 ~ P + K + Ca, data = c1.1)
#Model 3
c1.1.multi3 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                        data = c1.1)
#Model 4
c1.1.multi4 <- multinom(fate13.14.2 ~ canopy.13, data = c1.1)
#Model 5
c1.1.multi5 <- multinom(fate13.14.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 6
c1.1.multi6 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13, data = c1.1)
#Model 7
c1.1.multi7 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 8
c1.1.multi8 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = c1.1)
#Model 9
c1.1.multi9 <- multinom(fate13.14.2 ~ P + K + Ca + canopy.13, data = c1.1)
#Model 10
c1.1.multi10 <- multinom(fate13.14.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 11
c1.1.multi11 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           canopy.13 + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 12
c1.1.multi12 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           canopy.13, data = c1.1)
#Model 13
c1.1.multi13 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)
#Model 14
c1.1.multi14 <- multinom(fate13.14.2 ~ canopy.13 + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c1.1)

#Selecting the model
AIC(c1.1.multi1, c1.1.multi2, c1.1.multi3, c1.1.multi4, c1.1.multi5, c1.1.multi6, c1.1.multi7, c1.1.multi8, c1.1.multi9, c1.1.multi10,
    c1.1.multi11, c1.1.multi12, c1.1.multi13, c1.1.multi14)
#Best model: c1.1.multi4
summary(c1.1.multi4)

# p-values (Wald test)
dfc1.1Coef4 <- data.frame(t(summary(c1.1.multi4)$coefficients))
round(dfc1.1Coef4, digits = 3)
dfc1.1SE4 <- data.frame(t(summary(c1.1.multi4)$standard.errors))
round(dfc1.1SE4, digits = 3)
# Calculating the p-value using wald statistic
dfc1.1PValues4 <- data.frame(apply((dfc1.1Coef4/dfc1.1SE4)^2, 2, 
                                    pchisq, df = 1, lower.tail = FALSE))
round(dfc1.1PValues4, digits = 3)


#############################################################################################################
# CLimber 2
# Choosing the level to use as baseline: stasis (NEED TO INFORM THIS IN METHODS!)
c2.1$fate13.14.2 <- relevel(c2.1$fate13.14, ref = "stasis")

# Running the multinomial logistic regression
#Model 1: full model
c2.1.multi1 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13 + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 2
c2.1.multi2 <- multinom(fate13.14.2 ~ P + K + Ca, data = c2.1)
#Model 3
c2.1.multi3 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, 
                        data = c2.1)
#Model 4
c2.1.multi4 <- multinom(fate13.14.2 ~ canopy.13, data = c2.1)
#Model 5
c2.1.multi5 <- multinom(fate13.14.2 ~ tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 6
c2.1.multi6 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          canopy.13, data = c2.1)
#Model 7
c2.1.multi7 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                          tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 8
c2.1.multi8 <- multinom(fate13.14.2 ~ P + K + Ca + 
                          clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth, data = c2.1)
#Model 9
c2.1.multi9 <- multinom(fate13.14.2 ~ P + K + Ca + canopy.13, data = c2.1)
#Model 10
c2.1.multi10 <- multinom(fate13.14.2 ~ P + K + Ca + tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 11
c2.1.multi11 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           canopy.13 + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 12
c2.1.multi12 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           canopy.13, data = c2.1)
#Model 13
c2.1.multi13 <- multinom(fate13.14.2 ~ clay + silt + org.matter + fine.sand + available.water + river.dist + soil.depth + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)
#Model 14
c2.1.multi14 <- multinom(fate13.14.2 ~ canopy.13 + 
                           tree.biomass + n.trees.plot + max.canopy.h, data = c2.1)

#Selecting the model
AIC(c2.1.multi1, c2.1.multi2, c2.1.multi3, c2.1.multi4, c2.1.multi5, c2.1.multi6, c2.1.multi7, c2.1.multi8, c2.1.multi9, c2.1.multi10,
    c2.1.multi11, c2.1.multi12, c2.1.multi13, c2.1.multi14)
#Best model: c2.1.multi4
summary(c2.1.multi4)

# p-values (Wald test)
dfc2.1Coef4 <- data.frame(t(summary(c2.1.multi4)$coefficients))
round(dfc2.1Coef4, digits = 3)
dfc2.1SE4 <- data.frame(t(summary(c2.1.multi4)$standard.errors))
round(dfc2.1SE4, digits = 3)
# Calculating the p-value using wald statistic
dfc2.1PValues4 <- data.frame(apply((dfc2.1Coef4/dfc2.1SE4)^2, 2, 
                                   pchisq, df = 1, lower.tail = FALSE))
round(dfc2.1PValues4, digits = 3)


##############################################################################################################
#Predictions 2012 - 2013
plots <- read.csv("plots.csv", header = T, sep = ";")

#Upstanding
predup <- as.data.frame(predict(up.multi10, type = "probs"))
predup1 <- cbind(row.name = rownames(predup), predup) 

#Lying
predly <- as.data.frame(predict(ly.multi2, type = "probs"))
predly1 <- cbind(row.name = rownames(predly), predly) 

#Climber 1
predc1 <- as.data.frame(predict(c1.multi14, type = "probs"))
predc1.1 <- cbind(row.name = rownames(predc1), predc1) 

#Climber 2
predc2 <- as.data.frame(predict(c2.multi5, type = "probs"))
predc2.1 <- cbind(row.name = rownames(predc2), predc2) 

###############################
#Data to use to calculate the matrices
merged1 <- merge(plots, predup1, by = "row.name", all = TRUE)
merged2 <- merge(merged1, predly1, by = "row.name", all = TRUE)
merged3 <- merge(merged2, predc1.1, by = "row.name", all = TRUE)
merged4 <- merge(merged3, predc2.1, by = "row.name", all = TRUE)

names(merged4)
colnames(merged4) <- c("row.name", "plot", "stasis.up",  "dead.up",  "up.growth.to.climber1", "up.growth.to.lying",
                       "stasis.ly", "dead.ly", "ly.growth.to.climber1", "ly.shrink.to.upstanding", "stasis.c1",
                       "dead.c1", "c1.growth.to.climber2", "c1.shrink.to.lying", "c1.shrink.to.upstanding",
                       "stasis.c2", "dead.c2", "c2.shrink.to.climber1")
merged.data <- merged4[,-1]
#write.table(merged.data, "merged.data12.13.csv", row.names = FALSE)


##############################################################################################################
#Predictions 2013 - 2014
#plots <- read.csv("plots.csv", header = T, sep = ";")
#Upstanding
predupa <- as.data.frame(predict(up1.multi5, type = "probs"))
predup1a <- cbind(row.name = rownames(predupa), predupa) 

#Lying
predlya <- as.data.frame(predict(ly1.multi3, type = "probs"))
predly1a <- cbind(row.name = rownames(predlya), predlya) 

#Climber 1
predc1a <- as.data.frame(predict(c1.1.multi4, type = "probs"))
predc1.1a <- cbind(row.name = rownames(predc1a), predc1a) 

#CLimber 2
predc2a <- as.data.frame(predict(c2.1.multi4, type = "probs"))
predc2.1a <- cbind(row.name = rownames(predc2a), predc2a) 

###############################
#Data to use to calculate the matrices
merged1a <- merge(plots, predup1a, by = "row.name", all = TRUE)
merged2a <- merge(merged1a, predly1a, by = "row.name", all = TRUE)
merged3a <- merge(merged2a, predc1.1a, by = "row.name", all = TRUE)
merged4a <- merge(merged3a, predc2.1a, by = "row.name", all = TRUE)

names(merged4a)
colnames(merged4a) <- c("row.name", "plot", "stasis.up",  "dead.up", "up.growth.to.lying",
                        "stasis.ly", "dead.ly", "ly.growth.to.climber1", 
                        "stasis.c1","dead.c1", "c1.growth.to.climber2", "c1.shrink.to.lying",
                        "stasis.c2", "dead.c2", "c2.shrink.to.climber1", "c2.shrink.to.lying")
merged.dataa <- merged4a[,-1]
#write.table(merged.dataa, "merged.data13.14.csv", row.names = FALSE)