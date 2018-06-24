# Luciana Franci 13th jan 2015
# Matrices for 4 classes (upstanding, lying, climber1, climber2)

summ <- read.csv("Complete_data_Mansoa_150107.csv", header = T, sep = ";")
summary(summ)
names(summ)
class(summ)

######################################
### Matrix and eigenvalues for 2012-13

# Only data from 2012-2013
summ12.13 <- summ[summ$recruitment13.14=="FALSE",]
#summ12.13 <- summ[c(-(701:710)),] # Excluding the individuals that recruited in 2014

test <- table(summ12.13$stages13, summ12.13$stages12)  # check order -> Ok! [row (t+1), column (t)]
test
the.order <- c("upstanding", "lying", "climber 1", "climber 2")
the.order
the.matrix <- test[the.order, the.order]
the.matrix
tot.n.surv <- apply(the.matrix, MARGIN=2, FUN="sum")
tot.n.surv # here are only the survivors! We need to include the deads

# subtract the plants that died to avoid 100% survival !
mort <- table(summ12.13$mortality12.13, summ12.13$stages12)
mort
the.mort <- mort[,the.order]
the.mort
tot.n <- tot.n.surv + the.mort["TRUE",]
tot.n

# Run through columnns in matrix and in sum vector
the.matrix2 <- the.matrix
the.matrix2[] <- NA
for (i in 1:length(the.matrix[1,])) {
  the.matrix2[,i] <- the.matrix[,i]/tot.n[i]
}

# Checking the number of recruits in which stage
rec <- table(summ12.13$recruitment12.13, summ12.13$stages13)
rec

# Including fertility of adults
the.matrix2[1,4] <- (40/275) #infant.i: 20
the.matrix12.13 <- the.matrix2

save(the.matrix2, file="the.matrix2(12-13).RData")

# Sensitivity, elasticity and lambda (stable stage distribution and repro-ductive values are 
# the corresponding right and left eigenvectors, respectively)
library(popbio)
eig12.13 <- eigen.analysis(the.matrix2)
eig12.13
#?eigen
eigen(the.matrix2) # Comparing to popbio. Ok!

# Bootstrap

###############################
#2012-2013

boot <- list()
matrices <- list()
matrices2 <- list()
matrices3 <- list()
matrices3.1 <- list()
matrices4 <- list()
matrices5 <- list()
matrices6 <- list()
the.matrices <- list()
eigen.results <- list()
climber2 <- list()
infi.rec.true <- list()
climber2.infi.fert <- list()


set.seed(100)
for(i in 1:1000){
  boot[[i]] <- summ[sample(1:nrow(summ), replace = T),]
  matrices[[i]] <- table(boot[[i]]$stages13, boot[[i]]$stages12)
  matrices2[[i]] <- matrices[[i]][the.order, the.order]
  matrices3[[i]] <- apply(matrices[[i]], MARGIN=2, FUN="sum")
  matrices3.1[[i]] <- matrices3[[i]][the.order]
  matrices4[[i]] <- table(boot[[i]]$mortality12.13, boot[[i]]$stages12)
  matrices5[[i]] <- matrices4[[i]][,the.order]
  matrices6[[i]] <- matrices3.1[[i]] + matrices5[[i]]["TRUE",]
  the.matrices[[i]] <- matrices2[[i]]/matrices6[[i]]  
  climber2[[i]] <- table(boot[[1]]$stages12)[2]
  #Fertility: upstanding produced by climber 2
  infi.rec.true[[i]] <- tapply(as.numeric(boot[[i]]$stages13), list(boot[[i]]$recruitment12.13,(boot[[i]]$stages13)),sum, na.rm=T)[2,4] #[2,4] means I'm choosing only the usptanding recruited present in the bootstrap data frame
  climber2.infi.fert[[i]] <- infi.rec.true[[i]]/climber2[[i]]
  the.matrices[[i]][1,4] <- climber2.infi.fert[[i]]
  #Calculating lambda
  eigen.results[[i]] <- eigen(the.matrices[[i]])
  eigen.results[[i]] <- max(Re(eigen(the.matrices[[i]])$values))
}


eigen.results <- unlist(eigen.results)
head(eigen.results)
tail(eigen.results)
length(eigen.results)

# Confidence interval
sort(eigen.results)
shapiro.test(eigen.results) # normal!
qqnorm(eigen.results)
qqline(eigen.results)
quantile(eigen.results, probs=c(0.025,0.975))

#####################################################################################################################
### Matrix and eigenvalues for 2013-14

testa <- table(summ$stages14, summ$stages13)  # check order -> Ok! [row (t+1), column (t)]
testa
the.matrixa <- test[the.order, the.order]
the.matrixa
tot.n.surva <- apply(the.matrixa, MARGIN=2, FUN="sum")
tot.n.surva # here are only the survivors! We need to include the deads

# subtract the plants that died to avoid 100% survival !
morta <- table(summ$mortality13.14, summ$stages13)
morta
the.morta <- morta[,the.order]
the.morta
tot.na <- tot.n.surva+the.morta["TRUE",]
tot.na

# Run through columnns in matrix and in sum vector
the.matrix2a <- the.matrixa
the.matrix2a[] <- NA
for (i in 1:length(the.matrixa[1,])) {
  the.matrix2a[,i] <- the.matrixa[,i]/tot.na[i]
}
the.matrix2a

# Checking the number of recruits in which stage
rec1 <- table(summ$recruitment13.14, summ$stages14)
rec1
# Including fertility of climber 2
the.matrix2a[1,4] <- (7/282) #upstanding: 7
the.matrix13.14 <- the.matrix2a

save(the.matrix2a, file="the.matrix2a(13-14).RData")

# Sensitivity, elasticity and lambda (stable stage distribution and repro-ductive values are 
# the corresponding right and left eigenvectors, respectively)
library(popbio)
eig13.14 <- eigen.analysis(the.matrix2a)
eig13.14
eigen(the.matrix2a) # Comparing to popbio. Ok!


# Bootstrap

###############################
#2013-2014

boota <- list()
matricesa <- list()
matrices2a <- list()
matrices3a <- list()
matrices3.1a <- list()
matrices4a <- list()
matrices5a <- list()
matrices6a <- list()
the.matricesa <- list()
eigen.resultsa <- list()
climber2a <- list()
infi.rec.truea <- list()
climber2.infi.ferta <- list()

set.seed(1000)
for(i in 1:1000){
  boota[[i]] <- summ[sample(1:nrow(summ), replace = T),]
  matricesa[[i]] <- table(boota[[i]]$stages14, boota[[i]]$stages13)
  matrices2a[[i]] <- matricesa[[i]][the.order, the.order]
  matrices3a[[i]] <- apply(matricesa[[i]], MARGIN=2, FUN="sum")
  matrices3.1a[[i]] <- matrices3a[[i]][the.order]
  matrices4a[[i]] <- table(boota[[i]]$mortality13.14, boota[[i]]$stages13)
  matrices5a[[i]] <- matrices4a[[i]][,the.order]
  matrices6a[[i]] <- matrices3.1a[[i]] + matrices5a[[i]]["TRUE",]
  the.matricesa[[i]] <- matrices2a[[i]]/matrices6a[[i]] 
  climber2a[[i]] <- table(boota[[1]]$stages13)[1]  
  #Fertility: infant.i produced by climber2
  infi.rec.truea[[i]] <- tapply(as.numeric(boota[[i]]$stages14), list(boota[[i]]$recruitment13.14,(boota[[i]]$stages14)), sum, na.rm=T)[2,4]
  infi.rec.truea[[i]][is.na(infi.rec.truea[[i]])] <-0
  climber2.infi.ferta[[i]] <- infi.rec.truea[[i]]/climber2a[[i]]
  the.matricesa[[i]][1,4] <- the.matricesa[[i]][1,4] + climber2.infi.ferta[[i]]
  #Calculating lambda
  eigen.resultsa[[i]] <- eigen(the.matricesa[[i]])
  eigen.resultsa[[i]] <- max(Re(eigen(the.matricesa[[i]])$values))
}

eigen.resultsa <- unlist(eigen.resultsa)
head(eigen.resultsa)
tail(eigen.resultsa)
length(eigen.resultsa)

# Confidence interval
sort(eigen.resultsa)
shapiro.test(eigen.resultsa) # not normal! What should I do?
qqnorm(eigen.resultsa)
qqline(eigen.resultsa)
quantile(eigen.resultsa, probs=c(0.025,0.975))

#LTRE
library(popbio)
round(LTRE(the.matrix2a, the.matrix2), digits = 5)
m.ltre <- LTRE(the.matrix2a, the.matrix2)
y <- sapply(m.ltre, sum)
par(mai = c(1.2,1.2,.7,0.1), family = "serif")
png("LTRE_between_periods.tiff", width=10, height=7, units="in", res=300)
par(mai = c(1.2,1.2,.7,0.1), family = "serif")
bploty <- barplot(y, ylab = "Transition effect", xlab = "Trasition", main = "LTRE between 2012-13 and 2013-14", 
                  ann=FALSE,axes=FALSE, ylim = c(-0.012, 0.004), col = "gray65")
axis(2,at=seq(-0.012,0.004, 0.002))
axis(1, at = bploty, labels = c(expression('S'[1]), expression('G'[1][2]), expression('G'[3][1]), expression('G'[4][1]), 
                                expression('R'[1][2]), expression('S'[2]), expression('G'[3][2]), expression('G'[4][2]),
                                expression('R'[1][3]), expression('R'[2][3]), expression('S'[3]), expression('G'[4][3]),
                                expression('F'[1][4]), expression('R'[2][4]), expression('R'[3][4]), expression('S'[4])))
abline(h=0)
box()
dev.off()
