#3a
horse.df <- read.table(file="horse-colic.data", stringsAsFactors=FALSE)
dim(horse.df)

nms = c("Surgery", "Age", "Hosp_ID", "Temp_Rect", "Pulse", "Resp", "Temp_Extr",
        "Pulse_Peri", "Mucous", "Cap_Time", "Pain", "Peris", "Ab_Dist", "Tube",
        "Reflux", "Reflux_PH", "Feces", "Abdomen", "Cell_Vol", "Total_Protein",
        "Ab_Appear", "Ab_Total_Prot", "Outcome", "Lesion", "Lesion_Site",
        "Lesion_Type", "Lesion_Subt", "Lesion_Code")
colnames(horse.df) <- nms
head(horse.df, 5)

#3b
sum(sapply(horse.df, class) == "integer")
sum(sapply(horse.df, class) == "character")


colnames(horse.df)[grepl("?", horse.df)]

missing.values <- sum(horse.df == "?")
percentage <- (missing.values/8400)*100
percentage


#3c
horse.df[horse.df == "?"] <- NA

for(i in (sapply(horse.df, class) == "integer")){
  horse.df[i] <- lapply(horse.df, as.numeric)
}

class(horse.df)
head(horse.df, 5)

#3d
horse.df <- horse.df[order(horse.df$Surgery),]
horse.df <- horse.df[order(horse.df$Pulse, decreasing = TRUE),]

horse.df[(horse.df$Pulse) >= 130, c('Pulse', 'Surgery', 'Temp_Rect', 'Ab_Dist')]

#3e
pain.level <- tapply(horse.df$Pulse, horse.df$Pain, mean, na.rm=TRUE)
pain.level[order(pain.level, decreasing=TRUE)]




#4a
rio = read.csv("rio.csv")

sport1 <- rio$sport
sport <- sort(sport)

sports <- data.frame(sport)
sports <- unique(sports)

n_participants <- table(rio$sport)
n_gold <- aggregate(rio$gold, list(rio$sport), sum)

sports['n_participants'] <- n_participants
sports['n_gold'] <- n_gold$x

sports['ratio'] <- sports$n_gold/sports$n_participants

sports[which.max(sports$ratio),]
sports[which.min(sports$ratio),]

sports <- sports[,c(1,2,3)]
head(sports)

#4b
ave_weight <- tapply(rio$weight, list(rio$sport), mean, na.rm= TRUE)
head(ave_weight)

sports['ave_weight'] <- ave_weight

sort.weight <- sort(sports$ave_weight, decreasing=TRUE)
head(sort.weight)

sports4b <- sports[-6,]
sports4b$ave_weight <- sort.weight
sports4b

#4c
women_subset <- subset(rio, rio$sex != "male")
head(women_subset)

men_subset <- subset(rio, rio$sex != "female")
head(men_subset)

ave_weight_men <- tapply(men_subset$weight, list(men_subset$sport), mean, na.rm=TRUE)
ave_weight_women <- tapply(women_subset$weight, list(women_subset$sport), mean, na.rm=TRUE)

sports['ave_weight_men'] <- ave_weight_men
sports['ave_weight_women'] <- ave_weight_women
head(sports)

#4d
men_bmi_fun = function(x){
    men_bmi_calc <- men_subset$weight / men_subset$height^2
  return(men_bmi_calc)
}

men_bmi <- men_bmi_fun(men_susbet)
men_bmi_final <- tapply(men_bmi, list(men_subset$sport), mean, na.rm = TRUE)
sports['men_bmi'] <- men_bmi_final


women_bmi_fun = function(x){
  women_bmi_calc <- women_subset$weight / women_subset$height^2
  return(women_bmi_calc)
}

women_bmi <- women_bmi_fun(women_susbet)
women_bmi_final <- tapply(women_bmi, list(women_subset$sport), mean, na.rm = TRUE)
sports['women_bmi'] <- women_bmi_final

head(sports)


#1a

#Group A
X1 <- rnorm(1000, mean= 0, sd= 2)
X2 <- rnorm(1000,mean=0, sd=1)
a.mat <- matrix(c(X1, X2), ncol=2, byrow=FALSE)

#Group B
X3 <- rnorm(1000, mean=2.5, sd=1)
X4 <- rnorm(1000, mean=3, sd=2)

b.mat <- matrix(c(X3, X4), ncol=2, byrow=FALSE)

colMeans(a.mat)
colMeans(b.mat)

sd(a.mat[,1])
sd(a.mat[,2])
sd(b.mat[,1])
sd(b.mat[,2])

#1b
xlimit = c(-4,10)
ylimit = c(-8,8)
plot(X2, X1, xlab="X1", ylab="X2", xlim = xlimit, ylim= ylimit)
points(X4, X3, col="red")
legend(5, -4, legend=c("Group A", "Group B"), lty= 1, col=c("black", "red"), cex=0.8)


#1c
dat <- rbind(a.mat, b.mat)

dat.a <- dat[1:1000,]
dat.b <- dat[1001:2000,]

dat.a0 <- cbind(dat.a, c(0))
dat.b1 <- cbind(dat.b, c(1))

dat<- rbind(dat.a0, dat.b1)

colnames(dat) [3] <- "Y"
colnames(dat) [1] <- "X1"
colnames(dat) [2] <- "X2"
dat <- data.frame(dat)
head(dat, 3)
tail(dat, 3)

#1d
log.mod <- glm(Y ~ X1 + X2, data= dat, family ="binomial")
log.mod


#1e
a <- -(log.mod$coefficients[1]/ log.mod$coefficients[3])
b <- -(log.mod$coefficients[2]/log.mod$coefficients[3])
x2 <- (a + b*X1)

plot(dat$X2, dat$X1, xlab="X1", ylab="X2", xlim= xlimit, ylim=ylimit)
points(X4, X3, col="red")
abline(log.mod, lty=2, col="blue", lwd=3)
legend(5, -4, legend=c("Group A", "Group B", "Decision Boundary"), lty= c(1,1,2), 
       col=c("black", "red", "blue"), cex=0.8)

#2a
model.summary = summary(log.mod)
model.summary
model.summary$coefficients

lgsummary <- model.summary$coefficients[,2]
lgsummary


#2b
dat22 = cbind.data.frame(1:2000, 1:2000, 1:2000)
names(dat22) =c('X1', 'X2', 'Y')

for(i in 1:nrow(dat22)){
  replace = sample.int(2000,1)
  dat22[i,] = dat[replace,]
}


log.mod2 <- glm(dat22$Y ~ dat22$X1 + dat22$X2, data= dat22, family ="binomial")
model.summary2 = summary(log.mod2)
lgsummary2 <- model.summary2$coefficients
lgsummary2

log.mod.df <- cbind.data.frame(1:100, 1:100, 1:100)

for(i in 1:100){
 
  dat23 = cbind.data.frame(1:2000, 1:2000, 1:2000)
  names(dat23) =c('X1', 'X2', 'Y')
  
  for(j in 1:nrow(dat22)){
    replace2 = sample.int(2000,1)
    dat23[j,] = dat[replace2,]
  }
  
  log.mod3 <- glm(dat23[, 3] ~ dat23[,1] + dat[,2],data= dat23, family ="binomial")
  log.mod.df[i,] = log.mod3$coefficients 
}

sd.log.mod1 = sd(log.mod.df[,1])
sd.log.mod2 = sd(log.mod.df[,2])
sd.log.mod3 = sd(log.mod.df[,3])

sd.log.mod3
sd.log.mod2
sd.log.mod1




