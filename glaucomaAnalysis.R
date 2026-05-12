Sys.setlocale("LC_CTYPE","en-US.UTF-8")
Sys.setlocale("LC_CTYPE")
GlaucomaData <- read.csv("C:/Users/sarat/Documents/ds_whole.csv")
View(GlaucomaData)
library(ggplot2)

#---------------------------FIRST PART-------------------------------- 

table(GlaucomaData$glaucoma)
table(GlaucomaData$age)

# ---------------------------AGE ANALYSIS OF INDIVIDUALS-----------------------------------------
min_value <- min(GlaucomaData$age)
min_value
max_value <- max(GlaucomaData$age)
max_value 
n = length(GlaucomaData$age)
n 

sort(GlaucomaData$age)
breaks=seq(8,88,by=8)
GlaucomaData$age.int=cut(GlaucomaData$age,breaks,include.lowest = TRUE, include.highest = TRUE, right=FALSE)
Честоти=table(GlaucomaData$age.int)
Рел.честоти=Честоти/n
Кум.честоти=cumsum(Честоти)
R_Cumfreq=cumsum(Честоти)/n
R_Cumfreq2=cumsum(Рел.честоти)
П_фрек=Рел.честоти*100
П_Кум_Фрек=R_Cumfreq*100
age_table=cbind(Честоти,Рел.честоти,Кум.честоти,П_фрек,П_Кум_Фрек)
age_table

breaks=seq(8,88,by=8)
GlaucomaData$age.int=cut(GlaucomaData$age,breaks,right=FALSE)
freq=table(GlaucomaData$age.int)
mid=c()
for(i in 1:length(breaks)-1)
{mid=c(mid,(breaks[i]+breaks[i+1])/2)}
h1=hist(GlaucomaData$age,right = FALSE,breaks,col = "lightblue", main="Age of Individuals", xlab = "Mean Value of Interval" ,ylab = "Frequencies", border="blue")
lines(mid,freq)

P_Cumfreq0=c(0,П_Кум_Фрек)
plot(breaks,P_Cumfreq0, axes = F, main = "Age - polygon of cumulative frequencies in % (ogive)", xlab = "Intervals", ylab = "Cumulative frequencies %")
axis(side=1, at=breaks)
axis(side=2)
lines(breaks,P_Cumfreq0)

age_data=c(GlaucomaData$age)
stem(age_data)

#----------------------------------- AGE AGAINST EYE PRESSURE OF INDIVIDUALS ------------------------------------------

age_data=c(GlaucomaData$age)
ocular_p=c(GlaucomaData$ocular_pressure)
plot(age_data,ocular_p,pch=19,main = "Age of individuals against eye pressure", xlab = "Age (years)", ylab = "Ocular pressure (mmHg)")

freq_table=table(GlaucomaData$age)
mode_values <- names(freq_table)[which.max(freq_table)]
mode_values
median(GlaucomaData$age)
mean(GlaucomaData$age)

age_box_and_whisker=c(GlaucomaData$age)
boxplot(age_box_and_whisker,col="lightblue")
title(main = "Age of individuals - Boxplot",ylab = "Age (years)")
age_квартали <- quantile(GlaucomaData$age)
age_квартали
age_распон <- diff(range(GlaucomaData$age))
age_распон
age_IQR <- IQR(GlaucomaData$age)
age_IQR
age_Q1 <- quantile(GlaucomaData$age, 0.25)
age_Q1
age_Q3 <- quantile(GlaucomaData$age, 0.75)
age_Q3
долна_граница <- age_Q1 - 1.5 * age_IQR
горна_граница <- age_Q3 + 1.5 * age_IQR
age_outliers <- GlaucomaData$age[GlaucomaData$age < долна_граница | GlaucomaData$age > горна_граница]
age_outliers

age_дисперзија <- var(GlaucomaData$age)
age_дисперзија
age_стан_девијација <- sd(GlaucomaData$age)
age_стан_девијација

#-------------------------------------------COEFFICIENT OF CORRELATION--------------------------------------

x <- c(GlaucomaData$age)
y <- c(GlaucomaData$ocular_pressure)
коеф_корелација <- cor(x,y)
коеф_корелација

#------------------------------------------------------------------------------------------------------------------

# ---------------------------OCULAR PRESSURE ANALYSIS OF INDIVIDUALS--------------------------

min_value_очен_прит <- min(GlaucomaData$ocular_pressure)
min_value_очен_прит
max_value_очен_прит <- max(GlaucomaData$ocular_pressure)
max_value_очен_прит 
m = length(GlaucomaData$ocular_pressure)
m

sort(GlaucomaData$ocular_pressure)
breaks_op = seq(8,68,by=6)
GlaucomaData$ocular_pressure.int = cut(GlaucomaData$ocular_pressure,breaks_op,include.lowest = TRUE, include.highest = TRUE, right=FALSE)
Честоти_op = table(GlaucomaData$ocular_pressure.int)
Рел.честоти_op = Честоти_op/n
Кум.честоти_op = cumsum(Честоти_op)
R_Cumfreq_op = cumsum(Честоти_op)/n
R_Cumfreq2_op = cumsum(Рел.честоти_op)
П_фрек_op = Рел.честоти_op*100
П_Кум_Фрек_op = R_Cumfreq_op*100
ocular_pressure_table = cbind(Честоти_op,Рел.честоти_op,Кум.честоти_op,П_фрек_op,П_Кум_Фрек_op)
ocular_pressure_table

breaks_op=seq(8,68,by=6)
GlaucomaData$ocular_pressure.int=cut(GlaucomaData$ocular_pressure,breaks_op,right = FALSE)
freq_op=table(GlaucomaData$ocular_pressure.int)
mid_op=c()
for(i in 1:length(breaks_op)-1)
{mid_op=c(mid_op,(breaks_op[i]+breaks_op[i+1])/2)}
h1_op=hist(GlaucomaData$ocular_pressure,right = FALSE,breaks_op,col = "violet", main="Оcular pressure of Individuals", xlab = "Mean Value of Interval" ,ylab = "Frequencies", border="purple")
lines(mid_op,freq_op)

P_Cumfreq0_op=c(0,П_Кум_Фрек_op)
plot(breaks_op,P_Cumfreq0_op, axes = F, main = "Ocular pressure - polygon of cumulative frequencies in % (ogive)", xlab = "Intervals", ylab = "Cumulative frequencies %")
axis(side=1, at=breaks_op)
axis(side=2)
lines(breaks_op,P_Cumfreq0_op)

ocular_pressure_data=c(GlaucomaData$ocular_pressure)
stem(ocular_pressure_data)

freq_table_op=table(GlaucomaData$ocular_pressure)
mode_values_op <- names(freq_table_op)[which.max(freq_table_op)]
mode_values_op
median(GlaucomaData$ocular_pressure)
mean(GlaucomaData$ocular_pressure)

ocp_box_and_whisker=c(GlaucomaData$ocular_pressure)
boxplot(ocp_box_and_whisker,col="violet")
title(main = "Очен притисок на индивидуи - Boxplot",ylab = "Очен притисок (во mmHg)")
ocp_квартали <- quantile(GlaucomaData$ocular_pressure)
ocp_квартали
ocp_распон <- diff(range(GlaucomaData$ocular_pressure))
ocp_распон
ocp_IQR <- IQR(GlaucomaData$ocular_pressure)
ocp_IQR
ocp_Q1 <- quantile(GlaucomaData$ocular_pressure, 0.25)
ocp_Q1
ocp_Q3 <- quantile(GlaucomaData$ocular_pressure, 0.75)
ocp_Q3
ocp_долна_граница <- ocp_Q1 - 1.5 * ocp_IQR
ocp_горна_граница <- ocp_Q3 + 1.5 * ocp_IQR
ocp_outliers <- GlaucomaData$ocular_pressure[GlaucomaData$ocular_pressure < ocp_долна_граница | GlaucomaData$ocular_pressure > ocp_горна_граница]
ocp_outliers

ocp_дисперзија <- var(GlaucomaData$ocular_pressure)
ocp_дисперзија
ocp_стан_девијација <- sd(GlaucomaData$ocular_pressure)
ocp_стан_девијација

#---------------------------END OF PART ONE-------------------------------- 


#---------------------------PART TWO-------------------------------- 

хист <- hist(GlaucomaData$cornea_thickness, main="Histogram - cornea thickness", xlab = "Cornea thickness (mmHg)", col = "darkolivegreen2", border = "darkgreen")

qqnorm(GlaucomaData$cornea_thickness, main="Normal Q-Q scatter plot - cornea thickness" , xlab="Theoretical quartiles", ylab="Attribute quartiles")
qqline(GlaucomaData$cornea_thickness, col="black")

# WE CONCLUDE NORMAL DISTRIBUTION OF THE CORNEA THICKNESS ATTRIBUTE

податоци <- c(GlaucomaData$cornea_thickness)
дис_примерок_рожница <- var(податоци)
дис_примерок_рожница
обем <- length(податоци)
обем
alpha <- 0.01
степ_слобода <- обем - 1 
степ_слобода
долен_кван_хи_квадрат <- qchisq(1-alpha/2, степ_слобода)
долен_кван_хи_квадрат
горен_кван_хи_квадрат <- qchisq(alpha/2, степ_слобода)
горен_кван_хи_квадрат
долен_хи <- (обем - 1) * дис_примерок_рожница / долен_кван_хи_квадрат
долен_хи
горен_хи <- (обем - 1) * дис_примерок_рожница / горен_кван_хи_квадрат
горен_хи
cat("99% confidence interval за dispersion:", sprintf("[%0.2f, %0.2f]",долен_хи, горен_хи))

стан_дев <- sd(податоци)
стан_дев

#--------------------------------------------------------------

селект_прим <- GlaucomaData$cornea_thickness[1:50]
стан_дев_селект <- sd(селект_прим, na.rm = TRUE)
стан_дев_селект

#IT IS KNOWN THAT THE STANDARD DEVIATION OF THE CORENA THICKNESS OF INDIVIDUALS IS σ=33.59475 ~ 33.6.
#FROM THE FIRST n=50 INDIVIDUALS A STANDARD DEVIATION IS GATHERED - σ=40.76.
#IF α=0.01, CAN WE CONFIRM THAT THERE HAS BEEN AN INCREASE OF THE STANDARD DEVIATION?

#--------------------HYPOTHESES--------------------------
# H0 : σ = 33.6
# Ha : σ > 33.6
#------------------------------------------------------

стан_дев_др_примерок <- 40.76
обем_др_примерок <- 50 
степ_слоб_др_примерок <- обем_др_примерок - 1
степ_слоб_др_примерок
хи_квад_нул <- ( степ_слоб_др_примерок * стан_дев_др_примерок^2 ) / стан_дев^2
cat(sprintf("χ2(o) = %.4f", хи_квад_нул))   
крит_дом_долна_граница<- qchisq(1-alpha, степ_слоб_др_примерок)
cat(sprintf("χ2 with 0.01,49 = %.4f", крит_дом_долна_граница))
cat(sprintf("Critical domain: C = (%.4f, + inf)",крит_дом_долна_граница))
print("χ2(o) does not belong in the critical domain C, we reject the null hypothesis, therefore there has not been an increase of the standard deviation")

#-------PEARSON CHI-SQUARE TEST--------------

# H0 : The attribute has normal ( X~N(μ,σ^2) ) distribution
# Ha : The attribute does not have normal distribution

интервали <- seq(445, 625, length.out=11)
поделби <- table(cut(податоци, breaks=интервали, include.lowest = TRUE, include.highest = TRUE, right = FALSE))
поделби
View(поделби)

рожница.int=cut(податоци,breaks=интервали,include.lowest = TRUE, include.highest = TRUE, right=FALSE)
р_Честоти=table(рожница.int)
рожница_table=cbind(р_Честоти)
рожница_table

просек <- mean(податоци)
просек
очек_веројатности <- pnorm(интервали, mean=просек, sd=стан_дев)
очек_веројатности <- diff(очек_веројатности)
очек_честоти <- очек_веројатности * обем
пирсонов_хи_квад <- chisq.test(x = поделби, p = очек_честоти/sum(очек_честоти))
пирсонов_хи_квад$p.value <- round(пирсонов_хи_квад$p.value, digits = 3)
print(пирсонов_хи_квад)

#------MONTE CARLO SIMULATIONS - CHECK FOR CONFIRMATION-------

пирсонов_хи_квад_2 <- chisq.test(x = поделби, p = очек_честоти/sum(очек_честоти), simulate.p.value = TRUE, B = 10000)
пирсонов_хи_квад_2$p.value <- round(пирсонов_хи_квад_2$p.value, digits = 3)
print(пирсонов_хи_квад_2)

cat("Степени на слобода: ", пирсонов_хи_квад$parameter, "\n")
хи_вред <- qchisq(1-alpha, пирсонов_хи_квад$parameter)
cat(sprintf("χ2 со 0.01,9 = %.4f", хи_вред))
cat(sprintf("Critical domain: C = (%.4f, + inf)",хи_вред))
print("The value of the test statistic χ2 does not belong in the critical domain, therefore the null hypothesis is rejected")

#----------------------------TESTING HYPOTHESES FOR INDEPENDENCE OF TWO CATEGORICAL VARIABLES-------------------

# H0 : Attributes "Glaucoma" и "GHT" are independent
# Ha : Attributes "Glaucoma" и "GHT" are not independent

контингенција <- table(GlaucomaData$glaucoma, GlaucomaData$GHT)
rownames(контингенција) <- c("No glaucoma","Has glaucoma")
colnames(контингенција) <- c("Without asymmetry","Inconclusive","With asymmetry")
вкупно <- addmargins(контингенција)
print(вкупно)

хи_квадрат_контин <- chisq.test(контингенција)
хи_квадрат_контин

#-------------------------------------------------------------------------------------------------------------

Податоци_рожница <- c(GlaucomaData$cornea_thickness)
Податоци_очен_нерв <- c(GlaucomaData$rnfl4.mean)
рас_рожница <- plot(Податоци_рожница, pch=19, main="Scatter plot of cornea thickness", xlab="Indeces", ylab="Cornea in micrometers")
рас_деб_очн <- plot(Податоци_очен_нерв, pch=19, main="Scatter plot of opcular nerve" , xlab="Indeces", ylab="Average optic nerve value")
pас_сите_вред <- plot(x=Податоци_очен_нерв, y=Податоци_рожница, xlab="Average optic nerve value", ylab = "Cornea in micrometers", main = "Scatter plot of both attributes", pch=19, col="black")

r <- cor(Податоци_очен_нерв,Податоци_рожница,method = c("pearson"))
cat(sprintf("Coefficient of correlation is %.4f", r))

#THE RESULT IS CLOSER TO 0, THERE IS A LOW LINEAR CORRELATION

права_рег <- lm(Податоци_рожница ~ Податоци_очен_нерв)
summary(права_рег)
коеф <- coef(права_рег)
print(коеф)
пресек_у_оска <- коеф[1]
коеф_на_правец <- коеф[2]
cat(sprintf("Правата на регресија на Y по X е: Y = %.4f + %.4f * x", пресек_у_оска, коеф_на_правец))

права_рег <- lm(Податоци_рожница ~ Податоци_очен_нерв)
pас_сите_вред <- plot(x=Податоци_очен_нерв, y=Податоци_рожница, xlab="Average optic nerve value", ylab = "Cornea in micrometers", main = "Scatter plot with regression line", pch=19, col="black")
abline(права_рег, col="purple", lwd=3)

предвидени <- predict(права_рег)
n <- length(Податоци_рожница)
y_просек <- mean(Податоци_рожница)
SST <- sum((Податоци_рожница - y_просек)^2)
SSE <- sum((Податоци_рожница - предвидени)^2)
SSR <- sum((предвидени - y_просек)^2)
cat(sprintf("SST: %.4f\n", SST))
cat(sprintf("SSE: %.4f\n", SSE))
cat(sprintf("SSR: %.4f\n", SSR))
R2 <-  SSR / SST
R2
cat(sprintf("Coefficient of determination: %.4f, therefore there is no linear dependence between X and Y", R2))


colors()

greens <- grep("green", colors(), value = TRUE)
print(greens)


rnfl <- c(GlaucomaData$RNFL4.mean)
cct <- c(GlaucomaData$cornea_thickness)

correlation <- cor.test(cct,rnfl)
print(correlation)

model <- lm(rnfl ~ cct, data = GlaucomaData)
print(model)
summary(model)


plot(model$fitted.values, resid(model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

plot(model,1)



ggplot(GlaucomaData, aes(x = cornea_thickness, y = rnfl4.mean)) +
  geom_point(color = "blue", size = 2.2) +
  #geom_smooth(method = "lm", se = TRUE, color = "red") 
  ggtitle("Scatter Graph of rnfl4 mean and cct Measurements") +
  xlab("cct (μm)") +
  ylab("rnfl4 mean (μm)") +
  scale_x_continuous(breaks = seq(400, 650, by = 20)) +
  scale_y_continuous(breaks = seq(10, 170, by = 20)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



ggplot(GlaucomaData, aes(x = cornea_thickness, y = RNFL4.mean)) +
  geom_point(color = "blue", size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Scatter Graph and Regression Line of RNFL4 mean on CCT Measurements") +
  xlab("CCT (μm)") +
  ylab("RNFL4 mean (μm)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(400, 650, by = 20)) +
  scale_y_continuous(breaks = seq(10, 170, by = 20)) +
  theme(plot.title = element_text(hjust = 0.5))
  

predicted <- predict(model)
n <- length(rnfl)
y_mean <- mean(rnfl)
SST <- sum((GlaucomaData$RNFL4.mean - y_mean)^2)
print(SST)
SSE <- sum((GlaucomaData$RNFL4.mean - predicted)^2)
SSR <- sum((predicted - y_mean)^2)
print(SSR)
cat(sprintf("SST: %.4f\n", SST))
cat(sprintf("SSE: %.4f\n", SSE))
cat(sprintf("SSR: %.4f\n", SSR))
R2 <-  SSR / SST
R2
cat(sprintf("Coefficient of determination: %.4f, therefore there is no linear dependence between X and Y", R2))


