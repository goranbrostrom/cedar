## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='figs/jun3-', fig.height = 5
)

## ----surd2, echo=FALSE---------------------------------------------------
plot(c(1815, 1830), c(1, 1), type = "l", ylim = c(0.5, 4.5),
     xlim = c(1805, 1840), ylab = "ID", xlab = "Age")
lines(c(1830, 1838), c(1, 1), lty = 2)
points(1838.1, 1, pch = "+", cex = 2)
points(1830.1, 1, pch = "C")
abline(v = c(1810, 1830), lty = 3)
lines(c(1815, 1822), c(2, 2), col = 2)
points(1822.1, 2, pch = "+", col = 2, cex = 2)
lines(c(1808, 1810), c(3, 3), col = 3, lty = 2)
lines(c(1810, 1819), c(3, 3), col = 3)
points(1810, 3, pch = "|", col = 3)
points(1819.1, 3, pch = "+", col = 3, cex = 2)
lines(c(1819, 1830), c(4, 4), col = 4)
lines(c(1816, 1819), c(4, 4), col = 4, lty = 2)
lines(c(1830, 1832), c(4, 4), col = 4, lty = 2)
text(1819, 4.3, "moved in", col = 4)
points(1819, 4, pch = "|", col = 4)
points(1832.1, 4, pch = "+", cex = 2, col = 4)
points(1830.1, 4, pch = "C", col = 4)

## ----surdata, echo=FALSE-------------------------------------------------
plot(c(0, 15), c(1, 1), type = "l", ylim = c(0.5, 4.5), 
     ylab = "ID", xlab = "Age")
abline(v = 0)
points(15.1, 1, pch = "C", cex = 1)
text(8, 1.3, "Right censored")
lines(c(0, 7), c(2, 2), col = 2)
points(7, 2, pch = "+", cex = 2, col = 2)
text(3, 2.3, "Complete", col = 2)
lines(c(2, 11), c(3, 3), col = 3)
points(11, 3, pch = "+", cex = 2, col = 3)
points(2, 3, pch = "|", col = 3)
text(6, 3.3, "Left truncated", col = 3)
lines(c(3, 14), c(4, 4), col = 4)
points(14.1, 4, pch = "C", cex = 1, col = 4)
points(3, 4, pch = "|", col = 4)
text(9, 4.3, "Left truncated & right censored", col = 4)

## ----plotsam,echo=FALSE--------------------------------------------------
rep = 1000000; set.seed(101769)
start = runif(rep, min = 1806, max = 1813)
stopp = start + runif(rep)
plot(c(start[1], stopp[1]), c(1, 1), type = "l", ylim = c(0.5, 100.5), xlim = c(1805.5, 1814.5), ylab = "ID", xlab = "Date")
abline(v = 1810, lty = 2, col = "red")
for (i in 2:100) lines(c(start[i], stopp[i]), c(i, i))

## ----sampl,echo=TRUE-----------------------------------------------------
rep = 1000000; set.seed(101769)
start = runif(rep, min = 1806, max = 1813)
stopp = start + runif(rep)
cover <- (start < 1810) & (stopp > 1810)
sum(cover)
age <- stopp - start
mean(age)
mean(age[cover])

## ----lexis0,echo=FALSE---------------------------------------------------
source("R/lexis0.R")
lexis0(age = c(60, 100), cal = c(1860, 1880))

## ----agesc,echo=FALSE----------------------------------------------------
plot(c(60, 73), c(1, 1), type = "l",  
     ylim = c(0.5, 4.5), xlim = c(59.5, 101), ylab = "ID", xlab = "Age")
abline(v = 60)
points(74, 1, pch = "+", cex = 1.5)
lines(c(63, 74), c(2, 2))
points(75, 2, pch = "+", cex = 1.5)
lines(c(74, 94), c(3, 3), col = "blue")
points(95, 3, pch = "C", col = "blue")
lines(c(93, 100), c(4, 4), col = "blue")
points(101, 4, pch = "C", col = "blue")

## ----oldmort1------------------------------------------------------------
library(eha)
#data(oldmort)

## ----str11---------------------------------------------------------------
str(oldmort)

## ----head1---------------------------------------------------------------
head(oldmort[, c("id", "enter", "exit", "event", "birthdate", 
                 "sex", "ses.50", "region")])

## ----summary1------------------------------------------------------------
summary(oldmort[, c("id", "enter", "exit", "event", "birthdate", 
                    "sex", "ses.50", "region")])

## ----add1----------------------------------------------------------------
fit <- coxreg(Surv(enter, exit, event) ~ sex + region, 
              data = oldmort)

## ----plott,fig.height=4--------------------------------------------------
plot(fit, fn = "surv")
abline(h = 0.5, lty = 2, col = "green")

## ----surmod, echo = FALSE------------------------------------------------
plot(1, 1, ylim = c(0, 4), xlim = c(0, 8), axes = FALSE, type = "n",
     xlab = "", ylab = "")
symbols(x = c(1, 6), y = c(2, 2), circles = c(0.8, 0.8), add = TRUE, inches = FALSE)
text(1, 2, "Alive", col = "blue", cex = 2)
text(6, 2, "Dead", col = "black", cex = 2)
arrows(2, 2, 5, 2, lwd = 2)

## ----readcsv-------------------------------------------------------------
pop <- read.table("Data/BE0101N1.csv", skip = 2, header = TRUE, 
                  sep = ";", fileEncoding = "latin1")
summary(pop)

## ----chnam0--------------------------------------------------------------
names(pop) <- c("region", "civst", "age", "sex",
                "pop13", "pop14")

## ----fixla---------------------------------------------------------------
str(pop)
levels(pop$civst)
levels(pop$civst) <- c("widow", "married", "unmarried", "divorced")
levels(pop$sex) <- c("woman", "man")
levels(pop$age)[1:5]

## ----females,eval=FALSE--------------------------------------------------
## females <- pop[pop$sex == "woman", -1]

## ----femtwo,eval=TRUE----------------------------------------------------
females <- pop[pop$sex == "woman", 2:6]
summary(females)

## ----fixagefem,echo=TRUE-------------------------------------------------
rownames(females) <- 1:NROW(females)
females[c(1:2, 101:103, 202:204, 303:305, 404), c(1:2, 4:5)]

## ----anda----------------------------------------------------------------
females$age <- rep(0:100, times = 4)

## ----femtap--------------------------------------------------------------
fem <- data.frame(age = 0:100,
                  sex = "woman",
                  pop13 = with(females, tapply(pop13, age, sum)),
                  pop14 = with(females, tapply(pop14, age, sum))
                  )
head(fem)

## ----redea---------------------------------------------------------------
dead <- read.table("Data/BE0101D9.csv", skip = 2, 
                   header = TRUE, sep = ";", fileEncoding = "latin1")
head(dead)
fem$deaths <- dead$X2014[dead$kön == "kvinnor"]
head(fem)

## ----formen,echo=FALSE---------------------------------------------------
males <- pop[pop$sex == "man", -1]
males$age <- rep(0:100, times = 4)
mal <- data.frame(age = 0:100,
                  sex = "man",
                  pop13 = with(males, tapply(pop13, age, sum)),
                  pop14 = with(males, tapply(pop14, age, sum)))
mal$deaths <- dead$X2014[dead$kön == "män"]

## ----headmal-------------------------------------------------------------
head(mal)

## ----joinsex-------------------------------------------------------------
both <- rbind(fem, mal)

## ----pyears--------------------------------------------------------------
both$pop <- with(both, (pop13 + pop14) / 2)
both$pop13 <- both$pop14 <- NULL
head(both, 3)

## ----brisk,fig.height=4--------------------------------------------------
both$risk <- both$deaths / both$pop

## ----sortage-------------------------------------------------------------
both <- both[order(both$age, both$sex), ]
head(both)

## ----morbysex,fig.height=4-----------------------------------------------
mrisk <- both$risk[both$sex == "man"]
wrisk <- both$risk[both$sex == "woman"]
plot(0:99, mrisk[1:100], type = "s", col = "blue",
     ylab = "Age-specific mortality", xlab = "Age")
lines(0:99, wrisk[1:100], type = "s", col = "red")
text(78, 0.1, "Men", col = "blue")
text(92, 0.05, "Women", col = "red")
abline(h = 0)

## ----morbysex2,fig.height=5,echo=FALSE-----------------------------------
plot(0:25, both$risk[both$sex == "man" & both$age <= 25], 
     type = "s", col = "blue",
     ylab = "Age-specific mortality", xlab = "Age")
lines(0:25, both$risk[both$sex == "woman" & both$age <= 25], 
      type = "s", col = "red")
text(20, 0.001, "Boys", col = "blue")
text(24, 0.0001, "Girls", col = "red")
abline(h = 0)

## ----morbysex3,fig.height=5,echo=FALSE-----------------------------------
plot(25:60, both$risk[both$sex == "man" & both$age <= 60 & both$age >= 25], 
     type = "s", col = "blue", ylim = c(0, 0.007),
     ylab = "Age-specific mortality", xlab = "Age")
lines(25:60, both$risk[both$sex == "woman" & both$age <= 60 & both$age >= 25], 
      type = "s", col = "red")
text(45, 0.003, "Men", col = "blue")
text(55, 0.001, "Women", col = "red")
abline(h = 0)

## ----morbysex4,fig.height=5,echo=FALSE-----------------------------------
man = both$risk[both$sex == "man" & both$age <= 99 & both$age >= 60]
plot(60:99, man, 
     type = "s", col = "blue", ylim = c(0, 0.5),
     ylab = "Age-specific mortality", xlab = "Age")
woman = both$risk[both$sex == "woman" & both$age <= 99 & both$age >= 60]
lines(60:99, woman, 
      type = "s", col = "red")
abline(h = 0)

## ----morbysex4log,fig.height=5,echo=FALSE--------------------------------
plot(60:99, log(man), 
     type = "s", col = "blue",
     ylab = "Age-specific mortality", xlab = "Age")
lines(60:99, log(woman), 
      type = "s", col = "red")
abline(h = 0)

## ----ltaw----------------------------------------------------------------
fem <- both[both$sex == "woman", ]
surv <- numeric(NROW(fem)) # Reserves memory
surv[1] <- 100000 # Any number
for (i in 2:101){
    surv[i] <- surv[i-1] * (1 - fem$risk[i-1])
}
fem$survivors <- round(surv)
head(fem, 3)
tail(fem, 3)

## ----ltam----------------------------------------------------------------
man <- both[both$sex == "man", ]
surv <- numeric(NROW(man)) # Reserves memory
surv[1] <- 100000 # Any number
for (i in 2:101){
    surv[i] <- surv[i-1] * (1 - man$risk[i-1])
}
man$survivors <- round(surv)
head(man, 3)
tail(man, 3)

## ----suurvf,fig.height=4-------------------------------------------------
plot(0:100, fem$survivors / 100000, type = "l", col = "red", 
     xlab = "Age", ylab = "Surviving fraction", axes = FALSE)
axis(2, at = c(0, 0.5, 1)); axis(1); box()
lines(0:100, man$survivors / 100000, col = "blue")
abline(h = 0); abline(v = 0); text(65, 0.75, "Men", col = "blue")
abline(h = 0.5, lty = 3); text(88, 0.85, "Women", col = "red")

## ----womenmed------------------------------------------------------------
min(fem$age[fem$survivors < 50000])

## ----menmed--------------------------------------------------------------
min(man$age[man$survivors < 50000])

## ----simpdata,echo=FALSE,fig.height = 4.5--------------------------------
plot(c(0, 4), c(1, 1), type = "l", ylim = c(0.5, 5.5), xlim = c(0, 8),
     ylab = "ID", xlab = "Duration", col = "blue", axes = FALSE)
axis(2)
axis(1, at = c(0, 1, 4, 6))
box()
abline(v = 0)
abline(v = c(1, 4, 6), lty = 2)
points(4.1, 1, pch = "+", cex = 2)
lines(c(0, 2), c(2, 2), col = "blue")
points(2, 2, pch = "0")
lines(c(0, 6), c(3, 3), col = "blue")
points(6.1, 3, pch = "+", cex = 2)
lines(c(0, 1), c(4, 4), col = "blue")
points(1.1, 4, pch = "+", cex = 2)
lines(c(0, 3), c(5, 5), col = "blue")
points(3, 5, pch = "o")

## ----cumhazex,fig.height=4.5,echo=FALSE----------------------------------
plot(c(0, 1, 1, 4, 4, 6, 6), c(0, 0, 0.2, 0.2, 0.7, 0.7, 1.7), 
     type = "s", col = "blue", xlab = "Duration", ylab = "Cum. hazards",
     axes = FALSE)
axis(2, at = c(0, 0.2, 0.7, 1.7), cex.axis = 0.7)
axis(1)
box()

## ----echo=TRUE-----------------------------------------------------------
library(eha)
head(mort)

## ----esthas,eval=FALSE---------------------------------------------------
## par(mfrow = c(1, 2))# Two plots, "one row, two columns".
## with(mort, plot(Surv(enter, exit, event), fn = "cum"))
## with(mort, plot(Surv(enter, exit, event), fn = "surv"))

## ----nonp2,echo=FALSE,fig.height=4---------------------------------------
par(mfrow = c(1, 2)) 
with(mort, plot(Surv(enter, exit, event), fn = "cum"))
with(mort, plot(Surv(enter, exit, event), fn = "surv",
     col.con = "black")) # See > ?plot.Surv

## ----rsa2----------------------------------------------------------------
indx <- sample(NROW(mort), size = 50, replace = FALSE)
rsa <- mort[indx, ]
fit <- coxph(Surv(enter, exit, event) ~ 1, data = rsa)
s.fit <- survfit(fit)
summary(s.fit)

