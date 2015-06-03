## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
##engine='R',dev='postscript',dev='pdf',fig.width=10,fig.height=6.5,strip.white=all
engine='R',dev='pdf',fig.width=10,fig.height=5
)

## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='figs/',include=TRUE,cache=FALSE,cache.path="cache4/"
)

## ----figPH,fig.height=5,message=FALSE------------------------------------
library(eha)
curve(hlnorm(x, 1/3, 1), 0, 10, col = "blue",
      ylab = "Proportional hazards", xlab = "t")
curve(hlnorm(x, 1/3, 1) * 0.75, 0, 10, add = TRUE, col = "red")
abline(h = 0); abline(v = 0)
text(5, 0.5, "h(t)", cex = 1.5); 
text(5, 0.15, "0.75 h(t)", col = "red", cex = 1.5)

## ----echo=FALSE----------------------------------------------------------
plot(1, 2, type = "n", xlim = c(0, 40), ylim = c(0, 3), xlab = "Age", ylab = "", yaxt="n", axes = FALSE)
axis(1, at = c(0, 21, 35), cex.axis = 1.5)
lines(c(0, 21), c(1,1), col = "red")
points(21, 1, pch = "c", cex = 2)
lines(c(21, 35), c(2, 2), col= "blue")
points(21, 2, pch = "|")
points(35.5, 2, pch = "+", cex = 2)
text(10, 1.2, labels = "x = unmarried", col = "red", cex = 2)
text(30.2, 2.2, labels = "x = married", col = "blue", cex = 2)
abline(v = 0)
abline(v = 21, lty = 3)

## ----skel15, message=FALSE-----------------------------------------------
library(skel15)
names(obs)
head(obs[, c("id", "sex", "enter", "exit", "event", "mid")])

## ----IMR-----------------------------------------------------------------

infants <- age.window(obs, c(0, 1))
infants <- infants[!is.na(obs$mid), ]
infants <- infants[, c("id", "sex", "birthdate", "enter", 
                       "exit", "event", "parity", "mid")]
dim(infants)
infants <- infants[infants$mid %in% obs$id, ]
dim(infants)
infants <- infants[order(infants$mid, infants$parity), ]
rownames(infants) <- NULL # Remove row names
head(infants)

## ----m.age---------------------------------------------------------------
indx <- match(infants$mid, obs$id)
(infants$mid[1] == obs$id[indx[1]]) & 
   (infants$mid[2] == obs$id[indx[2]]) # and so on ...
infants$m.birthdate <- obs$birthdate[indx]
infants$m.age <- with(infants, birthdate - m.birthdate)
head(infants)

## ----suminf--------------------------------------------------------------
summary(infants[, c("sex", "birthdate", "enter", "exit", "event", 
                    "parity", "m.age")])

## ----checkmage-----------------------------------------------------------
infants[infants$m.age < 15, c("id", "mid", "m.age")]

## ----removerec-----------------------------------------------------------
infants <- infants[infants$m.age > 10, ]
levels(infants$sex)
levels(infants$sex) <- c("boy", "girl")

## ----moagnum-------------------------------------------------------------
fit.l <- coxreg(Surv(enter, exit, event) ~ sex + m.age, data = infants)
fit.l

## ----moagnum2,results='asis'---------------------------------------------
dr.l <- drop1(fit.l, test = "Chisq")
ltx(fit.l, dr = dr.l, digits = 4)

## ----drop1lr-------------------------------------------------------------
dr.l

## ----moagfac,results='asis'----------------------------------------------
infants$m.age.grp <- cut(infants$m.age, c(10, 25, 35, 51))
fit.o <- coxreg(Surv(enter, exit, event) ~ sex + m.age.grp, data = infants)
dr <- drop1(fit.o, test = "Chisq")
ltx(fit.o, digits = 4)

## ----relevel,results='asis'----------------------------------------------
infants$m.age.grp <- relevel(infants$m.age.grp, ref = "(25,35]")
fit <- coxreg(Surv(enter, exit, event) ~ sex + m.age.grp, data = infants)
dr <- drop1(fit, test = "Chisq")
ltx(fit, digits = 4)

## ----howto,results='asis'------------------------------------------------
ltx(fit, dr = dr, digits = 4)

## ----agraph--------------------------------------------------------------
bars <- exp(coef(fit))[-1] - 1
bars <- c(bars[1], 0, bars[2])
names(bars) <- c("(11, 25]", "(25-35]", "(35-51]") 
barplot(100 * bars, ylab = "Excess mortality (per cent)", 
        xlab = "Mother's age")

## ----finer---------------------------------------------------------------
cuts <- c(11, 18, seq(21, 51, by = 3))
infants$m.grp <- cut(infants$m.age, cuts)
res <- coxreg(Surv(enter, exit, event) ~ m.grp, data = infants)
barplot(exp(c(0, res$coef) - res$coef[5]) - 1, names.arg = cuts[-length(cuts)])

## ----finerzph------------------------------------------------------------
res <- coxreg(Surv(enter, exit, event) ~ m.age, data = infants)
(czph <- cox.zph(res))
plot(czph, resid = FALSE, df = 6)

## ----cumhazgrp,fig.height=5----------------------------------------------
fit0 <- coxreg(Surv(enter, exit, event) ~ sex + strata(m.age.grp), 
               data = infants, hazards = TRUE)
plot(fit0, col = 1:3, xlab = "Age")

## ----cumhazgrpll,fig.height=5--------------------------------------------
plot(fit0, col = 1:3, xlab = "log(Age)", fn = "loglog")

## ----relevellrt,results='asis'-------------------------------------------
ltx(fit, dr = dr, digits = 4)

## ----zph-----------------------------------------------------------------
(zph.l <- cox.zph(fit.l))
(zph <- cox.zph(fit))

## ----zphgraph,fig.height = 4---------------------------------------------
oldpar <- par(mfrow = c(1, 2))
plot(zph.l, resid = FALSE)
par(oldpar) # Restore plotting area

## ----zphgraph2,fig.height = 6, echo = FALSE------------------------------
oldpar <- par(mfrow = c(2, 2))
plot(zph, resid = FALSE)
par(oldpar)
save(infants, file = "infants.rda")

## ----inter, results='asis'-----------------------------------------------
res2  <- coxreg(Surv(enter, exit, event) ~ ses * birthdate, 
                data = mort, max.survs = 3000)
dr <- drop1(res2, test = "Chisq")
library(xtable)
xtable(dr)

## ----interpar,results='asis'---------------------------------------------
ltx(res2, digits = 4)

## ----center,results='asis'-----------------------------------------------
birthdate.c <- mort$birthdate - 1810
res3  <- coxreg(Surv(enter, exit, event) ~ ses * birthdate.c, 
                data = mort)
ltx(res3, digits = 4)

