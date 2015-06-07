## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='figs/', fig.height = 4,cache=TRUE,cache.path="cache8/"
)

## ----6hazs,echo=FALSE,fig.height=5,message=FALSE-------------------------
require(eha)
oldpar <- par(mfrow = c(2, 2))
x <- seq(0, 10, length = 1000)
plot(x, hweibull(x, shape = 2, scale = 2), main = "Weibull, p = 2", type = "l",
     ylab = "", xlab = "Time")
plot(x, hweibull(x, shape = 1/2, scale = 2), main = "Weibull, p = 1/2", type = "l",
     ylab = "", xlab = "Time", ylim = c(0, 2))
plot(x, hlnorm(x, shape = 1, scale = 2), main = "Lognormal", type = "l",
     ylab = "", xlab = "Time")
y <- hgompertz(x, scale = 5, shape = 1)
plot(x, y, main = "Gompertz", type = "l",
     ylab = "", xlab = "Time", ylim = c(0, max(y)))
par(oldpar)

## ----plotsex,fig.height=4------------------------------------------------
with(oldmort, plot(Surv(enter, exit, event),
                   strata = sex, col = c("blue", "red"),
                   xlab = "Age"))

## ----fert6.12------------------------------------------------------------
library(xtable)
require(eha)
data(fert)
f12 <- fert[fert$parity == 1, ]
f12$Y <- Surv(f12$next.ivl, f12$event)
f12$age <- f12$age - 25
f12$year <- f12$year - 1850
head(f12)

## ----Y12-----------------------------------------------------------------
is.matrix(f12$Y)
dim(f12$Y)

## ----phweib, results='asis'----------------------------------------------
fit.w <- phreg(Y ~ age + year + ses, data = f12)
xtable(drop1(fit.w, test = "Chisq"))

## ----fitw,results='asis'-------------------------------------------------
ltx(fit.w)
kof <- fit.w$coef[1]

## ----weibfert6,fig.height=3----------------------------------------------
oldpar <- par(mfrow = c(1, 2))
plot(fit.w, fn = "haz")
plot(fit.w, fn = "cum")
par(oldpar)

## ----phlognorm,results='asis'--------------------------------------------
fit.lognorm <- phreg(Y ~ age + year + ses, data = f12,
                     dist = "lognormal")
xtable(drop1(fit.lognorm, test = "Chisq"))

## ----fitlogist,results='asis'--------------------------------------------
dr = drop1(fit.lognorm, test = "Chisq")
ltx(fit.lognorm, dr = dr, digits = 4)

## ----secret16,echo=FALSE-------------------------------------------------
kof <- fit.lognorm$coef[1]

## ----lognormfert6,fig.height=3-------------------------------------------
oldpar <- par(mfrow = c(1, 2))
plot(fit.lognorm, fn = "haz", main = "Hazard function")
plot(fit.lognorm, fn = "cum", main = "Cumulative hazard function")
par(oldpar)

## ----coxr6---------------------------------------------------------------
fit.cr <- coxreg(Y ~ age + year + ses, data = f12)

## ----drycompw,fig.height=4-----------------------------------------------
check.dist(fit.cr, fit.w)

## ----trunc6f12cens,results='asis'----------------------------------------
f12$enter <- rep(0, NROW(f12))
f12.cens <- age.window(f12, c(0, 12),
                       surv = c("enter", "next.ivl", "event"))
f12.cens$Y <- Surv(f12.cens$enter, f12.cens$next.ivl,
                   f12.cens$event)
fit.wc <- phreg(Y ~ age + year + ses, data = f12.cens)
fit.c <- coxreg(Y ~ age + year + ses, data = f12.cens)
ltx(fit.wc)

## ----drycompwcens--------------------------------------------------------
check.dist(fit.c, fit.wc)

## ----trunc6f12cens.ln,results='asis'-------------------------------------
fit.lnc <- phreg(Y ~ age + year + ses, data = f12.cens,
                 dist = "lognormal")
dr <- drop1(fit.lnc, test = "Chisq")
ltx(fit.lnc, dr = dr)

## ----drycomplncens-------------------------------------------------------
check.dist(fit.c, fit.lnc)

## ----pch6fert, results='asis'--------------------------------------------
fit.pch <- phreg(Surv(next.ivl, event) ~ age + year + ses,
                 data = f12, dist = "pch", cuts = c(4, 8, 12))
fit.c <- coxreg(Surv(next.ivl, event) ~ age + year + ses, 
                data = f12)
dr = drop1(fit.pch, test = "Chisq")
ltx(fit.pch, dr = dr)

## ----drycomppchcens------------------------------------------------------
check.dist(fit.c, fit.pch)

## ----pchfert2,results='asis'---------------------------------------------
fit.pch <- phreg(Surv(next.ivl, event) ~ age + year + ses,
                data = f12, dist = "pch", cuts = 1:13)
dr = drop1(fit.pch, test = 'Chisq')
ltx(fit.pch, dr = dr)

## ----drycomppch13cens----------------------------------------------------
check.dist(fit.c, fit.pch)

## ----pchhaz6-------------------------------------------------------------
plot(fit.pch, fn = "haz", main = "")

## ----threecomp6,echo=FALSE-----------------------------------------------
##require(xtable)
##fit.c$coef
##fit.w$coef
##fit.lognorm$coef
##fit.pch$coef
ta <- cbind(fit.c$coef[1:5], fit.pch$coef[1:5], fit.lognorm$coef[2:6], fit.w$coef[1:5])
colnames(ta) <- c("Cox", "Pch", "Lognormal", "Weibull")
ta <- round(exp(ta), digits = 3)
print(ta)

## ----splitsurv6----------------------------------------------------------
f12 <- fert[fert$parity == 1, ]
f12$enter <- 0 # 0 expands to a vector
f12.split <- survSplit(f12, cut = 1:13, start = "enter",
                       end = "next.ivl", event = "event",
                       episode = "ivl")
head(f12.split)

## ----sortt---------------------------------------------------------------
f12.split <- f12.split[order(f12.split$id, f12.split$enter), ]
head(f12.split)

## ----poispch6------------------------------------------------------------
f12.split$offs <- log(f12.split$next.ivl - f12.split$enter)
f12.split$ivl <- as.factor(f12.split$ivl)
fit12.pn <- glm(event ~ offset(offs) +
                age + year + ses + ivl,
                family = "poisson", data = f12.split)
drop1(fit12.pn, test = "Chisq")

## ----intactage-----------------------------------------------------------
tapply(f12.split$event, f12.split$ivl, sum)

## ----collapsebir6--------------------------------------------------------
fc <- age.window(f12.split, c(0, 11),
                 surv = c("enter", "next.ivl", "event"))
levels(fc$ivl) <- c(0:6, rep("7-11", 7))
tapply(fc$event, fc$ivl, sum)

## ----rerun0pch6----------------------------------------------------------
fit <- glm(event ~  offset(offs) + age + year + ses + ivl,
           family = "poisson", data = fc)
drop1(fit, test = "Chisq")

## ----rerunpch6-----------------------------------------------------------
fit.ia <- glm(event ~  offset(offs) + (age + year + ses) * ivl,
              family = "poisson", data = fc)
drop1(fit.ia, test = "Chisq")

## ----oldage--------------------------------------------------------------
head(oldmort)

## ----oldmort6------------------------------------------------------------
summary(oldmort[, c(2:5, 8:13)])

## ----struct--------------------------------------------------------------
str(oldmort)


## ----oldmort6.reg,results='asis'-----------------------------------------
om <- oldmort
om$Y <- Surv(om$enter - 60, om$exit - 60, om$event)
om$birthdate <- om$birthdate - 1830
fit.w <- phreg(Y ~ birthdate + sex + civ + ses.50 +
                   birthplace + imr.birth + region , data = om)
dr = drop1(fit.w, test = "Chisq")
xtable(dr)

## ----oldmort6.reg2,results='asis'----------------------------------------
fit.w2 <- phreg(Y ~ birthdate + sex + civ + birthplace +
                    imr.birth + region , data = om)
dr2 = drop1(fit.w2, test = "Chisq")
xtable(dr2)

## ----oldmort6.reg3,results='asis'----------------------------------------
fit.w3 <- phreg(Y ~ birthdate + sex + civ + birthplace +
                    region , data = om)
dr3 = drop1(fit.w3, test = "Chisq")
xtable(dr3)

## ----oldmort6.reg4,results='asis'----------------------------------------
fit.w4 <- phreg(Y ~ birthdate + sex + civ +
                    region , data = om)
dr4 = drop1(fit.w4, test = "Chisq")
xtable(dr4)

## ----grtest, fig.height = 5, echo = FALSE--------------------------------
oldpar <- par(mfrow = c(2, 2))
fit.sex <- phreg(Y ~ birthdate + strata(sex) + civ +
                    region , data = om)
plot(fit.sex, main = "sex", fn = "cum", col = 1:2)
fit.civ <- phreg(Y ~ birthdate + sex + strata(civ) +
                    region , data = om)
plot(fit.civ, main = "Civil status", fn = "cum", col = c(1:2, 4))
fit.reg <- phreg(Y ~ birthdate + sex + civ +
                    strata(region), data = om)
plot(fit.reg, main = "Region", fn = "cum", col = c(1:2, 4))
fit.all <- phreg(Y ~ birthdate + sex + civ +
                    region , data = om)
plot(fit.all, main = "All", fn = "cum")
par(oldpar)

## ----grtesthaz, fig.height = 5, echo = FALSE-----------------------------
par(mfrow = c(2, 2))
fit.sex <- phreg(Y ~ birthdate + strata(sex) + civ +
                    region , data = om)
plot(fit.sex, main = "sex", fn = "haz", col = 1:2)
fit.civ <- phreg(Y ~ birthdate + sex + strata(civ) +
                    region , data = om)
plot(fit.civ, main = "Civil status", fn = "haz", col = c(1:2, 4))
fit.reg <- phreg(Y ~ birthdate + sex + civ +
                    strata(region), data = om)
plot(fit.reg, main = "Region", fn = "haz", col = c(1:2, 4))
fit.all <- phreg(Y ~ birthdate + sex + civ +
                    region , data = om)
plot(fit.all, main = "All", fn = "haz")

## ----grtestnon, fig.height = 5, echo = FALSE-----------------------------
oldpar = par(mfrow = c(2, 2))
fit.sex <- coxreg(Y ~ birthdate + strata(sex) + civ +
                    region , data = om)
plot(fit.sex, main = "sex", fn = "cum", col = 1:2)
fit.civ <- coxreg(Y ~ birthdate + sex + strata(civ) +
                    region , data = om)
plot(fit.civ, main = "Civil status", fn = "cum", col = 1:4)
fit.reg <- coxreg(Y ~ birthdate + sex + civ +
                    strata(region), data = om)
plot(fit.reg, main = "Region", fn = "cum", col = 1:2)
fit.allc <- coxreg(Y ~ birthdate + sex + civ +
                    region , data = om)
plot(fit.allc, main = "All", fn = "cum")
par(oldpar)

## ----checkdist-----------------------------------------------------------
check.dist(fit.allc, fit.all)


## ----allreg6-------------------------------------------------------------
ln <- phreg(Y ~ sex + civ + birthplace, data = om,
            dist = "lognormal")
ll <- phreg(Y ~ sex + civ + birthplace, data = om,
            dist = "loglogistic")
g <- phreg(Y ~ sex + civ + birthplace, data = om,
           dist = "gompertz")
ev <- phreg(Y ~ sex + civ + birthplace, data = om,
            dist = "ev")

## ----compare6------------------------------------------------------------
xx <- c(fit.w$loglik[2], ln$loglik[2], ll$loglik[2],
        g$loglik[2], ev$loglik[2])
names(xx) <- c("w", "ln", "ll", "g", "ev")
xx

## ----grainsp6------------------------------------------------------------
fit.c <- coxreg(Y ~ sex + civ + birthplace, data = om)
check.dist(fit.c, g, col = c("blue", "red"))
abline(h = 0)

## ----gomphaz6------------------------------------------------------------
plot(g, fn = "haz", col = "blue")
abline(h = 0)

## ----aftph6,echo=FALSE---------------------------------------------------
x <- seq(0, 3, length = 1000)
par(mfrow = c(1, 2))
plot(x, 2 * hllogis(x, shape = 5), type = "l", ylab = "", main = "PH", xlab = "Time")
lines(x, hllogis(x, shape = 5), lty = 2)
plot(x, 2 * hllogis(2 * x, shape = 5), type = "l", ylab = "", main = "AFT", xlab = "Time")
lines(x, hllogis(x, shape = 5), lty = 2)

## ----lnaft,results='asis'------------------------------------------------
fit.lna <- aftreg(Y ~ age + year + ses, data = f12.cens,
                  dist = "lognormal")
dr = drop1(fit.lna, test = "Chisq")
ltx(fit.lna, dr = dr)

## ----lnaftexp,results='asis'---------------------------------------------
fit.lnae <- aftreg(Y ~ age + year + ses, data = f12.cens,
                  param = 'lifeExp', dist = "lognormal")
dre = drop1(fit.lnae, test = "Chisq")
ltx(fit.lnae, dr = dre)

## ----lnaftexpgomp,results='asis'-----------------------------------------
fit.lnaeg <- aftreg(Y ~ age + year + ses, data = f12.cens,
                  param = 'lifeExp', dist = "loglogistic")
dreg = drop1(fit.lnaeg, test = "Chisq")
ltx(fit.lnaeg, dr = dreg)

## ----basesur-------------------------------------------------------------
plot(fit.lnaeg, fn = "haz")

## ----oldmort6.aft,results='asis'-----------------------------------------
fit.w1 <- aftreg(Y ~ sex + civ + birthplace, data = om)
dr <- drop1(fit.w1, test = "Chisq")
ltx(fit.w1, dr = dr)

## ----oldln6.aft----------------------------------------------------------
fit.ph <- phreg(Y ~ sex + civ + birthplace, data = om,
               dist = "gompertz")
fit.ph$loglik
fit.aft <- aftreg(Y ~ sex + civ + birthplace, data = om,
                 dist = "gompertz")
fit.aft$loglik

## ----gomfitph,results='asis'---------------------------------------------
dr <- drop1(fit.aft, test = "Chisq")
ltx(fit.aft, test = "Chisq")

## ----expect--------------------------------------------------------------
shape <- exp(fit.aft$coefficients["log(shape)"])
scale <- exp(fit.aft$coefficients["log(scale)"])
(expected <- integrate(pgompertz, 0, Inf, shape = shape, 
                       scale = scale, lower.tail = FALSE, 
                       param = "canonical"))

## ----calexpl-------------------------------------------------------------
integrate(pgompertz, 0, Inf, scale = 9.224, 
          shape = 0.205, param = "canonical", 
          lower.tail = FALSE)

## ----trimort6,echo=FALSE-------------------------------------------------
data(oldmort)
om <- oldmort[oldmort$enter == 60, ]
om <- age.window(om, c(60, 70))
om$m.id <- om$f.id <- om$imr.birth <- om$birthplace <- NULL
om$birthdate <- om$ses.50 <- NULL
om1 <- survSplit(om, cut = 61:69, start = "enter", end = "exit",
                 event = "event", episode = "agegrp")
om1$agegrp <- factor(om1$agegrp, labels = 60:69)
#head(om1)

## ----recode6,echo=FALSE--------------------------------------------------
om1$enter <- om1$exit <- NULL
om1$event <- as.logical(om1$event)
om1 <- om1[order(om1$id, om1$agegrp), ]
rownames(om1) <- 1:NROW(om1)
om1 <- om1[, c(1, 6, 2, 3:5)]
om1$id <- as.numeric(as.factor(om1$id))
head(om1)

## ----statistics----------------------------------------------------------
with(om1, table(agegrp, event))

## ----logreg,results='asis'-----------------------------------------------
fit <- glm(event ~ sex + civ + region + agegrp, 
           data = om1, family = binomial(link = "cloglog"))
xtable(summary(fit)$coef, digits = 4)

## ----withcoxreg,results='asis'-------------------------------------------
om1$age <- as.numeric(as.character(om1$age))
fit1 <- coxreg(Surv(age - 0.5, age, event) ~ sex + civ + region, 
               data = om1, method = "ml")
dr <- drop1(fit1, test = "Chisq")
ltx(fit1, dr = dr, digits = 4)

## ----plotta--------------------------------------------------------------
plot(fit1, fn = "surv")

## ----plotta2-------------------------------------------------------------
barplot(fit1$hazards[[1]][, 2], 
        names.arg = fit1$hazards[[1]][, 1], xlab = "age")

## ----recspp6-------------------------------------------------------------
recs <- tapply(om1$id, om1$id, length)
table(recs)

## ----barp6---------------------------------------------------------------
barplot(table(recs))

## ----wideform6-----------------------------------------------------------
om1$exit <- om1$enter <- NULL
om2 <- reshape(om1, v.names = c("event", "civ", "region"),
               idvar = "id", direction = "wide",
               timevar = "agegrp")
names(om2)

## ----reshap26------------------------------------------------------------
om3 <- reshape(om2, direction = "long", idvar = "id",
               varying = 3:32)
head(om3)

## ----sortin6-------------------------------------------------------------
om3 <- om3[order(om3$id, om3$time), ]
om3[1:11, ]

## ----remove6-------------------------------------------------------------
NROW(om3)
om3 <- om3[!is.na(om3$event), ]
NROW(om3)

## ----summm6--------------------------------------------------------------
summary(om3)

## ----turn6---------------------------------------------------------------
om3$time <- as.factor(om3$time)
summary(om3)

## ----glmreg6-------------------------------------------------------------
fit.glm <- glm(event ~ sex + civ + region + time,
               family = binomial(link = cloglog), data = om3)
summary(fit.glm)

## ----glmdrop6------------------------------------------------------------
drop1(fit.glm, test = "Chisq")

## ----glmmboot6-----------------------------------------------------------
fit.boot <- glmmboot(event ~ sex + civ + region, cluster = time,
                     family = binomial(link = cloglog),
                     data = om3)
fit.boot

## ----calchaz6------------------------------------------------------------
haz <- plogis(fit.boot$frail)
haz

## ----plothaz6------------------------------------------------------------
barplot(haz)

## ----mlreg6--------------------------------------------------------------
om3$exit <- as.numeric(as.character(om3$time))
om3$enter <- om3$exit - 0.5
fit.ML <- coxreg(Surv(enter, exit, event) ~ sex + civ + region,
                 method = "ml", data = om3)
fit.ML

## ----plotcum6------------------------------------------------------------
plot(fit.ML, fn = "cum", xlim = c(60, 70))

## ----plotsur6------------------------------------------------------------
plot(fit.ML, fn = "surv", xlim = c(60, 70))

## ----testph6-------------------------------------------------------------
fit2.glm <- glm(event ~ (sex + civ + region) * time,
                family = binomial(link = cloglog),
                data = om3)
drop1(fit2.glm, test = "Chisq")

