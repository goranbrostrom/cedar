discretize <- function(from = 1860, to = 1900, ages = c(0, 100), dat = obs){
    ## Yearly data:
    ## enter exit event replaced by
    ##   year, age (rounded)
    dat$year <- -1
    dat$age <- -1
    out <- dat[1, ,drop = FALSE]
    for (year in (from:(to - 1))){
        oo <- cal.window(dat, c(year, year + 1))
        oo$year <- year
        for (age in ages[1]:(ages[2] - 1)){
            cat("year = ", year, "age = ", age, "\n")
            ooa <- age.window(oo, age:(age + 1))
            ##ooa <- ooa[!duplicated(ooa$id), ,drop = FALSE]
            ooa$age <- age
            out <- rbind(out, ooa)
        }
    }
    out[-1, ]
}