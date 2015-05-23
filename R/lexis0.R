lexis0 <- function(age = c(50, 100), cal = c(1829, 1895)){

    ## Birth cohort:
    birth.min <- cal[1] - age[2]
    birth.max <- cal[2] - age[1]
    ##
    par(lab = c(3, 3, 7),
        cex = 1,
        ##cex.axis = 1.5,
        ##cex.lab = 1.5,
        lwd = 1.5)
    
    plot(c(cal), c(age[2], age[2]),
         xlim = c(birth.min - 5, cal[2] + 5),
         ylim = c(0, 110), type = "n",
                                        #main = "Sampling frame",
         xlab = "Calendar time",
         ylab = "Age", xaxt = 'n', yaxt = 'n')
    lines(c(cal[1], cal[1],cal[2], cal[2]),
          c(age[2], age[1], age[1], age[2]), col = "red")
    lines(cal, c(age[2], age[2]), col = "red")
    
    axis(1, c(cal[1] - age[2], cal[1], cal[2] - age[1], cal[2]))
    axis(2, c(0, age[1], age[2]))
    ## Diagonal:
    diag <- function(from, years, lty = 1, col = "black", lwd = 2){
        to <- from + years
        lines(c(from[1], to[1]), c(from[2], to[2]),
              lty = lty, col = col, lwd = lwd)
    }
    
    ## The 'romb' (not now):
    ##dag(c(1716, 30), 20, lty = 2, col = "red")
    ##diag(c(1870, 30), 20, lty = 2, col = "red")
    ##lines(c(1716, 1845+25), c(30, 30), lty = 2, col = "red")
    ##lines(c(1736, 1865+25), c(50, 50), lty = 2, col = "red")
    
    ## 'Cohort' help line, beginning of period:
    diag(c(birth.min, 0), age[2], lty = 3, lwd = 1, col = "blue")
    ## 'Cohort' help line, end of period:
    diag(c(birth.max, 0), age[1], lty = 3, lwd = 1, col = "blue")
    ##
    abline(h = 0)
    
    ## Life lines:
    
    life <- function(enter, exit.age, event){
        endp <- enter + exit.age - enter[2]
        endp[1] <- endp[1] + 3
        endp[2] <- min(age[2], endp[2])
        diff <- 30 - enter[2]
        diag(enter, diff, lty = 4)
        
        enter <- enter + diff
        diag(enter, 50 - enter[2], lty = 1)
        ##if (FALSE){
        enter <- enter + 20
        diag(enter, 55 - enter[2], lty = 2)
        
        enter <- enter + 5
        if (exit.age <= age[2]){
            diag(enter, exit.age - enter[2], lty = 1)
        }else{
            diag(enter, age[2] - enter[2], lty = 1)
            enter <- enter + 25
            diag(enter, exit.age - enter[2], lty = 2)
            text(enter[1], enter[2] + 3, "c")
        }
                                        #  }
        if (event && (exit.age < age[2])){
            text(endp[1], endp[2], "+")
        }else{
            if (exit.age < age[2])
              text(endp[1], endp[2], "c")
        }
    }
    ## First life line:
    start <- c(birth.min + 5, 0)
    years <- cal[1] - start[1]
    diag(start, years, lty = 2, lwd = 1)
    start <- start + years
    years <- age[2] - years
    diag(start, years, lty = 1, col = "blue")
    endp <- start + years
    text(endp[1] + 2, endp[2] + 2, "C", cex = 1, col = "blue")
    ####
    ## Second life line:
    start <- c(birth.min + 25, 0)
    years <- cal[1] - start[1]
    diag(start, years, lty = 2, lwd  = 1)
    start <- start + years
    years <- age[2] - years - 5
    diag(start, years, lty = 1, col = "blue")
    endp <- start + years
    text(endp[1] + 2, endp[2] + 2, "C", cex = 1, col = "blue")
    ####
    ## Second+ life line:
    start <- c(birth.min + 35, 0)
    years <- cal[1]-start[1] #age[1]
    diag(start, years, lty = 2, lwd = 1)
    start <- start + years
    years <- age[2] - years - 25
    diag(start, years, lty = 1)
    endp <- start + years
    text(endp[1] + 2, endp[2] + 2, "+", cex = 1.5)
    ####
    ## Third life line:
    start <- c(birth.min + 42, 0)
    years <- cal[1] - start[1] #age[1]
    years = age[1]
    diag(start, years, lty = 2, lwd = 1)
    start <- start + years
    years <- age[2] - years - 25 - 2
    diag(start, years, lty = 1)
    endp <- start + years
    text(endp[1] + 2, endp[2] + 2, "+", cex = 1.5)
    ####
    ## Fourth life line:
    start <- c(birth.min + age[2], 0)
    years <- age[1]
    ## diag(start, years, lty = 2)
    if (FALSE){
        start <- start + years
        years <- cal[2] - start[1]
        diag(start, years, lty = 1)
        endp <- start + years
        text(endp[1] + 2, endp[2] + 2, "coj", cex = 1.5)
    }
    ####
    text(birth.min, age[2] - 2, "C = censoring", pos = 4, cex = 1)
    text(birth.min, age[2] - 10, "+ = death", pos = 4, cex = 1)
    ##life(c(1780, 0), 68, 1)
    ##return(NULL)
    ##life(c(1777, 19), 61, 1)
    ##life(c(1800, 0), 78, 1)
    ##life(c(1825, 5), 75, 0)
    
    ## Some non-included lifes:
    if (FALSE){
        diag(c(1815, 0), 46, lwd = 2, lty = 3)
        text(1863, 48, "+", cex = 1.5)
        
        diag(c(1740, 0), 74, lwd = 2, lty = 3)
        text(1816, 76, "+", cex = 1.5)
        return(NULL)
        diag(c(1780, 49), 36, lty = 2)
        diag(c(1810, 38), 32, lty = 2)
        diag(c(1750, 38), 36, lty = 2)
    }
}
       
