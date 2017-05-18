## Author      : Frank Tamborello
## Contact     : frank.tamborello@cogscent.com
## License     : Creative Commons BY-NC-SA
## Filename     : Generate25Features.R
## Revision     : 
## Description  : Generate 187,500,000 data points in twenty-five features, most varying randomly between-subjects, some varying in other ways. Takes 2.5 - 5 minutes from subjN assignment through fwrite on most modern x86-64 Unixy systems. Takes advantage of anonymous functions & matrix operations. 
## Usage: Execute up to & optionally including writing the data to a file, then plot the results.
## Bugs: None known
## To do: 
## ----- History -----
## 1. 2017.05.18 fpt 
## Inception




library(data.table)

pt <- proc.time()
# Length of sample per subject
subjN <- 150
# Subject ID
# subjID is 1-indexed and repeats for every case of each of 50K subjects
subjID <- as.vector(t(matrix(data=rep(x=1:50000, times=subjN), ncol=subjN)))
# Timestamp (ms)
# Make timestamps via vectorized assignments starting from startTime
startTime <- 1495161790000
# Generate the inter-subject data points.
alfa <- seq(from=startTime, by=56894, length.out=tail(subjID, n=1))
# The intra-subject intervals
beta <- seq(from=0, by=25, length=subjN)
# What's an efficient way to get a m by n matrix where m is the inter-subject timestamps, n is the intra-subject timestamps, and each row is m + n? 
timestamp <- as.vector(t(sapply(1:subjN, function(i) alfa + beta[i])))
# clean up
rm(alfa, beta)
# Var3
# …is a float iterating by 0.001 over subjects from 0.0 to 0.7
var3 <-
    as.vector(
        t(
        matrix(
        data=rep(
            x=rep(
                x=seq(from=0.0, to=0.7, by=0.001), length.out=tail(subjID, n=1)),
            times=subjN),
        ncol=subjN)))
# var4 is linearly interpolated from 0 to 1 by subjN sequence
var4 <- rep(seq(from=0.0, to=1, length.out=subjN), tail(subjID, n=1))
# The rest is randomly generated between-subjects, stable within-subjects
additionalVars <- paste("var", 5:25, sep="")
data <- data.frame(
    sapply(
        additionalVars,
        function (var)
            assign(
                var,
                as.vector(
                    t(
                        matrix(
                            data=rep(x=runif(n=tail(x=subjID, n=1)),
                                     times=subjN),
                            ncol=subjN))))))
data <- data.frame("subjID" = subjID, "timestamp(ms)" = timestamp, "var3" = var3, "var4" = var4, data)
# Prepend the column headers line with a hash to C-style comment it out.
file <- "~/FeatureSet.CSV"
write.table(paste("#", paste(names(data), collapse=","), sep=""), file=file, row.names=F, col.names=F, quote=F, append=F)
fwrite(x=data, file=file, append=T, col.names=F)
proc.time() - pt

    
# Plot!
# Now check my work
xlim <- c(0,1); ylim <- c(0,1)

with(data[data$subjID==1,], plot(x=var4, y=var5, xlim=xlim, ylim=ylim))

# Scale & translate Timestamp to the smallest integer difference
scaleTimestamp <- function (ts) {
    (ts - startTime) * 0.1
    }

with(data[data$subjID==1,], symbols(x=var4, y=var5, circles=scaleTimestamp(timestamp[data$subjID==1]), inches=0.25, bg="steelblue2", fg=NULL, xlim=xlim, ylim=ylim))



    
