library("ggplot2")
heights <- c(170, 151, 174, 197, 177, 194, 173, 180, 186, 185, 175, 163, 160, 178, 190, 170, 169, 183, 182, 180, 176, 176, 164, 180, 166, 168, 167, 186, 165)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

result.mean <- mean(heights)
print(result.mean)

result.med <- median(heights)
print(result.med)

result.mode <- getmode(heights)
print(result.mode)

result.var <- sd(heights)
print(result.var)

result.gm_mean <- prod(heights)^(1/(length(heights)))
print(result.gm_mean)

result.hm_mean <- 1/mean(1/heights)
print(result.hm_mean)

hist(heights, plot=TRUE, xlab="Students' heights")

boxplot(heights, xlab="Students' heights", horizontal=TRUE)

n <- length(heights)
p <- (1 : n) / n - 0.5 / n

PP = data.frame(emp=(1:n)/n, theor=pnorm(sort(heights), result.mean, result.var))
ggplot(PP,aes(x=emp, y=theor)) + geom_abline(intercept=0, slope=1, colour="blue") + geom_point() + labs(y="", x="", title="P-P plot")

QQ = data.frame(emp=sort(heights), theor=qnorm((1:n)/n, result.mean, result.var))
QQ_plot <- ggplot(QQ, aes(x = emp, y = theor), colour = "blue")
QQ_plot + geom_abline(intercept=0, slope=1, colour="red") + geom_point() + labs(y="", x="", title="Q-Q plot")