# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution
ocupacao <- c(29, 28, 15, 13, 17)
monitores <- c(88, 75, 15, 15, 7)
autuacoes <- c(6.6, 14.7, 9.3, 6.2, 23.6) # *10^3
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


x <- seq(-5, 5, len = 50)
y <- seq(0.001, 10, len = 50)
z <- outer(x, y, function(x, y){
  dnorm(x) * dgamma(y, 5)
})

contour(x, y, z)

library(pbivnorm)
x <- rnorm(10)
y <- rnorm(10)
rho <- runif(10)
pbivnorm(x, y, rho)
plot(pbivnorm(ocupacao, autuacoes, rho))
