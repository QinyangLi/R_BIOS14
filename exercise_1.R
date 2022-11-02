# Day1 introduction, basic programming, bootstrap ####

# why use both SD and var if they're both measurement of dispersion?
# 1, get rid of negative
# 2, variances are additive C^2 = A^2 + B^2
# 3, sd keeps unit, therefore standard

# Houl et al. paper discussion, what examples is relavent to work i do? 

# log(a/b) = -log(b/a) = log(a) - log(b)
log(2) - log(3)
log(2/3)
-log(3/2)

# Simulating data
x <- rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
hist(x, las=3, main = "") # las rotates the axis labels, have values 0,1,2,3
hist(x)
?hist
out=NULL
class(out)
out[2]
# Calculating SE via theoretical means and Bootstraping method

set.seed(2)
x <- rnorm(50,10,2)
se_x <- sqrt(var(x)/length(x))
se_x

out <- NULL
for(i in 1:1000) {
  sample <- sample(x, replace = T)
  out[i] <- mean(sample)
}
hist(out, las=1, main="")

# 90% CI
quantile(out, c(0.025, 0.975))

# exercise: 
# plot the relationship between log SD and CV
# option 1
y <- rnorm(100, 20, 4)
sd_log <- matrix(nrow = 1000, ncol = 1)
cv <- matrix(nrow = 1000, ncol = 1)
for (i in 1:1000) {
  sample <- sample(y, replace = T)
  sd_log[i] <- sd(log(sample))
  cv[i] <- sd(sample)/mean(sample)
}
dat <- as.data.frame(cbind(sd_log, cv))
colnames(dat) <- c("sd_log", "cv")
head(dat)
?as.data.frame
plot(dat$cv, dat$sd_log, xlab="CV", ylab="SD_log")
abline(0,1) # 0 is intercept, 1 is slopt


# oystein's method:
out = matrix(NA, nrow=200, ncol=2)
sdvals = runif(200, 2, 5) # runif generates uniform distributions with between 2(min) and 5(max) in this case.
hist(sdvals)
for(i in 1:200){
  x = rnorm(200, 20, sdvals[i]) 
  cv = sd(x)/mean(x)
  sd_log = sd(log(x))
  out[i,1] = cv
  out[i,2] = sd_log
}
plot(out[,1], out[,2], xlab="CV", ylab="SD_log", las=1)
lines(0:1, 0:1)

a <- c(1,2,3,4,5,6)
sample(a,  replace = F)
