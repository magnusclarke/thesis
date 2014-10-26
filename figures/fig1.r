# Script for plots of values and variance under BM-like models
# For PhD literature review
# Magnus
# 8.11.13

####################################################
####	Model function definitions
####################################################

dt	<- 0.1
t      	<- seq(0, 100, by=dt)

BM 	<- function(sigma)
{
	X<- t
	dx	<- 0
	for (i in 2:length(t))
	{
        	dx      <- sigma * rnorm(1, mean = 0, sd = dt)
        	X[i] <- X[i-1] + dx 
	}
	return(X)
}

kappa 	<- function(sigma, kappa)
{
	X<- t
	dx	<- 0
	for (i in 2:length(t))
	{
        	dx      <- kappa *
				t[i]^(kappa-1) *
				sigma * 
				rnorm(1, mean = 0, sd = dt)
        	X[i] 	<- X[i-1] + dx 
	}
	return(X)
}

ACDC 	<- function(sigma, g)
{
	X<- t
	dx	<- 0
	for (i in 2:length(t))
	{
        	dx      <- ( g ^ -t[i] ) *
				sigma * 
				rnorm(1, mean = 0, sd = dt)
        	X[i] 	<- X[i-1] + dx 
	}
	return(X)
}


OU 	<- function(sigma, alpha, psi)
{
	X<- t
	dx	<- 0
	for (i in 2:length(t))
	{
        	dx      <-  -alpha * (X[i-1] - psi) * dt +
				sigma * rnorm(1, mean = 0, sd = dt)
        	X[i] 	<- X[i-1] + dx 
	}
	return(X)
}


####################################################
####	Plotting simulations
####################################################

png("sim.png", width=800, height=480)
	par(cex.lab=1.5, oma=c(2,2,2,2))
	plot(BM(3), type="l", lwd=2,
		xlab="Time",
		ylab="Trait value", 
		xaxt='n', yaxt='n')
	#lines(kappa(3, 0.9), col="red")
	#lines(ACDC(3, 1.05), col="blue")
	lines(OU(3, 0.6, 5), lwd=2, col="red")
dev.off()


####################################################
####	Plotting variances
####################################################

# BM
BM1	<- BM(5)
BMS	<- matrix(nrow=1000, ncol=length(BM1))
vary	<- 1:length(BM1)
for(i in 1:1000){
	BM2	<- BM(5)
	BMS[i,]	<- BM2
}
for (i in 1:length(BM1)){
	vary[i]	<- var(BMS[,i])
}

# kappa
kap1	<- kappa(25, 0.60)
kap	<- matrix(nrow=1000, ncol=length(kap1))
vkap	<- 1:length(kap1)
for(i in 1:1000){
	kap2	<- kappa(25, 0.60)
	kap[i,]	<- kap2
}
for (i in 1:length(kap1)){
	vkap[i]	<- var(kap[,i])
}

# OU with different optima
ou1	<- OU(3, 0.1, 20)
oum	<- matrix(nrow=1000, ncol=length(ou1))
vou	<- 1:length(kap1)
for(i in 1:1000){
	ou2	<- OU(3, 0.1, sample(1:40, 1))
	oum[i,]	<- ou2
}
for (i in 1:length(ou1)){
	vou[i]	<- var(oum[,i])
}

# OU with same optimum
ou1	<- OU(10, 0.02, 250)
oum	<- matrix(nrow=1000, ncol=length(ou1))
vou2	<- 1:length(ou1)
for(i in 1:1000){
	ou2	<- OU(10, 0.02, 250)
	oum[i,]	<- ou2
}
for (i in 1:length(ou1)){
	vou2[i]	<- var(oum[,i])
}

# NF variance
NF		<- t
NF[1:10]	<- 0
NF[11:1001]	<- 180

# Plot
png("var.png", width=800, height=600)
	par(cex.lab=1.5, oma=c(2,2,2,2))
	plot(vary, type="l", lwd=2,
		xlab="Branch length",
		ylab="Trait variance", 
		xaxt='n', yaxt='n')
	lines(vkap, lwd=2, col="red")
	lines(vou, lwd=2, col="blue")
	lines(NF, lwd=2, col="green")
dev.off()


