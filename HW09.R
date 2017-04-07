# Scratch
library(ump)

umpu.binom(0:10,10,p = .2,alpha = .1)
umpu.binom(0:10,10,p = .4,alpha = .05)

seq.p = seq(0,1,by = .01)
  
px1 = dbinom(0:10,10,p =.2)
px2 = dbinom(0:10,10,p = .4)

gammas1 = umpu.binom(0:10,10,p = .2,alpha = .1)
gammas2 = umpu.binom(0:10,10,p = .4, alpha = .05)

1 - gammas1 * px1
sum(gammas1 * px1) == .1
sum(0:10 * gammas1 * px1)
########################################
par(mar = c(1.8,1.8,1.3,1.3))
n = 10
# First case
p0.1 = .2
c11 = 0
c12 = 4
alpha.1 = .1
gamma11 = .55905
gamma12 = .0815
gamma.two.11 = .4657
gamma.two.12 = .195

# Second Case
p0.2 = .4
alpha.2 = .05
c21 = 1
c22 = 7
gamma21 = .5034
gamma22 = .267
gamma.two.21 = .4702
gamma.two.22 = .2992

p = seq(0,1,by = .01)
power.seq1 = gamma11 * pbinom(c11,n,p) + gamma12* (dbinom(c12,n,p)) + (1-pbinom(c12 + 1,n,p))
power.seq.two1 = gamma.two.11 * pbinom(c11,n,p) + gamma.two.12 * (dbinom(c12,n,p)) + (1-pbinom(c12 + 1,n,p))
plot(p,power.seq, type = "l",lty = 1,lwd = 2)
lines(p,power.seq.two1,type = "l",lty = 2,lwd = 2)
power.seq2 = dbinom(0,n,p) + gamma21 * dbinom(c21,n,p) + gamma21* (dbinom(c22,n,p)) + (1-pbinom(c22 + 1,n,p))
power.seq.two2 =dbinom(0,n,p) + gamma.two.21 * dbinom(c21,n,p) + gamma.two.22 * (dbinom(c22,n,p)) + (1-pbinom(c22 + 1,n,p))
plot(p,power.seq2, type = "l",lty = 1,lwd = 2)
lines(p,power.seq.two2,type = "l",lty = 2,lwd = 2)

# I will need to review this because they drop down lower than they should.