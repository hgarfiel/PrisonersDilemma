#Project 5 
#Hunter Garfield
#ECON 524

###This program takes a long time to run (on my laptop with 16GB RAM),
###because I create loops in a function that I use for nlminb.
###bootstrapping the standard errors takes up the most amount of time by far. 
###to speed it up, set m = to a smaller number (this is the number of times the program estimates the parameters before calculating sd)
### 13min for m=1000, ~6min for m=500

m = 500

setwd("~/Documents/MSE/ECON524")
dat = read.csv("pddata.csv")
n = length(dat$choice)
chooseA = numeric(n)
chooseB = numeric(n)

#creates binary vectors for an individuals choice
for( i in 1:n) {
  if(dat$choice[i]==1){
    chooseB[i] = 1
    chooseA[i] = 0
  }
  else {
    chooseB[i] = 0
    chooseA[i] = 1
  }
}

dat = cbind(dat$choice, chooseA, chooseB, dat[,3:8])

#initializes vectors to hold propensities
Aa = numeric(n)
Ab = numeric(n)
seq100 = seq(from = 1, to = 2001, by = 100)

j = 1 #calculates propensisties for one parameter case
i = 1
while(i <= n) {
  if(i == seq100[j]) {
    Aa[i] = 0
    Ab[i] = 0
    i = i+1
    j = j+1
  }
  else{
    Aa[i] = Aa[i-1] + dat$chooseA[i-1]*dat$payoff[i-1]
    Ab[i] = Ab[i-1] + dat$chooseB[i-1]*dat$payoff[i-1]
    i = i+1
  }
}

ll_rl = function(p){
 -sum(p[1]*chooseA*Aa+p[1]*chooseB*Ab-log(exp(p[1]*Aa)+exp(p[1]*Ab)))
}
#log likelihood function for one parameter case

phat = c(1,1)
res = nlminb(phat, ll_rl, lower = c(0, 0),upper = c(Inf, 1))
result = res$par
llresult = res$objective

phat = c(1,1) # this function recalculates propensisties and calls log likelihood for two parameter case
ll_rl2 = function(p) {
  i=1
  j=1
  while(i <= n) {
    if(i == seq100[j]) {
      Aa[i] = 0
      Ab[i] = 0
      i = i+1
      j = j+1
    }
    else{
      Aa[i] = p[2]*Aa[i-1] + dat$chooseA[i-1]*dat$payoff[i-1]
      Ab[i] = p[2]*Ab[i-1] + dat$chooseB[i-1]*dat$payoff[i-1]
      i = i+1
    }
  }
-sum(p[1]*chooseA*Aa+p[1]*chooseB*Ab-log(exp(p[1]*Aa)+exp(p[1]*Ab)))
}

res2 = nlminb(phat, ll_rl2, lower = c(0, 0),upper = c(Inf, 1))
result2 = res2$par
llresult2 = res2$objective

#creates same functions used above, but use samples instead of given data
ll_rl_sam = function(p){
  -sum(p[1]*chAsam*Aasam+p[1]*chBsam*Absam-log(exp(p[1]*Aasam)+exp(p[1]*Absam)))
}

ll_rl_sam2 = function(p) {
  i=1
  j=1
  while(i <= n) {
    if(i == seq100[j]) {
      Aasam[i] = 0
      Absam[i] = 0
      i = i+1
      j = j+1
    }
    else{
      Aasam[i] = p[2]*Aasam[i-1] + chAsam[i-1]*paysam[i-1]
      Absam[i] = p[2]*Absam[i-1] + chBsam[i-1]*paysam[i-1]
      i = i+1
    }
  }
  -sum(p[1]*chAsam*Aasam+p[1]*chBsam*Absam-log(exp(p[1]*Aasam)+exp(p[1]*Absam)))
}

ests = matrix(nrow = m, ncol = 2) #initializes vectors to store estimates based on sample
ests2 = matrix(nrow = m, ncol = 2)
for(i in 1:m){
  samv = numeric(n) #initializes a vector to store sample indexes
  for(j in 1:20) {
    ind = round(runif(1, min = 2, max = 20)) #picks a random person
    sam = c(seq100[(ind-1)]:seq100[ind]) #creates indices of picked person's data
    samv = c(samv, sam[-101]) #adds created indices to previously created ones
  }
  samv = samv[-c(1:2000)] #cleans sample indeces 
  Aasam = Aa[samv] #sets variables for the function
  Absam = Ab[samv]
  chAsam = dat$chooseA[samv]
  chBsam = dat$chooseB[samv]
  paysam = dat$payoff[samv]
  samres = nlminb(phat,ll_rl_sam, lower = c(0, 0),upper = c(Inf, 1))
  ests[i,] = samres$par
  samres2 = nlminb(phat,ll_rl_sam2, lower = c(0, 0),upper = c(Inf, 1))
  ests2[i,] = samres2$par
}
serror = sd(ests[,1]) #calculates and stores standard errors
serror2 = c(sd(ests2[,1]),sd(ests2[,2]))

