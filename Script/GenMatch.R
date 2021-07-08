# GenMatch

library(Matching)
data("lalonde")

Y <- lalonde$re78
Tr <- lalonde$treat
glm1 <-
  glm(
    Tr ~ age + educ + black + hisp + married + nodegr + re74 + re75,
    family = "binomial",
    data = lalonde
  )


rr1 <- Match(Y = Y, Tr = Tr, X = glm1$fitted.values)

# Compare balance 

MatchBalance(Tr ~ nodegr, match.out = rr1,nboots = 1000,data = lalonde)
MatchBalance(Tr ~ re74, match.out = rr1,nboots = 1000,data = lalonde)

# Perform GenMatch

X <-
  cbind(
    lalonde$age,
    lalonde$educ,
    lalonde$black,
    lalonde$hisp,
    lalonde$married,
    lalonde$nodegr,
    lalonde$re74,
    lalonde$re75,
    lalonde$u74,
    lalonde$u75
  )

BalanceMatrix <- 
  cbind(
    lalonde$age,
    lalonde$educ,
    lalonde$black,
    lalonde$hisp,
    lalonde$married,
    lalonde$nodegr,
    lalonde$re74,
    lalonde$re75,
    lalonde$u74,
    lalonde$u75
  )

gen1 <-GenMatch(Tr = Tr, X=X, pop.size = 1000)
mgen <- Match(Y = Y, Tr = Tr, X = X, Weight.matrix = gen1)
