library(Hmisc)
DEMO_A <- sasxport.get("DEMO.XPT")
DEMO_B <- sasxport.get("DEMO_B.XPT")
DEMO_C <- sasxport.get("DEMO_C.XPT")
DEMO_D <- sasxport.get("DEMO_D.XPT")
DEMO_E <- sasxport.get("DEMO_E.XPT")
DEMO_F <- sasxport.get("DEMO_F.XPT")
DEMO_G <- sasxport.get("DEMO_G.XPT")
DEMO_H <- sasxport.get("DEMO_H.XPT")
DEMO_I <- sasxport.get("DEMO_I.XPT")

write.csv(DEMO_A, file = "DEMO_A.csv")
write.csv(DEMO_B, file = "DEMO_B.csv")
write.csv(DEMO_C, file = "DEMO_C.csv")
write.csv(DEMO_D, file = "DEMO_D.csv")
write.csv(DEMO_E, file = "DEMO_E.csv")
write.csv(DEMO_F, file = "DEMO_F.csv")
write.csv(DEMO_G, file = "DEMO_G.csv")
write.csv(DEMO_H, file = "DEMO_H.csv")
write.csv(DEMO_I, file = "DEMO_I.csv")

HSQ_A <- sasxport.get("HSQ.XPT")
HSQ_B <- sasxport.get("HSQ_B.XPT")
HSQ_C <- sasxport.get("HSQ_C.XPT")
HSQ_D <- sasxport.get("HSQ_D.XPT")
HSQ_E <- sasxport.get("HSQ_E.XPT")
HSQ_F <- sasxport.get("HSQ_F.XPT")

write.csv(HSQ_A, file = "HSQ_A.csv")
write.csv(HSQ_B, file = "HSQ_B.csv")
write.csv(HSQ_C, file = "HSQ_C.csv")
write.csv(HSQ_D, file = "HSQ_D.csv")
write.csv(HSQ_E, file = "HSQ_E.csv")
write.csv(HSQ_F, file = "HSQ_F.csv")


install.packages("corrplot")
library(nnet)
library(mlogit)
library(MASS)
library(brant)
library(afex)
library(stargazer)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
my_data <- read.csv("dataPrepped.csv")
res <- cor(my_data)
write.csv(round(res, 2), file = "corrPlot.csv")


results <- multinom(as.factor(SRH) ~ female +age+ MexicanAmerican+ OtherHispanic+ NonHispanicWhite+ NonHispanicBlack+ citizenship+ X9to12NoDip+ DipOrGED+ someCollOrAA+ collGrad+ X5kTo9999+	X10kTo14999+	X15kTo19999+	X20kTo24999+	X25kTo34999+	X35kTo44999+	X45kTo54999+	X55kTo64999+	X65kTo74999+	X75kAndUp, data = my_data, Hess=TRUE)
orderdResults <- polr(as.factor(SRH) ~ female +age+ MexicanAmerican+ OtherHispanic+ NonHispanicWhite+ NonHispanicBlack+ citizenship+ X9to12NoDip+ DipOrGED+ someCollOrAA+ collGrad+ X5kTo9999+	X10kTo14999+	X15kTo19999+	X20kTo24999+	X25kTo34999+	X35kTo44999+	X45kTo54999+	X55kTo64999+	X65kTo74999+	X75kAndUp, data = my_data)
write.csv(brant(orderdResults), file = "brant.csv")
write.csv(results, file = "multinomResults")
summary(results)
stargazer(summary(results), type="text")

set_sum_contrasts()
library(car)
Anova(results,type="II")

z <- summary(results)$coefficients/summary(results)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


mldata = mlogit.data(my_data, choice = 'SRH', shape = "wide")
mlogit.results <- mlogit(as.factor(SRH) ~ female +age+ MexicanAmerican+ OtherHispanic+ NonHispanicWhite+ NonHispanicBlack+ citizenship+ X9to12NoDip+ DipOrGED+ someCollOrAA+ collGrad+ X5kTo9999+	X10kTo14999+	X15kTo19999+	X20kTo24999+	X25kTo34999+	X35kTo44999+	X45kTo54999+	X55kTo64999+	X65kTo74999+	X75kAndUp, data = my_data)
