# heteronormative-regimes-of-sexual-morality

library(lavaan)
library(ggplot2)
library(psych)
library(semTools)
library(semPlot)
library(igraph)
library(tidySEM)
library(dplyr)

#cfa model

kmodel <- ' factor1 =~ ac + ae + ai + aj + al + an + ao + ap + ar + ay
            factor2 =~ bb + bc + bd 
            factor3 =~ bg + bf '
fit <- cfa(model = kmodel, data = sexuality.MI.900)
summary(fit, standardized=T)
semPaths(fit, "std")
fitmeasures(fit, c("tli","cfi","srmr","rmsea" ))
graph_sem(model = fit)

lay <- get_layout("","","factor1","factor2","","",
                  "ac","ae", "ai", "ay","aj","al","an","ao", "ap", "ar","bb","bd","bc", "bg", "bf", rows = 3)
graph_sem(fit, layout = lay)

#item response theory

library(mirt)
library(stats4)
library(lattice)

model.kar <- 'karanitems = 1-33'
results.rsm <- mirt(data = irt, model = model.kar, itemtype ="rsm", verbose=F)
coef.rsm <- coef(results.rsm, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)
plot(results.rsm, type = 'trace', which.items = c(3,5,9,10,12,14,15,16,18,25,28,29,30,32,33), 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4))

#measurement invariance

#Configural inv
configural <- cfa(kmodel, data = sexuality.MI.900, group = "Sexuality", check.gradient = F)
summary(configural, fit.measures = T)

#weak/metric invariance
weak.invariance <- cfa(kmodel, data = sexuality.MI.900, group = "Sexuality", group.equal = "loadings")
summary(weak.invariance, fit.measures=TRUE)
anova(weak.invariance, configural)
fit.stats <-rbind(fitmeasures(configural, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                  fitmeasures(weak.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats) <- c("configural", "weak.invariance")
fit.stats

#strong invariance
strong.invariance <- cfa(kmodel, data = sexuality.MI.900, group = "Sexuality", group.equal = c( "loadings", "intercepts"))
summary(strong.invariance, fit.measures=TRUE)
anova(strong.invariance, weak.invariance)
lavTestScore(strong.invariance)
parTable(strong.invariance)

#freeing partials strong inv
strong.invariance.x13 <- cfa(kmodel, data = sexuality.MI.900, group = "Sexuality", group.equal =c( "loadings", "intercepts"), group.partial=c("x37 ~ 91"))
lavTestScore(strong.invariance.x13)

#freeing level 1 & 2
strong.invariance.x46x100 <- cfa(kmodel, data = sexuality.MI.900, group = "Sexuality", group.equal = c("loadings", "intercepts"), group.partial = c("x46 ~ 100", "x47~ 101"))
lavTestScore(strong.invariance.x46x100)
anova(strong.invariance.x46x100, weak.invariance)

#scalar invariance
fit.stats2 <-rbind(fitmeasures(strong.invariance, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")),
                   fitmeasures(strong.invariance.x46x100, fit.measures = c("chisq", "df", "rmsea", "srmr", "tli", "cfi", "aic")))
rownames(fit.stats2) <- c("strong", "strong with x46 x100")
fit.stats <- rbind(fit.stats, fit.stats2)
round(fit.stats, 4)
