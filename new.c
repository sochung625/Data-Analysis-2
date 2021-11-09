library(MASS)
library(leaps)
library(knitr)

getwd()
[1] "/Users/soyoungchung"

wdir <- "/Users/soyoungchung/Desktop/AMS315Project2"
setwd(wdir)
getwd()
[1] "/Users/soyoungchung/Desktop/AMS315Project2"

Dat <- read.csv('P2_46738.csv', header = TRUE)
M_E <- lm(Y ~ E1+E2+E3+E4, data=Dat)
summary(M_E)
## result
Call:
lm(formula = Y ~ E1 + E2 + E3 + E4, data = Dat)
Residuals:
    Min      1Q  Median      3Q     Max
-3.1695 -0.4417  0.0320  0.4882  2.1954

Coefficients:
             Estimate Std. Error t value Pr(>|t|)
(Intercept)  9.484490   0.226628  41.850   <2e-16 ***
E1           0.241296   0.015332  15.739   <2e-16 ***
E2           0.366165   0.015366  23.830   <2e-16 ***
E3          -0.001957   0.015219  -0.129    0.898
E4          -0.010313   0.015393  -0.670    0.503
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7487 on 1152 degrees of freedom
Multiple R-squared:  0.4166,    Adjusted R-squared:  0.4146
F-statistic: 205.7 on 4 and 1152 DF,  p-value: < 2.2e-16

## Assume that I only have up to 2nd orderinteractions.
## The command for a model reflecting this assumption is:

M_raw <- lm( Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=Dat)

plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')
boxcox(M_raw)

M_trans <- lm( I((Y)^1.8) ~ (.)^2, data=Dat )

summary(M_raw)$adj.r.square;
[1] 0.4589324

summary(M_trans)$adj.r.square
[1] 0.4572723

plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

##stepwise regression
M <- regsubsets( model.matrix(M_trans)[,-1], I((Dat$Y^1.8)),nbest = 1 , nvmax=6,method = 'forward', intercept = TRUE )

temp <- summary(M)
Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1,function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),caption='Model Summary')
##

|model                                       |adjR2             |BIC               |
|:-------------------------------------------|:-----------------|:-----------------|
|(Intercept)+E1:E2                           |0.390373046050379 |-559.502751925128 |
|(Intercept)+E1:E2+G10:G15                   |0.435262243664702 |-641.945337362752 |
|(Intercept)+E2+E1:E2+G10:G15                |0.45688143228578  |-681.05691854161  |
|(Intercept)+E1+E2+E1:E2+G10:G15             |0.461159680760704 |-684.157232582379 |
|(Intercept)+E1+E2+E1:E2+G1:G7+G10:G15       |0.463936186344419 |-684.085559315933 |
|(Intercept)+E1+E2+E1:E2+G1:G7+G3:G4+G10:G15 |0.464999681083313 |-680.335270690318 |


M_main <- lm( I((Y)^1.8) ~ ., data=Dat)
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.01, ], caption='Sig Coefficients')
##
|            |  Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|
|:-----------|---------:|----------:|---------:|------------------:|
|(Intercept) | 37.783591|  4.3200116|  8.746178|                  0|
|E1          |  3.505171|  0.2196870| 15.955295|                  0|
|E2          |  5.451874|  0.2195956| 24.826876|                  0|
|G10         |  5.224345|  0.7744352|  6.746007|                  0|
|G15         |  5.345126|  0.7939450|  6.732364|                  0|

##
M_2nd <- lm( I((Y)^1.8) ~ (.)^2, data=Dat)
temp  <- summary(M_2nd)

kable(temp$coefficients[ abs(temp$coefficients[,4]) <=  .005, ], caption='2nd Interaction')


| Estimate| Std. Error| t value| Pr(>&#124;t&#124;)|
|--------:|----------:|-------:|------------------:|

M_2stage <- lm( I((Y)^1.8) ~ (E1+E2+G10+G15)^3, data=Dat)

temp <- summary(M_2stage)

temp$coefficients[ abs(temp$coefficients[,3]) >= 1.5, ]
               Estimate Std. Error   t value   Pr(>|t|)
(Intercept)  92.8316078 31.8295252  2.916525 0.00360875
G15         -41.5835330 27.1232445 -1.533133 0.12552013
E1:E2         0.8279355  0.4784583  1.730424 0.08382477
E2:G15        4.9654554  3.0906194  1.606621 0.10841388

finalmodel <- lm((Y^1.8)~ (E1+E2+G15), data=Dat)
anova(finalmodel)

Analysis of Variance Table

Response: (Y^1.8)
            Df Sum Sq Mean Sq F value    Pr(>F)
E1           1  30862   30862 263.480 < 2.2e-16 ***
E2           1  69595   69595 594.156 < 2.2e-16 ***
G15          1   5049    5049  43.102 7.839e-11 ***
Residuals 1153 135053     117
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
>
>
