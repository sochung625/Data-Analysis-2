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

M_trans <- lm( I(sqrt(Y)) ~ (.)^2, data=Dat )

summary(M_raw)$adj.r.square;
[1] 0.4589324

summary(M_trans)$adj.r.square
[1] 0.4593001

plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

##stepwise regression
M <- regsubsets( model.matrix(M_trans)[,-1], I(sqrt(Dat$Y)),nbest = 1 , nvmax=5,method = 'forward', intercept = TRUE )

temp <- summary(M)
Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1,function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),caption='Model Summary')
##
|model                                 |adjR2             |BIC               |
|:-------------------------------------|:-----------------|:-----------------|
|(Intercept)+E1:E2                     |0.38524916087397  |-549.81886221196  |
|(Intercept)+E1:E2+G10:G15             |0.430856162944456 |-632.953461876042 |
|(Intercept)+E2+E1:E2+G10:G15          |0.452547636462171 |-671.861316460639 |
|(Intercept)+E1+E2+E1:E2+G10:G15       |0.459448155561175 |-680.488063244129 |
|(Intercept)+E1+E2+E1:E2+G1:G7+G10:G15 |0.462160593513677 |-680.259585543499 |

# The decrease from teh 4th to 5th model is much smaller than the other decreases.
# I choose 4th model as a candidate model.
# Assume that sqrt(Y) = β0+β1E1+β2E2+β3E1E2+β4G10G15+ε

##########################################################
#Q1: Is my asumption is correct? I don't think it is, how should I assume it based on what?
##########################################################

M_main <- lm( I(sqrt(Y)) ~ ., data=Dat)
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')
##
|            |  Estimate| Std. Error|   t value| Pr(>&#124;t&#124;)|
|:-----------|---------:|----------:|---------:|------------------:|
|(Intercept) | 3.0310012|  0.0395446| 76.647588|                  0|
|E1          | 0.0320679|  0.0020110| 15.946455|                  0|
|E2          | 0.0495500|  0.0020101| 24.649993|                  0|
|G10         | 0.0488831|  0.0070890|  6.895586|                  0|
|G15         | 0.0479475|  0.0072676|  6.597396|                  0|

##
M_2nd <- lm( I(sqrt(Y)) ~ (.)^2, data=Dat)
temp  <- summary(M_2nd)

##########################################################
#Q2: If I hit <= 0.005, the result table seems to be not helping, instead 0.01 it shows intercepts, but it which is ironic to me because in the sig coefficient table there is no G17 nor G18.
#    So, I have no idea of how interprete this.
##########################################################

kable(temp$coefficients[ abs(temp$coefficients[,4]) <=  .005, ], caption='2nd Interaction')
##
|                   |         x|
|:------------------|---------:|
|Estimate           | 2.4658799|
|Std. Error         | 0.4513760|
|t value            | 5.4630282|
|Pr(>&#124;t&#124;) | 0.0000001|

kable(temp$coefficients[ abs(temp$coefficients[,4]) <=  .01, ], caption='2nd Interaction')
##
|            |  Estimate| Std. Error|  t value| Pr(>&#124;t&#124;)|
|:-----------|---------:|----------:|--------:|------------------:|
|(Intercept) | 2.4658799|  0.4513760| 5.463028|          0.0000001|
|G17:G18     | 0.0585186|  0.0213558| 2.740176|          0.0062685|

##########################################################
#Q3: which one should I select between below two codes?
##########################################################

M_2stage <- lm( I(sqrt(Y)) ~ (E1+E2+G10+G15+G17+G18)^3, data=Dat)
M_2stage <- lm( I(sqrt(Y)) ~ (E1+E2+G10+G15)^3, data=Dat)

temp <- summary(M_2stage)

##########################################################
#Q4: If I choose first line, my final model would be
#    Y= (β0+β1*E2*G15+β2G10G15G17)^2?

#Q5: How should I choose t-value? For me, 4 is too high to show the result, so I tried all the numbers that shows the intercept table. It turns out 1.71 is the maxvalue that shows the intercepts.

#Q6: If this is correct, is my final model three factor interaction?
##########################################################

#Option 1.
>M_2stage <- lm( I(sqrt(Y)) ~ (E1+E2+G10+G15+G17+G18)^3, data=Dat)
> temp$coefficients[ abs(temp$coefficients[,3]) >= 1.71, ]
##
              Estimate Std. Error   t value     Pr(>|t|)
(Intercept)  3.4521339 0.41248901  8.369033 1.721292e-16
E2:G15       0.0578750 0.03360228  1.722354 8.528279e-02
G10:G15:G17 -0.1114802 0.05469790 -2.038108 4.177474e-02

##########################################################
#Q7: What should I choose as the significant level among this(1.3, 1.5 and 1.65)?
##########################################################

#Option 2.
>M_2stage <- lm( I((Y)) ~ (E1+E2+G10+G15)^3, data=Dat)

> temp$coefficients[ abs(temp$coefficients[,3]) >= 1.3, ]
               Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 12.01424038 2.15995262  5.562270 3.314928e-08
G15         -2.93303426 1.84058426 -1.593534 1.113171e-01
E1:E2        0.04980750 0.03246820  1.534040 1.252969e-01
E1:G15       0.31862194 0.22594333  1.410185 1.587573e-01
E2:G15       0.35133940 0.20972953  1.675202 9.416831e-02
E1:E2:G15   -0.03697005 0.02588873 -1.428036 1.535549e-01

> temp$coefficients[ abs(temp$coefficients[,3]) >= 1.5, ]
              Estimate Std. Error   t value     Pr(>|t|)
(Intercept) 12.0142404  2.1599526  5.562270 3.314928e-08
G15         -2.9330343  1.8405843 -1.593534 1.113171e-01
E1:E2        0.0498075  0.0324682  1.534040 1.252969e-01
E2:G15       0.3513394  0.2097295  1.675202 9.416831e-02

> temp$coefficients[ abs(temp$coefficients[,3]) >= 1.65, ]
              Estimate Std. Error  t value     Pr(>|t|)
(Intercept) 12.0142404  2.1599526 5.562270 3.314928e-08
E2:G15       0.3513394  0.2097295 1.675202 9.416831e-02

finalmodel <- lm(sqrt(Y)~ (E1+E2+G10+G15), data=Dat)
anova(finalmodel)

Response: sqrt(Y)
            Df  Sum Sq Mean Sq F value    Pr(>F)
E1           1  2.5776  2.5776 272.573 < 2.2e-16 ***
E2           1  5.7499  5.7499 608.039 < 2.2e-16 ***
G10          1  0.4183  0.4183  44.231 4.499e-11 ***
G15          1  0.4359  0.4359  46.100 1.795e-11 ***
Residuals 1152 10.8938  0.0095
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
