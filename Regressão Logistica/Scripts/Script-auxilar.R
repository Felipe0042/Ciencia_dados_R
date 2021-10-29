modelo_nulo <- glm(formula = atrasado ~ 1,
                   data = Atrasado,
                   family = 'binomial')

logLik(modelo_nulo)

chi2 <- -2*(logLik(modelo_nulo)-logLik(modelo_atrasos))
chi2

pchisq(chi2, df = 2 , lower.tail = F)


#########CRITERIOS##########

#AKAIKE INFO CRITERION
#QUANTO MENOR MELHOR O MODELO
AIC <- -2*(logLik(modelo_atrasos)) + 2*3
AIC


#BAYESIAN INFO CRITERION
#QUANTO MENOR MELHOR O MODELO
BIC <- -2*(logLik((modelo_atrasos))) + 3*log(100)
BIC







