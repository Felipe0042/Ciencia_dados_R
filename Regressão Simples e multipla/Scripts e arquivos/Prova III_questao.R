prova <- predict(object = modelo_bc,
        data.frame(idade = 20),
        interval = "confidence", level = 0.95)

prova[2]


bebes$yhat_linear <- modelo_linear$fitted.values
bebes$yhat_modelo_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                            1))^(1/(lambda_BC$lambda))

lambda_BC$lambda

(prova[2] * lambda_BC$lambda + 1) ^ ((1/ lambda_BC$lambda))
