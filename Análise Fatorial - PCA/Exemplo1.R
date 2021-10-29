pacotes <- c("tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando arquivo
arquivo1 <- load("notasfatorial.RData")

#criando tabela
notasfatorial %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#fatiando os valores
dados <- notasfatorial[1:9,2:5]

#calculando a correlação 
rho <- cor(dados)
rho
# Observando as correlações entre variáveis
chart.Correlation(notasfatorial[,2:5])

# A estatística KMO
KMO(r = rho)

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho)


#Extraindo os autovalores
eigenvalues_rho <- eigen(rho)
eigenvalues_rho$values

sum(eigenvalues_rho$values)


#variancia compartilhada
var_compartilhada <- (eigenvalues_rho$values/sum(eigenvalues_rho$values))
var_compartilhada


sum(var_compartilhada) 

principais_componentes <- 1:sum(eigenvalues_rho$values)
principais_componentes


# Juntando tudo o que temos até o momento:
data.frame(principais_componentes = paste0("PC", principais_componentes),
           eigenvalue = eigenvalues_rho$values,
           var_compartilhada = var_compartilhada) -> relatorio_eigen

# Overview dos resultados até o momento
relatorio_eigen %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# A determinação dos autovetores a partir de seus respectivos eigenvalues
eigenvalues_rho$vectors


# Estabelecendo a matriz diagonal de eigenvalues (L2)
L2 <- diag(eigenvalues_rho$values)
L2


# Assim, com os eigenvectors calculados, podemos provar que V'.rho.V = L2
prova_01 <- t(eigenvalues_rho$vectors) %*% rho %*% eigenvalues_rho$vectors
round(x = prova_01,
      digits = 14)


# Calculando os scores fatoriais
# Relembrando:
eigenvalues_rho$values
eigenvalues_rho$vectors


# De forma simplificada:
scores_fatoriais <- t(eigenvalues_rho$vectors)/sqrt(eigenvalues_rho$values)
(scores_fatoriais)


# Calculando os fatores

# O primeiro passo é o de padronização da base de dados pelo procedimento
# zscores, utilizando a função scale():

notasfatorial_std <- dados %>% 
  scale() %>% 
  data.frame()


# A seguir, vamos criar um objeto que servirá como um receptáculo para os
# k fatores (4, no caso estudado) a serem calculados:
fatores <- list()

# Agora, utilizaremos a função for
for(i in 1:nrow(scores_fatoriais)){
  fatores[[i]] <- rowSums(x = sweep(x = notasfatorial_std, 
                                    MARGIN = 2, 
                                    STATS = scores_fatoriais[i,], 
                                    FUN = `*`))
,3
  }

fatores_df <- data.frame((sapply(X = fatores, FUN = c)))
fatores_df


fatores_df %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)


# Podemos verificar que os fatores calculados, de fato, são ortogonais entre
# si, isto é, possuem correlações iguais a 0:
round(x = cor(fatores_df), 
      digits = 14)


# Combinando a base original 'notasfatorial' com o objeto 'fatores_df':
notasfatorial_final <-  cbind(dados,
                              fatores_df) %>% 
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) 

# Calculando as correlações entre as variáveis e originais e os fatores
correlacoes_entre_fatores <- cor(notasfatorial_final)






