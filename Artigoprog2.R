# Pacotes a serem instalados
install.packages("anytime")
install.packages("ggplot2")
install.packages("moments")
install.packages("devtools")
install.packages("Gran/FinTS",dependencies = T)
install.packages ("broom")




#Carregando os dados

setwd ("C:/Users/Marcos/Desktop/Econometria - Aula/Trabalho")
data <- read.csv("GLOBAL.csv")
portfolio <- read.csv("PORTFOLIO.csv")
data2 <- data[-7]/100
data3<- data2[-1]
data4<- data/sqrt(252)
portfolio2<- portfolio[-1]/100

#Criando as matrizes e manipulando os dados para criar variável

tabela <- cbind (data$Date,data3,data4$RF,portfolio2)
names(tabela)[1]<-c("Date")
names(tabela)[7]<-c("RF")
tabela$Y <- tabela$SMALL.LoBM - tabela$RF

#Só vizualizando uma pequena parte dos dados

head(tabela$Y) 


#Modificando a data nos dados

library (anytime)
tabela$Date <- anydate (tabela$Date)

#Criando uma função com as estatísticas básicas 

library(devtools)
install_github("Cran/FinTS",dependencies = TRUE)
library (FinTS)
library(moments)

stat <- function (data=x) #estou apenas dando um nome 
  
{
  ## x<-data uso isso apenas para depurar porque se manter isso sempre ele vai chamar o data quando quiser a função
  y=as.matrix( x[,-1] )
  nobs=NROW(y)
  N=NCOL(y)
  out<-matrix(0,ncol=N,nrow=6)
  for (i in 1:N){
    mean = mean(y[,i])
    std.dev = sd(y[,i])
    max=max(y[,i])
    min=min(y[,i])
    skewness=skewness(y[,i])
    kurtosis=kurtosis(y[,i])
    out[,i]=t(cbind(mean,std.dev,max,min,skewness,kurtosis))
    out=cbind(out)
  }
  colnames(out)<-colnames(y)
  rownames(out)<- c("mean","std.dev","max","min","skewness","kurtosis")
  out
}


#Apenas visualizando os valores encontrados

out
stat (tabela)
stat (data2)
stat (tabela)
stat (portfolio2)

## Imputar arquivos vistos em aula
setwd ("C:/Users/Marcos/Desktop/Econometria - Aula/Trabalho")
source("aulastat.R") 
stat(tabela)

table<-round(stat(tabela),digits=3)

table

write.table(table,file='muni-am1.csv',sep='\t',na="",quote=FALSE)

out

# Criando gráficos 

library(ggplot2)
plot_list<-list()
for (i in 2:ncol(tabela))  {
  plott<-ggplot(tabela, aes(x=tabela$Date, y=tabela[,i])) + 
    geom_line() +
    ggtitle(colnames(tabela[i])) + ##Título do gráfico é o nome da minha coluna
    xlab("Date") + 
    ylab(colnames(tabela[i])) + 
    theme(plot.title= element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%Y",date_breaks = "2 year")
  plot_list[[i]]<-plott
  ggsave (plott, file=paste0("image/",colnames(tabela[i]),".png"),width=14,height=10,units="cm") ##eu tenho que criar a pasta image antes
}

# Rodando a regressão

modelo <- lm(tabela$Y ~ tabela$Mkt.RF + tabela$SMB + tabela$HML +tabela$RMW + tabela$CMA)
summary(modelo)
teste<-summary(modelo)

plot(tabela$Y~tabela$Mkt.RF,col="lightblue")
abline(lm(tabela$Y~tabela$Mkt.RF))

teste
library(broom)
tidy(modelo)
teste2<-tidy(modelo)
teste2
write.table(teste2,file='teste3.xls',sep='\t',na="",quote=FALSE)


# Rodadando regressão na forma matricial

nobs <- length(tabela$Y)
Y <- tabela$Y
X <- cbind(1,tabela$Mkt.RF,tabela$SMB, tabela$HML,tabela$RMW,tabela$CMA)
   #gerando a X'X
XX <- t(X)%*%X
XY <- t(X)%*%Y
    # matriz inversa de X'X = (X'X)^-1
invXX<-solve(XX)
b<-invXX%*%XY 
e<-Y-X%*%b
M<-diag(1,nobs,nobs)-X%*%invXX%*%t(X)


## Regressão - gráficos da variável dependente contra a independente


ggplotRegression <- function(fit,beta){
  
  require(ggplot2) ##ou library(ggplot)?
  plot <- ggplot(fit$model, aes(x = fit$model[,beta], y = fit$model[,1]))+
    geom_point() + 
    stat_smooth(method = "lm", col="red") +
    ggtitle(paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                  "Intercept =",signif(fit$coef[[1]],5 ),
                  "Slope =",signif(fit$coef[[beta]], 5),
                  "P =",signif(summary(fit)$coef[beta,4], 5)))+
    xlab(colnames(fit$model[beta])) + ylab(colnames(fit$model[1]))
  print(plot)
}

ggplotRegression(modelo,2)
ggplotRegression(modelo,3)
ggplotRegression(modelo,4)
ggplotRegression(modelo,5)
ggplotRegression(modelo,6)
hl<-hist(tabela$RF)
plot(hl)
qplot(tabela$RF,
      geom="histogram",
      xlab = "Rf") +
      geom_vline(xintercept=mean(tabela$RF), lwd=1, linetype=1, color="blue")
qplot(tabela$SMALL.LoBM,
      geom="histogram",
      xlab = "SMALL.LoBM") +
  
  theme_bw()+
      geom_vline(xintercept=mean(tabela$SMALL.LoBM), lwd=1, linetype=1, color="blue")



