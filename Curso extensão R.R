library(foreign)
setwd("C://Users/eucli/Google Drive/Docência/Curso Extensão R/")
setwd("C://Users/eucli/Google Drive/Projeto Mestrado/Analises IDADI/")


bebes <- read.spss("IDADI (Curso R).sav",to.data.frame = T, use.value.labels = T,)
idadi <- read.spss("IDADI (Submissão).sav",to.data.frame = T, use.value.labels = T)
alturas <- read.spss("Alturas(curso R).sav",to.data.frame = T, use.value.labels = T) 
str(alturas)
levels(idadi$Age_Norms)

describeBy(idadi$COG, idadi$Idade_Norms)


Correcao_IDADI <- function(Faixa_etaria, Escore){
  z <- (Escore - mean(idadi[idadi$Idade_Norms == Faixa_etaria,]$COG, na.rm = T))/
    sd(idadi[idadi$Idade_Norms == Faixa_etaria,]$COG, na.rm = T)

  if(z < -1.2){
    cat("O nível de desenvolvimento da criança foi", z,"\nportanto, severamente abaixo da média. \nSe não houver diagnóstico formal, ela deverá ser encaminhada\nimediatamentepara avaliação a fim de aproveitar de \nmelhores cuidados médicos")
  }else{
    cat("O nível de desenvolvimento da criança foi de", z, "\n","Tal resultado aponta boa capacidade cognitiva")
  }}
  

KG <- c(96,95.5,94,93.7)
pontos <- c(1,2,3,4)
meses <- c("Dezembro","Janeiro", "Fevereiro", "Março")

Dieta <- function(kg,mês){
KG <<- c(KG, kg)
pontos <<- c(pontos,max(pontos)+1)
meses <<- c(meses, mês)
    plot(KG, x = pontos,type = "o", ylab = "Peso em KG", xlab = "Mês",ylim = c(88,98), ann=FALSE, axes = FALSE)
  axis(1,at = pontos,label = meses)
  axis(2,las=1,at=90:98)
  title(paste("Vamos lá Cléds! Só faltam", round(kg-88, digits = 2), "quilos!"))  
}

Dieta(mês = "Abril",kg = 92)

plot(idadi$COG_Dev ~ idadi$Age, col = idadi$Age_Norms)
abline(lm(idadi$COG_Dev ~ idadi$Age))
identify(x = idadi$Age, y = idadi$COG_Dev,labels = idadi$Which.diagnosis)



QI <- function(escore){
  if(escore<70){
    "Possível deficiência intelectual"
    }else if(escore >90|escore<110){
        "Escore mediano"
      }else{
          "Escore acima da média"}}

QI(escore = 50)


combustivel <- function(alcool, gasolina){
  proporcao <- alcool/gasolina
  if(proporcao>.7){"Gasolina"}else{"Alcool"}
}


combustivel(gasolina = 4,alcool = 3)


#### Aula 3 ####

# Inferenciais ####

cor(alturas[,2:4])
mean(alturas$altura_mae)

by(alturas$altura,list(alturas$genero, alturas$AlturaDicotomicaMae), describe)
leveneTest(alturas$altura, alturas$genero)
interaction(alturas$genero, alturas$AlturaDicotomicaMae)

alturas$genero <- factor(alturas$genero, labels = c("fem","masc"))
as.numeric(alturas$genero)

alturas$AlturaDicotomicaMae <- rep(NA,100)
alturas[alturas$altura_mae > 160.78,]$AlturaDicotomicaMae <- "Alta"
alturas[alturas$altura_mae < 160.78,]$AlturaDicotomicaMae <- "Baixa"

t.test(alturas$altura ~ alturas$AlturaDicotomicaMae)
t.test(alturas$altura ~ alturas$genero,var.equal =TRUE)


regAltura <- lm(alturas$altura ~alturas$genero )

plot(x = as.numeric(alturas$genero)-1, y = alturas$altura)
abline(lm(lm(alturas$altura ~alturas$genero )))

summary(regAltura)
cor(predict.lm(regAltura),alturas$altura)


# Stepwise Regression
library(MASS)

fitRegStepAltura <- lm(alturas$altura ~ as.numeric(alturas$nome) + alturas$altura_pai + alturas$altura_mae + alturas$genero)
step <- stepAIC(fitRegStepAltura,direction = 'backward')
step$anova
fitRegStepAltura2 <- lm(alturas$altura ~ alturas$altura_pai + alturas$altura_mae + alturas$genero)

summary(fitRegStepAltura2)


hist(alturas$Residuos)
regAltura$residuals
alturas$PreditosReg <- predict.lm(regAltura)
alturas$Residuos <- regAltura$residuals
ggplot(alturas, aes(x = Residuos, col=genero))+geom_density()

durbinWatsonTest(fitRegStepAltura)
vif(fitRegStepAltura)
1/vif(fitRegStepAltura)

fitcog <- lm(idadi$COG_Dev ~idadi$Age + idadi$Exp_Language_Dev + idadi$Socioemotional_Dev)
1/vif(fitcog)
durbinWatsonTest(fitcog)

par(mfrow=c(2,2))

plot(fitRegStepAltura2)

# bootstrap
library(boot)
library(car)






ggplot(alturas, aes(y = altura, x = AlturaDicotomicaMae, col = genero))+ 
  stat_summary(fun.y = mean, geom = "line",aes(group=genero)) + stat_summary(fun.y = mean, geom="point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",, width=0.15) 



#Anova
library(QuantPsyc)
library(car)
library(multcomp)
library(pastecs)
library(psych)
library(pgirmess)
library(Hmisc)
stat.desc()

regCog <- lm(bebes$COG_Dev ~ bebes$Age_band)
summary(regCog)
plot(x=as.numeric(bebes$Age_band)-1,y=bebes$COG_Dev)
abline(lm(bebes$COG_Dev ~ bebes$Age_band))
table(bebes$Age_band)

anova(regCog)
by(bebes$COG_Dev,INDICES = list(bebes$Age_band, describe), describe)


friedman.test(as.matrix(cbind(alturas$altura,alturas$genero,alturas$AlturaDicotomicaMae)))

friedmanmc(as.matrix(cbind(alturas$altura,as.numeric(alturas$genero),as.numeric(alturas$AlturaDicotomicaMae))))




Bancão <- MediaPoluente(pollutant = "nitrate")

rcorr(cbind(alturas$altura, alturas$altura_pai, alturas$altura_mae),type = "spearman")
rcorr(cbind(rank(alturas$altura), rank(alturas$altura_pai), rank(alturas$altura_mae)))

idadi <- idadi[idadi$Idade_filho<80,]
idadi$Quadratico <- idadi$Idade_filho^2
plot(idadi$ ~ idadi$Idade_filho)
regQuad <- lm(idadi$DominioCog ~ idadi$Idade_filho + idadi$Quadratico)

ponto <- predict(regQuad,newdata = list(Idade_filho =seq(0,100,0.01), Quadratico=seq(0,100,0.01)^2))
lines(x = idadi$Idade_filho,predict.lm(regQuad,list(Idade_filho =seq(0,100,0.001), Quadratico=seq(0,100,0.001)^2)))

library(boot)

bootReg <- function (formula, data, indices)
{
  d <- data [indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

idadi$Idade_filho

BootResults <- boot(statistic = bootReg,data = idadi,formula = COG ~ Idade_filho , R = 2500)

boot.ci(BootResults,type = "bca",index = 1)
boot.ci(BootResults,type = "bca",index = 2)


#bias corrected and accelerated, type = "bca"


# Finalizando....


dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l", main="OBRIGADO PELA PARTICIPAÇÃO!"))
with(dat, polygon(x,y, col="red"))  



MediaPoluente <- function(pollutant, id = 1:332){
  list_files <- list.files(full.names = TRUE)
  data <- data.frame()
  for (id in id){
    data <- rbind(data, read.csv(list_files[id]))
    
  }
  
  print(mean(data[,pollutant], na.rm=TRUE))
  return(data)
}