# PhD. Jose Wesley lima Silva
# Anova Medidas Repetidas

#Montando o Banco de dados
altura <- data.frame(Clone = c("C11", "C15", "C21", "C25", "C33", "C37", "C39", "C41"),
                     I18 = c(6.5, 6.6, 6, 5.9, 6.9, 5.9, 6.5, 6.2),
                     I24 = c(8.4, 8, 7.8, 7.6, 8.9, 8.3, 8.1, 8.2),
                     I30 = c(10, 9.2, 9, 9.3, 10.4, 9.6, 9.7, 9.4),
                     I36 = c(11, 9.9, 9.8, 10.2, 11.3, 10.4, 10.5, 10.2)) 


#Caso não tenha os pacotes instalados descomente os comandos abaixo
#install.packages("dplyr", dependencies = T)
#install.packages("tidyr", dependencies = T)

#Carregando os pacotes necessários
library(dplyr)
library(tidyr)

#Com o código abaixo estamos pedindo para a função pivot_longer receber 
#a base de dados altura, selecionar as colunas com os valores de altura
#nas diferentes idades e coloca-los um abaixo do outro para todos os 
#clones, estamos cirando duas coluns que recebem as identificações da 
#Idade e os valores da Altura.
altura.l <- altura %>% 
    pivot_longer(cols = 2:5, names_to = "Idade", values_to = "Altura")

#Vamos verificar o tipo de cada coluna
str(altura.l)

#A primeira coluna é do tipo "fator", a coluna com a identificação das alturas
#é do tipo "character" e a coluna com os valores das altura é do tipo "númerico"
#Para proseguir-mos com a análise temos que transformar a coluna Idade para o 
#tipo "fator"
altura.l$Idade <- factor(altura.l$Idade)

#Anova medidas repetidas, a estrutura é semelhante a anova independete a não ser pelo fator
#que adicionamos o termo Error(Clone) ou Error(Clone/Idade) para informar para o modelo que clone possui medidas
#repetidas.
anova.r1 <- aov(Altura ~ Idade + Error(Clone/Idade), data = altura.l)
summary(anova.r1)

#Esfericidade
mauchly.test(lm(as.matrix(altura[2:5]) ~ 1), X = ~1)

#Teste de Shapiro-Wilk
shapiro.test(residuals(anova.r1$`Clone:Idade`, type = "response"))

#Caso não possua o pacote instalado na sua máquina execute o comando abaixo
#install.packages("ggplot2", dependencies = T)
library(ggplot2)
#Lembre que a função ggplot só constroi gráficos com base em dados no
#formato data frame, por isso, montamos um data frame com os dados

#Teste do Pressuposto de Homogeneidade de Variâncias
ggplot(data = data.frame(Real = altura.l$Altura[9:32], 
                         Res = resid(anova.r1$`Clone:Idade`)/mean(altura.l$Altura)*100),
       aes(x = Real, y = Res)) +
    geom_point() + 
    labs(x = "Altura Real (m)", y = "Resíduos (%)") +
    ylim(-10, 10) +
    theme_classic()


#Testar a ausência de outliers com o gráfico de Box-Plot
ggplot(data = altura.l, aes(x = Idade, y = Altura, color = Idade)) +
    geom_boxplot(notch=TRUE) +
    scale_color_grey() +
    labs(y = "Altura Real (m)", x = "Idade (meses)") +
    theme_classic() +
    theme(legend.position="none") 

#Consultando os constrastes
contrasts(altura.l$Idade)

#Criando contasrtes ortogonais
I1824vsI3036 <- c(-1, -1, 1, 1)
I18vsI24 <- c(-1, 1, 0, 0)
I30vsI36 <- c(0, 0, -1, 1)

#Atribuindo os contrastes a variável Idade
contrasts(altura.l$Idade) <- cbind(I1824vsI3036, I18vsI24, I30vsI36)

#Visualizando os contrastes criados
contrasts(altura.l$Idade)

#Contrastes de Helmert
contr.helmert(4)

#Contrastes de Polinomiais
contr.poly(4)

#Contrastes Aditivos
contr.sum(4)

#Contrastes Semelhantes aos definidos pelo SAS
contr.SAS(4)

#Contrastes Semelhantes aos definidos pelo SAS, mas escolhendo o grupo de 
#tratamento que será utilizado como grupo inicial
contr.treatment(4, base = 1)

#Instale o pacote rstatix, descomente a linha abaixo
#install.packages("rstatix", dependencies = T)

#Carregue o pacote
library(rstatix)

#Anova de medidas repetidas
anova.rstat <- anova_test(data = altura.l, dv = Altura, wid = Clone, within = Idade)
anova.rstat

get_anova_table(anova.rstat, correction = c("auto", "GG", "HF", "none"))

#Instale o pacote rstatix, descomente a linha abaixo
#install.packages("ez", dependencies = T)

#Carregue o pacote
library(ez)
anova.ez <- ezANOVA(data = altura.l, dv = Altura, wid = .(Clone), within = .(Idade), 
                    detailed = T, type = 3)
anova.ez

#Instale o pacote
#install.packages("lme4")

#Carregue o pacote
library(lme4)
library(lmerTest) #use para computar o p valor para o coeficientes do modelo

#Ajuste do modelo linear misto
mod.misto <- lmer(Altura ~ Idade + (1|Clone), data = altura.l, REML = F)
summary(mod.misto)


pairwise.t.test(altura.l$Altura, altura.l$Idade, paired = T, p.adjust.method = "bonferroni")

#Vamos criar estatísticas de resumos para o conjunto de daods
altura.lm <- altura.l %>% 
    group_by(Idade) %>% 
    summarise(AlturaM = mean(Altura), AlturaSd = sd(Altura)/sqrt(n()))
altura.lm
#Criando um gráfico de barras
ggplot(data = altura.lm, aes(Idade, AlturaM, color = Idade)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = AlturaM - AlturaSd, ymax = AlturaM + AlturaSd),
                  width = 0.5, alpha = 1) +
    scale_color_grey() +
    labs(x = "Idade (meses)", y = "Altura (m)") +
    theme_classic() + 
    theme(legend.position = "none")


#Instale o pacote rstatix, descomente a linha abaixo
#install.packages("multicomp", dependencies = T)

#Carregue o pacote
library(multcomp)

compTest <- glht(mod.misto, linfct = mcp(Idade = "Tukey"))
summary(compTest)
confint(compTest)