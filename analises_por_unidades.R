## Carrega dados
source("prepara_dados.R")

################################################################################
## Analises para todos os grupos (superior, tecnico, basico) agregados
################################################################################

## Calculo de taxas e razões ##
## Proporcoes do n de pessoas e valores do PDV na USP ##
## em relação a total de funcionarios, celetistas e celetistas com pelo menos 20 anos de USP ##
## Total de funcionarios ##
## % dos incluidos no PDV 
(usp.p.nd <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.nd)*100)
## % do total gasto em PDV
(usp.p.pg.nd <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.nd)*100)
## Total de celetistas ##
## % dos incluidos no PDV
(usp.p.clt <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.clt)*100)
## % do pago em PDV 
(usp.p.pg.clt <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.clt)*100)
## Celetistas com pelo menos 20 anos de USP##
## % dos incluidos no PDV
(usp.p.clt.20 <- sum(pdv.unid$N.pdv)/sum(pdv.unid$N.clt.20)*100)
## % do pago em PDV 
(usp.p.pg.clt.20 <- sum(pdv.unid$pago.pdv)/sum(pdv.unid$sal.clt.20)*100)
## Adiciona desvios em relacao à proporcao
pdv.unid.p <- mutate(pdv.unid,
                     p.nd = N.pdv/N.nd*100,
                     desvio.nd = N.pdv - (N.nd*usp.p.nd)/100,
                     p.clt = N.pdv/N.clt*100,
                     desvio.clt = N.pdv - (N.clt*usp.p.clt)/100,
                     p.clt.20 = N.pdv/N.clt.20*100,
                     desvio.clt.20 = N.pdv - (N.clt.20*usp.p.clt.20)/100,
                     p.sal.nd = pago.pdv/sal.nd*100,
                     desvio.sal.nd = pago.pdv - (sal.nd*usp.p.pg.nd)/100,
                     p.sal.clt = pago.pdv/sal.clt*100,
                     desvio.sal.clt = pago.pdv - (sal.clt*usp.p.pg.clt)/100,
                     p.sal.clt.20 = pago.pdv/sal.clt.20*100,
                     desvio.sal.clt.20 = pago.pdv - (sal.clt.20*usp.p.pg.clt.20)/100
                     )

## Planilha ordenada pelos maiores desvios absolutos
head(pdv.unid.p[order(abs(pdv.unid.p$desvio.clt.20), decreasing=TRUE),c(1,9,8,32,38,39)])

## Relacao entre N de celetistas com 20+ anos de USP e pagos pelo pdv
plot(N.pdv ~ N.clt.20, data=pdv.unid.p, xlab="N de servidores", ylab="N incluídos no PDV",
     main="Celetistas com 20 ou mais anos de USP")
abline(0, usp.p.clt.20/100)
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(N.clt.20, N.pdv, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(N.clt.20, N.pdv, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(N.clt.20, N.pdv, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(N.clt.20, N.pdv, unidade, adj=c(0,1.5) ))

## Maiores desvios de total pago em relacao à proporcao na USP
head(pdv.unid.p[order(abs(pdv.unid.p$desvio.sal.clt.20), decreasing=TRUE),c(1,9,8,44,45)])


## Maior numeros
## Relacao entre soma salarios Jan/15 celetistas (pelo mesno 20 anos USP) e total pago pelo pdv
plot(pago.pdv ~ sal.clt.20, data=pdv.unid.p, xlab="Total de salários mensais Jan/2015 (milhões R$)",
     ylab="Total pago em PDV (milhões R$)",
     main="Celetistas com 20 ou mais anos de USP", axes=FALSE)
pts <- pretty(pdv.unid.p$sal.clt.20/1e6)
axis(1, pts*1e6, labels= pts)
pts <- pretty(pdv.unid.p$pago.pdv/1e6)
axis(2, pts*1e6, labels= pts)
box()
abline(0, usp.p.pg.clt.20/100)
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(sal.clt.20, pago.pdv, unidade, adj=c(1.5,0) ))
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(sal.clt.20, pago.pdv, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(sal.clt.20, pago.pdv, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(sal.clt.20, pago.pdv, unidade, adj=c(1,1.5) ))

## Analises das proporcoes em cada classe
## Celetistas com pelo menos 20 anos de USP##
## % dos incluidos no PDV
## Basico
(usp.p.clt.20.b <- sum(pdv.unid$N.pdv.basico)/sum(pdv.unid$N.clt.20.basico)*100)
## Tecnico
(usp.p.clt.20.t <- sum(pdv.unid$N.pdv.tecnico)/sum(pdv.unid$N.clt.20.tecnico)*100)
## Superior
(usp.p.clt.20.s <- sum(pdv.unid$N.pdv.superior)/sum(pdv.unid$N.clt.20.superior)*100)
## Planilha com proporcoes
pdv.unid.pc <- mutate(pdv.unid,
                      p.clt.20= N.pdv/N.clt.20,
                      p.clt.20.b = N.pdv.basico/N.clt.20.basico,
                      p.clt.20.t = N.pdv.tecnico/N.clt.20.tecnico,
                      p.clt.20.s = N.pdv.superior/N.clt.20.superior) %>%
    select(unidade, N.clt.20, N.clt.20.basico, N.clt.20.tecnico, N.clt.20.superior,
           N.pdv, N.pdv.basico, N.pdv.tecnico, N.pdv.superior,
           p.clt.20, p.clt.20.b, p.clt.20.t, p.clt.20.s)
rownames(pdv.unid.pc) <- pdv.unid.pc$unidade
head(pdv.unid.pc[order(pdv.unid.pc$N.clt.20, decreasing=TRUE),], 10)
## Graficos
##Basico
plot(N.pdv.basico ~ N.clt.20.basico, data=pdv.unid.p, xlab="N de servidores", ylab="N incluídos no PDV",
     main="Básico: celetistas com 20 ou mais anos de USP")
abline(0, usp.p.clt.20.b/100)
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(N.clt.20.basico, N.pdv.basico, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(N.clt.20.basico, N.pdv.basico, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(N.clt.20.basico, N.pdv.basico, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(N.clt.20.basico, N.pdv.basico, unidade, adj=c(0,1.5) ))
##Tecnico
plot(N.pdv.tecnico ~ N.clt.20.tecnico, data=pdv.unid.p, xlab="N de servidores", ylab="N incluídos no PDV",
     main="Técnico: celetistas com 20 ou mais anos de USP")
abline(0, usp.p.clt.20.t/100)
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(N.clt.20.tecnico, N.pdv.tecnico, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(N.clt.20.tecnico, N.pdv.tecnico, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(N.clt.20.tecnico, N.pdv.tecnico, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(N.clt.20.tecnico, N.pdv.tecnico, unidade, adj=c(0,1.5) ))
##Superior
plot(N.pdv.superior ~ N.clt.20.superior, data=pdv.unid.p, xlab="N de servidores", ylab="N incluídos no PDV",
     main="Superior: celetistas com 20 ou mais anos de USP")
abline(0, usp.p.clt.20.s/100)
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(N.clt.20.superior, N.pdv.superior, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(N.clt.20.superior, N.pdv.superior, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(N.clt.20.superior, N.pdv.superior, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(N.clt.20.superior, N.pdv.superior, unidade, adj=c(0,1.5) ))

## Grafico de barras
HU.FM <- pdv.unid$unidade=="HU"|pdv.unid$unidade=="FM"
indices <- c(2,4,6,8,26,28,30,32)
x1 <- apply(pdv.unid[!(HU.FM),indices], 2, sum)
x1 <- rbind(pdv.unid[HU.FM,indices], x1)
x2 <- mutate(x1,
             p.clt.20= N.pdv/N.clt.20,
             p.clt.20.b = N.pdv.basico/N.clt.20.basico,
             p.clt.20.t = N.pdv.tecnico/N.clt.20.tecnico,
             p.clt.20.s = N.pdv.superior/N.clt.20.superior) 
rownames(x2)<- c("FM", "HU", "Outras")
f2 <- function(x, ...){
    y <- as.matrix(x[,c("p.clt.20","p.clt.20.b", "p.clt.20.t", "p.clt.20.s")])
    blp <- barplot(y,
            beside=T, col=c("darkblue","darkorange","darkred"), border=NA,
            xlab="",
            ylab="Proporção no PIDV",
            legend.text=c("FM", "HU", "Outras"),
            args.legend=list(x="topright", border=NA, bty="n", cex=2),
            names.arg=c("Total", "Básico", "Técnico","Superior"), ...)
    counts <- as.matrix(x[,c("N.clt.20","N.clt.20.basico", "N.clt.20.tecnico", "N.clt.20.superior")])
    dim(counts) <- NULL
#    plot(blp)
    text(x=blp, y= y, labels=as.character(counts), xpd=TRUE, pos=3)
}

par(mfrow=c(1,1), cex.axis = 2, cex.lab = 2.25, cex.main=2.5, mar = c(6,6.5,4,2), mgp=c(5,1,0),bty = "l", las=1)
f2(x2)

