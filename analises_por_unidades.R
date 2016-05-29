## Carrega dados
source("prepara_dados.R")
## Planilha ordenada pelos maiores desvios absolutos
head(pdv.unid.p[order(abs(pdv.unid.p$desvio.clt.20), decreasing=TRUE),c(1,2,4,5,c(11,13,15,12,14,16))])

## Relacao entre N de celetistas com 20+ anos de USP e pagos pelo pdv
plot(N.pdv ~ N.clt.20, data=pdv.unid.p, xlab="N de servidores", ylab="N incluídos no PDV",
     main="Celetistas com 20 ou mais anos de USP")
abline(0, usp.p.clt.20/100)
with(pdv.unid.p[pdv.unid$unidade=="RUSP",], text(N.clt.20, N.pdv, unidade, adj=c(0,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="HU",], text(N.clt.20, N.pdv, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="FM",], text(N.clt.20, N.pdv, unidade, adj=c(1,1.5) ))
with(pdv.unid.p[pdv.unid$unidade=="ESALQ",], text(N.clt.20, N.pdv, unidade, adj=c(0,1.5) ))

## Maiores desvios de total pago em relacao à proporcao na USP
head(pdv.unid.p[order(abs(pdv.unid.p$desvio.sal.clt.20), decreasing=TRUE), c(1, 2, 3, 17, 19, 21, 18, 20, 22)])

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
## O que falta: comparar a proporcao de celetistas nivel basico, medio e superior que receberam pdv em cada unidade
## em relacao à proporcao na unidade em Jan15
## Vai ser preciso criar variavel com as classes para converter em basico, tecnico e superior
with(folhaJan15[folhaJan15$categoria=="Celetista",],table(classe))

