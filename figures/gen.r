phy	<- read.tree(text='((A:10,B:10):10,((E:3, F:3):13,(C:14, D:14):2):4);')

chronopl(phy, lambda=1) -> ultree
is.ultrametric(ultree)

phy	<- ultree

phy_k	<- transform(phy, model="kappa")

phy_ou	<- transform(phy, model="OU")

phy_d	<- transform(phy, model="delta")

phy_l	<- transform(phy, model="lambda")

CM_phy 		<- as.matrix(VCV.array(phy, dim=2, compact=TRUE))
Kphy	<- phy_k(0.5)
CM_phy_k 	<- as.matrix(VCV.array(Kphy, dim=2, compact=TRUE))
write.table(CM_phy_k, "k", sep=" ")
Lphy	<- phy_l(0.5)
CM_phy_l 	<- as.matrix(VCV.array(Lphy, dim=2, compact=TRUE))
write.table(CM_phy_l, "l", sep=" ")
Lphy2	<- phy_l(0.9)
CM_phy_l2 	<- as.matrix(VCV.array(Lphy2, dim=2, compact=TRUE))
write.table(CM_phy_l2, "l2", sep=" ")
Dphy	<- phy_d(0.5)
CM_phy_d 	<- as.matrix(VCV.array(Dphy, dim=2, compact=TRUE))
write.table(CM_phy_d, "d", sep=" ")
OUphy	<- phy_ou(0.5)
CM_phy_ou 	<- as.matrix(VCV.array(OUphy, dim=2, compact=TRUE))
write.table(CM_phy_ou, "ou", sep=" ")
OUphy2	<- phy_ou(2)
CM_phy_ou2 	<- as.matrix(VCV.array(OUphy2, dim=2, compact=TRUE))
write.table(CM_phy_ou2, "ou2", sep=" ")
OUphy3	<- phy_ou(10)
CM_phy_ou3 	<- as.matrix(VCV.array(OUphy3, dim=2, compact=TRUE))
write.table(CM_phy_ou2, "ou3", sep=" ")

png("kCM.png")
	heatmap.2(CM_phy_k, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_k, digits=2), main="kappa=0.5", cex.main=2)
dev.off()

png("CM.png")
	heatmap.2(CM_phy, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy, digits=2), main="BM", cex.main=2)
dev.off()

png("CMl.png")
	heatmap.2(CM_phy_l, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_l, digits=2), main="lambda=0.5", cex.main=2)
dev.off()

png("CMl2.png")
	heatmap.2(CM_phy_l2, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_l2, digits=2), main="lambda=0.9", cex.main=2)
dev.off()

png("CMd.png")
	heatmap.2(CM_phy_d, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_d, digits=2), main="delta=0.5", cex.main=2)
dev.off()

png("CMou.png")
	heatmap.2(CM_phy_ou, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_ou, digits=2), main="OU, alpha=0.5", cex.main=2)
dev.off()

png("CMou2.png")
	heatmap.2(CM_phy_ou2, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_ou2, digits=2), main="OU, alpha=2", cex.main=2)
dev.off()

png("CMou3.png")
	heatmap.2(CM_phy_ou3, revC=T, scale="row", key=F, trace="none", cellnote=round(CM_phy_ou3, digits=2), main="OU, alpha=10", cex.main=2)
dev.off()



png("figure.png", width=900, height=1600)
	par(mfrow=c(4,2))
	plot(phy, main="BM")
	plot(phy_k(0.5), main="kappa = 0.5")
	plot(phy_l(0.5), main="lambda = 0.5")
	plot(phy_l(0.9), main="lambda = 0.9")
	plot(phy_d(0.5), main="delta = 0.5")
	plot(phy_ou(0.05), main="OU, alpha=0.5")
	plot(phy_ou(0.5), main="OU, alpha=2")
dev.off()








