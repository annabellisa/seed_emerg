#Caitlin's script - script development - no longer need in main script


# diversity index - invsimpson ----

bgall.simp <- BGmat[, colnames(BGmat) %in% BG.all]
bgall.simp <- diversity(bgall.simp, index = "invsimpson")

bgnat.simp <-BGmat[, colnames(BGmat) %in% BG.native]
bgnat.simp <-diversity(bgnat.simp, index = "invsimpson")
bgnat.simp[is.infinite(bgnat.simp)] <- 0

bgexo.simp <-BGmat[, colnames(BGmat) %in% BG.exotic]
bgexo.simp <-diversity(bgexo.simp, index = "invsimpson")
bgexo.simp[is.infinite(bgexo.simp)] <- 0

bgann.simp <-BGmat[, colnames(BGmat) %in% BG.annual]
bgann.simp <-diversity(bgann.simp, index = "invsimpson")
bgann.simp[is.infinite(bgann.simp)] <- 0

bgper.simp <-BGmat[, colnames(BGmat) %in% BG.perr]
bgper.simp <-diversity(bgper.simp, index = "invsimpson")
bgper.simp[is.infinite(bgper.simp)] <- 0

#bgleg.simp <-BGmat[, colnames(BGmat) %in% BG.leg]
#bgleg.simp <-diversity(bgleg.simp, index = "invsimpson")

bgtree.simp <-BGmat[, colnames(BGmat) %in% BG.tree]
bgtree.simp <-diversity(bgtree.simp, index = "invsimpson")
bgtree.simp[is.infinite(bgtree.simp)] <- 0

bgfor.simp <-BGmat[, colnames(BGmat) %in% BG.forb]
bgfor.simp <-diversity(bgfor.simp, index = "invsimpson")
bgfor.simp[is.infinite(bgfor.simp)] <- 0

bggra.simp <-BGmat[, colnames(BGmat) %in% BG.grass]
bggra.simp <-diversity(bggra.simp, index = "invsimpson")
bggra.simp[is.infinite(bggra.simp)] <- 0

bgsed.simp <-BGmat[, colnames(BGmat) %in% BG.sedge]
bgsed.simp <-diversity(bgsed.simp, index = "invsimpson")
bgsed.simp[is.infinite(bgsed.simp)] <- 0

bgnatgra.simp <-BGmat[, colnames(BGmat) %in% BG.native_grass]
bgnatgra.simp <-diversity(bgnatgra.simp, index = "invsimpson")
bgnatgra.simp[is.infinite(bgnatgra.simp)] <- 0

bgexogra.simp <-BGmat[, colnames(BGmat) %in% BG.exotic_grass]
bgexogra.simp <-diversity(bgexogra.simp, index = "invsimpson")
bgexogra.simp[is.infinite(bgexogra.simp)] <- 0

bgnatfor.simp <-BGmat[, colnames(BGmat) %in% BG.native_forb]
bgnatfor.simp <-diversity(bgnatfor.simp, index = "invsimpson")
bgnatfor.simp[is.infinite(bgnatfor.simp)] <- 0

bgexofor.simp <-BGmat[, colnames(BGmat) %in% BG.exotic_forb]
bgexofor.simp <-diversity(bgexofor.simp, index = "invsimpson")
bgexofor.simp[is.infinite(bgexofor.simp)] <- 0

bglegfor.simp <-BGmat[, colnames(BGmat) %in% BG.leg_forb]
bglegfor.simp <-diversity(bglegfor.simp, index = "invsimpson")
bglegfor.simp[is.infinite(bglegfor.simp)] <- 0

bgnonlegfor.simp <-BGmat[, colnames(BGmat) %in% BG.nonleg_forb]
bgnonlegfor.simp <-diversity(bgnonlegfor.simp, index = "invsimpson")
bgnonlegfor.simp[is.infinite(bgnonlegfor.simp)] <- 0


agall.simp <- AGmat[, colnames(AGmat) %in% AG.all]
agall.simp <- diversity(agall.simp, index = "invsimpson")

agnat.simp <-AGmat[, colnames(AGmat) %in% AG.native]
agnat.simp <-diversity(agnat.simp, index = "invsimpson")
agnat.simp[is.infinite(agnat.simp)] <- 0

agexo.simp <-AGmat[, colnames(AGmat) %in% AG.exotic]
agexo.simp <-diversity(agexo.simp, index = "invsimpson")
agexo.simp[is.infinite(agexo.simp)] <- 0

agann.simp <-AGmat[, colnames(AGmat) %in% AG.annual]
agann.simp <-diversity(agann.simp, index = "invsimpson")
agann.simp[is.infinite(agann.simp)] <- 0

agper.simp <-AGmat[, colnames(AGmat) %in% AG.perr]
agper.simp <-diversity(agper.simp, index = "invsimpson")
agper.simp[is.infinite(agper.simp)] <- 0

#agleg.simp <-AGmat[, colnames(AGmat) %in% AG.leg]
#agleg.simp <-diversity(agleg.simp, index = "invsimpson")

agtree.simp <-AGmat[, colnames(AGmat) %in% AG.tree]
agtree.simp <-diversity(agtree.simp, index = "invsimpson")
agtree.simp[is.infinite(agtree.simp)] <- 0

agfor.simp <-AGmat[, colnames(AGmat) %in% AG.forb]
agfor.simp <-diversity(agfor.simp, index = "invsimpson")
agfor.simp[is.infinite(agfor.simp)] <- 0

aggra.simp <-AGmat[, colnames(AGmat) %in% AG.grass]
aggra.simp <-diversity(aggra.simp, index = "invsimpson")
aggra.simp[is.infinite(aggra.simp)] <- 0

agsed.simp <-AGmat[, colnames(AGmat) %in% AG.sedge]
agsed.simp <-diversity(agsed.simp, index = "invsimpson")
agsed.simp[is.infinite(agsed.simp)] <- 0

agnatgra.simp <-AGmat[, colnames(AGmat) %in% AG.native_grass]
agnatgra.simp <-diversity(agnatgra.simp, index = "invsimpson")
agnatgra.simp[is.infinite(agnatgra.simp)] <- 0

agexogra.simp <-AGmat[, colnames(AGmat) %in% AG.exotic_grass]
agexogra.simp <-diversity(agexogra.simp, index = "invsimpson")
agexogra.simp[is.infinite(agexogra.simp)] <- 0

agnatfor.simp <-AGmat[, colnames(AGmat) %in% AG.native_forb]
agnatfor.simp <-diversity(agnatfor.simp, index = "invsimpson")
agnatfor.simp[is.infinite(agnatfor.simp)] <- 0

agexofor.simp <-AGmat[, colnames(AGmat) %in% AG.exotic_forb]
agexofor.simp <-diversity(agexofor.simp, index = "invsimpson")
agexofor.simp[is.infinite(agexofor.simp)] <- 0

aglegfor.simp <-AGmat[, colnames(AGmat) %in% AG.leg_forb]
aglegfor.simp <-diversity(aglegfor.simp, index = "invsimpson")
aglegfor.simp[is.infinite(aglegfor.simp)] <- 0

agnonlegfor.simp <-AGmat[, colnames(AGmat) %in% AG.nonleg_forb]
agnonlegfor.simp <-diversity(agnonlegfor.simp, index = "invsimpson")
agnonlegfor.simp[is.infinite(agnonlegfor.simp)] <- 0

div4$simp <- c(agall.simp, bgall.simp)
div4$simp.nat <- c(agnat.simp,bgnat.simp)
div4$simp.exo <- c(agexo.simp,bgexo.simp)
div4$simp.ann <- c(agann.simp,bgann.simp)
div4$simp.per <- c(agper.simp,bgper.simp)
div4$simp.for <- c(agfor.simp,bgfor.simp)
div4$simp.gra <- c(aggra.simp,bggra.simp)
div4$simp.sed <- c(agsed.simp,bgsed.simp)
div4$simp.natgra <- c(agnatgra.simp,bgnatgra.simp)
div4$simp.exogra <- c(agexogra.simp,bgexogra.simp)
div4$simp.natfor <- c(agnatfor.simp,bgnatfor.simp)
div4$simp.exofor <- c(agexofor.simp,bgexofor.simp)
div4$simp.legfor <- c(aglegfor.simp,bglegfor.simp)
div4$simp.nlegfor <- c(agnonlegfor.simp,bgnonlegfor.simp)


#invsimpson modelling - lmer. interactions insignificant, all models used in further analysis were additive.
#all
#allsimp<-lmer(simp~ab*burn_trt+(1|transect), data=div4)
#summary(allsimp)

allsimp2<-lmer(simp~ab+burn_trt+(1|transect), data=div4)
summary(allsimp2)

#native
#natsimp<-lmer(simp.nat~ab*burn_trt+(1|transect), data=div4)
#summary(natsimp)

natsimp2<-lmer(simp.nat~ab+burn_trt+(1|transect), data=div4)
summary(natsimp2)

#exotic
#exosimp<-lmer(simp.exo~ab*burn_trt+(1|transect), data=div4)
#summary(exosimp)

exosimp2<-lmer(simp.exo~ab+burn_trt+(1|transect), data=div4)
summary(exosimp2)

#annual
#annsimp<-lmer(simp.ann~ab*burn_trt+(1|transect), data=div4)
#summary(annsimp)

annsimp2<-lmer(simp.ann~ab+burn_trt+(1|transect), data=div4)
summary(annsimp2)

#perennial
#persimp<-lmer(simp.per~ab*burn_trt+(1|transect), data=div4)
#summary(persimp)

persimp2<-lmer(simp.per~ab+burn_trt+(1|transect), data=div4)
summary(persimp2)

#forb
#forsimp<-lmer(simp.for~ab*burn_trt+(1|transect), data=div4)
#summary(forsimp)

forsimp2<-lmer(simp.for~ab+burn_trt+(1|transect), data=div4)
summary(forsimp2)

#grass
#grasimp<-lmer(simp.gra~ab*burn_trt+(1|transect), data=div4)
#summary(forsimp)

grasimp2<-lmer(simp.gra~ab+burn_trt+(1|transect), data=div4)
summary(grasimp2)

#sedge
#sedsimp<-lmer(simp.sed~ab*burn_trt+(1|transect), data=div4)
#summary(sedsimp)

sedsimp2<-lmer(simp.sed~ab+burn_trt+(1|transect), data=div4)
summary(sedsimp2)

#native grass
#natgrasimp<-lmer(simp.natgra~ab*burn_trt+(1|transect), data=div4)
#summary(natgrasimp)

natgrasimp2<-lmer(simp.natgra~ab+burn_trt+(1|transect), data=div4)
summary(natgrasimp2)

#exotic grass
#exograsimp<-lmer(simp.exogra~ab*burn_trt+(1|transect), data=div4)
#summary(exograsimp)

exograsimp2<-lmer(simp.exogra~ab+burn_trt+(1|transect), data=div4)
summary(exograsimp2)

#native forb
#natforsimp<-lmer(simp.natfor~ab*burn_trt+(1|transect), data=div4)
#summary(natforsimp)

natforsimp2<-lmer(simp.natfor~ab+burn_trt+(1|transect), data=div4)
summary(natforsimp2)

#exotic forb
#exoforsimp<-lmer(simp.exofor~ab*burn_trt+(1|transect), data=div4)
#summary(natforsimp)

exoforsimp2<-lmer(simp.exofor~ab+burn_trt+(1|transect), data=div4)
summary(exoforsimp2)

#non leg forb
#nlegforsimp<-lmer(simp.nlegfor~ab*burn_trt+(1|transect), data=div4)
#summary(nlegforsimp)

nlegforsimp2<-lmer(simp.nlegfor~ab+burn_trt+(1|transect), data=div4)
summary(nlegforsimp2)

#leg forb
#legforsimp<-lmer(simp.legfor~ab*burn_trt+(1|transect), data=div4)
#summary(nlegforsimp)

legforsimp2<-lmer(simp.legfor~ab+burn_trt+(1|transect), data=div4)
summary(legforsimp2)

#invsimpson predictSE
#all
srmod_simp.all <- predictSE(mod=allsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.all <- data.frame(nd1, fit = srmod_simp.all$fit, se = srmod_simp.all$se.fit)
srmod_simp.all$lci <- srmod_simp.all$fit-(srmod_simp.all$se*1.96)
srmod_simp.all$uci <- srmod_simp.all$fit+(srmod_simp.all$se*1.96)
head(srmod_simp.all)

#native
srmod_simp.nat <- predictSE(mod=natsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.nat <- data.frame(nd1, fit = srmod_simp.nat$fit, se = srmod_simp.nat$se.fit)
srmod_simp.nat$lci <- srmod_simp.nat$fit-(srmod_simp.nat$se*1.96)
srmod_simp.nat$uci <- srmod_simp.nat$fit+(srmod_simp.nat$se*1.96)
head(srmod_simp.nat)

#exotic
srmod_simp.exo <- predictSE(mod=exosimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.exo <- data.frame(nd1, fit = srmod_simp.exo$fit, se = srmod_simp.exo$se.fit)
srmod_simp.exo$lci <- srmod_simp.exo$fit-(srmod_simp.exo$se*1.96)
srmod_simp.exo$uci <- srmod_simp.exo$fit+(srmod_simp.exo$se*1.96)
head(srmod_simp.exo)

#annual
srmod_simp.ann <- predictSE(mod=annsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.ann <- data.frame(nd1, fit = srmod_simp.ann$fit, se = srmod_simp.ann$se.fit)
srmod_simp.ann$lci <- srmod_simp.ann$fit-(srmod_simp.ann$se*1.96)
srmod_simp.ann$uci <- srmod_simp.ann$fit+(srmod_simp.ann$se*1.96)
head(srmod_simp.ann)

#perennial
srmod_simp.per <- predictSE(mod=persimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.per <- data.frame(nd1, fit = srmod_simp.per$fit, se = srmod_simp.per$se.fit)
srmod_simp.per$lci <- srmod_simp.per$fit-(srmod_simp.per$se*1.96)
srmod_simp.per$uci <- srmod_simp.per$fit+(srmod_simp.per$se*1.96)
head(srmod_simp.per)

#forb
srmod_simp.for <- predictSE(mod=forsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.for <- data.frame(nd1, fit = srmod_simp.for$fit, se = srmod_simp.for$se.fit)
srmod_simp.for$lci <- srmod_simp.for$fit-(srmod_simp.for$se*1.96)
srmod_simp.for$uci <- srmod_simp.for$fit+(srmod_simp.for$se*1.96)
head(srmod_simp.for)

#grass
srmod_simp.gra <- predictSE(mod=grasimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.gra <- data.frame(nd1, fit = srmod_simp.gra$fit, se = srmod_simp.gra$se.fit)
srmod_simp.gra$lci <- srmod_simp.gra$fit-(srmod_simp.gra$se*1.96)
srmod_simp.gra$uci <- srmod_simp.gra$fit+(srmod_simp.gra$se*1.96)
head(srmod_simp.gra)

#sedge
srmod_simp.sed <- predictSE(mod=sedsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.sed <- data.frame(nd1, fit = srmod_simp.sed$fit, se = srmod_simp.sed$se.fit)
srmod_simp.sed$lci <- srmod_simp.sed$fit-(srmod_simp.sed$se*1.96)
srmod_simp.sed$uci <- srmod_simp.sed$fit+(srmod_simp.sed$se*1.96)
head(srmod_simp.sed)

#native grass
srmod_simp.natgra <- predictSE(mod=natgrasimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.natgra <- data.frame(nd1, fit = srmod_simp.natgra$fit, se = srmod_simp.natgra$se.fit)
srmod_simp.natgra$lci <- srmod_simp.natgra$fit-(srmod_simp.natgra$se*1.96)
srmod_simp.natgra$uci <- srmod_simp.natgra$fit+(srmod_simp.natgra$se*1.96)
head(srmod_simp.natgra)

#exotic grass
srmod_simp.exogra <- predictSE(mod=exograsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.exogra <- data.frame(nd1, fit = srmod_simp.exogra$fit, se = srmod_simp.exogra$se.fit)
srmod_simp.exogra$lci <- srmod_simp.exogra$fit-(srmod_simp.exogra$se*1.96)
srmod_simp.exogra$uci <- srmod_simp.exogra$fit+(srmod_simp.exogra$se*1.96)
head(srmod_simp.exogra)

#native forb
srmod_simp.natfor <- predictSE(mod=natforsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.natfor <- data.frame(nd1, fit = srmod_simp.natfor$fit, se = srmod_simp.natfor$se.fit)
srmod_simp.natfor$lci <- srmod_simp.natfor$fit-(srmod_simp.natfor$se*1.96)
srmod_simp.natfor$uci <- srmod_simp.natfor$fit+(srmod_simp.natfor$se*1.96)
head(srmod_simp.natfor)

#exotic forb
srmod_simp.exofor <- predictSE(mod=exoforsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.exofor <- data.frame(nd1, fit = srmod_simp.exofor$fit, se = srmod_simp.exofor$se.fit)
srmod_simp.exofor$lci <- srmod_simp.exofor$fit-(srmod_simp.exofor$se*1.96)
srmod_simp.exofor$uci <- srmod_simp.exofor$fit+(srmod_simp.exofor$se*1.96)
head(srmod_simp.exofor)

#non leg forb
srmod_simp.nlegfor <- predictSE(mod=nlegforsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.nlegfor <- data.frame(nd1, fit = srmod_simp.nlegfor$fit, se = srmod_simp.nlegfor$se.fit)
srmod_simp.nlegfor$lci <- srmod_simp.nlegfor$fit-(srmod_simp.nlegfor$se*1.96)
srmod_simp.nlegfor$uci <- srmod_simp.nlegfor$fit+(srmod_simp.nlegfor$se*1.96)
head(srmod_simp.nlegfor)

#leg forb
srmod_simp.legfor <- predictSE(mod=legforsimp2,newdata=nd1,type="response",se.fit = T)
srmod_simp.legfor <- data.frame(nd1, fit = srmod_simp.legfor$fit, se = srmod_simp.legfor$se.fit)
srmod_simp.legfor$lci <- srmod_simp.legfor$fit-(srmod_simp.legfor$se*1.96)
srmod_simp.legfor$uci <- srmod_simp.legfor$fit+(srmod_simp.legfor$se*1.96)
head(srmod_simp.legfor)

#invsimpson plotting
dev.new(width=9,height=15,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(5,3),mar=c(4,4,1.5,1), mgp=c(2.5,1,0))

#all
plot(c(1:4), srmod_simp.all$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.all$lci)), max(srmod_simp.all$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.all$lci, c(1:4), srmod_simp.all$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(a) All", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.all$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native
plot(c(1:4), srmod_simp.nat$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.nat$lci)), max(srmod_simp.nat$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.nat$lci, c(1:4), srmod_simp.nat$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(b) Native", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.nat$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic
plot(c(1:4), srmod_simp.exo$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.exo$lci)), max(srmod_simp.exo$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.exo$lci, c(1:4), srmod_simp.exo$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(c) Exotic", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.exo$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#annual
plot(c(1:4), srmod_simp.ann$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.ann$lci)), max(srmod_simp.ann$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.ann$lci, c(1:4), srmod_simp.ann$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(d) Annual", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.ann$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#perennial
plot(c(1:4), srmod_simp.per$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.per$lci)), max(srmod_simp.per$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.per$lci, c(1:4), srmod_simp.per$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(e) Perennial", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.per$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#forb
plot(c(1:4), srmod_simp.for$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.for$lci)), max(srmod_simp.for$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.for$lci, c(1:4), srmod_simp.for$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(f) Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.for$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#grass
plot(c(1:4), srmod_simp.gra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.gra$lci)), max(srmod_simp.gra$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.gra$lci, c(1:4), srmod_simp.gra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(g) Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.gra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native grass
plot(c(1:4), srmod_simp.natgra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.natgra$lci)), max(srmod_simp.natgra$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.natgra$lci, c(1:4), srmod_simp.natgra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(h) Native Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.natgra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic grass
plot(c(1:4), srmod_simp.exogra$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.exogra$lci)), max(srmod_simp.exogra$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.exogra$lci, c(1:4), srmod_simp.exogra$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(i) Exotic Grass", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.exogra$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#native forb
plot(c(1:4), srmod_simp.natfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.natfor$lci)), max(srmod_simp.natfor$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.natfor$lci, c(1:4), srmod_simp.natfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(j) Native Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.natfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#exotic forb
plot(c(1:4), srmod_simp.exofor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.exofor$lci)), max(srmod_simp.exofor$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.exofor$lci, c(1:4), srmod_simp.exofor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(k) Exotic Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.exofor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#non leg forb
plot(c(1:4), srmod_simp.nlegfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.nlegfor$lci)), max(srmod_simp.nlegfor$uci)), ylab="InvSimpson Diversity", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.nlegfor$lci, c(1:4), srmod_simp.nlegfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(l) Non-leg Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.nlegfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

#leg forb
plot(c(1:4), srmod_simp.legfor$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_simp.legfor$lci)), max(srmod_simp.legfor$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5,type="n")
arrows(c(1:4), srmod_simp.legfor$lci, c(1:4), srmod_simp.legfor$uci, length=0.05, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=0.8)
title(main = "(m) Leg Forb", line = 0.5,adj=0, cex.main=0.95)
points(c(1:4), srmod_simp.legfor$fit,col=c(rep("chartreuse4",2),rep("orange",2)), pch=20, cex=2.5)

par(xpd=NA)
legend(8,2.3, legend=c("Control", "Burn"), col = c("chartreuse4", "orange"),pch=c(20, 20), cex = (1.45), title = "Legend")
par(xpd=F)

#simp?
#div1$agsimp <- diversity(AGmat, index = "invsimpson")

#div1$agsr <- apply(AGmat, MARGIN = 1, FUN = function(x) length(which(x > 0)))

div1$agsimp <- diversity(AGmat, index ="invsimpson")
div1$bgsimp <- diversity(BGmat, index ="invsimpson")
div4$simp <- c(div1$agsimp, div1$bgsimp)

# ----

# pcoa & other analyses ----

head(div8[,1:10]);dim(div8)
which(duplicated(colnames(div8)))
dist1 <- vegdist(div8, method = "bray") # dissimilarity matrix using bray-curtis distance indices on the varespec dataset native to vegan
head(dist1)
length(dist1)
str(dist1)

pcoaVS <- pco(dist1, negvals = "zero", dround = 0) # if negvals = 0 sets all negative eigenvalues to zero; if = "rm" corrects for negative eigenvalues using method 1 of Legendre and Anderson 1999
summary(pcoaVS)

plot(pcoaVS$vectors[, 1], pcoaVS$vectors[, 2], 
     xlab = "PCoA1", ylab = "PCoA2", main = "PCoA Plot")

pov2 <- summary(pcoaVS)$importance[2,]


dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
plot(pcoaVS$vectors[,1], pcoaVS$vectors[,2], type = "p", xlab = "PCoA1", ylab = "PCoA2",
     axes = TRUE, main = "PCoA (ecodist) on varespec data")

text(pcoaVS$vectors[,1], pcoaVS$vectors[,2], labels(dist1), 
     cex = 0.9, xpd = TRUE)

pco1 <- pco(dist1, negvals = "zero", dround = 0)
dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
plot(pco1$vectors[,1], pco1$vectors[,2],
     xlab = "PCoA1", ylab = "PCoA2", main = "PCoA plot")



shapes<-c(15,17)

shapes<-shapes[as.factor(pcadata2$burn_trt)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcadata2$burn_trt)]
View(shapes)
View(col.1)

plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))


pcoaVS$values # eigenvalue for each component. This is a measure of the variance explained by each dimension
pcoaVS$vectors # eigenvectors. Each column contains the scores for that dimension.


# Project unstandardized and standardized species on the PCoA ordination plot

res <- pcoa(dist1)

dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mfrow=c(1,2))
biplot(res, div8)
biplot(res, div8.st)

par(mfrow=c(1,2))
biplot(res, div8, dir.axis1=-1, dir.axis2=-1)
biplot(res, div8.st, dir.axis1=-1, dir.axis2=-1)

summary(res)

#write.table(div8, file = "01_data/div8.txt", sep = "\t", row.names = TRUE, col.names = NA)







V1 <- data.frame(quadratID = rownames(pcoaVS$vectors[1]),X1 = pcoaVS$vectors[1])
head(div4);dim(div4)
div9 <- div4
div9$quadratID2 <- paste(div9$quadratID,div9$ab, sep = ".")
head(div9)
div10 <- merge(div9,V1,by.x = "quadratID2", by.y = "quadratID", all.x = T, all.y = F)
head(div10);dim(div10)


X1_mod<-lmer(X1~ab*burn_trt+(1|transect), data=div10)
summary(X1_mod)



unique_species_codes <- unique(BGspecid$code)

total_abundance <- 0

# Loop through each unique species code and sum the counts
for (code in unique_species_codes) {
  total_abundance <- total_abundance + sum(tdata$count[tdata$code == code])
}

# ----


#import plant full (already imported and subsetted to soil core sites)
#pfull<-read.table("01_data/plant_full.txt",header=T)
#head(pfull,4);dim(pfull)

#extract from pfull unique quadrats

soilq<-unique(sdata$quadratID)
soilq


#check that soilqs exist in pfull - should be 0, none missing

missingq<-sdata[which(!soilq %in% unique(pfull$quadratID)),]$quadratID

pfull <-read.table("01_data/plant_full.txt",header=T)

psoil<-pfull[which(pfull$quadratID %in% soilq),] #AG data for t37-46 quadratID's, including 1m and 5m quadrats
psoil1 <- subset(pfull, quadratID %in% soilq & cover == 1) #AG data for t37-46 quadratID's, excluding 5m quadrats
rownames(psoil)<-1:nrow(psoil)
#above-ground plant data for soil core sites
head(psoil);dim(psoil)
length(unique(psoil$sp))


#if need to write table, just run once below:
#write.table(psoil,file="01_data/psoil.txt",sep="\t",quote = F,row.names = F)


##
#merging datasets
library(tidyr)
merged_data <- merge(tdata, combospec, by = "code", all.x = TRUE)
merged_data <- merge(AGdata, combospec, by.x = "sp", by.y = "code", all.x = T)
merged_data <- merge(merged_data, sdata, by = "quadratID", all.x = TRUE)
#merged sitexspec matrix - code and species in different forms?
merged_ssm <- pivot_wider(merged_data,
                          id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                          names_from = "species",
                          values_from = "count",
                          values_fn = list(count = sum), #using quadratID results in duplicates due to separate entries for trays A and B, have summed
                          values_fill = list(count = 0))


#merged_id
mergedID_data <-merged_data[merged_data$speciesID == 1, ]
mergedID_ssm <- pivot_wider(mergedID_data,
                            id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                            names_from = "species",
                            values_from = "count",
                            values_fn = list(count = sum),
                            values_fill = list(count = 0))
#AG mergedID 
AGmergedID_data <- mergedID_data[mergedID_data$location.y == 1 | mergedID_data$location.y ==2, ]
AGmergedID_ssm <- pivot_wider(AGmergedID_data,
                              id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                              names_from = "species",
                              values_from = "count",
                              values_fn = list(count = sum),
                              values_fill = list(count = 0))

#BG mergedID
BGmergedID_data <- mergedID_data[mergedID_data$location.y == 0 | mergedID_data$location.y == 2, ]
BGmergedID_ssm <- pivot_wider(AGmergedID_data,
                              id_cols = c("code", "location.x", "quadrat.y", "burn_trt"),
                              names_from = "species",
                              values_from = "count",
                              values_fn = list(count = sum),
                              values_fill = list(count = 0))

#remove columns
#div1 <- subset(div1, select = -agmarg)


#---- initial ssm, shannon, sr, simps attempt
#species richness? wait this sums above and below data (1 and 2 or 0 and 2) wait but its based on tray data counts not plant data - so its only BG
AG.id_sr <- aggregate(count ~ species, data = AGmergedID_data, FUN = sum)
BG.id_sr <- aggregate(count ~ species, data = BGmergedID_data, FUN = sum)

#transect richness
transectAG.id_sr <-aggregate(count ~ transect.x + species, data = AGmergedID_data, FUN = sum)
transectBG.id_sr <- aggregate(count ~ transect.x + species, data = BGmergedID_data, FUN = sum)

#cntrl vs burn richness transect
BG.idburn_data <- BGmergedID_data[BGmergedID_data$burn_trt == 'Burn', ]
transburn_BG.id_sr <- aggregate(count ~ transect.x + species, data = BG.idburn_data, FUN = sum)
BG.idcntrl_data <- BGmergedID_data[BGmergedID_data$burn_trt == 'Control', ]
transcntrl_BG.id_sr <- aggregate(count ~ transect.x + species, data = BG.idcntrl_data, FUN = sum)

#quadrat richness
quadratBG.id_sr <- aggregate(count ~ quadratID +species, data = BGmergedID_data, FUN = sum)

quadcntrl_BG.id_sr <- aggregate(count ~ quadratID + species, data = BG.idcntrl_data, FUN = sum)

quadburn_BG.id_sr <- aggregate(count ~ quadratID + species, data = BG.idburn_data, FUN = sum)

#shannon?
library(vegan)
#transect shannon
calc_shannons <- function(data) {
  shannons <-diversity(data, index = "shannon") 
  return (shannons)
}
shannon_transect <- tapply(transectBG.id_sr$count, transectBG.id_sr$transect.x, calc_shannons)

shannon_df <- data.frame(transect = names(shannon_transect), shannons_diversity = unname(shannon_transect))

print(shannon_df)

#transect burn/trt shannon
burn.shannon_transect <- tapply(transburn_BG.id_sr$count, transburn_BG.id_sr$transect.x, calc_shannons)
transburn.shannon_df <- data.frame(
  transect = names(burn.shannon_transect),
  shannons_diversity = unname(burn.shannon_transect)
)

cntrl.shannon_transect <- tapply(transcntrl_BG.id_sr$count, transcntrl_BG.id_sr$transect.x, calc_shannons)
transcntrl.shannon_df <- data.frame(
  transect = names(cntrl.shannon_transect),
  shannons_diversity = unname(cntrl.shannon_transect)
)

#quadrat shannon id
shannon_quadrat <- tapply(quadratBG.id_sr$count, quadratBG.id_sr$quadratID, calc_shannons)
shannonq_df <- data.frame(quadrat = names(shannon_quadrat), shannons_diversity = unname(shannon_quadrat))
print(shannonq_df)

#quadrat burn shannon id
quadburn_BG.id_sr
burn.shannon_quadrat <- tapply(quadburn_BG.id_sr$count, quadburn_BG.id_sr$quadratID, calc_shannons)
quadburn.shannon_df <- data.frame(
  quadratID = names(burn.shannon_quadrat),
  shannons_diversity = unname(burn.shannon_quadrat)
)

#quadrat cntrl shannon id 
cntrl.shannon_quadrat <- tapply(quadcntrl_BG.id_sr$count, quadcntrl_BG.id_sr$quadratID, calc_shannons)
quadcntrl.shannon_df <- data.frame(
  quadratID = names(cntrl.shannon_quadrat),
  shannons_diversity = unname(cntrl.shannon_quadrat)
)

#site by species matrix? attempted to use tdata and add species names from combospec, however matrix used codes as collumns - reformatting using pivot_longer returned confusing result
library(reshape)
site.species_matrix <-cast(tdata, quadratID ~ code, value='count', fun.aggregate=sum)
site.species_matrix
named_ssm <- merge(site.species_matrix, combospec, by = "code", all.x = TRUE)
#aforementioned reformatting attempt:
long_ssm <- pivot_longer(site.species_matrix,
                         + cols = quadratID,
                         + names_to = "code",
                         + values_to = "count")
long_ssm
named_ssm <- merge(long_ssm,combospec, by = "code", all.x = TRUE)
print(named_ssm)

#formatting?
#
AG.code <- which(colnames(site.species_matrix) %in% AG)
AG.sm <- site.species_matrix[,c(1,AG.code)]
head(AG.sm[,1:10]);dim(AG.sm)
head(site.species_matrix[,1:10]);dim(site.species_matrix)
#
AG_id.code <- which(colnames(site.species_matrix) %in% AG_id)
AG_id.sm <- site.species_matrix[,c(1,AG_id.code)]
head(AG_id.sm[,1:10]);dim(AG_id.sm)
head(site.species_matrix[,1:10]);dim(site.species_matrix)
#-------

#sorensen
library(divo)

calc_sorensen <- function(data1, data2) {
  similarity <- li(data1, data2) 
  return(similarity)
}
#adding burn_trt columns to quadrat species info
quadratBG.id_sr$burn_trt <- ifelse(quadratBG.id_sr$quadratID %in% BG.idburn_data$quadratID, "burn", "control")

quadcntrl_BG.id_sr <- subset(quadratBG.id_sr, burn_trt == "control")
quadburn_BG.id_sr <- subset(quadratBG.id_sr, burn_trt == "burn")

quad_sim <- apply(quadcntrl_BG.id_sr$count, 1, function(cntrl_count) {
  sapply(quadburn_BG.id_sr$count, function(burn_count) {
    calc_sorensen(cntrl_count, burn_count)
  })
})



div3 <- div2[,1:(ncol(div2)-1)]
div3$ab <- "above"
colnames(div3) [which(colnames(div3) == "agsr")] <- "sr"

head(div3);dim(div3)

div4 <- div2[,c(1:5,ncol(div2))]
div4$ab <- "below"
colnames(div4) [which(colnames(div4) == "bgsr")] <- "sr"
head(div4);dim(div4)



#sr_mod2
nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
sr_mod3 <- predictSE(mod=sr_mod2,newdata=nd1,type="response",se.fit = T)
sr_mod3 <- data.frame(nd1, fit = sr_mod3$fit, se = sr_mod3$se.fit)
sr_mod3$lci <- sr_mod3$fit-(sr_mod3$se*1.96)
sr_mod3$uci <- sr_mod3$fit+(sr_mod3$se*1.96)
#view sr_mod3
head(sr_mod3)
summary(sr_mod2)$coefficients

x_labels <- with(sr_mod3, interaction(ab, burn_trt))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), sr_mod3$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(sr_mod3$lci)), max(sr_mod3$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), sr_mod3$lci, c(1:4), sr_mod3$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=x_labels, tick=F, cex.axis=1)
title(main = "Species Richness by Treatment")

#alpha glmer
nd1 <- expand.grid(ab = factor(c("above", "below"), levels = c("above", "below")),
                   burn_trt = factor(c("Control", "Burn"), levels = c("Control", "Burn")))
srmod_all1 <- predictSE(mod=srmod_all,newdata=nd1,type="response",se.fit = T)
srmod_all1 <- data.frame(nd1, fit = srmod_all1$fit, se = srmod_all1$se.fit)
srmod_all1$lci <- srmod_all1$fit-(srmod_all1$se*1.96)
srmod_all1$uci <- srmod_all1$fit+(srmod_all1$se*1.96)
#view srmod_nat1
head(srmod_all1)
summary(srmod_all)$coefficients
xall_labels <- with(srmod_all1, interaction(ab, burn_trt))
xall_labels <- str_replace_all(xall_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(1,2))
plot(c(1:4), srmod_all1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_all1$lci)), max(srmod_all1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_all1$lci, c(1:4), srmod_all1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xall_labels, tick=F, cex.axis=1)
title(main = "Alpha Species Richness")

#native
srmod_nat1 <- predictSE(mod=srmod_nat,newdata=nd1,type="response",se.fit = T)
srmod_nat1 <- data.frame(nd1, fit = srmod_nat1$fit, se = srmod_nat1$se.fit)
srmod_nat1$lci <- srmod_nat1$fit-(srmod_nat1$se*1.96)
srmod_nat1$uci <- srmod_nat1$fit+(srmod_nat1$se*1.96)
#view srmod_nat1
head(srmod_nat1)
summary(srmod_nat)$coefficients
xnat_labels <- with(srmod_nat1, interaction(ab, burn_trt))
xnat_labels <- str_replace_all(xnat_labels, c("above\\.Control" = "AG Control", "below\\.Control" = "BG Control", "above\\.Burn" = "AG Burn", "below\\.Burn" = "BG Burn"))
#dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_nat1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_nat1$lci)), max(srmod_nat1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_nat1$lci, c(1:4), srmod_nat1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xnat_labels, tick=F, cex.axis=0.8)
title(main = "Native Species Richness")

#exotic
srmod_exo1 <- predictSE(mod=srmod_exo,newdata=nd1,type="response",se.fit = T)
srmod_exo1 <- data.frame(nd1, fit = srmod_exo1$fit, se = srmod_exo1$se.fit)
srmod_exo1$lci <- srmod_exo1$fit-(srmod_exo1$se*1.96)
srmod_exo1$uci <- srmod_exo1$fit+(srmod_exo1$se*1.96)
#view srmod_exo1
head(srmod_exo1)
summary(srmod_exo)$coefficients
xexo_labels <- with(srmod_exo1, interaction(ab, burn_trt))
xexo_labels <- str_replace_all(xexo_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
#
plot(c(1:4), srmod_exo1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exo1$lci)), max(srmod_exo1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exo1$lci, c(1:4), srmod_exo1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xexo_labels, tick=F, cex.axis=1)
title(main = "Exotic Species Richness")

#annual
srmod_ann1 <- predictSE(mod=srmod_ann,newdata=nd1,type="response",se.fit = T)
srmod_ann1 <- data.frame(nd1, fit = srmod_ann1$fit, se = srmod_ann1$se.fit)
srmod_ann1$lci <- srmod_ann1$fit-(srmod_ann1$se*1.96)
srmod_ann1$uci <- srmod_ann1$fit+(srmod_ann1$se*1.96)
#view srmod_ann1
head(srmod_ann1)
summary(srmod_ann)$coefficients
head(srmod_ann1)
summary(srmod_ann)$coefficients
xann_labels <- with(srmod_ann1, interaction(ab, burn_trt))
xann_labels <- str_replace_all(xann_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_ann1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_ann1$lci)), max(srmod_ann1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_ann1$lci, c(1:4), srmod_ann1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xann_labels, tick=F, cex.axis=1)
title(main = "Annual Species Richness")

#perennial
srmod_per1 <- predictSE(mod=srmod_per,newdata=nd1,type="response",se.fit = T)
srmod_per1 <- data.frame(nd1, fit = srmod_per1$fit, se = srmod_per1$se.fit)
srmod_per1$lci <- srmod_per1$fit-(srmod_per1$se*1.96)
srmod_per1$uci <- srmod_per1$fit+(srmod_per1$se*1.96)
#view srmod_per1
head(srmod_per1)
summary(srmod_per)$coefficients
xper_labels <- with(srmod_per1, interaction(ab, burn_trt))
xper_labels <- str_replace_all(xper_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_per1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_per1$lci)), max(srmod_per1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_per1$lci, c(1:4), srmod_per1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xper_labels, tick=F, cex.axis=1)
title(main = "Perennial Species Richness")

#legume
srmod_leg1 <- predictSE(mod=srmod_leg,newdata=nd1,type="response",se.fit = T)
srmod_leg1 <- data.frame(nd1, fit = srmod_leg1$fit, se = srmod_leg1$se.fit)
srmod_leg1$lci <- srmod_leg1$fit-(srmod_leg1$se*1.96)
srmod_leg1$uci <- srmod_leg1$fit+(srmod_leg1$se*1.96)
#view srmod_leg1
head(srmod_leg1)
summary(srmod_leg)$coefficients
xleg_labels <- with(srmod_leg1, interaction(ab, burn_trt))
xleg_labels <- str_replace_all(xleg_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_leg1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_leg1$lci)), max(srmod_leg1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_leg1$lci, c(1:4), srmod_leg1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xleg_labels, tick=F, cex.axis=1)
title(main = "Legume Species Richness")

#tree
srmod_tree1 <- predictSE(mod=srmod_tree,newdata=nd1,type="response",se.fit = T)
srmod_tree1 <- data.frame(nd1, fit = srmod_tree1$fit, se = srmod_tree1$se.fit)
srmod_tree1$lci <- srmod_tree1$fit-(srmod_tree1$se*1.96)
srmod_tree1$uci <- srmod_tree1$fit+(srmod_tree1$se*1.96)
#view srmod_tree1
head(srmod_tree1)
summary(srmod_tree)$coefficients
xtree_labels <- with(srmod_tree1, interaction(ab, burn_trt))
xtree_labels <- str_replace_all(xtree_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_tree1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_tree1$lci)), max(srmod_tree1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_tree1$lci, c(1:4), srmod_tree1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xtree_labels, tick=F, cex.axis=1)
title(main = "Tree Species Richness")

#shrub
srmod_shr1 <- predictSE(mod=srmod_shr,newdata=nd1,type="response",se.fit = T)
srmod_shr1 <- data.frame(nd1, fit = srmod_shr1$fit, se = srmod_shr1$se.fit)
srmod_shr1$lci <- srmod_shr1$fit-(srmod_shr1$se*1.96)
srmod_shr1$uci <- srmod_shr1$fit+(srmod_shr1$se*1.96)
#view srmod_shr1 #only ci for burn below
head(srmod_shr1)
summary(srmod_shr)$coefficients
xshr_labels <- with(srmod_shr1, interaction(ab, burn_trt))
xshr_labels <- str_replace_all(xshr_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_shr1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_shr1$lci)), max(srmod_shr1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_shr1$lci, c(1:4), srmod_shr1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xshr_labels, tick=F, cex.axis=1)
title(main = "Shrub Species Richness")

#forb
srmod_for1 <- predictSE(mod=srmod_for,newdata=nd1,type="response",se.fit = T)
srmod_for1 <- data.frame(nd1, fit = srmod_for1$fit, se = srmod_for1$se.fit)
srmod_for1$lci <- srmod_for1$fit-(srmod_for1$se*1.96)
srmod_for1$uci <- srmod_for1$fit+(srmod_for1$se*1.96)
#view srmod_for1
head(srmod_for1)
summary(srmod_for)$coefficients
xfor_labels <- with(srmod_for1, interaction(ab, burn_trt))
xfor_labels <- str_replace_all(xfor_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_for1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_for1$lci)), max(srmod_for1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_for1$lci, c(1:4), srmod_for1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xfor_labels, tick=F, cex.axis=1)
title(main = "Forb Species Richness")

#grass
srmod_gra1 <- predictSE(mod=srmod_gra,newdata=nd1,type="response",se.fit = T)
srmod_gra1 <- data.frame(nd1, fit = srmod_gra1$fit, se = srmod_gra1$se.fit)
srmod_gra1$lci <- srmod_gra1$fit-(srmod_gra1$se*1.96)
srmod_gra1$uci <- srmod_gra1$fit+(srmod_gra1$se*1.96)
#view srmod_gra1
head(srmod_gra1)
summary(srmod_gra)$coefficients
xgra_labels <- with(srmod_gra1, interaction(ab, burn_trt))
xgra_labels <- str_replace_all(xgra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_gra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_gra1$lci)), max(srmod_gra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_gra1$lci, c(1:4), srmod_gra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xgra_labels, tick=F, cex.axis=1)
title(main = "Grass Species Richness")

#sedge
srmod_sed1 <- predictSE(mod=srmod_sed,newdata=nd1,type="response",se.fit = T)
srmod_sed1 <- data.frame(nd1, fit = srmod_sed1$fit, se = srmod_sed1$se.fit)
srmod_sed1$lci <- srmod_sed1$fit-(srmod_sed1$se*1.96)
srmod_sed1$uci <- srmod_sed1$fit+(srmod_sed1$se*1.96)
#view srmod_sed1
head(srmod_sed1)
summary(srmod_sed)$coefficients
xsed_labels <- with(srmod_sed1, interaction(ab, burn_trt))
xsed_labels <- str_replace_all(xsed_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_sed1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_sed1$lci)), max(srmod_sed1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_sed1$lci, c(1:4), srmod_sed1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xsed_labels, tick=F, cex.axis=1)
title(main = "Sedge Species Richness")

#native grass
srmod_natgra1 <- predictSE(mod=srmod_natgra,newdata=nd1,type="response",se.fit = T)
srmod_natgra1 <- data.frame(nd1, fit = srmod_natgra1$fit, se = srmod_natgra1$se.fit)
srmod_natgra1$lci <- srmod_natgra1$fit-(srmod_natgra1$se*1.96)
srmod_natgra1$uci <- srmod_natgra1$fit+(srmod_natgra1$se*1.96)
#view srmod_natgra1
head(srmod_natgra1)
summary(srmod_natgra)$coefficients
xnatgra_labels <- with(srmod_natgra1, interaction(ab, burn_trt))
xnatgra_labels <- str_replace_all(xnatgra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_natgra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_natgra1$lci)), max(srmod_natgra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_natgra1$lci, c(1:4), srmod_natgra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xnatgra_labels, tick=F, cex.axis=1)
title(main = "Native Grass Species Richness")

#exotic grass
srmod_exogra1 <- predictSE(mod=srmod_exogra,newdata=nd1,type="response",se.fit = T)
srmod_exogra1 <- data.frame(nd1, fit = srmod_exogra1$fit, se = srmod_exogra1$se.fit)
srmod_exogra1$lci <- srmod_exogra1$fit-(srmod_exogra1$se*1.96)
srmod_exogra1$uci <- srmod_exogra1$fit+(srmod_exogra1$se*1.96)
#view srmod_exogra1
head(srmod_exogra1)
summary(srmod_exogra)$coefficients
xexogra_labels <- with(srmod_exogra1, interaction(ab, burn_trt))
xexogra_labels <- str_replace_all(xexogra_labels, c("above\\.Control" = "Above Control", "below\\.Control" = "Below Control", "above\\.Burn" = "Above Burn", "below\\.Burn" = "Below Burn"))
dev.new(width=10,height=4,dpi=160,pointsize=12, noRStudioGD = T)
plot(c(1:4), srmod_exogra1$fit, xlim=c(0.5,4.5), pch=20, xaxt="n", ylim=c((min(srmod_exogra1$lci)), max(srmod_exogra1$uci)), ylab="Species Richness", xlab="", las=1, cex=2.5)
arrows(c(1:4), srmod_exogra1$lci, c(1:4), srmod_exogra1$uci, length=0.3, code=3, angle=90)
axis(side=1, at=c(1:4), labels=xexogra_labels, tick=F, cex.axis=1)
title(main = "Exotic Grass Species Richness")

#biplot(pca1, xlab="Component 1",ylab="Component 2",col=c("grey40","black"),var.axes=T,arrow.len=0.1, c(3:n), c(4:n))
#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, c(seq(3, 60)), c(seq(4, 60)))
#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, c(3:60), c(4:60))


#AG shannon and simpson

div1$agshan <- diversity(AGmat, index = "shannon")
div1$agsimp <- diversity(AGmat, index = "invsimpson")


bwplot(div1$agsimp ~ div1$burn_trt)

#BG shannon and simpson
div1$bgshan <- diversity(BGmat, index = "shannon")
div1$bgsimp <- diversity(BGmat, index = "invsimpson")


bwplot(div1$bgsimp ~ div1$burn_trt)

#sorensen

#div1$bgsoren <- li(BGmat)
#div1 <- subset(div1, select = -bgsoren)

#margalef?
#div1$agmarg <- margalef(AGmat) doesn't work as for community level comparison only
AGmarg <- margalef(AGmat)
Bgmarg <- margalef(BGmat)




for (i in 1:nrow(group.df)) {
  
  name.thisrun <- as.character(group.df$group[i])
  vec.thisrun <- get(name.thisrun)
  
  print(vec.thisrun)  # Print vec.thisrun
  
  if (length(vec.thisrun) == 0) {
    cat("Warning: Empty vector.\n")
    next  # Skip to next iteration if vec.thisrun is empty
  }
  
  if (substr(name.thisrun, 1, 3) == "BG.") data.thisrun <- BGmat[, colnames(BGmat) %in% vec.thisrun]
  if (substr(name.thisrun, 1, 3) == "AG.") data.thisrun <- AGmat[, colnames(AGmat) %in% vec.thisrun]
  
  print(dim(data.thisrun))
  
  if (is.null(dim(data.thisrun))) {
    if (substr(name.thisrun, 1, 3) == "AG.") rownames.thisrun <- rownames(AGmat) else rownames.thisrun <- rownames(BGmat)
    data.thisrun <- data.frame(data.thisrun) 
    colnames(data.thisrun) <- vec.thisrun
    rownames(data.thisrun) <- rownames.thisrun
  }
  
  # Convert non-numeric data to NA
  data.thisrun <- apply(data.thisrun, 2, function(x) {
    if (all(is.na(as.numeric(x)))) {
      return(NA)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Check for NA values
  if (any(is.na(data.thisrun))) {
    cat("Warning: Non-numeric values found in data.\n")
    # Handle or remove NA values as needed
    # For simplicity, we'll just remove rows with NA values here
    data.thisrun <- na.omit(data.thisrun)
  }
  
  #rich.data[[i]] <- apply(data.thisrun, 1, function(x) length(which(x > 0)))
  
  simp.data[[i]] <- diversity(data.thisrun, index = "invsimpson")
  
} # close i for

simp.res <-data.frame(do.call(cbind,simp.data))
groupnames <- tail(group.df$group, 34)[-c(1:2)]
colnames(simp.res)<-groupnames



for (i in 1:nrow(group.df)) {
  
  name.thisrun <- as.character(group.df$group[i])
  vec.thisrun <- get(name.thisrun)
  
  print(vec.thisrun)  # Print vec.thisrun
  
  if (length(vec.thisrun) == 0) {
    cat("Warning: Empty vector.\n")
    next  # Skip to next iteration if vec.thisrun is empty
  }
  
  if (substr(name.thisrun, 1, 3) == "BG.") data.thisrun <- BGmat[, colnames(BGmat) %in% vec.thisrun]
  if (substr(name.thisrun, 1, 3) == "AG.") data.thisrun <- AGmat[, colnames(AGmat) %in% vec.thisrun]
  
  print(dim(data.thisrun))
  
  if (is.null(dim(data.thisrun))) {
    if (substr(name.thisrun, 1, 3) == "AG.") rownames.thisrun <- rownames(AGmat) else rownames.thisrun <- rownames(BGmat)
    data.thisrun <- data.frame(data.thisrun) 
    colnames(data.thisrun) <- vec.thisrun
    rownames(data.thisrun) <- rownames.thisrun
  }
  
  # Convert non-numeric data to NA
  data.thisrun <- apply(data.thisrun, 2, function(x) {
    if (all(is.na(as.numeric(x)))) {
      return(NA)
    } else {
      return(as.numeric(x))
    }
  })
  
  # Check for NA values
  if (any(is.na(data.thisrun))) {
    cat("Warning: Non-numeric values found in data.\n")
    # Handle or remove NA values as needed
    # For simplicity, we'll just remove rows with NA values here
    data.thisrun <- na.omit(data.thisrun)
  }
  
  # Check for Inf values after diversity calculation
  simp_val <- diversity(data.thisrun, index = "invsimpson")
  if (any(is.infinite(simp_val))) {
    cat("Warning: Infinite value detected for group", name.thisrun, "\n")
    # Replace infinite values with zero
    simp_val[is.infinite(simp_val)] <- 0
  }
  
  simp.data[[i]] <- simp_val
  
} # close i for



shapes<-c(15,17)

shapes<-shapes[as.factor(pcadata2$burn_trt)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcadata2$burn_trt)]
View(shapes)
View(col.1)

plot(pcadata2$pca.comp1,pcadata2$pca.comp2,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))



pcadata <- data.frame(quadratID = div8[,1])
pcadata <- data.frame(quadratID = rownames(div8), row.names = NULL)
head(pcadata,3); dim(pcadata)

pcoadata <- data.frame(quadratID = div8[,1])
pcoadata <- data.frame(quadratID = rownames(div8), row.names = NULL)
head(pcoadata,3); dim(pcoadata)

pcoa_res <- cmdscale(dist1, k = 5, eig = TRUE)

pcoadata$pcoa1 <- pcoa_res$points[, 1]
pcoadata$pcoa2 <- pcoa_res$points[, 2]
pcoadata$pcoa3 <- pcoa_res$points[, 3]
pcoadata$pcoa4 <- pcoa_res$points[, 4]
pcoadata$pcoa5 <- pcoa_res$points[, 5]

#eig
pcoaeig <- eigenvals(pcoaVS)
pcoapov <- pcoaeig/sum(pcoaeig)
cumsum(pcoaeig/sum(pcoaeig))



pcoadata$pcoa1<-pcoaVS$x[,1]

#pcadata$pca.comp1<-pca1$x[,1]
#pcadata$pca.comp2<-pca1$x[,2]
#pcadata$pca.comp3<-pca1$x[,3]
#head(pca1$x[1:3])
#head(pca1$rotation[1:3])
#head(pca1);dim(pca1)

head(pcoa_res$points[1:5])
head(pcoa_res$eig[1:5])
head(pcoa_res);dim(pcoa_res)

#pcoaVS$eig[,1]
#pcoa_res$eig[,2]
#pcoa_res$eig[,3]
#pcoa_res$eig[,4]
#pcoa_res$eig[,5]

pov1<-summary(pcoaVS)$[2,]

dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0))
plot(x=1:length(pcoapov),y=pcoapov,ylab="Propotion Variance Explained",xlab="Components",type="p")
lines(x=1:length(pcoapov),y=pcoapov)
mtext("(a)",3,0.7,F,0)


#pca1$rotation[,1]
#pca1$rotation[,2]
#pca1$rotation[,3]
#summary(pca1)

nd2 <- div4[,1:6]
head(nd2);dim(nd2)
head(div4);dim(div4)
nd2$quadratID2 <- paste(nd2$quadratID,nd2$ab, sep=".")

pcadata2 <- merge(pcadata,nd2, by.x = "quadratID", by.y = "quadratID2", all.x = T, all.y = F)
head(pcadata2);dim(pcadata2)

pcoadata2 <- merge(pcoadata,nd2, by.x = "quadratID", by.y = "quadratID2", all.x = T, all.y = F)
head(pcoadata2);dim(pcoadata2)



comp1_lmer<-lmer(pcoa1~ab*burn_trt+(1|transect), data=pcoadata2)
summary(comp1_lmer)
comp2_lmer<-lmer(pcoa1~ab+burn_trt+(1|transect), data=pcoadata2)
summary(comp2_lmer)

library("lmerTest")

srmod_ann.int<-glmer(annual~ab*burn_trt+(1|transect), family="poisson", data=div4)
summary(srmod_ann.int)

library(ggplot2)

shapes<-c(15,17)

shapes<-shapes[as.factor(pcadata2$ab)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcadata2$burn_trt)]
View(shapes)
View(col.1)

shapes<-c(15,17)
shapes<-shapes[as.factor(pcoadata2$burn_trt)]
col.1<-c("grey60","grey20")
col.1<-col.1[as.factor(pcoadata2$burn_trt)]
View(shapes)
View(col.1)

shape.ab <- ifelse(pcoadata2$ab == "above", 15, 17) 
col.burn <- ifelse(pcoadata2$burn_trt == "Control", "grey60", "grey20") 

#
head(pcoadata2);dim(pcoadata2)

dev.new(width=12,height=12,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(3,3),mar=c(4,4,1.5,1), mgp=c(2.5,1,0))
#1 vs 2
plot(pcoadata2$pcoa1,pcadata2$pcoa2,pch=shape.ab, xlab="",ylab="",cex=2,col=col.burn)
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))


dev.new(height=8,width=8,dpi=80,pointsize=14,noRStudioGD = T)
par(mar=c(4,4,2,2),mfrow=c(2,2),mgp=c(2.5,1,0))
plot(x=1:length(pov1),y=pov1,ylab="Propotion Variance Explained",xlab="Components",type="p")
lines(x=1:length(pov1),y=pov1)
mtext("(a)",3,0.7,F,0)
#scree plot (usually explained by first three point) 

#biplot(pca1, xlab="Component 1", ylab="Component 2", col=c("grey40","black"), var.axes=TRUE, arrow.len=0.1, choices=c(3, 4))
#mtext("(b)",3,0.7,F,adj = 0)


#mds
mds1 <- 


#1 vs 2
plot(pcoadata2$pcoa1,pcadata2$pcoa2,pch=shape.ab, xlab="",ylab="",cex=2,col=col.burn)
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(b)",3,0.4,F,adj=0)
title(ylab="PC2",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(-0.61888015), y=c(-7.32836938), labels= c("T26_20"), pos=4)
text(x=c(14.84107197), y=c(-0.54987242), labels= c("T06_57"), pos=2)

#1 vs 3
plot(pcoadata2$pcoa1,pcadata2$pcoa3,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#1 vs 4
plot(pcoadata2$pcoa1,pcadata2$pcoa4,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomleft",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(d)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC2",cex=1.2)
text(x=c(0.69979374), y=c(-6.582940182), labels= c("T10_06"), pos=2)
text(x=c(-7.32836938), y=c(1.416516483), labels= c("T26_20"), pos=4)

#1 vs 5
plot(pcoadata2$pcoa1,pcadata2$pcoa5,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)


#2 vs 3
plot(pcoadata2$pcoa2,pcadata2$pcoa3,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#2 vs 4
plot(pcoadata2$pcoa2,pcadata2$pcoa4,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#2 vs 5
plot(pcoadata2$pcoa2,pcadata2$pcoa5,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#3 vs 4
plot(pcoadata2$pcoa3,pcadata2$pcoa4,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#3 vs 5
plot(pcoadata2$pcoa3,pcadata2$pcoa5,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

dev.new(width=12,height=12,dpi=160,pointsize=12, noRStudioGD = T)
par(mfrow=c(3,3),mar=c(4,4,1.5,1), mgp=c(2.5,1,0))

#4 vs 5
plot(pcoadata2$pcoa4,pcadata2$pcoa5,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#1 vs 1
plot(pcoadata2$pcoa1,pcadata2$pcoa1,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#2 v 2
plot(pcoadata2$pcoa2,pcadata2$pcoa2,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#3 vs 3
plot(pcoadata2$pcoa3,pcadata2$pcoa3,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#4 vs 4
plot(pcoadata2$pcoa4,pcadata2$pcoa4,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)

#5 vs 5
plot(pcoadata2$pcoa5,pcadata2$pcoa5,pch=shapes, xlab="",ylab="",cex=2,col=alpha(col.1,1))
legend("bottomright",legend=c("Control Sites", "Burn Sites"),pch=c(15,17),pt.cex=2,col=c("grey60","grey20"))
mtext("(c)",3,0.4,F,adj=0)
title(ylab="PC3",cex=1.2)
title(xlab="PC1",cex=1.2)
text(x=c(0.49994222), y=c(-6.582940182), labels= c("T10_06"), pos=4)
text(x=c(14.84107197), y=c(0.614645118), labels= c("T06_57"), pos=2)


#-----
#cleaning

#div8 <- div7[,which(colnames(div7)== "Aca_mai"):ncol(div7)]
#rownames(div8) <- div7$quadratID
#head(div8[1:10],3);dim(div8)

#PCA matrices
#abundance above and below ssm
#div6 <- div4
#div6$quadratID <-paste(div6$quadratID,div6$ab,sep=".")
#div6 <- subset(div6, select = -ab)
#head(div6[1:10]);dim(div6)

#overwrite
#row.names(AGmat) <- paste0(row.names(AGmat), ".above")
#row.names(BGmat) <- paste0(row.names(BGmat), ".below")
#ALLmat <- cbind(AGmat,BGmat)
#head(ALLmat[1:10]);dim(ALLmat)

#div6 <- cbind(div6,ALLmat)
#head(div6[1:60]);dim(div6)
#remove functional

#ssmat <- div6[,which(colnames(div6) == "Aca_mai"):ncol(div6)]
#rownames(ssmat) <- div6$quadratID
#head(ssmat[1:10],3);dim(ssmat)

#---- four-way venn? 
#merging datasets
merged_tdata <- merge(combospec, tdata, by = "code", all.x = T)
merged_AGdata <- merge(combospec, AGdata, by = "code", all.x = T)
merged_data <- merge(merged_tdata, merged_AGdata, by = "code", all = TRUE)

merged_data$burn <- ifelse(merged_data$burn_trt == "Control", 0, 
                           ifelse(merged_data$burn_trt == "Burn", 1, 2))

merged_data_summary <- merged_data %>%
  group_by(code) %>%
  summarize(burn = case_when(
    all(burn == 0) ~ 0,   # If all burns are 0, assign 0
    all(burn == 1) ~ 1,   # If all burns are 1, assign 1
    TRUE ~ 2              # Otherwise, assign 2
  ),
  species.x = first(species.x),
  origin.x = first(origin.x),
  life_span.x = first(life_span.x),
  form.x = first(form.x),
  family.x = first(family.x),
  speciesID.x = case_when(
    all(speciesID.x == 1) ~ 1,
    all(speciesID.x == 0) ~ 0,),
  location.x.y = case_when(
    all(location.x.y == 1) ~ 0,
    all(location.x.y == 1) ~ 1,
    all(location.x.y == 2) ~ 1
  )  )

merged_data_sum <- as.data.frame(merged_data_summary)
merged_data_sum <- merge(merged_data_sum, combospec, by = "code", all.x = T)
remove_columns <- c("species.x", "origin.x", "life_span.x", "form.x", "family.x", "speciesID.x", "location.x.y")
merged_data_sum <- merged_data_sum[, !(names(merged_data_sum) %in% remove_columns)]
merged_data_sumID <- merged_data_sum[merged_data_sum$speciesID == 1, ]

#quad.venn
draw.quad.venn(
  area1 = sum((dimv4$location=="" combospec$location == "0" & combospec$speciesID == "1") + (combospec$location == "2" & combospec$speciesID == "1")) , area2, area3, area4, n12, n13, n14, n23, n24,
  n34, n123, n124, n134, n234, n1234, category = rep("",
                                                     4), lwd = rep(2, 4), lty = rep("solid", 4), col =
    rep("black", 4), fill = NULL, alpha = rep(0.5, 4),
  label.col = rep("black", 15), cex = rep(1, 15),
  fontface = rep("plain", 15), fontfamily = rep("serif",
                                                15), cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22,
                                                                                              0.22, 0.11, 0.11), cat.col = rep("black", 4), cat.cex
  = rep(1, 4), cat.fontface = rep("plain", 4),
  cat.fontfamily = rep("serif", 4), cat.just =
    rep(list(c(0.5, 0.5)), 4), rotation.degree = 0,
  rotation.centre = c(0.5, 0.5), ind = TRUE, cex.prop =
    NULL, print.mode = "raw", sigdigs = 3, direct.area =
    FALSE, area.vector = 0, ...)
#-----
library(venneuler)
area_0_0 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
area_0_1 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
area_1_0 <- sum(merged_data_sum$burn == 1 & merged_data_sum$location == 0)
area_1_1 <- sum(merged_data_sum$burn == 1 & merged_data_sum$location == 1)

# Define the intersection sizes
n_00_00 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_01 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_10 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_11 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_12 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 2)
n_00_20 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 0)
n_00_21 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 1)
n_00_22 <- sum(merged_data_sum$burn == 0 & merged_data_sum$location == 2)

# Draw the Venn diagram
venn_plot <- draw.quad.venn(
  area1 = area_0_0,
  area2 = area_0_1,
  area3 = area_1_0,
  area4 = area_1_1,
  n12 = n_00_00,
  n13 = n_00_01,
  n14 = n_00_10,
  n23 = n_00_02,
  n24 = n_00_11,
  n34 = n_00_12,
  n123 = n_00_20,
  n124 = n_00_21,
  n134 = n_00_22,
  n234 = 0,  # No data for this intersection
  n1234 = 0, # No data for this intersection
  category = c("Control", "Control above", "Treatment", "Treatment above"),
  fill = c("blue", "red", "green", "yellow"),
  label.col = rep("black", 15),
  cex = rep(1, 15),
  fontface = rep("plain", 15),
  cat.dist = c(0.15, 0.15, 0.15, 0.15), # Adjust label distance
  cat.cex = 1.5, # Adjust label size
  cat.fontface = 1, # Adjust label fontface
  cat.just = list(c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5), c(0.5, 0.5)) # Center label text
)

# Plot the Venn diagram
grid.draw(venn_plot)

