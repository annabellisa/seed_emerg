

# Original script, first submission
# all
head(div4);dim(div4)
head(AGmat[,1:10]);dim(AGmat)
head(BGmat[,1:10]);dim(BGmat)

range(colSums(AGmat))
range(colSums(BGmat))

AGgam <- ncol(AGmat)
BGgam <- ncol(BGmat)

AGbeta <- AGgam/(div4$all[which(div4$ab == "above")])
BGbeta <- BGgam/(div4$all[which(div4$ab == "below")])

div4$beta.all <- c(AGbeta, BGbeta)

# beta native
AGbeta.nat <- group.df$no_species[group.df$group == "AG.native"]/(div4$native[which(div4$ab == "above")])
BGbeta.nat <- group.df$no_species[group.df$group == "BG.native"]/(div4$native[which(div4$ab == "below")])

div4$beta.nat <- c(AGbeta.nat, BGbeta.nat)

# beta exotic
AGbeta.exo <- group.df$no_species[group.df$group == "AG.exotic"]/(div4$exotic[which(div4$ab == "above")])
BGbeta.exo <- group.df$no_species[group.df$group == "BG.exotic"]/(div4$exotic[which(div4$ab == "below")])

div4$beta.exo <- c(AGbeta.exo, BGbeta.exo)

# beta exotic
AGbeta.exo <- group.df$no_species[group.df$group == "AG.exotic"]/(div4$exotic[which(div4$ab == "above")])
BGbeta.exo <- group.df$no_species[group.df$group == "BG.exotic"]/(div4$exotic[which(div4$ab == "below")])

div4$beta.exo <- c(AGbeta.exo, BGbeta.exo)

# beta annual
AGbeta.ann <- ifelse(div4$annual[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.annual"] /div4$annual[which(div4$ab == "above")])
BGbeta.ann <- ifelse(div4$annual[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.annual"] /div4$annual[which(div4$ab == "below")])

div4$beta.ann <- c(AGbeta.ann, BGbeta.ann)

# beta perennial
AGbeta.per <- group.df$no_species[group.df$group == "AG.perr"]/(div4$perr[which(div4$ab == "above")])
BGbeta.per <- group.df$no_species[group.df$group == "BG.perr"]/(div4$perr[which(div4$ab == "below")])

div4$beta.per <- c(AGbeta.per, BGbeta.per)

# beta forb
AGbeta.for <- group.df$no_species[group.df$group == "AG.forb"]/(div4$forb[which(div4$ab == "above")])
BGbeta.for <- group.df$no_species[group.df$group == "BG.forb"]/(div4$forb[which(div4$ab == "below")])

div4$beta.for <- c(AGbeta.for, BGbeta.for)

# beta grass
AGbeta.gra <- group.df$no_species[group.df$group == "AG.grass"]/(div4$grass[which(div4$ab == "above")])
BGbeta.gra <- group.df$no_species[group.df$group == "BG.grass"]/(div4$grass[which(div4$ab == "below")])

div4$beta.gra <- c(AGbeta.gra, BGbeta.gra)

# beta native grass
AGbeta.natgra <- group.df$no_species[group.df$group == "AG.native_grass"]/(div4$native_grass[which(div4$ab == "above")])
BGbeta.natgra <- group.df$no_species[group.df$group == "BG.native_grass"]/(div4$native_grass[which(div4$ab == "below")])

div4$beta.natgra <- c(AGbeta.natgra, BGbeta.natgra)

# beta exotic grass
AGbeta.exogra <- ifelse(div4$exotic_grass[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.exotic_grass"] /div4$exotic_grass[which(div4$ab == "above")])
BGbeta.exogra <- ifelse(div4$exotic_grass[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.exotic_grass"] /div4$exotic_grass[which(div4$ab == "below")])

div4$beta.exogra <- c(AGbeta.exogra, BGbeta.exogra)

# beta native forb
AGbeta.natfor <- group.df$no_species[group.df$group == "AG.native_forb"]/(div4$native_forb[which(div4$ab == "above")])
BGbeta.natfor <- group.df$no_species[group.df$group == "BG.native_forb"]/(div4$native_forb[which(div4$ab == "below")])

div4$beta.natfor <- c(AGbeta.natfor, BGbeta.natfor)

# beta exotic forb
AGbeta.exofor <- ifelse(div4$exotic_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.exotic_forb"] /div4$exotic_forb[which(div4$ab == "above")])
BGbeta.exofor <- ifelse(div4$exotic_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.exotic_forb"] /div4$exotic_forb[which(div4$ab == "below")])

div4$beta.exofor <- c(AGbeta.exofor, BGbeta.exofor)

# beta non leguminous forb
AGbeta.nlegfor <- ifelse(div4$nonleg_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.nonleg_forb"] /div4$nonleg_forb[which(div4$ab == "above")])
BGbeta.nlegfor <- ifelse(div4$nonleg_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.nonleg_forb"] /div4$nonleg_forb[which(div4$ab == "below")])

div4$beta.nlegfor <- c(AGbeta.nlegfor, BGbeta.nlegfor)

# beta leguminous forb
AGbeta.legfor <- ifelse(div4$leg_forb[which(div4$ab == "above")] == 0, 0, group.df$no_species[group.df$group == "AG.leg_forb"] /div4$leg_forb[which(div4$ab == "above")])
BGbeta.legfor <- ifelse(div4$leg_forb[which(div4$ab == "below")] == 0, 0, group.df$no_species[group.df$group == "BG.leg_forb"] /div4$leg_forb[which(div4$ab == "below")])

div4$beta.legfor <- c(AGbeta.legfor, BGbeta.legfor)





# Beta diversity: model estimates ----

# predictSE beta diversity

# all
srmod_beta.all <- predictSE(mod=allbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.all <- data.frame(nd1, fit = srmod_beta.all$fit, se = srmod_beta.all$se.fit)
srmod_beta.all$lci <- srmod_beta.all$fit-(srmod_beta.all$se*1.96)
srmod_beta.all$uci <- srmod_beta.all$fit+(srmod_beta.all$se*1.96)
head(srmod_beta.all)

# native
srmod_beta.nat <- predictSE(mod=natbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.nat <- data.frame(nd1, fit = srmod_beta.nat$fit, se = srmod_beta.nat$se.fit)
srmod_beta.nat$lci <- srmod_beta.nat$fit-(srmod_beta.nat$se*1.96)
srmod_beta.nat$uci <- srmod_beta.nat$fit+(srmod_beta.nat$se*1.96)
head(srmod_beta.nat)

# exotic
srmod_beta.exo <- predictSE(mod=exobeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exo <- data.frame(nd1, fit = srmod_beta.exo$fit, se = srmod_beta.exo$se.fit)
srmod_beta.exo$lci <- srmod_beta.exo$fit-(srmod_beta.exo$se*1.96)
srmod_beta.exo$uci <- srmod_beta.exo$fit+(srmod_beta.exo$se*1.96)
head(srmod_beta.exo)

# annual
srmod_beta.ann <- predictSE(mod=annbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.ann <- data.frame(nd1, fit = srmod_beta.ann$fit, se = srmod_beta.ann$se.fit)
srmod_beta.ann$lci <- srmod_beta.ann$fit-(srmod_beta.ann$se*1.96)
srmod_beta.ann$uci <- srmod_beta.ann$fit+(srmod_beta.ann$se*1.96)
head(srmod_beta.ann)

# perennial
srmod_beta.per <- predictSE(mod=perbeta,newdata=nd1,type="response",se.fit = T)
srmod_beta.per <- data.frame(nd1, fit = srmod_beta.per$fit, se = srmod_beta.per$se.fit)
srmod_beta.per$lci <- srmod_beta.per$fit-(srmod_beta.per$se*1.96)
srmod_beta.per$uci <- srmod_beta.per$fit+(srmod_beta.per$se*1.96)
head(srmod_beta.per)

# forb
srmod_beta.for <- predictSE(mod=forbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.for <- data.frame(nd1, fit = srmod_beta.for$fit, se = srmod_beta.for$se.fit)
srmod_beta.for$lci <- srmod_beta.for$fit-(srmod_beta.for$se*1.96)
srmod_beta.for$uci <- srmod_beta.for$fit+(srmod_beta.for$se*1.96)
head(srmod_beta.for)

# grass
srmod_beta.gra <- predictSE(mod=grabeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.gra <- data.frame(nd1, fit = srmod_beta.gra$fit, se = srmod_beta.gra$se.fit)
srmod_beta.gra$lci <- srmod_beta.gra$fit-(srmod_beta.gra$se*1.96)
srmod_beta.gra$uci <- srmod_beta.gra$fit+(srmod_beta.gra$se*1.96)
head(srmod_beta.gra)

# native grass
srmod_beta.natgra <- predictSE(mod=natgrabeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.natgra <- data.frame(nd1, fit = srmod_beta.natgra$fit, se = srmod_beta.natgra$se.fit)
srmod_beta.natgra$lci <- srmod_beta.natgra$fit-(srmod_beta.natgra$se*1.96)
srmod_beta.natgra$uci <- srmod_beta.natgra$fit+(srmod_beta.natgra$se*1.96)
head(srmod_beta.natgra)

# exotic grass
srmod_beta.exogra <- predictSE(mod=exograbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exogra <- data.frame(nd1, fit = srmod_beta.exogra$fit, se = srmod_beta.exogra$se.fit)
srmod_beta.exogra$lci <- srmod_beta.exogra$fit-(srmod_beta.exogra$se*1.96)
srmod_beta.exogra$uci <- srmod_beta.exogra$fit+(srmod_beta.exogra$se*1.96)
head(srmod_beta.exogra)

# native forb
srmod_beta.natfor <- predictSE(mod=natforbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.natfor <- data.frame(nd1, fit = srmod_beta.natfor$fit, se = srmod_beta.natfor$se.fit)
srmod_beta.natfor$lci <- srmod_beta.natfor$fit-(srmod_beta.natfor$se*1.96)
srmod_beta.natfor$uci <- srmod_beta.natfor$fit+(srmod_beta.natfor$se*1.96)
head(srmod_beta.natfor)

# exotic forb
srmod_beta.exofor <- predictSE(mod=exoforbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.exofor <- data.frame(nd1, fit = srmod_beta.exofor$fit, se = srmod_beta.exofor$se.fit)
srmod_beta.exofor$lci <- srmod_beta.exofor$fit-(srmod_beta.exofor$se*1.96)
srmod_beta.exofor$uci <- srmod_beta.exofor$fit+(srmod_beta.exofor$se*1.96)
head(srmod_beta.exofor)

# non-leguminous forb
srmod_beta.nlegfor <- predictSE(mod=nlegbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.nlegfor <- data.frame(nd1, fit = srmod_beta.nlegfor$fit, se = srmod_beta.nlegfor$se.fit)
srmod_beta.nlegfor$lci <- srmod_beta.nlegfor$fit-(srmod_beta.nlegfor$se*1.96)
srmod_beta.nlegfor$uci <- srmod_beta.nlegfor$fit+(srmod_beta.nlegfor$se*1.96)
head(srmod_beta.nlegfor)

# leguminous forb
srmod_beta.legfor <- predictSE(mod=legbeta2,newdata=nd1,type="response",se.fit = T)
srmod_beta.legfor <- data.frame(nd1, fit = srmod_beta.legfor$fit, se = srmod_beta.legfor$se.fit)
srmod_beta.legfor$lci <- srmod_beta.legfor$fit-(srmod_beta.legfor$se*1.96)
srmod_beta.legfor$uci <- srmod_beta.legfor$fit+(srmod_beta.legfor$se*1.96)
head(srmod_beta.legfor)

# save.image("04_workspaces/seedbank_analysis.RData")

# ----