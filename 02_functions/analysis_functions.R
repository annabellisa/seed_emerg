
#########################################
####  	       FUNCTIONS:    	    	 ####
#########################################

# author: Annabel Smith

# Drop levels and re-assign rownames to subsetted data frames:
tidy.df<-function(df){
df<-droplevels(df)
rownames(df)<-1:length(df[,1])
return(df)
}

# Randomly check rows of a data frame:
check.rows<-function(df,n=6){df[sample(1:length(df[,1]),n),]}

# p value for lmer model:
lmer.pval<-function(x) 2*(1-pnorm(abs(x))) # x is the t-value from the model summary

# Variance inflation factor for multicollinearity analysis (from https://onlinecourses.science.psu.edu/stat501/node/347):
my_vif<-function(r2){1/(1-r2)}

# Blank plot. plot.new() does this but I don't understand the defaults; I defined this so I know what's where.
blankplot<-function()plot(1:10,1:10,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")

# Plot pairwise correlations between multiple terms in a data frame:
plot.pw<-function(data,cor.mat,pw.names){
# data: the data frame with variables to be correlated, IN QUOTES
# cor.mat: the correlation matrix, no quotes
# pw.names: pairs of names of variables to be correlated (result of a combn()), no quotes

# make matrix a data.frame so they can be sorted by correlation
cor.df<-data.frame(var1=pw.names[1,],var2=pw.names[2,],cor=cor.mat[lower.tri(cor.mat)])
cor.df<-cor.df[order(-abs(cor.df$cor)),]
cor.df<-tidy.df(cor.df)

for (i in 1:nrow(cor.df)){

data.thisrun<-cor.df[i,]

n1<-paste(data,"$",data.thisrun$var1,sep="")
n2<-paste(data,"$",data.thisrun$var2,sep="")
r.thisrun<-data.thisrun[,"cor"]

if (length(unique(eval(parse(text=n1))))>2) x.thisrun<-eval(parse(text=n1)) else x.thisrun<-as.factor(eval(parse(text=n1)))

plot(x.thisrun,eval(parse(text=n2)), xlab="", ylab=data.thisrun$var2, bty="l",pch=20, las=1,main="")
title(xlab=data.thisrun$var1,line=2)
par(xpd=NA)
mtext(bquote(bold("r = "~.(as.character(round(r.thisrun,3))))),adj=0, col="red",cex=1.5)

} # close i no. correlations

} # close plot.pw function

# Analyse multicollinearity among many terms in a data frame. This has been simplified from mcl_v2 in CATFORD_E93 to make it more general:
mcl_v3<-function(datasets){
# datasets: vector containing the datasets to be analysed, IN QUOTES

res.list<-list()

for (m in 1:length(datasets)){

data.thisrun<-get(datasets[m])
head(data.thisrun,3)

cnames<-colnames(data.thisrun)
out.mat1<-matrix(data=NA, nrow=length(cnames), ncol=3)

for(i in 1:length(cnames)){
resp.thisrun<-cnames[i]
preds.thisrun<-cnames[-which(cnames==resp.thisrun)]

formula.thisrun<-paste(resp.thisrun,"~",paste(preds.thisrun,collapse="+"), sep="")
r2<-summary(lm(eval(parse(text=formula.thisrun)),data=data.thisrun))$r.squared
VIF<-my_vif(r2)
out.mat1[i,1]<-resp.thisrun
out.mat1[i,2]<-r2
out.mat1[i,3]<-VIF
}
res.r2<-data.frame(out.mat1)
colnames(res.r2)<-c("response","r2","VIF")
res.r2[,2]<-as.numeric(as.character(res.r2[,2]))
res.r2[,3]<-as.numeric(as.character(res.r2[,3]))

res.list[[m]]<-res.r2
}
return(data.frame(do.call(rbind,res.list)))
} # close multicollinearity function

# predCI
predCI<-function(model, new.data, se.fit=T)
{
  works.on<-c("lm")
  if(class(model)[1] %in% works.on){
  p1<-predict(model, newdata = new.data, se.fit=T)
  p1<-data.frame(new.data, fit=p1$fit, se=p1$se.fit)
  p1$lci<-p1$fit-(1.96*p1$se)
  p1$uci<-p1$fit+(1.96*p1$se)
  return(p1)
  }
  if(class(model)[1]!="lm") stop("function not defined for this class")
}

# PREDICT function
# extends predictSE to calculate CIs and dataframe-ise predictions with 'new data'
# updated Feb 2026 for glmmTMB; haven't re-tested for glmerMod after update

pred<-function(model,new.data,se.fit=T,type="response"){
  
  library("arm")
  
  if(class(model)[1]=="glmerMod"){
    pr1<-predictSE(model,new.data,se.fit=se.fit, type=type)
    df1<-data.frame(new.data,fit=pr1$fit,se=pr1$se.fit, lci=pr1$fit-(1.96*pr1$se.fit), uci=pr1$fit+(1.96*pr1$se.fit))
  } # close lme4 models
  
  if(class(model)=="glmmadmb"){
    pr1<-suppressWarnings(predict(model,new.data,se.fit=se.fit, type="link"))
    df1<-data.frame(new.data,fit.link=pr1$fit,se.link=pr1$se.fit, lci.link=pr1$fit-(1.96*pr1$se.fit), uci.link=pr1$fit+(1.96*pr1$se.fit))	
    
    if(summary(model)$link=="log"){
      df1$fit.resp<-round(exp(df1$fit.link),4)
      df1$lci.resp<-round(exp(df1$lci.link),4)
      df1$uci.resp<-round(exp(df1$uci.link),4)
    } # close log
    
    if(summary(model)$link=="logit"){
      df1$fit.resp<-round(invlogit(df1$fit.link),6)
      df1$lci.resp<-round(invlogit(df1$lci.link),6)
      df1$uci.resp<-round(invlogit(df1$uci.link),6)
    } # close logit 
    
  } # close admb models
  
  if(class(model)=="glmmTMB"){
    pr1<-predict(model, new.data, se.fit = se.fit, re.form=NA, type=type)
    df1<-data.frame(new.data,fit=pr1$fit,se=pr1$se.fit, lci=pr1$fit-(1.96*pr1$se.fit), uci=pr1$fit+(1.96*pr1$se.fit))
  } # close TMB models
  
  return(df1)
  
} # close predict function








