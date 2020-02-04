library(meta)
library(plyr)
library(xtable)
library(ggplot2)
library(reshape2)
setwd("/Users/coraschefft/Documents/Review inpatPT/R_meta")

dir(pattern="endpoint")
endpoint<-read.table("inpatient_1.txt", header=TRUE)
endpoint<-endpoint[order(endpoint$Author),]
endpoint$Author<-c("Barth CBT", "Bowers CBT", "Bowers CBT", "Bowers CBT", "Bowers CBT",
                   "Bowers c-CBT","Bowers c-CBT",
                   "Brand BAT", "Brand BAT", "De Jong-Meyer CBT", "De Jong-Meyer CBT", "De Jong COMB",
                   "De Jong COMB", "De Jong CR", "De Jong CR", "De Roten PDT", "Hauksson ICBT",
                   "Hauksson GCBT", "Hautzinger CT", "Hautzinger CT", "Hautzinger CT+AD",
                   "Hautzinger CT+AD", "Hopko BAT", "Köhler CBT", "Köhler CBT", 
                   "Miller CBT", "Miller CBT", "Miller SST", "Miller SST", "Rieu UCBT", "Rieu UCBT",
                   "Schramm IPT", "Schramm IPT")



##############nur Fremd vs Selbst: Alle Studien, die sowohl fremd- als auch selbstratings haben. 
a<-endpoint[-c(1,17,18,23),]  # Selektion der rows, die sowohl fremd- als auch clinicianrating haben in neuen Dataframe namens a
a$instru<-ifelse(a$instr=="BDI",0,1) 
a$instru<-factor(a$instru, levels=c(0, 1), labels=c("self", "clinician")) # Faktorisieren der Instrumentenvariable als self/clnician
ma<-metacont(Nc,ti_Mc,ti_Sc, Ne,ti_Me,ti_Se, studlab=paste(Author, Year, instr), 
             sm="SMD", data=a, comb.fixed = FALSE, comb.random = TRUE) #meta-analyse über a
print(summary(ma), digits=2)
madf<-as.data.frame(ma) #errechnete Metaanalyse in dataframe umwandelnt
madf<-madf[,c(7,1,4,8,9)] #selektieren von coloumns
madf<-cbind(madf,a$instru) #Einfügen von intrumentenvariable
names(madf)<-c("studlab","n.e", "n.c","TE", "seTE","instr") #bennenen der columns
#add DeRoten Data QIQS-SR
#deroten<-data.frame("De Roten PDT 2017", 76,73,0.19,0.158163265,2,0,12)

#Erstellen eines dataframes mit den QIDS-SR Daten der deRoten-Studie, die separat berechnet wurden, da die Studie die 
#fertig berechnete Effektstärke angab
derotenslim<-data.frame("De Roten PDT 2017 QIDS-SR", 76,73,0.19,0.158163265,"self") 
names(derotenslim)<-c("studlab","n.e","n.c","TE","seTE","instr")

#Zusammenfügen des dataframes a und des deRoten-dataframes
svsc<-rbind(madf,derotenslim)
svsc<-svsc[order(svsc$studlab),]


#Metaanalyse des kompletten dataframes self vs clinician
svscmeta<-metagen(TE, seTE, studlab=studlab, data=svsc,sm="SMD",
                  comb.fixed=FALSE,  byvar=instr, print.byvar=FALSE, tau.common=TRUE)

print(summary(svscmeta), digits=2)
#forestplot der Metanalyse
forest(svscmeta,xlab="Diff", fontsize=8)
metareg(svscmeta, svsc$instr) # als Metaregression, um DELTA g zu bestimmen inkl Konfidenzintervall

########subset dataframe für fremd rating mit berechnung für baseline severity
other<-subset(endpoint, endpoint$instr!="BDI")
other<-other[order(other$Author),]

# Metaanalyse nur fremdrating
othermeta<-metacont(Nc, ti_Mc, ti_Sc, Ne,ti_Me, ti_Se, studlab=paste(Author, Year),
                      sm="SMD", data=other[1:15,], comb.fixed = FALSE, comb.random = TRUE)
print(summary(othermeta), digits=2)
#Forestplot für Fremdrating
forest(othermeta, calcwidth.tests = TRUE, 
       xlab.pos=c(-1.7,1), cex=1.5, 
       xlab=c("Favors control", "Favors intervention"), xlim=c(-2,1))

#Berechnung der Gesamtstichprobengröße
nother<-other$Ne+other$Nc 
sum(nother)


#########subset dataframes nur self-report
self<-subset(endpoint, endpoint$instr=="BDI")
self<-self[order(self$Author),]


#metanalyse und forest plots für den self-report dataframe nur bdi      
selfmeta<-metacont(Nc, ti_Mc, ti_Sc,Ne,ti_Me, ti_Se,  studlab=paste(Author, Year),
                   sm="SMD", data=self, comb.fixed = FALSE, comb.random = TRUE)
forest(selfmeta, xlab="Diff")
print(summary(selfmeta), digits=2)


#Umwandeln von selsfmeta in dataframe und in selfmeta zusätzlich die QIDS-SR Daten von deRoten einfügen
selfmetadf<-as.data.frame(selfmeta)
selfmetadf<-selfmetadf[,c(7,1,4,8,9)]
cg<-as.data.frame(subset(self,select=CG:sess))
names(cg)<-c("CG","bal","sessions")
selfmetadf<-cbind(selfmetadf,cg)


deroten<-data.frame("De Roten PDT 2017", 76,73,0.19,0.158163265,2,0,12)
names(deroten)<-c("studlab" ,"n.e","n.c","TE","seTE","CG","bal","sessions")

selfmslim<-rbind(selfmetadf,deroten) #Zusammenfügen der dataframes zu gesamtem self-report dataframe
selfmslim<-selfmslim[order(substr(selfmslim$studlab,1,2)),]
selfmslim$CG<-factor(selfmslim$CG, levels=c("1","2"), labels=c("WL or unstructured control condition",
                                                                   "structured control condition"))

nself<-sum(selfmslim$n.e+selfmslim$n.c) # gemsates n für die self-report Daten

########## Gruppieren der Sitzungsanzahlen und Faktoriesien als Variable "Mediansplit"
selfmslim[selfmslim$sessions>20,]
selfmslim[selfmslim$sessions>10 & selfmslim$sessions <=20,]
selfmslim$mediansplit<-1
selfmslim$mediansplit[selfmslim$sessions>20]<-3
selfmslim$mediansplit[selfmslim$sessions>10 & selfmslim$sessions <=20]<-2

selfmslim$mediansplit <- factor(selfmslim$mediansplit, labels=c ("<=10 sessions","11-20 sessions",">=21"))

sum(selfmslim$n.e+selfmslim$n.c)


##########################################################################
##########################################################################
#Metaanalyse und forest plot mit selfmslim 
##########################################################################
##########################################################################


selfmain<-metagen(TE, seTE, studlab = studlab, data=selfmslim,
                sm="SMD", comb.fixed = FALSE)
print(summary(selfmain), digits=2)

forest(selfmain,leftcols = "studlab", calcwidth.tests = TRUE, 
       smlab="Standardized mean\ndifference", 
       xlab.pos=c(-1,1.7), cex=1.5,
       xlab=c("Favors control", "Favors intervention"), xlim=c(-1.5, 2))

####leave- one -out Analyse

metainf(selfmain,sortvar = selfmain$studlab)


######## Control condition Subgruppen-Analyse
##########################################
#########################################

selfmaincontrol<-update(selfmain, byvar=selfmslim$CG, print.byvar=FALSE, tau.common=TRUE, comb.random = FALSE, 
                        comb.fixed = TRUE)
print(summary(selfmaincontrol),digits=2)

selfctrlreg<-metareg(selfmain, CG) #als Metaregression, für DELTA g und Konfidenzintervall

forest(selfmaincontrol,leftcols = "studlab", calcwidth.tests = TRUE, 
       smlab="Standardized mean\ndifference", test.effect.subgroup=TRUE,digits.pval = 3,
       xlab.pos=c(-1,1.7), cex=1.5,
       xlab=c("Favors control", "Favors intervention"), xlim=c(-1.5, 2))

selfmainlowc<-subset(selfmslim,selfmslim$CG=="WL or unstructured control condition")
selfmainhighc<-subset(selfmslim,selfmslim$CG=="structured control condition")
selfmainlow<-metagen(TE, seTE, studlab = studlab, data=selfmainlowc,
                     sm="SMD", comb.fixed = FALSE)
print(summary(selfmainlow),digits=2)
selfmainhigh<-metagen(TE, seTE, studlab = studlab, data=selfmainhighc,
                     sm="SMD", comb.fixed = FALSE)
print(summary(selfmainhigh),digits=2)
######## Subgruppen-Analyse für Anzahl der Therapiesitzunden
##########################################
#########################################

selfmainsessions<-update(selfmain, byvar=selfmslim$mediansplit,print.byvar=FALSE, tau.common=TRUE)
print(summary(selfmainsessions),digits=3)
xtable(summary(selfmainsessions),digits=3)
?forest.meta
forest(selfmainsessions,leftcols = "studlab", calcwidth.tests = TRUE, 
       smlab="Standardized mean\ndifference", test.effect.subgroup=TRUE,digits.pval = 3,
       xlab.pos=c(-1.7,1), cex=1.5,
       xlab=c("Favors control", "Favors intervention"), xlim=c(-2, 1.5))


#################fixed effects
selfmainfixed<-metagen(TE, seTE, studlab=studlab, data=selfmslim,sm="SMD",
                 comb.fixed=TRUE)
print(summary(selfmainfixed), digits=2)




##########################################################################
##########################################################################
##########################################################################
###################


##########Follow- up metaanalyse
############################################################
############################################################
############################################################

setwd("/Users/coraschefft/Documents/Review inpatPT/R_meta")

fu<-read.table("follow_up.txt", header=TRUE)
fu<-fu[order(fu$Author),]
fu$Author<- c("De Jong-Meyer CBT", "De Jong-Meyer CBT", "De Roten PDT", "Haukkson GCBT", "Hauksson ICBT",
              "Hautzinger AD+VT","Hautzinger VT",
              "Köhler CBT", "Köhler CBT", "Miller", "Miller CBT+SST", "Schramm IPT",
              "Schramm IPT")
fuself<-subset(fu, fu$instr=="BDI",select = Author:timepoint)
fuselfmeta<-metacont(Nc, ti_Mc, ti_Sc, Ne, ti_Me, ti_Se,  studlab=paste(Author, Year),
                     data=fuself, sm="SMD", comb.fixed = FALSE, comb.random = TRUE)
print(summary(fuselfmeta), digits = 2)
forest(fuselfmeta, leftcols = c("studlab") , calcwidth.tests = TRUE, 
       xlab.pos=c(-1.7,1), cex=1.5, 
       xlab=c("Favors control", "Favors intervention"), xlim=c(-2,1))

######De Roten einfügen
fsmp<-as.data.frame(fuselfmeta)
fsmpl<-fsmp[,c(7,1,4,8,9)]
follow_up<-fuself$timepoint
names(follow_up)<-"timepoint"
fsmpl<-cbind(fsmpl,follow_up)
derotenfu<-data.frame("De Roten PDT 2017", 76, 73, 0.24, 0.163265306, 12)
names(derotenfu)<-c("studlab" ,"n.e","n.c","TE","seTE","follow_up" )
fsmplus<-rbind(derotenfu,fsmpl)
fsmplus<-fsmplus[order(fsmplus$studlab),]
c<-sum(fsmplus$n.e+fsmplus$n.c) 


fsmplusmeta<-metagen(TE, seTE, studlab=studlab, data=fsmplus, sm="SMD",
                     comb.fixed=FALSE)
print(summary(fsmplusmeta), digits = 2)

forest(fsmplusmeta, leftcols = c("studlab","n.e","n.c") , calcwidth.tests = TRUE, 
       xlab.pos=c(-1.5,1), cex=1.5, 
       xlab=c("Favors control", "Favors intervention"), xlim=c(-1.5,1))


###################################################### ende follow up

#funnel plots small study effects, fixed effects modell
selfsse<-update(smsm12,comb.fixed=TRUE,comb.random = TRUE)



funnel(selfsse, pch=16,
       contour=c(0.9,0.95,0.99),xlim=c(-1.75,1.75),col.contour = c("darkgrey","gray","lightgray"))
legend(1,0.05,cex=1,c("0.1>p>0.05","0.05>p>0.01","p<0.01")
       ,fill=c("darkgrey","gray","lightgray"),bty="n")


metabias(selfsse,method="rank")


#TRIM and FIll method
tflself<-trimfill(smsm12,comb.fixed = TRUE,comb.random = TRUE)
print(tflself, digits=2, comb.fixed=TRUE)

funnel(tflself)






#####################################################risk of bias

setwd("/Users/coraschefft/Documents/Review inpatPT/R_meta")

rob<-read.table("risk_of_bias.txt",header=TRUE)
rob$Study<-paste(rob$Author, rob$Year)
rob<-rob[-c(6,7),-c(1,2,4)]
rob.m<-melt(rob, id="Study")
base_size <- 14
r<-ggplot(rob.m,aes(variable, Study, fill=as.factor(value)))+geom_tile(colour="black")+  coord_equal()+
  xlab("")+ylab("")+
  scale_x_discrete(position="top")+
  theme( axis.ticks=element_blank(),
         axis.text.y = element_text(size=base_size,hjust=1,colour="black"),
         axis.text.x =element_text(size=base_size, angle=320, hjust = 1, colour="black"))+
  scale_fill_manual("Risk of bias",values = c("light green", "yellow", "red"), 
                    labels = c("low","unclear","high"))

r




# The included numbers will per calulate power for a meta-analysis to detect a summary effect size of 0.2, with an average sample size per group of = 50, a total of 15 effect sizes, and moderate heterogeneity.

es <- 0.24 # Enter your summary effect size
as <-28.42105# Average per number per group
mk <- 19  # Number of effect sizes
hg <- 0   # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
Power <- (1-pnorm(1.96-eq5)) # Two-tailed
Power

#####
p<-ggplot(rob.m,aes(variable, Study, colour=value))+
  geom_tile(aes(fill=value))+xlab("Risk of bias")+
  scale_fill_gradientn(colours = c("green", "yellow", "red"), values = c(0,0.5,1))+theme_minimal() 
base_size <- 9
p+theme_minimal(base_size = base_size) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  theme(legend.position="none", axis.ticks=element_blank())
#######number needed to treat                                                                                                             

#CER <- 0.2
#d <- 0.24
#1 / (pnorm(d + qnorm(CER))-CER)

nnt2 <- 1/((2*pnorm(0.24/sqrt(2))-1))

##########################################
#################gepoolte Standardabweichung
#############################################
self<-subset(endpoint, endpoint$instr=="BDI", select=Author:SDb)
self<-self[order(self$Author),]
selfcopy<-self

selfcopy$df<-selfcopy$Nb-1
selfcopy$s2<-selfcopy$SDb^2
SDpool<-sqrt(sum(selfcopy$s2*selfcopy$df)/sum(selfcopy$df))



