
library(Biobase)
library(sva)
library(impute)
library(limma)
library(dplyr)
library(GridOnClusters)
library(bnlearn)

new_pheno<-read.csv("RAS_DE_Pheno_8_7.csv",check.names=FALSE)
RAS_BE<-read.csv("RAS_BE.csv",check.names=FALSE)[,-c(1)]
RAS_LEEK<-read.csv("RAS_LEEK.csv",check.names=FALSE)[,-c(1)]

join_BE<-dplyr::inner_join(new_pheno,RAS_BE,by="SampleID")[,-c(8)]
join_LEEK<-dplyr::inner_join(new_pheno,RAS_LEEK,by="SampleID")[,-c(8)]

pheno_factors<-apply(join_BE[,c(5,7)],2,as.factor)
pheno_cont_BE<-apply(join_BE[,c(4,6,8:11)],2,as.numeric)
pheno_cont_LEEK<-apply(join_LEEK[,c(4,6,8,9)],2,as.numeric)
discrete_BE<-cbind(as.data.frame(discretize.jointly(pheno_cont_BE,k=c(2:50))$D),as.data.frame(pheno_factors))
discrete_LEEK<-cbind(as.data.frame(discretize.jointly(pheno_cont_LEEK,k=c(2:50))$D),as.data.frame(pheno_factors))

discrete_BE[,1:dim(discrete_BE)[2]]<-lapply(discrete_BE[,1:dim(discrete_BE)[2]],as.factor)
discrete_LEEK[,1:dim(discrete_LEEK)[2]]<-lapply(discrete_LEEK[,1:dim(discrete_LEEK)[2]],as.factor)


G<-h2pc(discrete_BE)
H<-arc.strength(G,discrete_BE)
strength.plot(G,H)


G<-h2pc(discrete_LEEK)
H<-arc.strength(G,discrete_LEEK)
strength.plot(G,H)

####################################
####################################
####################################

T<-read.csv("Complete_Pheno.csv",check.names=FALSE)

pheno_factors<-apply(as.data.frame(T[,c(7,9)]),2,as.factor)
pheno_cont_BE<-apply(T[,c(6,11,15:24)],2,as.numeric)
discrete_BE<-cbind(as.data.frame(discretize.jointly(pheno_cont_BE,k=c(2:50))$D),as.data.frame(pheno_factors))
discrete_BE[,1:dim(discrete_BE)[2]]<-lapply(discrete_BE[,1:dim(discrete_BE)[2]],as.factor)


G<-h2pc(discrete_BE)
H<-arc.strength(G,discrete_BE)
strength.plot(G,H)




new_pheno<-read.csv("RAS_DE_Pheno_8_7.csv",check.names=FALSE)
colnames(new_pheno)[4]<-"Age"
T<-read.csv("Complete_Pheno.csv",check.names=FALSE)
T$DiseaseState<-as.factor(T$DiseaseState)
levels(T$DiseaseState)<-list("Control"=c("Control"),"Converter"=c("Case"))
bind<-cbind(rep("ADRC",dim(T)[1]),T)
colnames(bind)[1]<-"Cohort"
index<-intersect(colnames(bind),colnames(new_pheno))
tog<-as.data.frame(rbind(bind[,index],new_pheno[,index]))
tog[,c(1,3,4,6)]<-lapply(tog[,c(1,3,4,6)],as.factor)

A<-model.matrix(`APOE4+`~DiseaseState:Cohort, data=tog)
anova(lm(Age~`APOE`:DiseaseState, data=tog))