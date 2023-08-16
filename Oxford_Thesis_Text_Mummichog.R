library(Biobase)
library(sva)
library(impute)
library(limma)
library(dplyr)

###########START HERE TO REPRODUCE FINDINGS!!!
####Read in negative mode data
negative<-read.csv("Oxford_NEG.csv",header=FALSE,na.strings="0")
rt_neg<-as.numeric(as.character(negative[4:dim(negative)[1],3]))
mz_neg<-as.numeric(as.character(negative[4:dim(negative)[1],2]))
neg_SampleID<-t(negative[2,4:dim(negative)[2]])
neg_PhenoGroup<-t(negative[1,4:dim(negative)[2]])
neg_abunds<-as.data.frame(t(negative[4:dim(negative)[1],4:dim(negative)[2]]))
colnames(neg_abunds)<-mz_neg
negativeIDs<-cbind(as.data.frame(neg_SampleID),as.data.frame(neg_PhenoGroup),as.data.frame(neg_abunds))
colnames(negativeIDs)[c(1:2)]<-c("SampleID","DiseaseState")
negativeIDs<-negativeIDs[negativeIDs$SampleID!="QC",]


####Read in positive mode data 
positive<-read.csv("Oxford_POS.csv",header=FALSE,na.strings="0")
rt_pos<-as.numeric(as.character(positive[4:dim(positive)[1],3]))
mz_pos<-as.numeric(as.character(positive[4:dim(positive)[1],2]))
pos_SampleID<-t(positive[2,4:dim(positive)[2]])
pos_PhenoGroup<-t(positive[1,4:dim(positive)[2]])
pos_abunds<-as.data.frame(t(positive[4:dim(positive)[1],4:dim(positive)[2]]))
colnames(pos_abunds)<-mz_pos
positiveIDs<-cbind(as.data.frame(pos_SampleID),as.data.frame(pos_PhenoGroup),as.data.frame(pos_abunds))
colnames(positiveIDs)[c(1:2)]<-c("SampleID","DiseaseState")
positiveIDs<-positiveIDs[positiveIDs$SampleID!="QC",]


####Should evaluate TRUE 
all.equal(negativeIDs[,c(1:2)],positiveIDs[,c(1:2)])


pheno_meta<-positiveIDs[,1:2]
pheno_meta$DiseaseState<-as.factor(pheno_meta$DiseaseState)
levels(pheno_meta$DiseaseState)<-list("Control"=c("Group B"),"PD"=c("Group C"),"PDD"=c("Group A"))
rownames(pheno_meta)<-seq(1,dim(pheno_meta)[1],1)
pheno_plot<-pheno_meta
pheno_meta<-AnnotatedDataFrame(pheno_meta)


abunds<-apply(cbind(as.data.frame(positiveIDs[,-c(1:2)]),as.data.frame(negativeIDs[,-c(1:2)])),2,as.numeric)
colnames(abunds)<-seq(1,dim(abunds)[2])
index<-caret::nearZeroVar(abunds)
T_abunds<-t(abunds[,-index])
edata<-impute.knn(T_abunds,k = 10, rowmax = 1.0, colmax = 1.0, maxp = 1500, rng.seed=362436069)$data
edata<-log2(edata)
rownames(edata)<-rownames(T_abunds)

mode<-as.factor(c(rep("ESI+",dim(positiveIDs[,-c(1:2)])[2]),rep("ESI-",dim(negativeIDs[,-c(1:2)])[2])))
feature_meta<-cbind(rbind(cbind(mz_pos,rt_pos),cbind(mz_neg,rt_neg)),as.data.frame(as.factor(mode)))
feature_meta<-feature_meta[-index,]
rownames(feature_meta)<-rownames(T_abunds)
colnames(feature_meta)<-c("MZ","RT","Mode")

####Features for Output 
feature_meta_use<-cbind(rownames(T_abunds),feature_meta)
colnames(feature_meta_use)[1]<-"Feature"

####Complete Data Object Construction 
feature_meta<-AnnotatedDataFrame(feature_meta)
combined<-ExpressionSet(assayData=edata,phenoData=pheno_meta,featureData=feature_meta)


mod<-model.matrix(~as.factor(DiseaseState),data=combined)
mod0<-model.matrix(~1,data=combined)
n.sv_BE<-num.sv(edata,mod,seed=122)
svobj_BE<-sva(edata,mod,mod0,n.sv=n.sv_BE)
n.sv_LEEK<-num.sv(edata,mod,seed=122,method="leek")
svobj_LEEK<-sva(edata,mod,mod0,n.sv=n.sv_LEEK)

design_table_BE<-cbind(model.matrix(~0+as.factor(DiseaseState),data=combined),as.data.frame(svobj_BE$sv))
design_table_LEEK<-cbind(model.matrix(~0+as.factor(DiseaseState),data=combined),as.data.frame(svobj_LEEK$sv))
colnames(design_table_BE)[1:3]<-c("Control","PD","PDD")
colnames(design_table_LEEK)[1:3]<-c("Control","PD","PDD")

####Fit linear model for pairwise contrasts **********BE*********** 
arrayw<-arrayWeights(edata, design=design_table_BE)
fit1<-lmFit(edata,design_table_BE,weights=arrayw)
cm1<- makeContrasts(
	`PDD-PD` = PDD-PD, 
	levels=design_table_BE)
cm2<-makeContrasts(
	`PD-Control` = PD-Control, 
	levels=design_table_BE)
cm3<-makeContrasts(
	`PDD-Control`= PDD-Control, 
	levels=design_table_BE)

fit1_F <- contrasts.fit(fit1, cm1)
fit1_F <- eBayes(fit1_F,trend=TRUE)
T<-topTableF(fit1_F,adjust="BH",number=100000)
T<-cbind(rownames(T),T)
colnames(T)[1]<-"Feature"
joinT<-dplyr::inner_join(feature_meta_use,T)
write.table(joinT%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PDD.PD"),
			sep="\t",file="Oxford_POS_5_28_PDD_PD_BE.txt",row.names=FALSE)
write.table(joinT%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PDD.PD"),
			sep="\t",file="Oxford_NEG_5_28_PDD_PD_BE.txt",row.names=FALSE)

fit1_F <- contrasts.fit(fit1, cm2)
fit1_F <- eBayes(fit1_F,trend=TRUE)
T<-topTableF(fit1_F,adjust="BH",number=100000)
U<-cbind(rownames(T),T)
colnames(U)[1]<-"Feature"
joinU<-dplyr::inner_join(feature_meta_use,U)
write.table(joinU%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PD.Control"),
			sep="\t",file="Oxford_POS_5_28_PD_Control_BE.txt",row.names=FALSE)
write.table(joinU%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PD.Control"),
			sep="\t",file="Oxford_NEG_5_28_PD_Control_BE.txt",row.names=FALSE)

fit1_F <- contrasts.fit(fit1, cm3)
fit1_F <- eBayes(fit1_F,trend=TRUE)
T<-topTableF(fit1_F,adjust="BH",number=100000)
V<-cbind(rownames(T),T)
colnames(V)[1]<-"Feature"
joinV<-dplyr::inner_join(feature_meta_use,V)
write.table(joinV%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PDD.Control"),
			sep="\t",file="Oxford_POS_5_28_PDD_Control_BE.txt",row.names=FALSE)
write.table(joinV%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PDD.Control"),
			sep="\t",file="Oxford_NEG_5_28_PDD_Control_BE.txt",row.names=FALSE)

################################
################################
####Fit linear model for pairwise contrasts **********LEEK*********** 
arrayw<-arrayWeights(edata, design=design_table_LEEK)
fit2<-lmFit(edata,design_table_LEEK,weights=arrayw)
cm4<- makeContrasts(
	`PDD-PD` = PDD-PD, 
	levels=design_table_LEEK)
cm5<-makeContrasts(
	`PD-Control` = PD-Control, 
	levels=design_table_LEEK)
cm6<-makeContrasts(
	`PDD-Control`= PDD-Control, 
	levels=design_table_LEEK)

fit2_F <- contrasts.fit(fit2, cm4)
fit2_F <- eBayes(fit2_F,trend=TRUE)
T<-topTableF(fit2_F,adjust="BH",number=100000)
T<-cbind(rownames(T),T)
colnames(T)[1]<-"Feature"
joinT<-dplyr::inner_join(feature_meta_use,T)
write.table(joinT%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PDD.PD"),
			sep="\t",file="Oxford_POS_5_28_PDD_PD_LEEK.txt",row.names=FALSE)
write.table(joinT%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PDD.PD"),
			sep="\t",file="Oxford_NEG_5_28_PDD_PD_LEEK.txt",row.names=FALSE)

fit2_F <- contrasts.fit(fit2, cm5)
fit2_F <- eBayes(fit2_F,trend=TRUE)
T<-topTableF(fit2_F,adjust="BH",number=100000)
U<-cbind(rownames(T),T)
colnames(U)[1]<-"Feature"
joinU<-dplyr::inner_join(feature_meta_use,U)
write.table(joinU%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PD.Control"),
			sep="\t",file="Oxford_POS_5_28_PD_Control_LEEK.txt",row.names=FALSE)
write.table(joinU%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PD.Control"),
			sep="\t",file="Oxford_NEG_5_28_PD_Control_LEEK.txt",row.names=FALSE)

fit2_F <- contrasts.fit(fit2, cm6)
fit2_F <- eBayes(fit2_F,trend=TRUE)
T<-topTableF(fit2_F,adjust="BH",number=100000)
V<-cbind(rownames(T),T)
colnames(V)[1]<-"Feature"
joinV<-dplyr::inner_join(feature_meta_use,V)
write.table(joinV%>%filter(Mode=="ESI+")%>%select("MZ","RT","P.Value","PDD.Control"),
			sep="\t",file="Oxford_POS_5_28_PDD_Control_LEEK.txt",row.names=FALSE)
write.table(joinV%>%filter(Mode=="ESI-")%>%select("MZ","RT","P.Value","PDD.Control"),
			sep="\t",file="Oxford_NEG_5_28_PDD_Control_LEEK.txt",row.names=FALSE)

cd /Volumes/NO\ NAME/Active_Projects_4_9_2021/Oxford_TEST 

mummichog -f Oxford_POS_5_28_PDD_PD_BE.txt -o Oxford_POS_5_28_PDD_PD_BE -m positive
mummichog -f Oxford_NEG_5_28_PDD_PD_BE.txt -o Oxford_NEG_5_28_PDD_PD_BE -m negative 

mummichog -f Oxford_POS_5_28_PD_Control_BE.txt -o Oxford_POS_5_28_PD_Control_BE -m positive
mummichog -f Oxford_NEG_5_28_PD_Control_BE.txt -o Oxford_NEG_5_28_PD_Control_BE -m negative 

mummichog -f Oxford_POS_5_28_PDD_Control_BE.txt -o Oxford_POS_5_28_PDD_Control_BE -m positive
mummichog -f Oxford_NEG_5_28_PDD_Control_BE.txt -o Oxford_NEG_5_28_PDD_Control_BE -m negative 


mummichog -f Oxford_POS_5_28_PDD_PD_LEEK.txt -o Oxford_POS_5_28_PDD_PD_LEEK -m positive
mummichog -f Oxford_NEG_5_28_PDD_PD_LEEK.txt -o Oxford_NEG_5_28_PDD_PD_LEEK -m negative 

mummichog -f Oxford_POS_5_28_PD_Control_LEEK.txt -o Oxford_POS_5_28_PD_Control_LEEK -m positive
mummichog -f Oxford_NEG_5_28_PD_Control_LEEK.txt -o Oxford_NEG_5_28_PD_Control_LEEK -m negative 

mummichog -f Oxford_POS_5_28_PDD_Control_LEEK.txt -o Oxford_POS_5_28_PDD_Control_LEEK -m positive
mummichog -f Oxford_NEG_5_28_PDD_Control_LEEK.txt -o Oxford_NEG_5_28_PDD_Control_LEEK -m negative 
