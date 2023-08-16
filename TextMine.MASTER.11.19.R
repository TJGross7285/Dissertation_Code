library(readr)
library(tidytext)
library(dplyr)
library(Matrix)

adrc_txt.pos<-read_lines("activity_network_ADRC_POS.txt")[-c(1)]%>%as.data.frame()
adrc_txt.neg<-read_lines("activity_network_ADRC_NEG.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(adrc_txt.pos,adrc_txt.neg)
Mode<-c(rep("ESI+",dim(adrc_txt.pos)[1]),rep("ESI-",dim(adrc_txt.neg)[1]))
Study<-rep("ADRC",length(Mode))

ADRC_frame<-cbind(Text,Mode,Study)
colnames(ADRC_frame)[1]<-"Text"


DSLegacy_txt.pos<-read_lines("activity_network_DSLegacy_POS.txt")[-c(1)]%>%as.data.frame()
DSLegacy_txt.neg<-read_lines("activity_network_DSLegacy_NEG.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(DSLegacy_txt.pos, DSLegacy_txt.neg)
Mode<-c(rep("ESI+",dim(DSLegacy_txt.pos)[1]),rep("ESI-",dim(DSLegacy_txt.neg)[1]))
Study<-rep("DS_Legacy",length(Mode))

DS_frame<-cbind(Text,Mode,Study)
colnames(DS_frame)[1]<-"Text"

Oxford_txt.pos<-read_lines("activity_network_Oxford_POS.txt")[-c(1)]%>%as.data.frame()
Oxford_txt.neg<-read_lines("activity_network_Oxford_NEG.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(Oxford_txt.pos, Oxford_txt.neg)
Mode<-c(rep("ESI+",dim(Oxford_txt.pos)[1]),rep("ESI-",dim(Oxford_txt.neg)[1]))
Study<-rep("Oxford",length(Mode))

Oxford_frame<-cbind(Text,Mode,Study)
colnames(Oxford_frame)[1]<-"Text"

Ringman_txt.pos<-read_lines("activity_network_Ringman_POS.txt")[-c(1)]%>%as.data.frame()
Ringman_txt.neg<-read_lines("activity_network_Ringman_NEG.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(Ringman_txt.pos, Ringman_txt.neg)
Mode<-c(rep("ESI+",dim(Ringman_txt.pos)[1]),rep("ESI-",dim(Ringman_txt.neg)[1]))
Study<-rep("Ringman",length(Mode))

Ringman_frame<-cbind(Text,Mode,Study)
colnames(Ringman_frame)[1]<-"Text"

ROCAS_txt.pos<-read_lines("activity_network_RAS_POS_10_30.txt")[-c(1)]%>%as.data.frame()
ROCAS_txt.neg<-read_lines("activity_network_RAS_NEG_10_30.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(ROCAS_txt.pos, ROCAS_txt.neg)
Mode<-c(rep("ESI+",dim(ROCAS_txt.pos)[1]),rep("ESI-",dim(ROCAS_txt.neg)[1]))
Study<-rep("ROCAS",length(Mode))

ROCAS_frame<-cbind(Text,Mode,Study)
colnames(ROCAS_frame)[1]<-"Text"


Autism_txt.pos<-read_lines("activity_network_Autism_POS.txt")[-c(1)]%>%as.data.frame()
Autism_txt.neg<-read_lines("activity_network_Autism_NEG.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(Autism_txt.pos, Autism_txt.neg)
Mode<-c(rep("ESI+",dim(Autism_txt.pos)[1]),rep("ESI-",dim(Autism_txt.neg)[1]))
Study<-rep("Autism",length(Mode))

Autism_frame<-cbind(Text,Mode,Study)
colnames(Autism_frame)[1]<-"Text"

ABCDS_txt.pos<-read_lines("activity_network_ABCDS_POS_NEW.txt")[-c(1)]%>%as.data.frame()
ABCDS_txt.neg<-read_lines("activity_network_ABCDS_NEG_NEW.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(ABCDS_txt.pos, ABCDS_txt.neg)
Mode<-c(rep("ESI+",dim(ABCDS_txt.pos)[1]),rep("ESI-",dim(ABCDS_txt.neg)[1]))
Study<-rep("ABCDS_WAVE1",length(Mode))

ABCDS_frame<-cbind(Text,Mode,Study)
colnames(ABCDS_frame)[1]<-"Text"

R56_txt.pos<-read_lines("activity_network_R56_pos.txt")[-c(1)]%>%as.data.frame()
R56_txt.neg<-read_lines("activity_network_R56_neg.txt")[-c(1)]%>%as.data.frame()
Text<-rbind(R56_txt.pos,R56_txt.neg)
Mode<-c(rep("ESI+",dim(R56_txt.pos)[1]),rep("ESI-",dim(R56_txt.neg)[1]))
Study<-rep("R56_DS",length(Mode))

R56_frame<-cbind(Text,Mode,Study)
colnames(R56_frame)[1]<-"Text"


final_frame<-rbind(ADRC_frame,DS_frame,Oxford_frame,Ringman_frame,ROCAS_frame,ABCDS_frame,R56_frame,Autism_frame)
final_frame$Text<-as.character(final_frame$Text)
final_frame_words<-final_frame%>%unnest_tokens(word,Text)%>%count(word,Study, sort = TRUE)%>%bind_tf_idf(word, Study, n)%>%cast_sparse(Study,word, tf_idf)
final_frame_bigram<-final_frame%>%unnest_tokens(word,Text,token='ngrams',n=2)%>%count(word,Study, sort = TRUE)%>%bind_tf_idf(word, Study, n)%>%cast_sparse(Study,word, tf_idf)
final_frame_trigram<-final_frame%>%unnest_tokens(word,Text,token='ngrams',n=3)%>%count(word,Study, sort = TRUE)%>%bind_tf_idf(word, Study, n)%>%cast_sparse(Study,word, tf_idf)

final_frame_total<-cbind(final_frame_words,final_frame_bigram,final_frame_trigram)%>%as.matrix%>%t()
write.csv(final_frame_total,file="Big_Data_10_30_MJ.csv")


suppressMessages(library(S4Vectors))
corpus<-scNMFSet(count=final_frame_total,rowData=DataFrame(1:nrow(final_frame_total)),colData=DataFrame(1:ncol(final_frame_total)))
sb <- vb_factorize(corpus, ranks = seq(1,6), nrun = 10, verbose = 2, Tol = 1e-4, progress.bar = TRUE)






cd 

mv