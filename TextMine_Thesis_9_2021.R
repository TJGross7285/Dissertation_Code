#############TEXT THESIS 9/2021
library(readr)
library(tidytext)
library(dplyr)
library(Matrix)
library(quanteda)
library(stm)

DS.Legacy_BE.pos<-read_lines("activity_network.Legacy.POS_BE.txt")[-c(1)]%>%as.data.frame()
DS.Legacy_BE.neg<-read_lines("activity_network.Legacy.NEG_BE.txt")[-c(1)]%>%as.data.frame()
DS.Legacy_LEEK.pos<-read_lines("activity_network.Legacy.POS_LEEK.txt")[-c(1)]%>%as.data.frame()
DS.Legacy_LEEK.neg<-read_lines("activity_network.Legacy.NEG_LEEK.txt")[-c(1)]%>%as.data.frame()

SVA_Method<-c(rep("BE",dim(DS.Legacy_BE.pos)[1]+dim(DS.Legacy_BE.neg)[1]),
	          rep("LEEK",dim(DS.Legacy_LEEK.pos)[1]+dim(DS.Legacy_LEEK.neg)[1]))
Text<-rbind(DS.Legacy_BE.pos,DS.Legacy_BE.neg,DS.Legacy_LEEK.pos,DS.Legacy_LEEK.neg)
Mode<-c(rep("ESI+",dim(DS.Legacy_BE.pos)[1]),rep("ESI-",dim(DS.Legacy_BE.neg)[1]),
	    rep("ESI+",dim(DS.Legacy_LEEK.pos)[1]),rep("ESI-",dim(DS.Legacy_LEEK.neg)[1]))
Study<-rep("DS_Legacy",length(Mode))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
DS.Legacy_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(DS.Legacy_frame)[1]<-"Text"

#####################
#####################

#RAS_BE.pos<-read_lines("activity_network.RAS.POS_BE.txt")[-c(1)]%>%as.data.frame()
#RAS_BE.neg<-read_lines("activity_network.RAS.NEG_BE.txt")[-c(1)]%>%as.data.frame()
#RAS_LEEK.pos<-read_lines("activity_network.RAS.POS_LEEK.txt")[-c(1)]%>%as.data.frame()
#RAS_LEEK.neg<-read_lines("activity_network.RAS.NEG_LEEK.txt")[-c(1)]%>%as.data.frame()

#SVA_Method<-c(rep("BE",dim(RAS_BE.pos)[1]+dim(RAS_BE.neg)[1]),
#	          rep("LEEK",dim(RAS_LEEK.pos)[1]+dim(RAS_LEEK.neg)[1]))
#Text<-rbind(RAS_BE.pos,RAS_BE.neg,RAS_LEEK.pos,RAS_LEEK.neg)
#Mode<-c(rep("ESI+",dim(RAS_BE.pos)[1]),rep("ESI-",dim(RAS_BE.neg)[1]),
#	    rep("ESI+",dim(RAS_LEEK.pos)[1]),rep("ESI-",dim(RAS_LEEK.neg)[1]))
#Study<-rep("R/OC/AS",length(Mode))

#study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
#RAS_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
#colnames(RAS_frame)[1]<-"Text"

#####################
#####################
RAS_POS_5_26_Conv_Con_BE<-read_lines("activity_network.RAS_POS_5_26_Conv_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Conv_Con_BE<-read_lines("activity_network.RAS_NEG_5_26_Conv_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_Conv_Con_LEEK<-read_lines("activity_network.RAS_POS_5_26_Conv_Con_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Conv_Con_LEEK<-read_lines("activity_network.RAS_NEG_5_26_Conv_Con_LEEK.txt")[-c(1)]%>%as.data.frame()

RAS_POS_5_26_MCIAD_Con_BE<-read_lines("activity_network.RAS_POS_5_26_MCIAD_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_MCIAD_Con_BE<-read_lines("activity_network.RAS_NEG_5_26_MCIAD_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_MCIAD_Con_LEEK<-read_lines("activity_network.RAS_POS_5_26_MCIAD_Con_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_MCIAD_Con_LEEK<-read_lines("activity_network.RAS_NEG_5_26_MCIAD_Con_LEEK.txt")[-c(1)]%>%as.data.frame()

RAS_POS_5_26_Super_Con_BE<-read_lines("activity_network.RAS_POS_5_26_Super_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_Con_BE<-read_lines("activity_network.RAS_NEG_5_26_Super_Con_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_Super_Con_LEEK<-read_lines("activity_network.RAS_POS_5_26_Super_Con_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_Con_LEEK<-read_lines("activity_network.RAS_NEG_5_26_Super_Con_LEEK.txt")[-c(1)]%>%as.data.frame()

RAS_POS_5_26_Super_Conv_BE<-read_lines("activity_network.RAS_POS_5_26_Super_Conv_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_Conv_BE<-read_lines("activity_network.RAS_NEG_5_26_Super_Conv_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_Super_Conv_LEEK<-read_lines("activity_network.RAS_POS_5_26_Super_Conv_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_Conv_LEEK<-read_lines("activity_network.RAS_NEG_5_26_Super_Conv_LEEK.txt")[-c(1)]%>%as.data.frame()

RAS_POS_5_26_MCIAD_Conv_BE<-read_lines("activity_network.RAS_POS_5_26_MCIAD_Conv_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_MCIAD_Conv_BE<-read_lines("activity_network.RAS_NEG_5_26_MCIAD_Conv_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_MCIAD_Conv_LEEK<-read_lines("activity_network.RAS_POS_5_26_MCIAD_Conv_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_MCIAD_Conv_LEEK<-read_lines("activity_network.RAS_NEG_5_26_MCIAD_Conv_LEEK.txt")[-c(1)]%>%as.data.frame()

RAS_POS_5_26_Super_MCIAD_BE<-read_lines("activity_network.RAS_POS_5_26_Super_MCIAD_BE.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_MCIAD_BE<-read_lines("activity_network.RAS_NEG_5_26_Super_MCIAD_BE.txt")[-c(1)]%>%as.data.frame()
RAS_POS_5_26_Super_MCIAD_LEEK<-read_lines("activity_network.RAS_POS_5_26_Super_MCIAD_LEEK.txt")[-c(1)]%>%as.data.frame()
RAS_NEG_5_26_Super_MCIAD_LEEK<-read_lines("activity_network.RAS_NEG_5_26_Super_MCIAD_LEEK.txt")[-c(1)]%>%as.data.frame()

SVA_Method<-c(rep("BE",dim(RAS_POS_5_26_Conv_Con_BE)[1]),rep("BE",dim(RAS_NEG_5_26_Conv_Con_BE)[1]),
	    	  rep("LEEK",dim(RAS_POS_5_26_Conv_Con_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_Conv_Con_LEEK)[1]),
              rep("BE",dim(RAS_POS_5_26_MCIAD_Con_BE)[1]),rep("BE",dim(RAS_NEG_5_26_MCIAD_Con_BE)[1]),
              rep("LEEK",dim(RAS_POS_5_26_MCIAD_Con_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_MCIAD_Con_LEEK)[1]),
		      rep("BE",dim(RAS_POS_5_26_Super_Con_BE)[1]),rep("BE",dim(RAS_NEG_5_26_Super_Con_BE)[1]),
		      rep("LEEK",dim(RAS_POS_5_26_Super_Con_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_Super_Con_LEEK)[1]),
		      rep("BE",dim(RAS_POS_5_26_Super_Conv_BE)[1]),rep("BE",dim(RAS_NEG_5_26_Super_Conv_BE)[1]),
		      rep("LEEK",dim(RAS_POS_5_26_Super_Conv_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_Super_Conv_LEEK)[1]),
		      rep("BE",dim(RAS_POS_5_26_MCIAD_Conv_BE)[1]),rep("BE",dim(RAS_NEG_5_26_MCIAD_Conv_BE)[1]),
		      rep("LEEK",dim(RAS_POS_5_26_MCIAD_Conv_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_MCIAD_Conv_LEEK)[1]),
		      rep("BE",dim(RAS_POS_5_26_Super_MCIAD_BE)[1]),rep("BE",dim(RAS_NEG_5_26_Super_MCIAD_BE)[1]),
		      rep("LEEK",dim(RAS_POS_5_26_Super_MCIAD_LEEK)[1]),rep("LEEK",dim(RAS_NEG_5_26_Super_MCIAD_LEEK)[1]))

Text<-rbind(RAS_POS_5_26_Conv_Con_BE,RAS_NEG_5_26_Conv_Con_BE,RAS_POS_5_26_Conv_Con_LEEK,RAS_NEG_5_26_Conv_Con_LEEK,
	        RAS_POS_5_26_MCIAD_Con_BE,RAS_NEG_5_26_MCIAD_Con_BE,RAS_POS_5_26_MCIAD_Con_LEEK,RAS_NEG_5_26_MCIAD_Con_LEEK,
	        RAS_POS_5_26_Super_Con_BE,RAS_NEG_5_26_Super_Con_BE,RAS_POS_5_26_Super_Con_LEEK,RAS_NEG_5_26_Super_Con_LEEK,
	        RAS_POS_5_26_Super_Conv_BE,RAS_NEG_5_26_Super_Conv_BE,RAS_POS_5_26_Super_Conv_LEEK,RAS_NEG_5_26_Super_Conv_LEEK,
	        RAS_POS_5_26_MCIAD_Conv_BE,RAS_NEG_5_26_MCIAD_Conv_BE,RAS_POS_5_26_MCIAD_Conv_LEEK,RAS_NEG_5_26_MCIAD_Conv_LEEK,
	        RAS_POS_5_26_Super_MCIAD_BE,RAS_NEG_5_26_Super_MCIAD_BE,RAS_POS_5_26_Super_MCIAD_LEEK,RAS_NEG_5_26_Super_MCIAD_LEEK)

Mode<-c(rep("ESI+",dim(RAS_POS_5_26_Conv_Con_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_Conv_Con_BE)[1]),
	    rep("ESI+",dim(RAS_POS_5_26_Conv_Con_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_Conv_Con_LEEK)[1]),
        rep("ESI+",dim(RAS_POS_5_26_MCIAD_Con_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_MCIAD_Con_BE)[1]),
        rep("ESI+",dim(RAS_POS_5_26_MCIAD_Con_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_MCIAD_Con_LEEK)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_Con_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_Con_BE)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_Con_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_Con_LEEK)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_Conv_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_Conv_BE)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_Conv_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_Conv_LEEK)[1]),
		rep("ESI+",dim(RAS_POS_5_26_MCIAD_Conv_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_MCIAD_Conv_BE)[1]),
		rep("ESI+",dim(RAS_POS_5_26_MCIAD_Conv_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_MCIAD_Conv_LEEK)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_MCIAD_BE)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_MCIAD_BE)[1]),
		rep("ESI+",dim(RAS_POS_5_26_Super_MCIAD_LEEK)[1]),rep("ESI-",dim(RAS_NEG_5_26_Super_MCIAD_LEEK)[1]))

Study<-c(rep("RAS_Conv_Con",dim(RAS_POS_5_26_Conv_Con_BE)[1]+dim(RAS_NEG_5_26_Conv_Con_BE)[1]+dim(RAS_POS_5_26_Conv_Con_LEEK)[1]+dim(RAS_NEG_5_26_Conv_Con_LEEK)[1]),
	     rep("RAS_MCIAD_Con",dim(RAS_POS_5_26_MCIAD_Con_BE)[1]+dim(RAS_NEG_5_26_MCIAD_Con_BE)[1]+dim(RAS_POS_5_26_MCIAD_Con_LEEK)[1]+dim(RAS_NEG_5_26_MCIAD_Con_LEEK)[1]),
	     rep("RAS_Super_Con",dim(RAS_POS_5_26_Super_Con_BE)[1]+dim(RAS_NEG_5_26_Super_Con_BE)[1]+dim(RAS_POS_5_26_Super_Con_LEEK)[1]+dim(RAS_NEG_5_26_Super_Con_LEEK)[1]),
         rep("RAS_Super_Conv",dim(RAS_POS_5_26_Super_Conv_BE)[1]+dim(RAS_NEG_5_26_Super_Conv_BE)[1]+dim(RAS_POS_5_26_Super_Conv_LEEK)[1]+dim(RAS_NEG_5_26_Super_Conv_LEEK)[1]),
         rep("RAS_MCIAD_Conv",dim(RAS_POS_5_26_MCIAD_Conv_BE)[1]+dim(RAS_NEG_5_26_MCIAD_Conv_BE)[1]+dim(RAS_POS_5_26_MCIAD_Conv_LEEK)[1]+dim(RAS_NEG_5_26_MCIAD_Conv_LEEK)[1]),
         rep("RAS_Super_MCIAD",dim(RAS_POS_5_26_Super_MCIAD_BE)[1]+dim(RAS_NEG_5_26_Super_MCIAD_BE)[1]+dim(RAS_POS_5_26_Super_MCIAD_LEEK)[1]+dim(RAS_NEG_5_26_Super_MCIAD_LEEK)[1]))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
RAS_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(RAS_frame)[1]<-"Text"

#####################
#####################
ADRC_BE.pos<-read_lines("activity_network.ADRC.POS_BE.txt")[-c(1)]%>%as.data.frame()
ADRC_BE.neg<-read_lines("activity_network.ADRC.NEG_BE.txt")[-c(1)]%>%as.data.frame()


SVA_Method<-c(rep("BE",dim(ADRC_BE.pos)[1]+dim(ADRC_BE.neg)[1]))
Text<-rbind(ADRC_BE.pos,ADRC_BE.neg)
Mode<-c(rep("ESI+",dim(ADRC_BE.pos)[1]),rep("ESI-",dim(ADRC_BE.neg)[1]))
Study<-rep("ADRC",length(Mode))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
ADRC_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(ADRC_frame)[1]<-"Text"

#####################
#####################

Ringman.AD_MCI_BE.pos<-read_lines("activity_network.Ringman.AD_MCI.POS_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.AD_MCI_BE.neg<-read_lines("activity_network.Ringman.AD_MCI.NEG_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.Carrier_Control_BE.pos<-read_lines("activity_network.Ringman.Carrier_Control.POS_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.Carrier_Control_BE.neg<-read_lines("activity_network.Ringman.Carrier_Control.NEG_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.MCI_Carrier_BE.pos<-read_lines("activity_network.Ringman.MCI_Carrier.NEG_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.MCI_Carrier_BE.neg<-read_lines("activity_network.Ringman.MCI_Carrier.POS_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.Carrier_MCIAD_BE.pos<-read_lines("activity_network.Ringman_Carrier_MCIAD.NEG_BE.txt")[-c(1)]%>%as.data.frame()
Ringman.Carrier_MCIAD_BE.neg<-read_lines("activity_network.Ringman_Carrier_MCIAD.POS_BE.txt")[-c(1)]%>%as.data.frame()



SVA_Method<-rep("BE",dim(Ringman.AD_MCI_BE.pos)[1]+dim(Ringman.AD_MCI_BE.neg)[1]+dim(Ringman.Carrier_Control_BE.pos)[1]
                     +dim(Ringman.Carrier_Control_BE.neg)[1]+dim(Ringman.MCI_Carrier_BE.pos)[1]+dim(Ringman.MCI_Carrier_BE.neg)[1]
                     +dim(Ringman.Carrier_MCIAD_BE.pos)[1]+dim(Ringman.Carrier_MCIAD_BE.neg)[1])

Text<-rbind(Ringman.AD_MCI_BE.pos,Ringman.AD_MCI_BE.neg,Ringman.Carrier_Control_BE.pos,Ringman.Carrier_Control_BE.neg,Ringman.MCI_Carrier_BE.pos,Ringman.MCI_Carrier_BE.neg,Ringman.Carrier_MCIAD_BE.pos,Ringman.Carrier_MCIAD_BE.neg)
Mode<-c(c(rep("ESI+",dim(Ringman.AD_MCI_BE.pos)[1]),rep("ESI-",dim(Ringman.AD_MCI_BE.neg)[1]),rep("ESI+",dim(Ringman.Carrier_Control_BE.pos)[1])),
	    c(rep("ESI-",dim(Ringman.Carrier_Control_BE.neg)[1]),rep("ESI+",dim(Ringman.MCI_Carrier_BE.pos)[1]),rep("ESI-",dim(Ringman.MCI_Carrier_BE.neg)[1]),rep("ESI+",dim(Ringman.Carrier_MCIAD_BE.pos)[1]),rep("ESI-",dim(Ringman.Carrier_MCIAD_BE.neg)[1])))
Study<-c(rep("Ringman.AD_MCI",dim(Ringman.AD_MCI_BE.pos)[1]+dim(Ringman.AD_MCI_BE.neg)[1]),
	     rep("Ringman.Carrier_Control",dim(Ringman.Carrier_Control_BE.pos)[1]+dim(Ringman.Carrier_Control_BE.neg)[1]),
	     rep("Ringman.MCI_Carrier",dim(Ringman.MCI_Carrier_BE.pos)[1]+dim(Ringman.MCI_Carrier_BE.neg)[1]),
	     rep("Ringman.Carrier_MCIAD",dim(Ringman.Carrier_MCIAD_BE.pos)[1]+dim(Ringman.Carrier_MCIAD_BE.neg)[1]))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
Ringman_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(Ringman_frame)[1]<-"Text"

#####################
#####################
Oxford_POS_5_28_PDD_Control_BE<-read_lines("activity_network.Oxford_POS_5_28_PDD_Control_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PDD_Control_BE<-read_lines("activity_network.Oxford_NEG_5_28_PDD_Control_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_POS_5_28_PDD_Control_LEEK<-read_lines("activity_network.Oxford_POS_5_28_PDD_Control_LEEK.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PDD_Control_LEEK<-read_lines("activity_network.Oxford_NEG_5_28_PDD_Control_LEEK.txt")[-c(1)]%>%as.data.frame()

Oxford_POS_5_28_PD_Control_BE<-read_lines("activity_network.Oxford_POS_5_28_PD_Control_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PD_Control_BE<-read_lines("activity_network.Oxford_NEG_5_28_PD_Control_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_POS_5_28_PD_Control_LEEK<-read_lines("activity_network.Oxford_POS_5_28_PD_Control_LEEK.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PD_Control_LEEK<-read_lines("activity_network.Oxford_NEG_5_28_PD_Control_LEEK.txt")[-c(1)]%>%as.data.frame()

Oxford_POS_5_28_PDD_PD_BE<-read_lines("activity_network.Oxford_POS_5_28_PDD_PD_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PDD_PD_BE<-read_lines("activity_network.Oxford_NEG_5_28_PDD_PD_BE.txt")[-c(1)]%>%as.data.frame()
Oxford_POS_5_28_PDD_PD_LEEK<-read_lines("activity_network.Oxford_POS_5_28_PDD_PD_LEEK.txt")[-c(1)]%>%as.data.frame()
Oxford_NEG_5_28_PDD_PD_LEEK<-read_lines("activity_network.Oxford_NEG_5_28_PDD_PD_LEEK.txt")[-c(1)]%>%as.data.frame()


SVA_Method<-c(rep("BE",dim(Oxford_POS_5_28_PDD_Control_BE)[1]),rep("BE",dim(Oxford_NEG_5_28_PDD_Control_BE)[1]),
	    	  rep("LEEK",dim(Oxford_POS_5_28_PDD_Control_LEEK)[1]),rep("LEEK",dim(Oxford_NEG_5_28_PDD_Control_LEEK)[1]),
              rep("BE",dim(Oxford_POS_5_28_PD_Control_BE)[1]),rep("BE",dim(Oxford_NEG_5_28_PD_Control_BE)[1]),
              rep("LEEK",dim(Oxford_POS_5_28_PD_Control_LEEK)[1]),rep("LEEK",dim(Oxford_NEG_5_28_PD_Control_LEEK)[1]),
		      rep("BE",dim(Oxford_POS_5_28_PDD_PD_BE)[1]),rep("BE",dim(Oxford_NEG_5_28_PDD_PD_BE)[1]),
		      rep("LEEK",dim(Oxford_POS_5_28_PDD_PD_LEEK)[1]),rep("LEEK",dim(Oxford_NEG_5_28_PDD_PD_LEEK)[1]))

Text<-rbind(Oxford_POS_5_28_PDD_Control_BE,Oxford_NEG_5_28_PDD_Control_BE,Oxford_POS_5_28_PDD_Control_LEEK,Oxford_NEG_5_28_PDD_Control_LEEK,
	        Oxford_POS_5_28_PD_Control_BE,Oxford_NEG_5_28_PD_Control_BE,Oxford_POS_5_28_PD_Control_LEEK,Oxford_NEG_5_28_PD_Control_LEEK,
	        Oxford_POS_5_28_PDD_PD_BE,Oxford_NEG_5_28_PDD_PD_BE,Oxford_POS_5_28_PDD_PD_LEEK,Oxford_NEG_5_28_PDD_PD_LEEK)

Mode<-c(rep("ESI+",dim(Oxford_POS_5_28_PDD_Control_BE)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PDD_Control_BE)[1]),
	    rep("ESI+",dim(Oxford_POS_5_28_PDD_Control_LEEK)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PDD_Control_LEEK)[1]),
        rep("ESI+",dim(Oxford_POS_5_28_PD_Control_BE)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PD_Control_BE)[1]),
        rep("ESI+",dim(Oxford_POS_5_28_PD_Control_LEEK)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PD_Control_LEEK)[1]),
		rep("ESI+",dim(Oxford_POS_5_28_PDD_PD_BE)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PDD_PD_BE)[1]),
		rep("ESI+",dim(Oxford_POS_5_28_PDD_PD_LEEK)[1]),rep("ESI-",dim(Oxford_NEG_5_28_PDD_PD_LEEK)[1]))

Study<-c(rep("Oxford_PDD_Control",dim(Oxford_POS_5_28_PDD_Control_BE)[1]+dim(Oxford_NEG_5_28_PDD_Control_BE)[1]+dim(Oxford_POS_5_28_PDD_Control_LEEK)[1]+dim(Oxford_NEG_5_28_PDD_Control_LEEK)[1]),
	     rep("Oxford_PD_Control",dim(Oxford_POS_5_28_PD_Control_BE)[1]+dim(Oxford_NEG_5_28_PD_Control_BE)[1]+dim(Oxford_POS_5_28_PD_Control_LEEK)[1]+dim(Oxford_NEG_5_28_PD_Control_LEEK)[1]),
	     rep("Oxford_PDD_PD",dim(Oxford_POS_5_28_PDD_PD_BE)[1]+dim(Oxford_NEG_5_28_PDD_PD_BE)[1]+dim(Oxford_POS_5_28_PDD_PD_LEEK)[1]+dim(Oxford_NEG_5_28_PDD_PD_LEEK)[1]))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
Oxford_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(Oxford_frame)[1]<-"Text"

#####################
#####################
ASD_POS_LFA_TYP<-read_lines("activity_network.ASD_POS_LFA_TYP.txt")[-c(1)]%>%as.data.frame()
ASD_NEG_LFA_TYP<-read_lines("activity_network.ASD_NEG_LFA_TYP.txt")[-c(1)]%>%as.data.frame()
ASD_POS_HFA_TYP<-read_lines("activity_network.ASD_POS_HFA_TYP.txt")[-c(1)]%>%as.data.frame()
ASD_NEG_HFA_TYP<-read_lines("activity_network.ASD_NEG_HFA_TYP.txt")[-c(1)]%>%as.data.frame()
ASD_POS_HFA_LFA<-read_lines("activity_network.ASD_POS_HFA_LFA.txt")[-c(1)]%>%as.data.frame()
ASD_NEG_HFA_LFA<-read_lines("activity_network.ASD_NEG_HFA_LFA.txt")[-c(1)]%>%as.data.frame()



SVA_Method<-c(rep("BE",dim(ASD_NEG_LFA_TYP)[1]+dim(ASD_POS_LFA_TYP)[1]+dim(ASD_NEG_HFA_TYP)[1]+dim(ASD_POS_HFA_TYP)[1]+dim(ASD_NEG_HFA_LFA)[1]+dim(ASD_POS_HFA_LFA)[1]))
Text<-rbind(ASD_POS_LFA_TYP,ASD_NEG_LFA_TYP,ASD_POS_HFA_TYP,ASD_NEG_HFA_TYP,ASD_POS_HFA_LFA,ASD_NEG_HFA_LFA)
Mode<-c(rep("ESI+",dim(ASD_POS_LFA_TYP)[1]),rep("ESI-",dim(ASD_NEG_LFA_TYP)[1]),
	    rep("ESI+",dim(ASD_POS_HFA_TYP)[1]),rep("ESI-",dim(ASD_NEG_HFA_TYP)[1]),
        rep("ESI+",dim(ASD_POS_HFA_LFA)[1]),rep("ESI-",dim(ASD_NEG_HFA_LFA)[1]))

Study<-c(rep("ASD.LFA_NeuroTYP",dim(ASD_POS_LFA_TYP)[1]+dim(ASD_NEG_LFA_TYP)[1]),
	     rep("ASD.HFA_NeuroTYP",dim(ASD_POS_HFA_TYP)[1]+dim(ASD_NEG_HFA_TYP)[1]),
	     rep("ASD.HFA_LFA",dim(ASD_POS_HFA_LFA)[1]+dim(ASD_NEG_HFA_LFA)[1]))

study.method<-paste(paste(Study,SVA_Method,sep="."),Mode,sep="//")
ASD_frame<-cbind(Text,SVA_Method,Mode,Study,study.method)
colnames(ASD_frame)[1]<-"Text"

###########
####################
#########################################
#########################################

final_frame<-rbind(DS.Legacy_frame,RAS_frame,ADRC_frame,Ringman_frame) #########,Oxford_frame,ASD_frame
final_frame$Text<-as.character(final_frame$Text)
final_frame_words<-final_frame%>%unnest_tokens(word,Text)%>%count(word,study.method, sort = TRUE)
final_frame_bigram<-final_frame%>%unnest_tokens(word,Text,token='ngrams',n=2)%>%count(word,study.method, sort = TRUE)
final_frame_trigram<-final_frame%>%unnest_tokens(word,Text,token='ngrams',n=3)%>%count(word,study.method, sort = TRUE)
final_frame_lines<-final_frame%>%unnest_tokens(word,Text,token='lines')%>%count(word,study.method, sort = TRUE)
final_frame_total<-rbind(final_frame_words,final_frame_bigram,final_frame_trigram,final_frame_lines)%>%filter(word!="NA")%>%arrange(study.method)%>%cast_dfm(word,study.method, n)%>%t()


######## USE FOR findThoughts()
rawTEXT<-aggregate(Text~study.method,toString,data=final_frame)%>%arrange(study.method)

quanteda::docvars(final_frame_total, "DocName")<-quanteda::docnames(final_frame_total)
quanteda::docvars(final_frame_total, "Contrast")<-c(rep("ADRC.Converter_Control",2),rep("DS Legacy.AD_NOAD",4),##########rep("ASD.HFA_LFA",2),rep("ASD.HFA_NeuroTYP",2),rep("ASD.LFA_NeuroTYP",2),
	                                                ########rep("Oxford_PD_Control",4),rep("Oxford_PDD_Control",4),rep("Oxford_PDD_PD",4),
	                                                rep("RAS.Conv_Con",4),rep("RAS.MCIAD_Con",4),rep("RAS.MCIAD_Conv",4),
	                                                rep("RAS.Super_Con",4),rep("RAS.Super_Conv",4),rep("RAS.Super_MCIAD",4),
	                                                rep("Ringman.AD_MCI",2),rep("Ringman.Carrier_Control",2),rep("Ringman.Carrier_MCIAD",2),rep("Ringman.MCI_Carrier",2))
quanteda::docvars(final_frame_total, "Mode")<-c(rep(c("ESI-","ESI+"),19))
quanteda::docvars(final_frame_total, "SVA")<-c("BE","BE","BE","BE","LEEK","LEEK",
                                                rep(c("BE","BE","LEEK","LEEK"),6),
                                                rep("BE",8)) #########c(rep("BE",10),rep("LEEK",2),rep(c("BE","BE","LEEK","LEEK"),3),rep(c("BE","BE","LEEK","LEEK"),6),rep("BE",6))
quanteda::docvars(final_frame_total, "Text")<-rawTEXT$Text


######SHOULD RETURN TRUE!!!!!!!!!!
all.equal(quanteda::docnames(final_frame_total),rawTEXT$study.method,
	      quanteda::docvars(final_frame_total)$DocName)

stmcorpus<-dfm_trim(final_frame_total, min_docfreq=.075, max_docfreq=.9, docfreq_type="prop")%>%asSTMCorpus()  #####dfm_trim(final_frame_total, min_docfreq=.075, max_docfreq=.9, docfreq_type="prop")

######### K=4  
set.seed(122)
stm.search <- searchK(documents = stmcorpus$documents,
	                  vocab = stmcorpus$vocab,
                      K = 2:10,
                      init.type = "Spectral",
                      data=stmcorpus$data,
                      seed=122)

plot(stm.search)


set.seed(122)
stm_mod.file_8.8<- stm(documents = stmcorpus$documents,
                      vocab = stmcorpus$vocab,
                      K = 0,#######5,
                      init.type = "Spectral",
                      prevalence = ~Contrast*Mode*SVA,
                      content = ~Contrast,
                      data = stmcorpus$data,
                      max.em.its=75,
                      seed = 122)
######set.seed(122)
#tm_mod.six<- stm(documents = stmcorpus$documents,
#                      vocab = stmcorpus$vocab,
#                      K = 6,
#                     init.type = "Spectral",
#                     prevalence = ~Contrast*Mode*SVA,
#                     content = ~Contrast,
#                     data = stmcorpus$data,
#                    max.em.its=75,
#                    seed = 122)
stm_mod.five.prep_8.8<-estimateEffect(1:5 ~ Contrast+Mode*SVA, stm_mod.file_8.8, meta = stmcorpus$data, uncertainty = "Global")

out <- list(documents = stmcorpus$documents,
            vocab = stmcorpus$vocab,
            meta = stmcorpus$data)
save(out,stmcorpus,stm_mod.five.prep_8.8,stm_mod.file_8.8,file="THESIS_STM_INTERACTION_8_8.RData")
library(stminsights)
run_stminsights()