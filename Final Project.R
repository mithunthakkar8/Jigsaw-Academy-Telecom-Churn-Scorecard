library("readxl")
library("dplyr")
getwd()
setwd("C:\\Jig18209\\Mithun")

for(j in names(dat1))
{
  if (!is.factor(dat1[[j]])&!is.character(dat1[[j]])) {
    {
      for(i in 10:2)
      {
        if(i%%2==0|i==3)
        {
          dat1%>% mutate(dec=ntile(dat1[[j]],i))%>%count(churn,dec)%>%filter(churn==1)->dat45
          dat46<-NA
          dat46<-dat1%>%mutate(dec=ntile(dat1[[j]],i))%>%group_by(dec)%>%summarise(N=n())
          
          for (k in 0:10) {
            if ((nrow(dat45))==(nrow(dat46)-k)) {
              dat46<-dat1%>%mutate(dec=ntile(dat1[[j]],i-k))%>%group_by(dec)%>%summarise(N=n())
              dat45$churn_perc<-dat45$n/dat46$N
              dat45$N<-dat46$N
              dat45$varname<-rep(j,nrow(dat45))
              dat45$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(get(j), n=i-k))%>%group_by(dec)%>%summarise(GreaterThan=min(get(j),na.rm=TRUE)))[[2]]
              dat45$LesserThan<-unclass(dat1%>%mutate(dec=ntile(get(j), n=i-k))%>%group_by(dec)%>%summarise(LesserThan=max(get(j),na.rm=TRUE)))[[2]]
              print(dat45)
              write.table(dat45,"ContinuousDecileBinning.txt", append = TRUE, row.names = FALSE)
              
              
            }
          }
          
        }
        
      }
    }
  }
}


for(j in names(dat1))
{
  if (is.factor(dat1[[j]])| length(unique(dat1[[j]]))<56|is.character(dat1[[j]])) { 
    
    table1<-table(dat1[[j]],dat1$churn)
    as.data.frame(table1)%>%filter(Var2==1)->table2
    names(table2)=c("levels","churn","n")
    table2$N<-rowSums(table1)
    table2$ChurnPerc<-table2$n/table2$N
    ind2<-which(is.na(dat1[[j]]))
    table3<-table(dat1$churn[ind2])/length(ind2)
    
    table2$varname<-rep(j,nrow(table2))
    
    write.table(table2,"CategoricalBins.txt", append = TRUE, row.names = FALSE)
    write.table(table3,"CategoricalBins.txt", append = TRUE, row.names = FALSE)
  }
}


dat1<-read.csv("telecomfinal.csv")
Variables<-names(dat1)
class<-names(dat1)
nobs<-1:ncol(dat1)
unique<-1:ncol(dat1)
miss<-1:ncol(dat1)
min<-1:ncol(dat1)
max<-1:ncol(dat1)
five<-1:ncol(dat1)
ten<-1:ncol(dat1)
twofive<-1:ncol(dat1)
sevenfive<-1:ncol(dat1)
ninefive<-1:ncol(dat1)
ninenine<-1:ncol(dat1)

Rep<-list(Variables, class, nobs, unique, miss, min, max, five, ten, twofive, sevenfive, ninefive, ninenine)

for(i in 1:ncol(dat1))
{
  Rep[[2]][i]<-class(dat1[,i])
  Rep[[3]][i]<-nrow(dat1)
  Rep[[4]][i]<-length(unique(dat1[,i]))
  Rep[[5]][i]<-sum(is.na(dat1[,i]))
  Rep[[6]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",min(dat1[,i],na.rm=T),0)
  Rep[[7]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",max(dat1[,i],na.rm=T),0)
  Rep[[8]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.05,na.rm=T),0)
  Rep[[9]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.1,na.rm=T),0)
  Rep[[10]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.25,na.rm=T),0)
  Rep[[11]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.75,na.rm=T),0)
  Rep[[12]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.9,na.rm=T),0)
  Rep[[13]][i]<-ifelse(class(dat1[,i])=="numeric"|class(dat1[,i])=="integer",quantile(dat1[,i],p=0.99,na.rm=T),0)
}

dat2=data.frame(Rep)

names(dat2)<-c("Variables", "class", "nobs", "unique", "miss", "min", "max", "five", "ten", "twofive", "sevenfive", "ninefive", "ninenine")





dat1<-dat1[,-c(12,46,47,48,49,52,53,62,61,72,63,55,64,66)]


dat1$age1<-as.numeric(dat1$age1)

dat1%>%filter(age1==0)%>%nrow()
dat1%>%filter(age2==0)%>%nrow()
dat1<-dat1[,-c(38,39)]

dat1<-dat1[,-c(51)]


dat1<-select(dat1,-Customer_ID)












#missing value imputation categorical variables

               
ind2<-which(is.na(dat1$prizm_social_one))
dat1$prizm_social_one[ind2]<-"T"



ind2<-which(is.na(dat1$area))
dat1$area[ind2]<-"OHIO AREA"

ind2<-which(is.na(dat1$hnd_webcap))
dat1$hnd_webcap[ind2]<-"WC"
ind2<-which(dat1$hnd_webcap=="UNKW")
dat1$hnd_webcap[ind2]<-"WCMB"


ind2<-which(is.na(dat1$ethnic))
dat1$ethnic[ind2]<-"N"


ind2<-which(is.na(dat1$hnd_price))
dat1$hnd_price[ind2]<-299.9899902

ind2<-which(is.na(dat1$forgntvl))
dat1$forgntvl[ind2]<-1

ind2<-which(is.na(dat1$mtrcycle))
dat1$mtrcycle[ind2]<-0

ind2<-which(is.na(dat1$truck))
dat1$truck[ind2]<-1

ind2<-which(is.na(dat1$eqpdays))
dat1$eqpdays[ind2]<-191


#missing value imputation continuous variables

ind2<-which(is.na(dat1$mou_Mean))
dat1$mou_Mean[ind2]<-27

ind2<-which(is.na(dat1$change_mou))
dat1$change_mou[ind2]<-2051

ind2<-which(is.na(dat1$rev_Range))
dat1$rev_Range[ind2]<-58

ind2<-which(is.na(dat1$totmrc_Mean))
dat1$totmrc_Mean[ind2]<-19.99


ind2<-which(is.na(dat1$ovrrev_Mean))
dat1$ovrrev_Mean[ind2]<-0

ind2<-which(is.na(dat1$ovrmou_Mean))
dat1$ovrmou_Mean[ind2]<-0

ind2<-which(is.na(dat1$avg6mou))
dat1[c(ind2),]
dat1$avg6mou[ind2]<-4200

ind2<-which(is.na(dat1$avg6qty))
dat1$avg6qty[ind2]<-1600

ind2<-which(is.na(dat1$datovr_Mean))
quantile(dat1$datovr_Mean, p=(0:10)/10, na.rm = T)
summary(dat1$datovr_Mean)
dat1$datovr_Mean[ind2]<-0

ind2<-which(is.na(dat1$rev_Mean))
dat1$rev_Mean[ind2]<-10

ind2<-which(is.na(dat1$roam_Mean))
dat1$roam_Mean[ind2]<-0

ind2<-which(is.na(dat1$da_Mean))
dat1$da_Mean[ind2]<-0

ind2<-which(is.na(dat1$mou_Range))
dat1$mou_Range[ind2]<-25

ind2<-which(is.na(dat1$da_Range))
dat1$da_Range[ind2]<-0

ind2<-which(is.na(dat1$datovr_Range))
dat1$datovr_Range[ind2]<-0


#Categorical Variables Data Preparation







dat1$models_derived<- ifelse(dat1$models==1, "one", "NotOne")
dat1$models_derived<-as.factor(dat1$models_derived)
summary(dat1$models_derived)
dat1<-select(dat1,-models)
ind2<-which(is.na(dat1$models_derived))
dat1$models_derived[ind2]<-"NotOne"



dat1$hnd_price_derived<-as.character(dat1$hnd_price)
index<-which(dat1$hnd_price_derived=="9.989997864"|dat1$hnd_price_derived=="29.98999023"|dat1$hnd_price_derived=="39.98999023")
dat1$hnd_price_derived[index]<-"Low"
dat1$hnd_price_derived[-index]<-"High"
chisq.test(table(dat1$churn, dat1$hnd_price))
chisq.test(table(dat1$churn, dat1$hnd_price_derived))
dat1<-select(dat1,-hnd_price)




dat1$actvsubs_derived<-as.character(dat1$actvsubs)
index<-which(dat1$actvsubs_derived=="2"|dat1$actvsubs_derived=="4"|dat1$actvsubs_derived=="8")
dat1$actvsubs_derived[index]<-"2or4or8"
index<--grep("2or", dat1$actvsubs_derived)
dat1$actvsubs_derived[index]<-"01357"
chisq.test(table(dat1$churn, dat1$actvsubs))
chisq.test(table(dat1$churn, dat1$actvsubs_derived))
dat1<-select(dat1,-actvsubs)



dat1$uniqsubs_derived<-as.character(dat1$uniqsubs)
index<-which(dat1$uniqsubs_derived=="1")
dat1$uniqsubs_derived[index]<-"One"
dat1$uniqsubs_derived[-index]<-"NotOne"
chisq.test(table(dat1$churn, dat1$uniqsubs))
chisq.test(table(dat1$churn, dat1$uniqsubs_derived))
dat1<-select(dat1,-uniqsubs)


table1<-table(dat1$csa,dat1$churn)
as.data.frame(table1)%>%filter(Var2==1)->table2
names(table2)=c("csa","churn","n")
table2$N<-rowSums(table1)
table2$ChurnPerc<-table2$n/table2$N
table2<-select(table2, -churn)

dat1<-merge(dat1,table2, by="csa", all.x = TRUE)
dat1$csa_Derived<-ifelse(dat1$ChurnPerc>0.25, "High", "Low")
dat1<-select(dat1,-csa, -n, -N, -ChurnPerc)




dat1$MOUperCall<-dat1$avgmou/dat1$avgqty
dat1$comp_vce_perc<-dat1$comp_vce_Mean/dat1$plcd_vce_Mean
dat1$comp_dat_Perc<-dat1$comp_dat_Mean/dat1$plcd_dat_Mean
#dat1$drop_dat_Perc<-dat1$drop_dat_Mean/dat1$plcd_dat_Mean
dat1$blck_dat_Perc<-dat1$blck_dat_Mean/dat1$plcd_dat_Mean
dat1$opk_dat_Perc<-dat1$opk_dat_Mean/dat1$plcd_dat_Mean
dat1$pk_dat_Perc<-dat1$mou_pead_Mean/dat1$plcd_dat_Mean

ind2<-which(is.na(dat1$comp_vce_perc))
dat1$comp_vce_perc[ind2]<-0


ind2<-which(is.na(dat1$MOU3mon_Calls3mon))
dat1$MOU3mon_Calls3mon[ind2]<-0






ind2<-which(is.na(dat1$MOUperCall))
dat1$MOUperCall[ind2]<-0






ind2<-which(is.na(dat1$comp_dat_Perc))
dat1$comp_dat_Perc[ind2]<-0

ind2<-which(is.na(dat1$drop_dat_Perc))
dat1$drop_dat_Perc[ind2]<-0

ind2<-which(is.na(dat1$blck_dat_Perc))
dat1$blck_dat_Perc[ind2]<-0

ind2<-which(is.na(dat1$opk_dat_Perc))
dat1$opk_dat_Perc[ind2]<-0

ind2<-which(is.na(dat1$pk_dat_Perc))
dat1$pk_dat_Perc[ind2]<-0

ind2<-which(is.na(dat1$refurb_new))
dat1$refurb_new[ind2]<-"N"




ind2<-which(is.na(dat1$drop_dat_Perc))




#Continuous Variables Data Preparation

library(caret)



dat1$iwylis_vce_Mean_derived<-as.character(dat1$iwylis_vce_Mean)
index<-which(dat1$iwylis_vce_Mean<=0)
dat1$iwylis_vce_Mean_derived[index]<-"Zero"
dat1$iwylis_vce_Mean_derived[-index]<-"MoreThan0"
dat1$iwylis_vce_Mean_derived<-as.factor(dat1$iwylis_vce_Mean_derived)
summary(glm(churn~iwylis_vce_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$iwylis_vce_Mean_derived))
dat1<-select(dat1,-iwylis_vce_Mean)
summary(dat1$iwylis_vce_Mean_derived)

dat1$callwait_Range_derived<-as.character(dat1$callwait_Range)
index<-which(dat1$callwait_Range<=0)
dat1$callwait_Range_derived[index]<-"LessThan0"
index<-which(dat1$callwait_Range>0)
dat1$callwait_Range_derived[index]<-"MoreThan0"
summary(glm(churn~callwait_Range, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$callwait_Range_derived))
dat1<-select(dat1,-callwait_Range)



dat1$adjqty_derived<-ifelse(dat1$adjqty<6262&dat1$adjqty>998, "High", "Low")
dat1$adjqty_derived<-as.factor(dat1$adjqty_derived)
summary(dat1$adjqty_derived)
dat1<-select(dat1,-adjqty)

dat1$mou_pead_Mean_derived<-as.character(dat1$mou_pead_Mean)
index<-which(dat1$mou_pead_Mean<=0)
dat1$mou_pead_Mean_derived[index]<-"LessThan0"
index<-which(dat1$mou_pead_Mean>0)
dat1$mou_pead_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~mou_pead_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$mou_pead_Mean_derived))
dat1<-select(dat1,-mou_pead_Mean)

dat1$ovrrev_Mean_derived<-as.character(dat1$ovrrev_Mean)
index<-which(dat1$ovrrev_Mean==0)
dat1$ovrrev_Mean_derived[index]<-"No_OvrRev"
index<-which(dat1$ovrrev_Mean>0)
dat1$ovrrev_Mean_derived[index]<-"Morethan0"
summary(glm(churn~ovrrev_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$ovrrev_Mean_derived))
dat1<-select(dat1,-ovrrev_Mean)

dat1$blck_dat_Perc_derived<-as.character(dat1$blck_dat_Perc)
index<-which(dat1$blck_dat_Perc==0)
dat1$blck_dat_Perc_derived[index]<-"Zero"
index<-which(dat1$blck_dat_Perc>0)
dat1$blck_dat_Perc_derived[index]<-"Morethan0"
summary(glm(churn~blck_dat_Perc, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$blck_dat_Perc_derived))
dat1<-select(dat1,-blck_dat_Perc)

dat1$opk_dat_Perc_derived<-as.character(dat1$opk_dat_Perc)
index<-which(dat1$opk_dat_Perc==0)
dat1$opk_dat_Perc_derived[index]<-"Zero"
index<-which(dat1$opk_dat_Perc>0)
dat1$opk_dat_Perc_derived[index]<-"Morethan0"
summary(glm(churn~opk_dat_Perc, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$opk_dat_Perc_derived))
dat1<-select(dat1,-opk_dat_Perc)

dat1$comp_dat_Perc_derived<-as.character(dat1$comp_dat_Perc)
index<-which(dat1$comp_dat_Perc==0)
dat1$comp_dat_Perc_derived[index]<-"Zero"
index<-which(dat1$comp_dat_Perc>0)
dat1$comp_dat_Perc_derived[index]<-"Morethan0"
summary(glm(churn~comp_dat_Perc, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$comp_dat_Perc_derived))
dat1<-select(dat1,-comp_dat_Perc)

dat1$pk_dat_Perc_derived<-as.character(dat1$pk_dat_Perc)
index<-which(dat1$pk_dat_Perc==0)
dat1$pk_dat_Perc_derived[index]<-"Zero"
index<-which(dat1$pk_dat_Perc>0)
dat1$pk_dat_Perc_derived[index]<-"Morethan0"
summary(glm(churn~pk_dat_Perc, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$pk_dat_Perc_derived))
dat1<-select(dat1,-pk_dat_Perc)

dat1$da_Range_derived<-as.character(dat1$da_Range)
index<-which(dat1$da_Range==0)
dat1$da_Range_derived[index]<-"Zero"
index<-which(dat1$da_Range>0)
dat1$da_Range_derived[index]<-"Morethan0"
summary(glm(churn~da_Range, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$da_Range_derived))
dat1<-select(dat1,-da_Range)


















dat1$callwait_Mean_derived<-as.character(dat1$callwait_Mean)
index<-which(dat1$callwait_Mean<=0)
dat1$callwait_Mean_derived[index]<-"LessThan0"
index<-which(dat1$callwait_Mean>0)
dat1$callwait_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~callwait_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$callwait_Mean_derived))
#dat1<-select(dat1,-callwait_Mean)


dat1$totcalls_derived<-ifelse(dat1$totcalls<6339&dat1$totcalls>1018,"High", "Low")
dat1$totcalls_derived<-as.factor(dat1$totcalls_derived)
summary(dat1$totcalls_derived)
#dat1<-select(dat1,-totcalls)








dat1$ovrmou_Mean_derived<-as.character(dat1$ovrmou_Mean)
index<-which(dat1$ovrmou_Mean<=0)
dat1$ovrmou_Mean_derived[index]<-"LessThan0"
index<-which(dat1$ovrmou_Mean>0)
dat1$ovrmou_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~ovrmou_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$ovrmou_Mean_derived))
dat1<-select(dat1,-ovrmou_Mean)

dat1$avg3mou_derived<-as.character(dat1$avg3mou)
index<-which(dat1$avg3mou<=276)
dat1$avg3mou_derived[index]<-"LessThan276"
index<-which(dat1$avg3mou>276)
dat1$avg3mou_derived[index]<-"MoreThan276"
summary(glm(churn~avg3mou, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$avg3mou_derived))
dat1<-select(dat1,-avg3mou)

dat1$avg3qty_derived<-as.character(dat1$avg3qty)
index<-which(dat1$avg3qty<=128)
dat1$avg3qty_derived[index]<-"LessThan128"
index<-which(dat1$avg3qty>128)
dat1$avg3qty_derived[index]<-"MoreThan128"
summary(glm(churn~avg3qty, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$avg3qty_derived))
dat1<-select(dat1,-avg3qty)

dat1$avgqty_derived<-as.character(dat1$avgqty)
index<-which(dat1$avgqty<=1618&dat1$avgqty>29.46)
dat1$avgqty_derived[index]<-"High"
dat1$avgqty_derived[-index]<-"Low"
summary(glm(churn~avgqty, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$avgqty_derived))
dat1<-select(dat1,-avgqty)

#dat1$avg6mou_derived<-as.character(dat1$avg6mou)
#index<-which(dat1$avg6mou<=387)
#dat1$avg6mou_derived[index]<-"High"
#dat1$avg6mou_derived[-index]<-"Low"
#summary(glm(churn~avg6mou, data=dat1, family = "binomial"))
#chisq.test(table(dat1$churn, dat1$avg6mou_derived))
#dat1<-select(dat1,-avg6mou)

dat1$avg6qty_derived<-as.character(dat1$avg6qty)
index<-which(dat1$avg6qty<=134)
dat1$avg6qty_derived[index]<-"High"
dat1$avg6qty_derived[-index]<-"Low"
summary(glm(churn~avg6qty, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$avg6qty_derived))
dat1<-select(dat1,-avg6qty)

dat1$opk_dat_Mean_derived<-as.character(dat1$opk_dat_Mean)
index<-which(dat1$opk_dat_Mean<=0)
dat1$opk_dat_Mean_derived[index]<-"LessThan0"
index<-which(dat1$opk_dat_Mean>0)
dat1$opk_dat_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~opk_dat_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$opk_dat_Mean_derived))
dat1<-select(dat1,-opk_dat_Mean)

dat1$roam_Mean_derived<-as.character(dat1$roam_Mean)
index<-which(dat1$roam_Mean<=0)
dat1$roam_Mean_derived[index]<-"LessThan0"
index<-which(dat1$roam_Mean>0)
dat1$roam_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~roam_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$roam_Mean_derived))
dat1<-select(dat1,-roam_Mean)

dat1$recv_sms_Mean_derived<-as.character(dat1$recv_sms_Mean)
index<-which(dat1$recv_sms_Mean<=0)
dat1$recv_sms_Mean_derived[index]<-"LessThan0"
index<-which(dat1$recv_sms_Mean>0)
dat1$recv_sms_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~recv_sms_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$recv_sms_Mean_derived))
dat1<-select(dat1,-recv_sms_Mean)

dat1$blck_dat_Mean_derived<-as.character(dat1$blck_dat_Mean)
index<-which(dat1$blck_dat_Mean<=0)
dat1$blck_dat_Mean_derived[index]<-"Zero"
index<-which(dat1$blck_dat_Mean>0)
dat1$blck_dat_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~blck_dat_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$blck_dat_Mean_derived))



dat1$da_Mean_derived<-as.character(dat1$da_Mean)
index<-which(dat1$da_Mean<=0)
dat1$da_Mean_derived[index]<-"LessThan0"
dat1$da_Mean_derived[-index]<-"MoreThan0"
summary(glm(churn~da_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$da_Mean_drived))
dat1<-select(dat1,-da_Mean)
dat1$da_Mean_derived<-as.factor(dat1$da_Mean_derived)
summary(dat1$da_Mean_derived)

dat1$datovr_Mean_derived<-as.character(dat1$datovr_Mean)
index<-which(dat1$datovr_Mean==0)
dat1$datovr_Mean_derived[index]<-"No_DatOvrRev"
index<-which(dat1$datovr_Mean>0)
dat1$datovr_Mean_derived[index]<-"Gen_DatOvrRev"
dat1$datovr_Mean_derived<-as.factor(dat1$datovr_Mean_derived)
summary(glm(churn~datovr_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$datovr_Mean_derived))
dat1<-select(dat1,-datovr_Mean)

dat1$drop_dat_Mean_derived<-as.character(dat1$drop_dat_Mean)
index<-which(dat1$drop_dat_Mean==0)
dat1$drop_dat_Mean_derived[index]<-"Zero"
dat1$drop_dat_Mean_derived[-index]<-"MoreThanZero"
dat1$drop_dat_Mean_derived<-as.factor(dat1$drop_dat_Mean_derived)
summary(glm(churn~drop_dat_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$drop_dat_Mean_derived))
dat1<-select(dat1,-drop_dat_Mean)

dat1$totrev_derived<-ifelse(dat1$totrev>=559.01&dat1$totrev<=1427.15, "High", "Low")
dat1$totrev_derived<-as.factor(dat1$totrev_derived)
summary(dat1$totrev_derived)
dat1<-select(dat1,-totrev)

dat1$avgrev_derived<-ifelse(dat1$avgrev<=29.99|dat1$avgrev>=42.74&dat1$avgrev<=77.18, "High", "Low")
dat1$avgrev_derived<-as.factor(dat1$avgrev_derived)
summary(dat1$avgrev_derived)
dat1<-select(dat1,-avgrev)

dat1$comp_dat_Mean_derived<-as.character(dat1$comp_dat_Mean)
index<-which(dat1$comp_dat_Mean<=0)
dat1$comp_dat_Mean_derived[index]<-"LessThan0"
index<-which(dat1$comp_dat_Mean>0)
dat1$comp_dat_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~comp_dat_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$comp_dat_Mean_derived))
dat1<-select(dat1,-comp_dat_Mean)



dat1$plcd_dat_Mean_derived<-as.character(dat1$plcd_dat_Mean)
index<-which(dat1$plcd_dat_Mean<=0)
dat1$plcd_dat_Mean_derived[index]<-"LessThan0"
index<-which(dat1$plcd_dat_Mean>0)
dat1$plcd_dat_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~plcd_dat_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$plcd_dat_Mean_derived))
dat1<-select(dat1,-plcd_dat_Mean)

dat1$custcare_Mean_derived<-as.character(dat1$custcare_Mean)
index<-which(dat1$custcare_Mean<=0)
dat1$custcare_Mean_derived[index]<-"LessThan0"
index<-which(dat1$custcare_Mean>0)
dat1$custcare_Mean_derived[index]<-"MoreThan0"
summary(glm(churn~custcare_Mean, data=dat1, family = "binomial"))
chisq.test(table(dat1$churn, dat1$custcare_Mean_derived))
dat1<-select(dat1,-custcare_Mean)





ind2<-which(is.na(dat1$csa_Derived))
dat1$csa_Derived[ind2]<-"Low"


dat1$csa_Derived<-as.character(dat1$csa)
ind2<-which(is.na(dat1$csa))
dat1$csa_Derived[ind2]<-"Low"










#dat1$drop_blk_Mean_derived<- ifelse(dat1$drop_blk_Mean<=1.333333333|dat1$drop_blk_Mean<=7.666666667&dat1$drop_blk_Mean>=5.333333333, "High", "Low")
#dat1$drop_blk_Mean_derived<-as.factor(dat1$drop_blk_Mean_derived)
#summary(dat1$drop_blk_Mean_derived)
#dat1<-select(dat1,-drop_blk_Mean)





#dat1$rev_Mean_derived<-ifelse(dat1$rev_Mean<=35.2825, "High", "Low")
#dat1$rev_Mean_derived<-as.factor(dat1$rev_Mean_derived)
#summary(dat1$rev_Mean_derived)
#dat1<-select(dat1,-rev_Mean)

#dat1$adjmou_derived<-ifelse(dat1$adjmou>=3878&dat1$adjmou<=11309, "High", "Low")
#dat1$adjmou_derived<-as.factor(dat1$adjmou_derived)
#summary(dat1$adjmou_derived)
#dat1<-select(dat1,-adjmou)



#dat1$adjrev_derived<-ifelse(dat1$adjrev>=490.44&dat1$adjrev<=1351.84, "High", "Low")
#dat1$adjrev_derived<-as.factor(dat1$adjrev_derived)
#summary(dat1$adjrev_derived)
#dat1<-select(dat1,-adjrev)

#dat1$drop_vce_Mean_derived<-ifelse(dat1$drop_vce_Mean>=1.333333333, "High", "Low")
#dat1$drop_vce_Mean_derived<-as.factor(dat1$drop_vce_Mean_derived)
#summary(dat1$drop_vce_Mean_derived)
#dat1<-select(dat1,-drop_vce_Mean)
















#dat1<-select(dat1,-callwait_Range_derived)











#dat1$avg3mou_derived<-as.character(dat1$avg3mou)
#index<-which(dat1$avg3mou<=276)
#dat1$avg3mou_derived[index]<-"LessThan276"
#index<-which(dat1$avg3mou>276)
#dat1$avg3mou_derived[index]<-"MoreThan276"
#summary(glm(churn~avg3mou, data=dat1, family = "binomial"))
#chisq.test(table(dat1$churn, dat1$avg3mou_derived))
#dat1<-select(dat1,-avg3mou)


















#dat1$MOUperCall_derived<-as.character(dat1$MOUperCall)
#index<-which(dat1$MOUperCall<=2.04120589336146)
#dat1$MOUperCall_derived[index]<-"LessThan2"
#index<-which(dat1$MOUperCall>2.04120589336146)
#dat1$MOUperCall_derived[index]<-"MoreThan2"
#summary(glm(churn~MOUperCall, data=dat1, family = "binomial"))
#chisq.test(table(dat1$churn, dat1$MOUperCall_derived))
#dat1<-select(dat1,-MOUperCall_derived)

#dat1$comp_vce_perc_derived<-as.character(dat1$comp_vce_perc)
#index<-which(dat1$comp_vce_perc<=0.653061224827155)
#dat1$comp_vce_perc_derived[index]<-"LessThan0.65"
#index<-which(dat1$comp_vce_perc>0.653061224827155)
#dat1$comp_vce_perc_derived[index]<-"MoreThan0.65"
#summary(glm(churn~comp_vce_perc, data=dat1, family = "binomial"))
#chisq.test(table(dat1$churn, dat1$comp_vce_perc_derived))
#dat1<-select(dat1,-comp_vce_perc_derived)



#dat1$drop_dat_Perc_derived<-as.character(dat1$drop_dat_Perc)
#index<-which(dat1$drop_dat_Perc<=0)
#dat1$drop_dat_Perc_derived[index]<-"LessThan0"
#index<-which(dat1$drop_dat_Perc>0)
#dat1$drop_dat_Perc_derived[index]<-"MoreThan0"
#summary(glm(churn~drop_dat_Perc, data=dat1, family = "binomial"))
#chisq.test(table(dat1$churn, dat1$drop_dat_Perc_derived))
#dat1$drop_dat_Perc_derived<-as.factor(dat1$drop_dat_Perc_derived)
#summary(dat1$drop_dat_Perc_derived)




#Stepwise Regression  

#removing rows below based on insignificance of variables found through stepwise regression










#final Phase data preparationg for categorical variables

dat1$crclscod_derived<-as.factor(dat1$crclscod_derived)
dat1$prizm_social_one_derived<-as.factor(dat1$prizm_social_one_derived)
dat1$marital_derived<-as.factor(dat1$marital_derived)


dat1$area_CN_NE_NF_NRM_SF_dummy<-ifelse(dat1$area=="CALIFORNIA NORTH AREA"|dat1$area=="NEW ENGLAND AREA"|dat1$area=="NORTH FLORIDA AREA"|dat1$area=="CALIFORNIA NORTH AREA"|dat1$area=="NORTHWEST/ROCKY MOUNTAIN AREA"|dat1$area=="SOUTH FLORIDA AREA",1,0)
#dat1$area_NYC_dummy<-ifelse(dat1$area=="NEW YORK CITY AREA",1,0)
#dat1$area_northwestrocky_dummy<-ifelse(dat1$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
#dat1$area_southflorida_dummy<-ifelse(dat1$area=="SOUTH FLORIDA AREA",1,0)
#dat1$area_philadelphia_dummy<-ifelse(dat1$area=="PHILADELPHIA AREA",1,0)
#dat1$area_atlanticSouth_dummy<-ifelse(dat1$area=="ATLANTIC SOUTH AREA",1,0)
dat1<-select(dat1,-area)


summary(dat1$ethnic)
dat1$ethnic_B_D_F_I_J_O_G_H_R_dummy<-ifelse(dat1$ethnic=="B"|dat1$ethnic=="D"|dat1$ethnic=="F"|dat1$ethnic=="I"|dat1$ethnic=="J"|dat1$ethnic=="O"|dat1$ethnic=="G"|dat1$ethnic=="H"|dat1$ethnic=="R",1,0)
#dat1$ethnic_C_dummy<-ifelse(dat1$ethnic=="C",1,0)
#dat1$ethnic_N_dummy<-ifelse(dat1$ethnic=="N",1,0)
#dat1$ethnic_Z_dummy<-ifelse(dat1$ethnic=="Z",1,0)
#dat1$ethnic_S_dummy<-ifelse(dat1$ethnic=="S",1,0)
#dat1$ethnic_U_dummy<-ifelse(dat1$ethnic=="U",1,0)
#dat1$ethnic_B_dummy<-ifelse(dat1$ethnic=="B",1,0)
dat1<-select(dat1,-ethnic)

#dat1<-select(dat1,-models)

dat1$crclscod_AorA2orAAorBA_dummy<-ifelse(dat1$crclscod=="A"|dat1$crclscod=="A2"|dat1$crclscod=="AA"|dat1$crclscod=="BA",1,0)
dat1<-select(dat1,-crclscod)
dat1$prizmsocialoneR_dummy<-ifelse(dat1$prizm_social_one=="R"|dat1$prizm_social_one=="T",1,0)
dat1<-select(dat1,-prizm_social_one)

dat1$marital_B_dummy<-ifelse(dat1$marital=="B"|dat1$marital=="U",1,0)
ind2<-which(is.na(dat1$marital_B_dummy))
dat1$marital_B_dummy[ind2]<-0

dat1$marital_A_dummy<-ifelse(dat1$marital=="A",1,0)





dat1$opk_dat_Mean_derived<-as.factor(dat1$opk_dat_Mean_derived)
dat1$blck_dat_Mean_derived<-as.factor(dat1$blck_dat_Mean_derived)
dat1$comp_dat_Mean_derived<-as.factor(dat1$comp_dat_Mean_derived)



set.seed(200)
indexP<-createDataPartition(y=dat1$churn,times=1,p=0.70, list=F)
train_dat1<-dat1[indexP,]
test_dat1<-dat1[-indexP,]
head(indexP)
dim(train_dat1)
dim(test_dat1)

table(train_dat1$churn)/nrow(train_dat1)
table(test_dat1$churn)/nrow(test_dat1)

library(gains)
library(irr)
mod1<-glm(churn~mou_Mean + totmrc_Mean + eqpdays  + 
            iwylis_vce_Mean_derived + rev_Mean + 
            avgmou + avg6mou + asl_flag + refurb_new + drop_vce_Mean + 
            adjmou + totrev_derived + adjrev + totcalls_derived + ovrmou_Mean_derived + 
            avg3mou_derived + hnd_price_derived + uniqsubs_derived + 
            avgqty_derived + avg6qty_derived  + avgrev_derived + 
            comp_vce_perc + ethnic_B_D_F_I_J_O_G_H_R_dummy + crclscod_AorA2orAAorBA_dummy + 
            prizmsocialoneR_dummy + marital_B_dummy + csa_Derived  + 
            rev_Range + mou_Range + datovr_Range, data=train_dat1, family = "binomial")
#step(mod1,direction = "both")


summary(mod1)

df<-as.data.frame(mod1$coefficients)
df$`mod1$coefficients`<-abs(df$`mod1$coefficients`)

dat1<-select(dat1, churn, mou_Mean, totmrc_Mean, eqpdays, iwylis_vce_Mean_derived, rev_Mean, avgmou, avg6mou, asl_flag, refurb_new, drop_vce_Mean, adjmou, totrev_derived, adjrev, totcalls_derived, ovrmou_Mean_derived, avg3mou_derived, hnd_price_derived, uniqsubs_derived, avgqty_derived, avg6qty_derived, avgrev_derived,comp_vce_perc, ethnic_B_D_F_I_J_O_G_H_R_dummy, crclscod_AorA2orAAorBA_dummy, prizmsocialoneR_dummy, marital_B_dummy, csa_Derived, rev_Range, mou_Range, datovr_Range)



pred<-predict(mod1, type="response", newdata=test_dat1)
names(dat1)


table(dat1$churn)/nrow(dat1)
pred1<-ifelse(pred>=0.2392114, 1,0)


table(pred1, test_dat1$churn)

library(ROCR)


pred2<-prediction(pred, test_dat1$churn)

perf<-performance(pred2,"tpr", "fpr")

plot(perf, col="red")
cutoffs<-data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])
head(cutoffs)


auc<-performance(pred2, "auc")
auc<-unlist(slot(auc,"y.values"))
auc

dat3<-data.frame(ifelse(dat1$totmrc_Mean<=37.025, "Low", ifelse(dat1$totmrc_Mean<50&dat1$totmrc_Mean>37.025, "Medium", "High")),as.data.frame(predict(mod1, type="response", newdata=dat1)))
names(dat3)<-c("totmrc_Mean", "churn")
summary(dat3)
index1<-(dat3$churn<=0.21)
index2<-(dat3$churn>0.21&dat3$churn<=0.60)
index3<-(dat3$churn>0.60&dat3$churn<=1)




dat3$churn[index1]="LowP"
dat3$churn[index2]<-"MediumP"
dat3$churn[index3]<-"HighP"

summary(dat3$churn)


table(dat3$totmrc_Mean, dat3$churn)

66297*0.2

