#Data Cleaning. ANZICS database 2006 to 2016 obtained from Dave Pilcher @ datathon
#Inclusion criteria:
# ICU admission >24 h, no null values (died_hosp, weight, urineoutput, creatinine), adults
#readmission allowed

ANZICS=read.csv("/media/alanw/smalldrive/aw-medical-datasets/anzics-datathon-2018/anzics2/Sydney_datathon_APD2006to2016.csv")
ANZICS24=dplyr::filter(ANZICS, icu_hrs>=24)

#Remove variables missing >10% data
MissingVar=ANZICS24 %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))/length(.)*100))
sort(MissingVar)

#New data set without the missing values
ANZICS24FilterNA=subset(ANZICS24, select=c(died_hosp,sex, elect, readmitted, chr_resp, lymphoma, metast, leukaem, immunsup, cirrhos, weight, height, aids, hepfail, chr_cvs, chr_ren, immundis, immunrx, chr_liv, age, arf, ventilated, intubated, temp_ap2, map_ap2, hr_ap2, rr_ap2, na_ap2, k_ap2, creat_ap2, wcc_ap2, gcs, cardarrest, urineop))
NROW(ANZICS24FilterNA) #have 941124 records in total with variables that have <10% missing data
#Enusring have urineop as this is a key variable
ANZICS24UO = dplyr::filter(ANZICS24FilterNA, !is.na(ANZICS24FilterNA$urineop))
NROW(ANZICS24UO) #860600 records

#Changing variable types to suit regressions/Tree
ANZICS24UO$sex=ifelse(ANZICS24UO$sex == 'M', 0, 1)
ANZICS24UO$sex=factor(ANZICS24UO$sex)
#Not sure about GCS... it is ordinal but also not. Below code changes the GCS to categorical
#and sets 3 as dummy variable = 0
ANZICS24UO$gcs=factor(ANZICS24UO$gcs)
ANZICS24UO$gcs=ifelse(ANZICS24UO$gcs == '3', 0, ANZICS24UO$gcs)
#Changing variable types to suit regressions/Tree
ANZICS24UO$elect=factor(ANZICS24UO$elect)
ANZICS24UO$died_hosp=factor(ANZICS24UO$died_hosp)
ANZICS24UO$readmitted=factor(ANZICS24UO$readmitted)
ANZICS24UO$chr_resp=factor(ANZICS24UO$chr_resp)
ANZICS24UO$lymphoma=factor(ANZICS24UO$lymphoma)
ANZICS24UO$metast=factor(ANZICS24UO$metast)
ANZICS24UO$leukaem=factor(ANZICS24UO$leukaem)
ANZICS24UO$immunsup=factor(ANZICS24UO$immunsup)
ANZICS24UO$cirrhos=factor(ANZICS24UO$cirrhos)
ANZICS24UO$aids=factor(ANZICS24UO$aids)
ANZICS24UO$hepfail=factor(ANZICS24UO$hepfail)
ANZICS24UO$chr_cvs=factor(ANZICS24UO$chr_cvs)
ANZICS24UO$chr_ren=factor(ANZICS24UO$chr_ren)
ANZICS24UO$immundis=factor(ANZICS24UO$immundis)
ANZICS24UO$immunrx=factor(ANZICS24UO$immunrx)
ANZICS24UO$chr_liv=factor(ANZICS24UO$chr_liv)
ANZICS24UO$arf=factor(ANZICS24UO$arf)
ANZICS24UO$ventilated=factor(ANZICS24UO$ventilated)
ANZICS24UO$intubated=factor(ANZICS24UO$intubated)

#Subset of patients with UO/wt/hr 
ANZICS24UOwt1=dplyr::filter(ANZICS24UO, (ANZICS24UO$weight>1))
NROW(ANZICS24UOwt1) #213355 records
ANZICS24UOwt1$uo_wt_hr=(ANZICS24UOwt1$urineop/ANZICS24UOwt1$weight)/24
ANZICS24UOwt1=subset(ANZICS24UOwt1, select=-c(urineop))

#Train and Test data sets
set.seed(1)
sample=sample.split(ANZICS24UOwt1$died_hosp, SplitRatio=0.8)
ANZICS24UOwt1.train=subset(ANZICS24UOwt1, sample==TRUE)
ANZICS24UOwt1.test=subset(ANZICS24UOwt1, sample==FALSE)

sample=sample.split(ANZICS24UO$died_hosp, SplitRatio=0.7)
ANZICS24UO.train=subset(ANZICS24UO, sample==TRUE)
ANZICS24UO.test=subset(ANZICS24UO, sample==FALSE)