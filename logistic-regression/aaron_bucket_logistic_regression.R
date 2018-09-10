
library(data.table)
library(caret)

anzics_data_directory <- './'

socio_economic_data <- 'SocioEconomicData_forDatathon.csv.gz'
ccr_data <- 'Sydney_datathon_CCR.csv.gz'
anzics_data <- 'Sydney_datathon_APD2006to2016.csv.gz'

path <- paste(anzics_data_directory,anzics_data, sep="")



#Data Cleaning
ANZICS=read.csv(path)
ANZICS24=dplyr::filter(ANZICS, icu_hrs>=24)

ANZICS24FilterNA=subset(ANZICS24,
                        select=c(
                          died_hosp,
                          sex,
                          elect,
                          readmitted,
                          chr_resp,
                          lymphoma,
                          metast,
                          leukaem,
                          immunsup,
                          cirrhos,
                          weight,
                          height,
                          aids,
                          hepfail,
                          chr_cvs,
                          chr_ren,
                          immundis,
                          immunrx,
                          chr_liv,
                          age,
                          arf,
                          ventilated,
                          intubated,
                          temp_ap2,
                          map_ap2,
                          hr_ap2,
                          rr_ap2,
                          na_ap2,
                          k_ap2,
                          creat_ap2,
                          wcc_ap2,
                          gcs,
                          cardarrest,
                          urineop))


ANZICS24UO = dplyr::filter(ANZICS24FilterNA, !is.na(ANZICS24FilterNA$urineop))
ANZICS24UO = dplyr::filter(ANZICS24UO, ANZICS24UO$chr_ren==0)
ANZICS24UO$sex=ifelse(ANZICS24UO$sex == 'M', 0, 1)
ANZICS24UO$sex=factor(ANZICS24UO$sex)
#Not sure about GCS... it is ordinal but also not. Below code changes the GCS to categorical
#and sets 3 as dummy variable = 0
ANZICS24UO$gcs=ifelse(ANZICS24UO$gcs == '3', 0, ANZICS24UO$gcs)
ANZICS24UO$gcs=factor(ANZICS24UO$gcs)
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
ANZICS24UO$cardarrest=factor(ANZICS24UO$cardarrest)
ANZICS24UOwt1=dplyr::filter(ANZICS24UO, (ANZICS24UO$weight>1))

#Alter variables
ANZICS24UOwt1$uo_wt_hr=(ANZICS24UOwt1$urineop/ANZICS24UOwt1$weight)/24
# ANZICS24UOwt1=subset(ANZICS24UOwt1, select=-c(urineop, arf, chr_ren, height))
ANZICS24UOwt1$uo_wt_hr <- cut(ANZICS24UOwt1$uo_wt_hr, breaks=c(-Inf, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, Inf))
ANZICS24UOwt1$creat10=ifelse(ANZICS24UOwt1$creat_ap2<= 108, 0, 1)
# ANZICS24UOwt1=subset(ANZICS24UOwt1, select=-c(creat_ap2))
ANZICS24UOwt1$creat10=as.factor(ANZICS24UOwt1$creat10)


ANZICS24UOwt1$creat_ap2_cut <- cut(ANZICS24UOwt1$creat_ap2, breaks=c(-Inf, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280, 300, 320, 340, 360, 380, 400, 420, 440, 460, 480, 500, Inf))


#Logistic regression
summary(glm(
  died_hosp~sex+
    elect+
    readmitted+
    chr_resp+
    lymphoma+
    metast+
    leukaem+
    immunsup+
    cirrhos+
    weight+
    aids+
    hepfail+
    chr_cvs+
    immundis+
    immunrx+
    chr_liv+
    age+
    ventilated+
    temp_ap2+
    map_ap2+
    hr_ap2+
    rr_ap2+
    na_ap2+
    k_ap2+
    creat10+
    wcc_ap2+
    gcs+
    cardarrest+
    uo_wt_hr+
    uo_wt_hr:creat10, data=ANZICS24UOwt1, na.action = na.omit, family=binomial))


summary(glm(formula=died_hosp~sex+
      elect+
      readmitted+
      chr_resp+
      lymphoma+
      metast+
      leukaem+
      immunsup+
      cirrhos+
      weight+
      aids+
      hepfail+
      chr_cvs+
      immundis+
      immunrx+
      chr_liv+
      age+
      intubated+
      temp_ap2+
      map_ap2+
      hr_ap2+
      rr_ap2+
      na_ap2+
      k_ap2+
      creat_ap2+
      wcc_ap2+
      gcs+
      cardarrest+
      uo_wt_hr,
    family=binomial,data=ANZICS24UOwt1,na.action=na.omit))



glm(formula=died_hosp~sex+
      elect+
      readmitted+
      chr_resp+
      lymphoma+
      metast+
      leukaem+
      immunsup+
      cirrhos+
      weight+
      aids+
      hepfail+
      chr_cvs+
      immundis+
      immunrx+
      chr_liv+
      age+
      intubated+
      temp_ap2+
      map_ap2+
      hr_ap2+
      rr_ap2+
      na_ap2+
      k_ap2+
      creat_ap2+
      wcc_ap2+
      gcs+
      cardarrest+
      uo_wt_hr+
      uo_wt_hr:creat_ap2_cut,
    family=binomial,data=ANZICS24UOwt1,
    na.action=na.omit)




