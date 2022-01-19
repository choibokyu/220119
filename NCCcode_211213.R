'''
C18 ~ C20 condition_concept_id in NCC: 74582, 436635, 4247719, 438699, 432257, 441800, 197500, 432837, 438979, 433143
Chemotherapy drug concept id in NCC: 955632, 43259208, 1318011, 42959014, 1367268, 35202021, 40244266, 42921872, 1397141, 1338512, 1315411, 1378382, 43269829, 1344905, 1388798, 1308290, 42936747
antithromboticstics: 1301025, 40228152, 1350310, 1112807, 40241331, 1301065, 19042778, 40241186, 1322184, 1315865, 45892847, 42935771, 43013024, 1310149, 42960517, 42962067
Antihypertensive: 1332418, 19078080, 42950421, 1373225, 1347384, 19023453, 1308842, 40167849, 1307046, 42950393, 40167852, 19102171, 42960031, 1334456, 1363053, 1318853, 956874, 1346823, 1367500, 40235487, 19078101, 40184217, 978555, 19127434, 40224172, 1317640, 1332527, 1332497, 1318137, 19127432, 19102491, 42950354, 21130969, 19023454, 19096740, 970250, 974166, 1314002, 1328165, 1386957, 1319133, 1346686, 21091811, 42950567, 974474, 1314577, 19127433, 1353766, 40184187, 40235491, 19096752, 42960035, 974473, 1307863, 19122209, 1340128, 21141332, 1341927, 1332525
DM:1516766, 40164922, 19059796, 42953740, 42942982, 1503297, 45775620, 42968754, 36887702, 42961500, 1525215, 1597756, 42961331, 42942973, 40166035, 42708168, 1502826, 40231403, 1529331, 44785829, 43013884, 40239216, 40164892, 21081251, 45774751, 45775456, 42960653, 40231394, 1580747, 42942980, 42942984
statin:19022956, 1539403, 1545958, 42969306, 1551860, 42969173, 1592085, 1551803, 40165636, 43527029, 1526479, 1510813, 42969232, 19122209, 43527032, 1332497
'''

'''
1.PERSON 테이블 -> pperson
2-1.VISIT_OCCURRENCE 테이블 -> visit_occ
3.DEATH 테이블 -> death
4.CONDITION_OCCURRENCE -> cond_occ
6.DRUG 테이블 -> drug
'''

library(skimr)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

## read table
pperson <- read.csv(file = "PERSON.csv")
visit_occ <- read.csv(file = "VISIT_OCCURRENCE.csv")
death <- read.csv(file = "DEATH.csv")
cond_occ <- read.csv(file = "CONDITION_OCCURRENCE.csv")
drug <- read.csv(file = "DRUG.csv")

## C18 ~ C20에 해당하는 환자 추출
colon_list <- c('74582', '436635', '4247719', '438699', '432257', '441800', '197500', '432837', '438979', '433143')
colon_cond <- subset(cond_occ, cond_occ$condition_concept_id %in% colon_list)
colon_cond <- arrange(colon_cond, condition_start_date)
colon_cond <- colon_cond[-which(duplicated(colon_cond$person_id)),]

## colon cancer 환자의 성별,나이,사망 기록 join
visit_occ <- arrange(visit_occ, -visit_end_date)
visit_occ <- visit_occ[-which(duplicated(visit_occ$person_id)),]
visit_occ <- subset(visit_occ, select = c(person_id, visit_end_date))

colon_cond <- left_join(colon_cond, pperson, by = 'person_id')
colon_cond <- left_join(colon_cond, visit_occ, by = 'person_id')
colon_cond <- left_join(colon_cond, death, by = 'person_id')
colon_cond$outcomeCount <- ifelse(is.na(colon_cond$death_time) == TRUE, 0 , 1)

colon_cond$age <- as.integer(substr(colon_cond$condition_start_date, 1, 4)) - colon_cond$year_of_birth
colon_cond$deathtime <- as.integer(colon_cond$death_date - colon_cond$condition_start_date)
colon_cond$followlosstime <- as.integer(colon_cond$visit_end_date - colon_cond$condition_start_date)
colon_cond$surtime <- ifelse(colon_cond$outcomeCount == 1, colon_cond$deathtime, colon_cond$followlosstime)
colon_cond$survivalTime <- ifelse(colon_cond$survtime>1825, 1825, colon_cond$survtime)


## 각 약제 추출
drug$end_date <- ifelse(is.na(drug$drug_exposure_end_date) == FALSE, drug$drug_exposure_end_date, ifelse(is.na(drug$days_supply) == FALSE, drug$drug_exposure_start_date+drug$days_supply, drug$drug_exposure_start_date+1))
drug$end_date <- as.Date(drug$end_date, origin="1970-01-01")

CTx_list <- c('955632',	'43259208',	'1318011',	'42959014',	'1367268',	'35202021',	'40244266',	'42921872',	'1397141',	'1338512',	'1315411',	'1378382',	'43269829',	'1344905',	'1388798',	'1308290',	'42936747')
BPmed_list <- c('1332418',	'19078080',	'42950421',	'1373225',	'1347384',	'19023453',	'1308842',	'40167849',	'1307046',	'42950393',	'40167852',	'19102171',	'42960031',	'1334456',	'1363053',	'1318853',	'956874',	'1346823',	'1367500',	'40235487',	'19078101',	'40184217',	'978555',	'19127434',	'40224172',	'1317640',	'1332527',	'1332497',	'1318137',	'19127432',	'19102491',	'42950354',	'21130969',	'19023454',	'19096740',	'970250',	'974166',	'1314002',	'1328165',	'1386957',	'1319133',	'1346686',	'21091811',	'42950567',	'974474',	'1314577',	'19127433',	'1353766',	'40184187',	'40235491',	'19096752',	'42960035',	'974473',	'1307863',	'19122209',	'1340128',	'21141332',	'1341927',	'1332525')
DMmed_list <- c('1516766',	'40164922',	'19059796',	'42953740',	'42942982',	'1503297',	'45775620',	'42968754',	'36887702',	'42961500',	'1525215',	'1597756',	'42961331',	'42942973',	'40166035',	'42708168',	'1502826',	'40231403',	'1529331',	'44785829',	'43013884',	'40239216',	'40164892',	'21081251',	'45774751',	'45775456',	'42960653',	'40231394',	'1580747',	'42942980',	'42942984')
antithromboticstics_list <- c('1301025',	'40228152',	'1350310',	'1112807',	'40241331',	'1301065',	'19042778',	'40241186',	'1322184',	'1315865',	'45892847',	'42935771',	'43013024',	'1310149',	'42960517',	'42962067')
statin_list <- c('19022956',	'1539403',	'1545958',	'42969306',	'1551860',	'42969173',	'1592085',	'1551803',	'40165636',	'43527029',	'1526479',	'1510813',	'42969232',	'19122209',	'43527032',	'1332497')


CTx <- subset(drug, drug$drug_concept_id %in% CTx_list)
CTx <- arrange(CTx, drug_exposure_start_date)
CTx <- CTx[-which(duplicated(CTx$person_id)),]
HTN <- subset(drug, drug$drug_concept_id %in% BPmed_list)
HTN <- arrange(HTN, drug_exposure_start_date)
HTN <- HTN[-which(duplicated(HTN$person_id)),]
DM <- subset(drug, drug$drug_concept_id %in% DMmed_list)
DM <- arrange(DM, drug_exposure_start_date)
DM <- DM[-which(duplicated(DM$person_id)),]
statin <- subset(drug, drug$drug_concept_id %in% statin_list)
statin <- arrange(statin, drug_exposure_start_date)
statin <- statin[-which(duplicated(statin$person_id)),]
antithrombotics <- subset(drug, drug$drug_concept_id %in% antithromboticstics_list)
antithrombotics <- arrange(antithrombotics, drug_exposure_start_date)
antithrombotics <- antithrombotics[-which(duplicated(antithrombotics$person_id)),]
HTN$HTN <- 1
DM$DM <- 1
statin$statin <- 1
antithrombotics$antithrombotics <- 1
HTN <- subset(HTN, select = c(person_id, HTN))
DM <- subset(DM, select = c(person_id, DM))
statin <- subset(statin, select = c(person_id, statin))
antithrombotics <- subset(antithrombotics, select = c(person_id, antithrombotics))

## condition table과 drug table 결합
colon_CTx <- left_join(colon_cond, CTx, by = 'person_id')
colon_CTx <- subset(colon_CTx, is.na(colon_CTx$drug_concept_id) == FALSE)

colon_CTx <- left_join(colon_CTx, HTN, by = 'person_id')
colon_CTx <- left_join(colon_CTx, DM, by = 'person_id')
colon_CTx <- left_join(colon_CTx, statin, by = 'person_id')
colon_CTx <- left_join(colon_CTx, antithrombotics, by = 'person _id')

finaldata <- subset(colon_CTx, select = c(person_id, HTN, DM, statin, gender_concept_id, age, antithrombotics, survivalTime, outcomeCount))

finaldata[is.na(finaldata)] <- 0

finaldata$HTN <- as.factor(finaldata$HTN)
finaldata$DM <- as.factor(finaldata$DM)
finaldata$statin <- as.factor(finaldata$statin)
finaldata$antithrombotics <- as.factor(finaldata$antithrombotics)
finaldata$gender_concept_id <- as.factor(finaldata$gender_concept_id)
finaldata$outcomeCount <- as.factor(finaldata$outcomeCount)

write.csv(finaldata, file = "finaldata.csv", row.names = FALSE)

sink("demographic1.txt")
print(skim(finaldata))
sink()

sink("demographic2.txt")
print(summary(finaldata))
sink()

res.cox <- coxph(Surv(survivalTime, outcomeCount) ~ HTN + DM + statin + antithrombotics + age + gender_concept_id, data = finaldata)
sink("cox.txt")
print(summary(res.cox))
sink()

surv_object <- Surv(time = finaldata$survivalTime, event = finaldata$outcomeCount)

fit_HTN <- survfit(surv_object ~ HTN, data=finaldata)
survplot_HTN <- ggsurvplot(fit_HTN, data = finaldata, pval=TRUE)
png("survplot_HTN.png")
print(survplot_HTN, newpage = FALSE)
dev.off()

fit_DM <- survfit(surv_object ~ DM, data=finaldata)
survplot_DM <- ggsurvplot(fit_DM, data = finaldata, pval=TRUE)
png("survplot_DM.png")
print(survplot_DM, newpage = FALSE)
dev.off()

fit_statin <- survfit(surv_object ~ statin, data=finaldata)
survplot_statin <- ggsurvplot(fit_statin, data = finaldata, pval=TRUE)
png("survplot_statin.png")
print(survplot_statin, newpage = FALSE)
dev.off()

fit_antithrombotics <- survfit(surv_object ~ antithrombotics, data=finaldata)
survplot_antithrombotics <- ggsurvplot(fit_antithrombotics, data = finaldata, pval=TRUE)
png("survplot_antithrombotics.png")
print(survplot_antithrombotics, newpage = FALSE)
dev.off()

fit_gender_concept_id <- survfit(surv_object ~ gender_concept_id, data=finaldata)
survplot_gender_concept_id <- ggsurvplot(fit_gender_concept_id, data = finaldata, pval=TRUE)
png("survplot_gender_concept_id.png")
print(survplot_gender_concept_id, newpage = FALSE)
dev.off()