
library(data.table)
library(caret)

logreg_function <- function(df, creat_filter){

    logreg_columns <- c("hospitaldischargestatus",
                        "uo_wt_hr",
                        "CRcat",
                        "age",
                        "gender",
                        "albumin",
                        "bilirubin",
                        "glucose",
                        "hematocrit",
                        "heartrate",
                        "meanbp",
                        "sodium",
                        "fio2",
                        "ph",
                        "respiratoryrate",
                        "temperature",
                        "bun",
                        "wbc",
                        "eyes",
                        "motor",
                        "verbal")

    logreg_data <- df[, ..logreg_columns]
    logreg_data <- logreg_data[CRcat == creat_filter]

    nrow(logreg_data)

    logreg_data = logreg_data[, c("CRcat"):=NULL]

    logmod_dfs <-train(hospitaldischargestatus ~ .,
                  data=logreg_data,
                  method="glm",
                  family="binomial",
                  na.action=na.omit)

    return(logmod_dfs)
}


logmod_dfs <- logreg_function(df, "Low Creatinine")
summary(logmod_dfs)

logmod_dfs <- logreg_function(df, "High Creatinine")
summary(logmod_dfs)


# "immundis",
# "immunrx",
# "aids",
# "hepfail",
# "lymphoma",
# "metast",
# "leukaem",
# "immunsup",
# "cirrhos",
# "iddm", #
# "agescore",