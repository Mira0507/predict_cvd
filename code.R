library(ggplot2)
library(data.table)
library(readxl)
library(tidyverse)

g <- glimpse 
h <- head
s <- summary

# Importing data 
region <- read_excel("CLASS.xls", sheet = 1, skip = 4)[-1, c(3, 4, 6, 7)]
names(region) <- c("Country", 
                   "Code",
                   "Region",
                   "Income_Group")

# Data cleaning 
MakeTable_fn <- function(data, cause) {
        df <- fread(data)[, 1:3] %>%
                mutate(Cause = cause) 
        names(df) <- c("Country_or_Area", 
                       "Year",
                       "Value",
                       "Cause")
        return(df)
}

all<- MakeTable_fn("UNdata_Export_all_causes.csv", 
                   "All")
cancer <- MakeTable_fn("UNdata_Export_cancer.csv", 
                       "Cancer")
resp <- MakeTable_fn("UNdata_Export_chronic_respiratory.csv", 
                     "Respiratory_Disease")
comm <- MakeTable_fn("UNdata_Export_communicable.csv",
                     "Communicable")
cvd <- MakeTable_fn("UNdata_Export_cvd.csv",
                    "Cardiovascular_Disease")
injur <- MakeTable_fn("UNdata_Export_injuries.csv",
                      "Injuries")
noncomm <- MakeTable_fn("UNdata_Export_noncommunicable.csv",
                        "Noncommucable")

de <- rbind(all, cancer, resp, comm, cvd, injur, noncomm) %>%
        spread(Cause, Value)

de_temp <- de[!(de$Country_or_Area %in% region$Country), ]


new_name <- c("Bahamas, The",
              "Bolivia",
              "Congo, Rep.",
              "orea, Dem. People's Rep.",
              "Congo, Dem. Rep.",
              "Gambia, The",
              "Iran, Islamic Rep.",
              "Kyrgyz Republic",
              "Lao PDR",
              "Micronesia, Fed. Sts.",
              "Korea, Rep.",
              "Moldova",
              "St. Kitts and Nevis",
              "St. Lucia",
              "St. Vincent and the Grenadines",
              "S?o Tom? and Principe",
              "Slovak Republic",
              "Eswatini",
              "North Macedonia",
              "Tanzania",
              "United States",
              "Venezuela, RB",
              "Vietnam",
              "Yemen, Rep",
              "Egypt")


names(new_name) <- c("Bahamas",
                     "Bolivia (Plurinational State of)",
                     "Congo",
                     "Democratic Peoples Republic of Korea",
                     "Democratic Republic of the Congo",
                     "Gambia",
                     "Iran (Islamic Republic of)",
                     "Kyrgyzstan",
                     "Lao People's Democratic Republic",
                     "Micronesia (Federated States of)",
                     "Republic of Korea",
                     "Republic of Moldova",
                     "Saint Kitts and Nevis",
                     "Saint Lucia",
                     "Saint Vincent and the Grenadines",
                     "Sao Tome and Principe",
                     "Slovakia",
                     "Swaziland",
                     "The former Yugoslav Republic of Macedonia",
                     "United Republic of Tanzania",
                     "United States of America",
                     "Venezuela (Bolivarian Republic of)",
                     "Viet Nam",
                     "Yemen",
                     "Egypt, Arab Rep.")

de <- de %>% 
        mutate(Country_or_Area = ifelse(Country_or_Area %in% region$Country, 
                                        Country_or_Area,
                                        new_name[Country_or_Area]))

de1 <- de %>% 
        inner_join(region, 
                   by = c("Country_or_Area" = "Country")) %>%
        na.omit()






# Outlier filtering
de2 <- de1 %>% 
        filter(Cardiovascular_Disease < 750)



# Comparing mean vs variance: var >>>>> mean. Do quasipoisson regression
mn <- mean(de2$Cardiovascular_Disease)
vr <- var(de2$Cardiovascular_Disease)


# formula 
fm <- as.formula(Cardiovascular_Disease ~ 
                         Cancer + 
                         Communicable + 
                         Injuries + 
                         Noncommucable + 
                         Respiratory_Disease)



library(vtreat) 
library(ranger)
de2 <- de2 %>%
        mutate(predQ = 0,
               predR = 0)

# quasi-poisson regression
set.seed(1326)
splitPlan <- kWayCrossValidation(nrow(de2), 4, NULL, NULL)

for (i in 1:4) {
        split <- splitPlan[[i]]
        modelQ <- glm(fm, data = de2[split$train, ], family = quasipoisson)
        de2$predQ[split$app] <- predict(modelQ, 
                                        newdata = de2[split$app, ],
                                        type = "response")
}


# random forest (ranger)

set.seed(1326)
splitPlan <- kWayCrossValidation(nrow(de2), 4, NULL, NULL)

for (i in 1:4) {
        split <- splitPlan[[i]]
        modelR <- ranger(fm, 
                         data = de2[split$train, ], num.trees = 500, 
                         respect.unordered.factors = "order",
                         seed = 251)
        de2$predR[split$app] <- predict(modelR, de2[split$app, ])$predictions
}




de2 <- de2 %>%
        mutate(residQ = predQ - Cardiovascular_Disease,
               residR = predR - Cardiovascular_Disease)

# RMSE 
RMSE <- de2 %>% 
        summarize(RMSE_QuassiPoisson = sqrt(mean(residQ^2)),
                  RMSE_RandomForests = sqrt(mean(residR^2)),
                  SD = sd(Cardiovascular_Disease)) %>%
        gather(Category, Value)

# data cleaning for plotting 
de3 <- de2 %>%
        gather(Prediction_Model, 
               Prediction_Value, 
               c(predQ, predR)) %>%
        mutate(Prediction_Model = ifelse(Prediction_Model == "predQ", 
                                         "Quasi-Poisson Regression",
                                         "Random Forests"))  


#################################### Plotting ####################################


# Outlier clarification
check_outlier_plot <- 
        ggplot(de1, 
               aes(y = Cardiovascular_Disease)) + 
        geom_boxplot(aes(fill = "#669900")) + 
        geom_hline(yintercept = 750, color = "red", size = 1) + 
        theme_bw() + 
        theme(axis.text.x = element_blank(),
              legend.position = "none") +
        ggtitle("Deaths from Cardiovascular Disease per Country") +
        ylab("Number")


# Check distribution of outcome
check_distribution_plot <-
        ggplot(de2, 
               aes(x = Cardiovascular_Disease,
                   fill = "#669900")) +
        geom_density() + 
        theme_bw() + 
        ggtitle("Distribution of Deaths from Cardiovascular Disease per Country") +
        ylab("Density") +
        xlab("Deaths") + 
        theme(legend.position = "none")

# outcome vs prediction
outcome_vs_prediction <- 
        ggplot(de3, aes(x = Prediction_Value, 
                        y = Cardiovascular_Disease,
                        color = Prediction_Model)) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() + 
        ggtitle("Deaths from Cardiovascular Disease (Actual Outcome vs Prediction)") +
        xlab("Prediction") + 
        ylab("Actual Outcome")

# RMSE 
RMSE_plot <- 
        ggplot(RMSE, aes(x = Category,
                         y = Value,
                         fill = Category)) + 
        geom_bar(stat = "identity", width = 0.8) +
        theme_bw() + 
        theme(axis.text.x = element_blank()) + 
        ggtitle("Comparison of RMSE between Quasi-Poisson Regression and Random Forests")

# Residuals 
resid_fn <- function(df, xcol, ycol, c, tit) {
        ggplot(df, 
               aes(x = xcol,
                   y = ycol)) + 
                geom_point(alpha = 0.5, color = c) + 
                geom_smooth(method = "lm", se = F) + 
                theme_bw() + 
                xlab("Prediction") + 
                ylab("Residual") + 
                ggtitle(tit)
}
residual_plotQ <- resid_fn(de2, 
                           de2$predQ,
                           de2$residQ,
                           "#FF9999",
                           "Residuals in Quasi-Poisson Regression")

residual_plotR <- resid_fn(de2, 
                           de2$predR,
                           de2$residR,
                           "#009933",
                           "Residuals in Random Forests")

library(gridExtra)
grid.arrange(residual_plotQ,
             residual_plotR, nrow = 1)


# Gain Curves
library(WVPlots)
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "Cardiovascular_Disease", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Deaths from Cardiovascular Disease")
}

GainCurveQ <- gain_curve(de2, "predQ", "Quasi-Poisson Regression")
GainCurveR <- gain_curve(de2, "predR", "Random Forests")

grid.arrange(GainCurveQ, 
             GainCurveR,
             ncol = 1)