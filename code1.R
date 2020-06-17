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
                      "Injury")
noncomm <- MakeTable_fn("UNdata_Export_noncommunicable.csv",
                        "Noncommunicable")

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



de1_gathered <- gather(de1, Cause, Number, c(All, 
                                             Cancer, 
                                             Cardiovascular_Disease,
                                             Communicable,
                                             Injury,
                                             Noncommunicable,
                                             Respiratory_Disease))
de1_subset <- subset(de1_gathered, Cause %in% c("Cardiovascular_Disease",
                                                "Cancer",
                                                "Communicable",
                                                "Injury",
                                                "Noncommunicable",
                                                "Respiratory_Disease"))      

# Outlier filtering
de2 <- de1 %>% 
        filter(Cancer < 250,
               Cardiovascular_Disease < 750,
               Communicable < 1000,
               Injury < 250,
               Noncommunicable < 1250,
               Respiratory_Disease < 150)

de2_gathered <- gather(de2, Cause, Number, c(All, 
                                             Cancer, 
                                             Cardiovascular_Disease,
                                             Communicable,
                                             Injury,
                                             Noncommunicable,
                                             Respiratory_Disease)) 

de2_subset <- subset(de2_gathered, Cause %in% c("Cardiovascular_Disease",
                                                "Cancer",
                                                "Communicable",
                                                "Injury",
                                                "Noncommunicable",
                                                "Respiratory_Disease"))      

de4 <- de2

# formula 

fm1 <- as.formula(Cardiovascular_Disease ~ 
                         Cancer + 
                         Communicable + 
                         Injury + 
                         Noncommunicable + 
                         Respiratory_Disease)


fm2 <- as.formula(Cardiovascular_Disease ~ 
                         Cancer + 
                         Communicable + 
                         Injury + 
                         Noncommunicable + 
                         Respiratory_Disease + 
                         Region + 
                         Income_Group)


library(vtreat) 
library(ranger)
de4 <- de4 %>%
        mutate(pred1 = 0,
               pred2 = 0)

# quasi-poisson regression
set.seed(1326)
splitPlan <- kWayCrossValidation(nrow(de4), 4, NULL, NULL)

for (i in 1:4) {
        split <- splitPlan[[i]]
        model1 <- ranger(fm1, 
                         data = de4[split$train, ], num.trees = 500, 
                         respect.unordered.factors = "order",
                         seed = 251)
        model2 <- ranger(fm2, 
                         data = de4[split$train, ], num.trees = 500, 
                         respect.unordered.factors = "order",
                         seed = 251)
        de4$pred1[split$app] <- predict(model1, de4[split$app, ])$predictions
        de4$pred2[split$app] <- predict(model2, de4[split$app, ])$predictions
}


de4 <- de4 %>%
        mutate(resid1 = pred1 - Cardiovascular_Disease,
               resid2 = pred2 - Cardiovascular_Disease)

# RMSE 
RMSE_R <- de4 %>% 
        summarize(RMSE1 = sqrt(mean(resid1^2)),
                  RMSE2 = sqrt(mean(resid2^2)),
                  SD = sd(Cardiovascular_Disease)) %>%
        gather(Category, Value)

# Correlation 
Corr_R <- de4 %>% 
        summarize(cor1 = cor(Cardiovascular_Disease, pred1),
                  cor2 = cor(Cardiovascular_Disease, pred2))

# data cleaning for plotting 
de5 <- de4 %>%
        gather(Prediction_Model, 
               Prediction_Value, 
               c(pred1, pred2)) %>%
        mutate(Prediction_Model = ifelse(Prediction_Model == "pred1", 
                                         "With Region and Income",
                                         "Without Region and Income"))  


#################################### Plotting ####################################


# outcome vs prediction
outcome_vs_prediction_R <- 
        ggplot(de5, aes(x = Prediction_Value, 
                        y = Cardiovascular_Disease,
                        color = Prediction_Model)) + 
        geom_point(alpha = 0.3) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() + 
        ggtitle("Deaths from Cardiovascular Disease (Actual Outcome vs Prediction)") +
        xlab("Prediction") + 
        ylab("Actual Outcome")

# RMSE 
RMSE_R_plot <- 
        ggplot(RMSE_R, aes(x = Category,
                         y = Value,
                         fill = Category)) + 
        geom_bar(stat = "identity", width = 0.8) +
        theme_bw() + 
        theme(axis.text.x = element_blank()) + 
        ggtitle("RMSE")

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
                ggtitle(tit) + 
                ylim(NA, 200)
}
residual_plot_R1 <- resid_fn(de4, 
                           de4$pred1,
                           de4$resid1,
                           "#FF9999",
                           "Residuals in Model 1")

residual_plot_R2 <- resid_fn(de4, 
                           de4$pred2,
                           de4$resid2,
                           "#009933",
                           "Residuals in Model 2") 


grid.arrange(residual_plot_R1,
             residual_plot_R2, nrow = 1)


# Gain Curves
library(WVPlots)
gain_curve <- function(df, model, tit) {
        GainCurvePlot(df, model, "Cardiovascular_Disease", tit) + 
                theme_bw() +
                xlab("Fraction Items in Sort Order") + 
                ylab("Fraction Total Sum Deaths from Cardiovascular Disease")
}

GainCurve_R1 <- gain_curve(de4, "pred1", "Model 1")
GainCurve_R2 <- gain_curve(de4, "pred2", "Model 2")

grid.arrange(GainCurve_R1, 
             GainCurve_R2,
             ncol = 1)