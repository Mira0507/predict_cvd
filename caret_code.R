library(ggplot2)
library(data.table)
library(readxl)
library(tidyverse)

g <- glimpse 
h <- head
s <- summary


#################################### Importing and Data Cleaning ####################################


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

#################################### Raw Data Inspection ####################################

rd_density <- subset(rbind(all, cancer, resp, comm, cvd, injur, noncomm),
                     Cause != "All") %>%
  ggplot(aes(x = Value, fill = Cause, color = Cause)) + 
  geom_density(alpha = 0.3) +
  theme_bw() + 
  xlab("Number of Deaths") +
  ylab("Density") +
  ggtitle("Distribution of Death Number from Various Causes")


rd_scatter <- gather(de, Cause, Number, c(Cancer, 
                                          Communicable, 
                                          Injury, 
                                          Noncommunicable, 
                                          Respiratory_Disease)) %>%
  ggplot(aes(x = Number, y = Cardiovascular_Disease, color = Cause)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(se = F) + 
  theme_bw() + 
  xlab("Number of Deaths (from Other Causes)") +
  ylab("Number of Deaths (from Cardiovascular Disease)") +
  ggtitle("Number of Deaths from Various Causes")



library(gridExtra)

grid.arrange(rd_density, rd_scatter, ncol = 1)



#################################### Fitting Models ####################################



fm <- as.formula(Cardiovascular_Disease ~ 
                          Cancer + 
                          Communicable + 
                          Injury + 
                          Noncommunicable + 
                          Respiratory_Disease)

mean(de$Cardiovascular_Disease)
var(de$Cardiovascular_Disease)

library(caret)

set.seed(23)
myFold <- createFolds(de$Cardiovascular_Disease, k = 10)

# Inspecting folds 
s(de$Cardiovascular_Disease)
s(de$Cardiovascular_Disease[myFold$Fold01])
s(de$Cardiovascular_Disease[myFold$Fold02])

myControl <- trainControl(method = "cv",
                          number = 10, 
                          savePrediction = TRUE,
                          verboseIter = TRUE,
                          index = myFold)

gm <- train(fm, 
            de, 
            method = "glm", 
            family = "quasipoisson", 
            trControl = myControl)

myGrid = expand.grid(mtry = 3:5,
                     splitrule = c("variance",
                                   "extratrees"),
                     min.node.size = 1:7)

set.seed(916)
rf <- train(fm, de, method = "ranger", 
            tuneLength = 10, 
            tuneGrid = myGrid, 
            trControl = myControl)



####################################### Comparing Models #########################################


models <- list(glm = gm, 
               RandomForest = rf)

# rsm: resamples object
rsm <- resamples(models)

# rsm_tb: evaluation table
rsm_tb <- rsm$values


# data cleaning for plotting
new_name <- colnames(rsm_tb) %>%
        str_replace("~", "_") %>%
        str_replace("Resample", "Fold")

names(rsm_tb) <- new_name

mtr <- c("RMSE", "MAE", "Rsquared")
rsm_tb1 <- rsm_tb %>%
        gather(key = "Model", value = "Value", -Fold) %>%
        separate(Model, c("Model", "Metric"), sep = "_") %>%
        mutate(Model = factor(Model,
                              levels = c("glm", "RandomForest")),
               Metric = factor(Metric, 
                               levels = mtr)) 

rsm_tb2 <- ggplot(rsm_tb1, aes(x = Model, y = Value, fill = Model)) + 
        geom_boxplot() + 
        facet_grid(Metric ~., scales = "free_y") +
        theme_bw() + 
        ggtitle("Training Fit")
        
sd(de$Cardiovascular_Disease)


####################################### Prediction #########################################

# extracting predicted values and residuals

m <- c("glm", "RandomForest")

de <- de %>%
        mutate(gPred = predict(gm, de),
               rPred = predict(rf, de),
               gResid = gPred - Cardiovascular_Disease,
               rResid = rPred - Cardiovascular_Disease)

actual_vs_pred <- gather(de, Model, Value, c(gPred, rPred)) %>%
        mutate(Model = ifelse(Model == "gPred", "glm", "RandomForest"),
               Model = factor(Model, levels = m)) %>%
        ggplot(aes(x = Value, 
                   y = Cardiovascular_Disease, 
                   color = Model)) + 
        geom_point(alpha = 0.4) + 
        geom_smooth(method = "lm", se = F) +
        theme_bw() +
        ylab("Deaths from Cardiovascular Disease") + 
        xlab("Predicted Deaths from Cardiovascular Disease") + 
        ggtitle("Comparison between Prediction and Actual Outcome")

correlation <- data.frame(glm = cor(de$gPred, 
                                   de$Cardiovascular_Disease),
                         RandomForest = cor(de$rPred, 
                                            de$Cardiovascular_Disease)) %>%
        gather(Model, Value) %>% 
        mutate(Model = factor(Model, levels = m)) %>%
        ggplot(aes(x = Model, 
                   y = Value,
                   fill = Model,
                   label = round(Value, 2))) +
        geom_bar(stat = "identity", width = 0.8) + 
        theme_bw() +
        ylab("Correlation Coefficient (Pearson's r)") +
        ggtitle("Correlation between Prediction and Actual Outcome") +
        geom_text(nudge_y = 0.03) 

# Plotting Residuals
resid_fn <- function(df, xcol, ycol, c, tit) {
  ggplot(df, 
         aes(x = xcol,
             y = ycol)) + 
    geom_point(alpha = 0.3, color = c) + 
    geom_smooth(method = "lm", se = F) + 
    theme_bw() + 
    xlab("Prediction") + 
    ylab("Residual") + 
    ggtitle(tit) 
}

glm_residuals <- resid_fn(de, 
                          de$gPred,
                          de$gResid,
                          "#FF9999",
                          "Residuals in Quasi-poisson Regression")

rf_residuals <- resid_fn(de, 
                         de$rPred,
                         de$rResid,
                         "#009933",
                         "Residuals in Random Forests")

grid.arrange(glm_residuals,
             rf_residuals,
             nrow = 1)

library(WVPlots)
# Gain Curves
gain_curve <- function(df, model, tit) {
  GainCurvePlot(df, model, "Cardiovascular_Disease", tit) + 
    theme_bw() +
    xlab("Fraction Items in Sort Order") + 
    ylab("Fraction Deaths from Cardiovascular Disease")
}





grid.arrange(gain_curve(de, "gPred", "Quasi-poisson Regression"), 
             gain_curve(de, "rPred", "Random Forests"),
             ncol = 1)

Fit_fn <- function(mod) {
        postResample(pred = predict(mod, de),
                     obs = de$Cardiovascular_Disease)
}

Fit_data <- as.data.frame(rbind(Fit_fn(gm), Fit_fn(rf))) %>% 
  mutate(Model = factor(m, levels = m)) %>%
  gather(key = "Metric", value = "Value", -Model)  %>%
  mutate(Metric = factor(Metric, levels = mtr))

Fit_plot <- Fit_data %>%
  ggplot(aes(x = Model,
             y = Value,
             fill = Model)) +
  geom_bar(stat = "identity", alpha = 0.5, width = 0.8) +
  geom_boxplot(data = rsm_tb1,
               aes(x = Model, y = Value, fill = Model)) + 
  facet_grid(Metric ~., scales = "free_y") +
  theme_bw() + 
  ggtitle("Model Fit on Test Data") 
