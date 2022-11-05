# Analysis for R certification

# Loading packages
library(tidyverse)
library(ggplot2)
library(moments)
library(ggthemes)
library(colorspace)
library(caret)
library(broom)
library(ggrepel)
library(xgboost)
library(rsample)
library(forcats)
library(ggforce)
library(vtreat)
library(WVPlots)
library(pROC)

# Importing data from csv
moped <- read_csv("School/datacamp/Datacamp-Cert-Project-2/moped.csv")

# cleaning
#####

# Inspecting data for variables to ensure we're not missing anything
head(moped)

# all present and accounted for
# varnames all have quotation marks though, those are hideous

# renaming variables
moped <- 
moped %>% 
  mutate(used_for = `Used it for`,
         duration_owned = `Owned for`,
         model = `Model Name`,
         visual_appeal = `Visual Appeal`,
         extra_features = `Extra Features`,
         maint_cost = `Maintenance cost`,
         value = `Value for Money`,
         reliability = Reliability,
         comfort = Comfort,
         .keep = "unused") 

# corrected some capital letters too. naming is now consistent. 

# now to correct issues mentioned in the documentation
# "owned for" ownership column should be changed to indicate ownership as a dummy

moped <- 
  moped %>%
  mutate(owned = ifelse(duration_owned == "Never owned", 0, 1), .keep = "unused")

# storing the original moped df for when I want the NAs for vtreat
moped_og <- moped

# checking other variables for NA values
# storing for use later validating manipulation prior to analysis
colSums(is.na(moped))

# replacing NA values with 0 

moped[is.na(moped)] <- 0

colSums(is.na(moped))

# checking for entries outside expected values

summary(moped)

# exploratory analysis
#####

# One density plot
moped |> 
  ggplot(aes(
    comfort, fill = "#440154FF"
  )) + 
  geom_density() + 
  labs(
    title = "Density plot of comfort ratings in moped reviews",
    x = "Comfort",
    y = "Density"
  ) + 
  theme_minimal() + 
  scale_fill_viridis_d() +
  theme(legend.position = "none")

# density bars of all numerical variables, sorted by ownership
  # pivot longer
  moped %>%
    pivot_longer(visual_appeal:comfort) %>%
    select(value, name, owned) %>%
  # recoding ownership values 
    mutate(owned = ifelse(owned == 0, "Not owned", "Owned")) %>%
  # recoding names of variables for facet titles
    mutate(name = recode(name,
                         "comfort" = "Comfort",
                         "extra_features" = "Extra features",
                         "maint_cost" = "Maintenance cost",
                         "reliability" = "Reliability", 
                         "value" = "Value", 
                         "visual_appeal" = "Visual appeal")) %>%
  # only NA values encoded to 0, we'll leave those out
    filter(value > 0) %>%
  # plot generation
  ggplot(aes(
    value, fill = as.factor(owned), alpha = 0.9
  )) + 
    geom_histogram(breaks = seq(0.5, 5.5, 1), position = "identity", aes(y = ..density..)) + 
    facet_wrap(vars(name), scales = "free_y") + 
    labs(
      title = "Density histogram of ratings, faceted by category, colored by ownership",
      x = "Rating",
      y = "Density", 
      fill = "Ownership"
    ) + 
    guides(fill = "legend", alpha = "none") + 
    scale_fill_manual(values = c(
      'Not owned' = '#EE6A50',
      'Owned' = '#87CEFA'
    ))
      
# proportion of ownership by use 
moped %>%
  group_by(used_for) %>%
  summarize(prop_owned = mean(owned), n = n()) %>%
  arrange(prop_owned)

# proportion of ownership by model
moped %>% 
  group_by(model) %>%
  summarize(prop_owned = mean(owned), n = n()) %>%
  arrange(prop_owned)

# pie plot to investigate ownership rate by model
  # generating a list of desired models
  model_list <- 
    moped |>
      group_by(model) |>
      summarize(n = n()) |>
      arrange(desc(n)) |>
      head(n = 20) |>
      pull(var = model) 

  # generating a table with clear aesthetic assignments for ggplot
  moped |>
    # culling low-n models using the list from earlier
    filter(model %in% model_list) |>
    # modifying owned to a factor
    mutate(owned = ifelse(owned == 0, "Not owned", "Owned")) |>
    group_by(model, owned) |>
    summarize(n = n()) |>
  # generating pie chart 
  ggplot(
    aes(
        x0 = 0, y0 = 0,
        r0 = 0, r = 1,
        amount = n,
        fill = owned,
      )
    ) + 
    geom_arc_bar(stat = "pie") + 
    theme_void() + 
    coord_fixed() + 
    labs(title = "Ownership proportion in moped reviews, by model", fill = "Reviewer ownership") + 
    facet_wrap(vars(model)) + 
    scale_fill_manual(values = c(
      'Not owned' = '#EE6A50',
      'Owned' = '#87CEFA'
    )) + 
    theme(
      panel.spacing = unit(0.5, "cm"),
      strip.text = element_text(size = 7)
    )
  
# PCA
  # arrow style object
  arrow_style <- arrow(
    angle = 20, length = grid::unit(8, "pt"),
    ends = "first", type = "closed")
  
  # generating PCA object
  pca_fit <- 
    moped %>%
    # recoding the use variable to a dummy
    mutate(commuter = ifelse(used_for == "Commuting", 1, 0)) %>% 
    # removing legacy/categorical variables
    select(-c(model, used_for)) %>%
    na.omit() %>%
    scale() %>%
    prcomp()
  pca_fit
  
  # rotation matrix
  pca_fit |>
    tidy(matrix = "rotation") |>
    pivot_wider(
      names_from = "PC", values_from = "value",
      names_prefix = "PC"
    ) |>
    # biplot
    ggplot(aes(PC1, PC2)) +
    geom_segment(
      xend = 0, yend = 0,
      arrow = arrow_style
    ) +
    geom_text_repel(aes(label = column)) +
    xlim(-0.75, 0.75) + ylim(-1, 0.5) + 
    coord_fixed()
  
  # fetching the r-squared values for the principle components via eigenvalue plot
  pca_fit |>
    tidy(matrix = "eigenvalues") |>
    # scree plot
    ggplot(aes(PC, percent, fill = PC)) + 
    geom_col() + 
    scale_x_continuous(
      breaks = 1:8
    ) + 
    scale_y_continuous(
      name = "Variance Explained",
      label = scales::label_percent(accuracy = 1)
    ) + 
    scale_fill_viridis_c() +
    theme(legend.position = "none")
  
# bar graph of counts by model name
  moped |>
    ggplot(aes(
      fct_infreq(model), fill = after_stat(count)
    )) + 
    geom_bar() + 
    coord_flip() + 
    labs(
      title = "Total number of reviews for each moped model",
      x = "Model", 
      y = "Count"
    ) + 
    # including line showing the minimum count for inclusion as a group in splitting
    geom_hline(yintercept = 713 * .10, color = "#440154FF") + 
    theme_bw() + 
    scale_fill_viridis_c() + 
    theme(legend.position = "none") 
  
# counts by brand
  moped |> 
    separate(model, into = c("make", "model"), sep = "\\s", extra = "merge") |>
    ggplot(aes(
      fct_infreq(make), fill = after_stat(count)
    )) + 
    geom_bar() + 
    coord_flip() + 
    labs(
      title = "Total number of reviews for each moped manufacturer",
      x = "Make", 
      y = "Count"
    ) + 
    geom_hline(yintercept = 713 * .10, color = "#440154FF") + 
    theme_bw() + 
    scale_fill_viridis_c() + 
    theme(legend.position = "none") 
  
# bar graph of observations in makes vs models meeting the requirements
  # generating the desired summary stats
  model_n_1 <- 
    moped |>
    group_by(model) |>
    mutate(n = n()) |>
    filter(n > 71.3) |>
    count() |>
    mutate(make = NA)
  
  model_n_2 <- 
    moped |> 
    separate(model, into = c("make", "model"), sep = "\\s", extra = "merge") |>
    group_by(make) |>
    mutate(n = n()) |>
    filter(n > 71.3) |>
    count() |>
    mutate(model = NA)
  
  # forming final matrix of information
  model_n <- 
    rbind(model_n_1, model_n_2) |>
    mutate(type = ifelse(is.na(model) == TRUE, 1, 0),
           model = ifelse(type == 1, make, model)) |>
    select(-make) 
  # generating the bar graph 
  model_n |>
    ggplot(aes(
      as.factor(type), n, fill = model
    )) + 
    geom_col() + 
    labs(
      x = "Grouping",
      y = "Count", 
      title = "Captured observations by grouping type",
      fill = "Make/Model name"
    ) +
    scale_x_discrete(
      labels = c("Make", "Model")
    )

# predictive analysis
#####
# problem type is binary classification

# seed for replication
set.seed(100)
  ###https://win-vector.com/2017/04/15/encoding-categorical-variables-one-hot-and-beyond/
  
# manually converting one variable to a dummy
moped <-
  moped_og |>
  mutate(commuter = ifelse(used_for == "Commuting", 1, 0), .keep = "unused")
  
# test/train split
split <-   
  initial_split(moped, prop = 0.75)

moped_train <- 
  training(split)

moped_test <- 
  testing(split)

# storing vtreat plan
treatplan <- designTreatmentsZ(moped_train, colnames(moped_train), minFraction = 1/20) 

# executing treatment
train_treated <-  prepare(treatplan, moped_train)
test_treated <- prepare(treatplan, moped_test)  

# log reg
  
  # model definition
  logreg_model <- 
    glm(owned ~ ., data = train_treated, family = "binomial")
  
  logreg_model
  
  # summary of model
  summary(logreg_model)
  
  # glance to get model stats
  (perf <- glance(logreg_model))
  
  # calculating pseudo-R-squared
  (pseudoR2 <- 1 - perf$deviance/perf$null.deviance)
  
  # test model
  test_treated$pred <- 
    predict(logreg_model, test_treated, type = "response")
  
  # measure performance
    # gain curve plot
    GainCurvePlot(test_treated, xvar = "pred", "owned", "Moped reviewer ownership status prediction model")
    
    # ROC curve #2
    ROCPlot(test_treated, 
            xvar = "pred", 
            truthVar = "owned", 
            truthTarget = TRUE,
            title = "Moped reviewer ownership status prediction model", 
            add_beta_ideal_curve = TRUE)
    
# xgboost
  ### investigate full documentation of vtreat in R
    
  # defining dataframes sans outcome
    xgb_train <- 
      train_treated |>
      select(-owned) |>
      as.matrix()
      
    xgb_test <- 
      test_treated |>
      select(-c(pred, owned)) |>
      as.matrix()
      
  # running cross validation to find the ideal parameters
    cv <- xgb.cv(data = xgb_train, 
                 label = train_treated$owned,
                 nrounds = 100,
                 nfold = 5,
                 objective = "binary:logistic",
                 max_depth = 5,
                 early_stopping_rounds = 5,
                 verbose = FALSE   # silent
      )
  # fetching evaluation log
    cv$evaluation_log |>
      summarize(ntrees.train = which.min(train_logloss_mean),
                ntrees.test = which.min(test_logloss_mean))
  
  # defining final model
  xgb_model <- xgboost(data = xgb_test, 
                       label = test_treated$owned,
                       objective = "binary:logistic",
                       max.depth = 5, 
                       nrounds = 15, 
                       verbose = FALSE)
  
  # predictions 
  test_treated$pred <- 
    predict(xgb_model, xgb_test)
  
  # measure performance
  # gain curve plot
    GainCurvePlot(test_treated, xvar = "pred", "owned", "Moped reviewer ownership status prediction model")
  
  # ROC curve #2
    ROCPlot(test_treated, 
            xvar = "pred", 
            truthVar = "owned", 
            truthTarget = TRUE,
            title = "Moped reviewer ownership status prediction model", 
            add_beta_ideal_curve = TRUE)
