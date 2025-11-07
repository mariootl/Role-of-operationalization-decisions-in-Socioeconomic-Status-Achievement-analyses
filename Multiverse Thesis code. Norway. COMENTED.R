#########################################################################
# THESIS FInal CODE 
#########################################################################

# University of Oslo
# Centre for Educational Measurement
# Manuel Mario Olivera Toro Lopez (AKA EL MArio)
# Master thesis code

# To whom it may concern:
#  Thank you for taking an interest into my project. 
# any inquiries, corrections, insults, or invites for a beer (or more)
# please refer to mariootl20@gmail.com

################################################################
# PART 1: enviroment setup
################################################################
###################
# set the enviroment 
###################

library(foreign)
library(ggplot2)
library(mice)
library(survey)
library(mitools)
library(tidyr)
library(dplyr)
library(cowplot)
library(broom)
library(moments)
library(purrr)
library(patchwork)

set.seed(9597)


###################
# Descriptive functions
###################

# Mode
ModeFun <- function(x) {
  if (length(x) == 0) return(NA)
  freq_table <- table(x, useNA = "ifany")
  if (length(freq_table) == 0) return(NA)
  max_freq <- max(freq_table)
  modes <- names(freq_table)[freq_table == max_freq]
  if (length(modes) > 1) {
    return(modes[1])
  } else {
    return(modes)
  }
}

# DEscriptives
Descriptives <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- seq_along(data)
  }
  summary_stats <- data.frame(
    Variable = names(data)[cols],
    Mean = round(sapply(data[cols], mean, na.rm = TRUE), 4),
    SD = round(sapply(data[cols], sd, na.rm = TRUE), 4),
    Skewness = round(sapply(data[cols], moments::skewness, na.rm = TRUE), 4),
    Kurtosis = round(sapply(data[cols], moments::kurtosis, na.rm = TRUE), 4),
    Mode = sapply(data[cols], ModeFun),
    Median = round(sapply(data[cols], median, na.rm = TRUE), 4),
    stringsAsFactors = FALSE
  )
  return(summary_stats)
}

##################################################################################################################
# THE CASE OF NORWAY
##################################################################################################################
##############
# Loading the data
##############

lab.Base = c("IDSCHOOL","IDCLASS","IDSTUD","TOTWGT","JKREP","JKZONE","BSSSCI01","BSSSCI02","BSSSCI03","BSSSCI04","BSSSCI05")
lab.HomeR = c("BSBG05A","BSBG05B","BSBG05C","BSBG05D","BSBG05E","BSBG05F","BSBG05G")
lab.SES <- c("BSBG04","BSBG06A","BSBG06B","BSBGHER","BSDGHER")
lab.HERIndex <- c(lab.HomeR, "BSBG04", "BSBG06C")
data <- read.spss("bsgnorm7.sav", to.data.frame=TRUE, max.value.labels=7)
data <- data[, c(lab.Base, lab.HomeR, lab.SES)]

##############
# Recoding
##############


# Recoding Home resources as 0 = No; 1 = YEs
data[, lab.HomeR] <- sapply(data[, lab.HomeR], function(x){ abs(as.numeric(x)-2) })

# recoding Books at home 
data$BSBG04 <- as.character((data$BSBG04))
data$BSBG04 <- ifelse(data$BSBG04=="Enough to fill three or more bookcases (more than 200)",5,
                      ifelse(data$BSBG04=="Enough to fill two bookcases (101-200 books)",4,
                             ifelse(data$BSBG04=="Enough to fill one bookcase (26-100 books)",3,
                                    ifelse(data$BSBG04=="Enough to fill one shelf (11-25 books)",2,
                                           ifelse(data$BSBG04=="None or very few (0-10 books)",1,NA)))))
data$BSBG04 <- as.numeric(data$BSBG04)

# Recoding parents' education levels
data$BSBG06A[data$BSBG06A %in% c(8, 9)] <- NA
data$BSBG06B[data$BSBG06B %in% c(8, 9)] <- NA

# Creating the new variable for highest education of either parent
data$BSBG06C <- apply(data, 1, function(row) {
  valA <- row["BSBG06A"]
  valB <- row["BSBG06B"]
  if (is.na(valA) && is.na(valB)) {
    return(NA)
  } else if (is.na(valA)) {
    return(valB)
  } else if (is.na(valB)) {
    return(valA)
  } else {
    return(max(valA, valB))
  }
})
data$BSBG06C <- as.numeric(data$BSBG06C)

# Recoding SES original assignment (by TIMSS' SCORE)
data$BSDGHER <- as.character(data$BSDGHER)
data$BSDGHER <- ifelse(data$BSDGHER=="Many Resources",2,
                       ifelse(data$BSDGHER=="Some Resources",1,
                              ifelse(data$BSDGHER=="Few Resources",0,NA)))
data$BSDGHER <- as.numeric(data$BSDGHER)

# Creating the synthetic variable for home possessions score
data$BSBG05_Possessions_Score <- rowSums(data[, lab.HomeR], na.rm = TRUE)


###################
# Exploring NAs
###################
TableOfNAs <- data.frame(variable=names(data),
                         NumOfNAs=colSums(is.na(data)),
                         Percentage=round(colMeans(is.na(data))*100,2),
                         TotalObservations = sapply(data, length)
);TableOfNAs


MissingnessPattern <- md.pattern(data[,lab.HERIndex]);MissingnessPattern

# There are 262 cases where non of the indicators for the home possessions, books at home and highiest education of a parent.  
#In this case I decided to listwise

###################
# Listwise cases with no answers to the index items
###################
all_na_cases <- which(rowSums(is.na(data[, lab.HERIndex])) == length(c(lab.HERIndex)))
dataAfterListwise <- data[-all_na_cases, ]
md.pattern(dataAfterListwise[,lab.HERIndex])

###################
# Imputation of the rest of the missing cases
###################
ImpResults <- mice(dataAfterListwise, m = 5, seed = 9597)
DataImp <- complete(ImpResults, action = "all")

sum(is.na(DataImp[[1]])) 

table(DataImp[[3]][,"BSDGHER"],useNA = "always")


################################################################
# PART 3: Multiverse generation 
################################################################
########################
# CLUSTER 1. SINGLE VARIABLE MULTIVERSES
########################
##############
# Defining multiverses
##############

multiverse_rules_cluster1 <- list(
  # BSBG04 (Books at home)
  list(name = "BSBG04_MV1", variable = "BSBG04", low_levels = 1, medium_levels = 2, high_levels = c(3, 4, 5)),
  list(name = "BSBG04_MV2", variable = "BSBG04", low_levels = 1, medium_levels = c(2, 3), high_levels = c(4, 5)),
  list(name = "BSBG04_MV3", variable = "BSBG04", low_levels = 1, medium_levels = c(2, 3, 4), high_levels = 5),
  list(name = "BSBG04_MV4", variable = "BSBG04", low_levels = c(1, 2), medium_levels = 3, high_levels = c(4, 5)),
  list(name = "BSBG04_MV5", variable = "BSBG04", low_levels = c(1, 2), medium_levels = c(3, 4), high_levels = 5),
  list(name = "BSBG04_MV6", variable = "BSBG04", low_levels = c(1, 2, 3), medium_levels = 4, high_levels = 5),
  # BSBG06C (Parents' education)
  list(name = "BSBG06C_MV1", variable = "BSBG06C", low_levels = 1, medium_levels = 2, high_levels = c(3, 4, 5, 6, 7)),
  list(name = "BSBG06C_MV2", variable = "BSBG06C", low_levels = 1, medium_levels = c(2, 3), high_levels = c(4, 5, 6, 7)),
  list(name = "BSBG06C_MV3", variable = "BSBG06C", low_levels = 1, medium_levels = c(2, 3, 4), high_levels = c(5, 6, 7)),
  list(name = "BSBG06C_MV4", variable = "BSBG06C", low_levels = 1, medium_levels = c(2, 3, 4, 5), high_levels = c(6, 7)),
  list(name = "BSBG06C_MV5", variable = "BSBG06C", low_levels = 1, medium_levels = c(2, 3, 4, 5, 6), high_levels = 7),
  list(name = "BSBG06C_MV6", variable = "BSBG06C", low_levels = c(1, 2), medium_levels = 3, high_levels = c(4, 5, 6, 7)),
  list(name = "BSBG06C_MV7", variable = "BSBG06C", low_levels = c(1, 2), medium_levels = c(3, 4), high_levels = c(5, 6, 7)),
  list(name = "BSBG06C_MV8", variable = "BSBG06C", low_levels = c(1, 2), medium_levels = c(3, 4, 5), high_levels = c(6, 7)),
  list(name = "BSBG06C_MV9", variable = "BSBG06C", low_levels = c(1, 2), medium_levels = c(3, 4, 5, 6), high_levels = 7),
  list(name = "BSBG06C_MV10", variable = "BSBG06C", low_levels = c(1, 2, 3), medium_levels = 4, high_levels = c(5, 6, 7)),
  list(name = "BSBG06C_MV11", variable = "BSBG06C", low_levels = c(1, 2, 3), medium_levels = c(4, 5), high_levels = c(6, 7)),
  list(name = "BSBG06C_MV12", variable = "BSBG06C", low_levels = c(1, 2, 3), medium_levels = c(4, 5, 6), high_levels = 7),
  list(name = "BSBG06C_MV13", variable = "BSBG06C", low_levels = c(1, 2, 3, 4), medium_levels = 5, high_levels = c(6, 7)),
  list(name = "BSBG06C_MV14", variable = "BSBG06C", low_levels = c(1, 2, 3, 4), medium_levels = c(5, 6), high_levels = 7),
  list(name = "BSBG06C_MV15", variable = "BSBG06C", low_levels = c(1, 2, 3, 4, 5), medium_levels = 6, high_levels = 7),
  # BSBG05_Score (possessions score)
  list(name = "BSBG05_Score_MV1", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = 1, high_levels = c(2, 3, 4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV2", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = c(1, 2), high_levels = c(3, 4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV3", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = c(1, 2, 3), high_levels = c(4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV4", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = c(1, 2, 3, 4), high_levels = c(5, 6, 7)),
  list(name = "BSBG05_Score_MV5", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = c(1, 2, 3, 4, 5), high_levels = c(6, 7)),
  list(name = "BSBG05_Score_MV6", variable = "BSBG05_Possessions_Score", low_levels = 0, medium_levels = c(1, 2, 3, 4, 5, 6), high_levels = 7),
  list(name = "BSBG05_Score_MV7", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1), medium_levels = 2, high_levels = c(3, 4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV8", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1), medium_levels = c(2, 3), high_levels = c(4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV9", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1), medium_levels = c(2, 3, 4), high_levels = c(5, 6, 7)),
  list(name = "BSBG05_Score_MV10", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1), medium_levels = c(2, 3, 4, 5), high_levels = c(6, 7)),
  list(name = "BSBG05_Score_MV11", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1), medium_levels = c(2, 3, 4, 5, 6), high_levels = 7),
  list(name = "BSBG05_Score_MV12", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2), medium_levels = 3, high_levels = c(4, 5, 6, 7)),
  list(name = "BSBG05_Score_MV13", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2), medium_levels = c(3, 4), high_levels = c(5, 6, 7)),
  list(name = "BSBG05_Score_MV14", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2), medium_levels = c(3, 4, 5), high_levels = c(6, 7)),
  list(name = "BSBG05_Score_MV15", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2), medium_levels = c(3, 4, 5, 6), high_levels = 7),
  list(name = "BSBG05_Score_MV16", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3), medium_levels = 4, high_levels = c(5, 6, 7)),
  list(name = "BSBG05_Score_MV17", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3), medium_levels = c(4, 5), high_levels = c(6, 7)),
  list(name = "BSBG05_Score_MV18", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3), medium_levels = c(4, 5, 6), high_levels = 7),
  list(name = "BSBG05_Score_MV19", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3, 4), medium_levels = 5, high_levels = c(6, 7)),
  list(name = "BSBG05_Score_MV20", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3, 4), medium_levels = c(5, 6), high_levels = 7),
  list(name = "BSBG05_Score_MV21", variable = "BSBG05_Possessions_Score", low_levels = c(0, 1, 2, 3, 4, 5), medium_levels = 6, high_levels = 7)
)

ordered_multiverse_ids_cluster1 <- sapply(multiverse_rules_cluster1, `[[`, "name")

##############
# Applying Multiverse Rules to Imputed Data ---
##############

for (i in seq_along(DataImp)) {
  current_df <- DataImp[[i]]
  for (rule in multiverse_rules_cluster1) {
    new_col_name <- paste0("SES_", rule$name)
    target_var <- rule$variable
    current_df[[new_col_name]] <- NA_character_
    if (target_var %in% names(current_df)) {
      current_df <- current_df %>%
        mutate(!!new_col_name := case_when(
          .data[[target_var]] %in% rule$low_levels ~ "Low",
          .data[[target_var]] %in% rule$medium_levels ~ "Medium",
          .data[[target_var]] %in% rule$high_levels ~ "High",
          TRUE ~ NA_character_
        ))
    }
  }
  DataImp[[i]] <- current_df
}

# LEaving one plausible value in each Imputed dataset BSSSCI01-DATAIMP[[1]]... etc...
# Loop through each imputed dataset
for (i in seq_along(DataImp)) {
  
  # Determine which PV to keep based on the loop index
  pv_to_keep <- paste0("BSSSCI0", i)
  
  # Get the names of all other PVs
  all_pvs <- c("BSSSCI01", "BSSSCI02", "BSSSCI03", "BSSSCI04", "BSSSCI05")
  pvs_to_remove <- all_pvs[all_pvs != pv_to_keep]
  
  # Remove the unnecessary PVs from the current dataframe
  DataImp[[i]] <- DataImp[[i]] %>%
    select(-all_of(pvs_to_remove))
}

################################################################
# DESCRIPTIVES
################################################################


pool_descriptives <- function(imputed_list, varname) {
  res_list <- lapply(imputed_list, function(dat) {
    vals <- dat[[varname]]
    data.frame(
      mean   = mean(vals, na.rm = TRUE),
      median = median(vals, na.rm = TRUE),
      var    = var(vals, na.rm = TRUE),
      skew   = skewness(vals, na.rm = TRUE)
    )
  })
  
  m <- length(res_list)
  res_df <- do.call(rbind, res_list)
  
  # Pooled mean (Rubin's rules)
  q_bar <- mean(res_df$mean)
  within_var  <- mean(res_df$var)
  between_var <- var(res_df$mean)
  total_var   <- within_var + (1 + 1/m) * between_var
  pooled_sd <- sqrt(total_var)
  
  # Pooled median = mean of medians
  pooled_median <- mean(res_df$median)
  
  # Pooled skewness = mean of skewnesses (pragmatic)
  pooled_skew <- mean(res_df$skew)
  
  return(data.frame(
    pooled_mean   = q_bar,
    pooled_median = pooled_median,
    pooled_sd     = pooled_sd,
    pooled_skew   = pooled_skew
  ))
}

#########
# Descriptives Books at home
#########

pool_descriptives(DataImp,"BSBG04")

#########
# Descriptives highest parental education
#########

pool_descriptives(DataImp,"BSBG06C")

#########
#Descriptives Possessions Score
#########

pool_descriptives(DataImp,"BSBG05_Possessions_Score")

#########
# Overall descriptives
#########

pool_descriptives(DataImp,"BSDGHER")




################################################################
# Analysis
################################################################
########################
# Applying Multiverse Rules to Imputed Data ---
########################

for (i in seq_along(DataImp)) {
  current_df <- DataImp[[i]]
  for (rule in multiverse_rules_cluster1) {
    new_col_name <- paste0("SES_", rule$name)
    target_var <- rule$variable
    current_df[[new_col_name]] <- NA_character_
    if (target_var %in% names(current_df)) {
      current_df <- current_df %>%
        mutate(!!new_col_name := case_when(
          .data[[target_var]] %in% rule$low_levels ~ "Low",
          .data[[target_var]] %in% rule$medium_levels ~ "Medium",
          .data[[target_var]] %in% rule$high_levels ~ "High",
          TRUE ~ NA_character_
        ))
    }
  }
  DataImp[[i]] <- current_df
}

########################
# Estimating proportion of respondents in each SES level in each multiverse
########################
##############
# Pooling of proportions
##############

options(survey.lonely.psu = "adjust")

rubins_rule_proportions <- function(data) {
  M <- length(unique(data$Imputation_ID))
  if (M < 2) return(data.frame(pooled_proportion = NA, pooled_se = NA, ci_lower = NA, ci_upper = NA, df_rubin = NA))
  
  p_m <- data$Proportion_m
  U_m <- data$Variance_m
  pooled_p <- mean(p_m, na.rm = TRUE)
  mean_U <- mean(U_m, na.rm = TRUE)
  B <- var(p_m, na.rm = TRUE)
  Total_Variance <- mean_U + (1 + 1/M) * B
  
  if (Total_Variance == 0) df_rubin <- Inf else {
    lambda <- ((1 + 1/M) * B) / Total_Variance
    df_rubin <- (M - 1) / (lambda^2 + .Machine$double.eps)
    if (df_rubin < (M - 1)) df_rubin <- (M - 1)
  }
  
  pooled_se <- sqrt(Total_Variance)
  t_critical <- qt(0.975, df = df_rubin)
  ci_lower <- pooled_p - t_critical * pooled_se
  ci_upper <- pooled_p + t_critical * pooled_se
  
  ci_lower <- max(0, ci_lower); ci_upper <- min(1, ci_upper); pooled_p <- max(0, min(1, pooled_p))
  return(data.frame(pooled_proportion = pooled_p, pooled_se = pooled_se, ci_lower = ci_lower, ci_upper = ci_upper, df_rubin = df_rubin))
}


all_proportions_for_pooling_cluster1 <- list()
for (i in seq_along(DataImp)) {
  current_df <- DataImp[[i]]
  imputation_id <- paste0("Imp", i)
  
  svy_design <- svydesign(
    ids = ~IDSCHOOL,
    strata = ~JKZONE,
    weights = ~TOTWGT,
    data = current_df,
    nest = TRUE
  )
  
  ses_cols_df <- select(current_df, starts_with("SES_BSBG04"), starts_with("SES_BSBG06C"), starts_with("SES_BSBG05_Score"))
  for (col_name in names(ses_cols_df)) {
    # Recode the SES variable to be a factor for the survey analysis
    current_df[[col_name]] <- factor(current_df[[col_name]], levels = c("Low", "Medium", "High"))
    
    # Create dummy variables for each SES level
    current_df$is_low <- ifelse(current_df[[col_name]] == "Low", 1, 0)
    current_df$is_medium <- ifelse(current_df[[col_name]] == "Medium", 1, 0)
    current_df$is_high <- ifelse(current_df[[col_name]] == "High", 1, 0)
    
    # Update the survey design object with the new dummy variables
    svy_design_with_dummies <- update(svy_design, 
                                      is_low = current_df$is_low, 
                                      is_medium = current_df$is_medium,
                                      is_high = current_df$is_high
    )
    
    # Calculate weighted proportions (using svymean on the dummy variables)
    low_prop_svy <- svymean(~is_low, design = svy_design_with_dummies, na.rm = TRUE)
    medium_prop_svy <- svymean(~is_medium, design = svy_design_with_dummies, na.rm = TRUE)
    high_prop_svy <- svymean(~is_high, design = svy_design_with_dummies, na.rm = TRUE)
    
    all_proportions_for_pooling_cluster1[[length(all_proportions_for_pooling_cluster1) + 1]] <- data.frame(
      Imputation_ID = imputation_id,
      Multiverse_ID = sub("SES_", "", col_name),
      SES_Level = c("Low", "Medium", "High"),
      Proportion_m = c(low_prop_svy[1], medium_prop_svy[1], high_prop_svy[1]),
      Variance_m = c(SE(low_prop_svy)^2, SE(medium_prop_svy)^2, SE(high_prop_svy)^2)
    )
  }
}
raw_pooling_data_cluster1 <- bind_rows(all_proportions_for_pooling_cluster1)
pooled_results_cluster1 <- raw_pooling_data_cluster1 %>%
  group_by(Multiverse_ID, SES_Level) %>%
  summarise(rubin_pooled = list(rubins_rule_proportions(cur_data())), .groups = 'drop') %>%
  unnest(rubin_pooled) %>%
  mutate(SES_Level = factor(SES_Level, levels = c("Low", "Medium", "High")))

########################
# Ploting the SES PRoportions
########################


# Define the 9-color gradient palette for the combined plots
color_palette_cluster1 <- c(
  "High_BSBG04" = "#2E86AB", "High_BSBG06C" = "#A23B72", "High_BSBG05_Score" = "#2E8B57",
  "Medium_BSBG04" = "#5FA7C1", "Medium_BSBG06C" = "#BD6C96", "Medium_BSBG05_Score" = "#66B98D",
  "Low_BSBG04" = "#A7CDDD", "Low_BSBG06C" = "#DAAAC3", "Low_BSBG05_Score" = "#B9DEC7"
)

# Define the full 9-item legend labels and breaks for the final figure
full_legend_breaks <- names(color_palette_cluster1)
full_legend_labels <- c(
  "High SES (Books)", "High SES (Education)", "High SES (Possessions)",
  "Medium SES (Books)", "Medium SES (Education)", "Medium SES (Possessions)",
  "Low SES (Books)", "Low SES (Education)", "Low SES (Possessions)"
)

# Helper function to create the simple labels (B_MV1, Ed_MV2, etc.)
create_label_df <- function(multiverse_rules_cluster1) {
  map_dfr(multiverse_rules_cluster1, function(rule) {
    prefix <- case_when(
      grepl("^BSBG04", rule$name) ~ "B",
      grepl("^BSBG06C", rule$name) ~ "Ed", 
      grepl("^BSBG05_Score", rule$name) ~ "P"
    )
    
    simple_id <- paste0(prefix, "_", sub(".*(MV[0-9]+)", "\\1", rule$name))
    
    data.frame(
      Multiverse_ID = rule$name, 
      Simple_ID = simple_id,
      Variable_Type = case_when(
        grepl("^BSBG04", rule$name) ~ "Books at Home",
        grepl("^BSBG06C", rule$name) ~ "Parent Education",
        grepl("^BSBG05_Score", rule$name) ~ "Home Possessions"
      ),
      Raw_Variable_Name = case_when(
        grepl("^BSBG04", rule$name) ~ "BSBG04",
        grepl("^BSBG06C", rule$name) ~ "BSBG06C",
        grepl("^BSBG05_Score", rule$name) ~ "BSBG05_Score"
      )
    )
  })
}

# Function to create Decision Table Data for the geom_tile plot
create_decision_data <- function(multiverse_rules_cluster1, labels_df) {
  
  decision_data_raw <- map_dfr(multiverse_rules_cluster1, function(rule) {
    levels_list <- list(
      data.frame(Category_Level = rule$low_levels, SES_Group = "Low"),
      data.frame(Category_Level = rule$medium_levels, SES_Group = "Medium"),
      data.frame(Category_Level = rule$high_levels, SES_Group = "High")
    )
    
    bind_rows(levels_list) %>%
      mutate(Multiverse_ID = rule$name)
  })
  
  decision_data <- decision_data_raw %>%
    left_join(labels_df, by = "Multiverse_ID") %>%
    mutate(
      Simple_ID = factor(Simple_ID, levels = labels_df$Simple_ID),
      
      # Order SES groups for consistent coloring/tiling
      SES_Group = factor(SES_Group, levels = c("High", "Medium", "Low")), 
      
      # Category Level must be a factor for proper plotting
      Category_Level = factor(Category_Level),
      
      # Create the complex Color Key for the 9-color palette
      Color_Key = paste0(SES_Group, "_", Raw_Variable_Name)
    ) %>%
    mutate(Color_Key = factor(Color_Key, levels = names(color_palette_cluster1))) %>%
    arrange(Simple_ID, Category_Level)
  
  return(decision_data)
}


#############################
# 1. Data Preparation (Both Plots)
#############################

# Assuming pooled_results_cluster1 and multiverse_rules_cluster1 are available
labels_df <- create_label_df(multiverse_rules_cluster1)

# 1.1 Proportion Plot Data Preparation (Based on user's code)
final_pooled_results <- pooled_results_cluster1 %>%
  
  # Join the simple labels
  left_join(labels_df, by = "Multiverse_ID") %>%
  
  mutate(
    # Ensure Original_Variable is correctly identified and factored
    Original_Variable = case_when(
      grepl("^BSBG04", Multiverse_ID) ~ "BSBG04",
      grepl("^BSBG05_Score", Multiverse_ID) ~ "BSBG05_Score",
      grepl("^BSBG06C", Multiverse_ID) ~ "BSBG06C",
      TRUE ~ "Unknown"
    ),
    Original_Variable = factor(Original_Variable, levels = c("BSBG04", "BSBG06C", "BSBG05_Score")),
    
    # Reverse the order of SES_Level to put "Low" at the bottom of the stack
    SES_Level = factor(SES_Level, levels = c("High", "Medium", "Low")),
    
    # Create Fill_Group (Color Key) and factor Simple_ID
    Fill_Group = interaction(SES_Level, Original_Variable, sep = "_", drop = TRUE),
    Simple_ID = factor(Simple_ID, levels = labels_df$Simple_ID)
  ) %>%
  
  arrange(Original_Variable, Multiverse_ID)

# 1.2 Decision Plot Data Preparation
decision_plot_data <- create_decision_data(multiverse_rules_cluster1, labels_df)


#############################
# 2. Plot Objects
#############################

# 2.1 Top Plot: Proportions of Respondents
proportion_plot <- ggplot(final_pooled_results, aes(x = Simple_ID, y = pooled_proportion, fill = Fill_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  
  # Use the full color palette
  scale_fill_manual(
    values = color_palette_cluster1, 
    # Hide the legend for the top plot
    guide = "none" 
  ) +
  labs(
    title = NULL,
    x = NULL, # X-axis label removed
    y = "Proportion of respondents" # Sentence case
  ) +
  theme_minimal(base_size = 12) +
  theme(
    # Hide X-axis elements for shared axis alignment
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14, margin = margin(b = 10)), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    plot.margin = margin(10, 10, 0, 10) # Reduced bottom margin
  )

# 2.2 Bottom Plot: Multiverse Decisions (Cutpoints)
# CHANGED: Removed faceting (facet_grid)
decision_plot <- ggplot(decision_plot_data, aes(x = Simple_ID, y = Category_Level, fill = Color_Key)) +
  
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Use the full color palette, and define the full 9-item legend here
  scale_fill_manual(
    values = color_palette_cluster1, 
    name = "SES Level and Source Variable",
    breaks = full_legend_breaks,
    labels = full_legend_labels
  ) +
  
  labs(
    x = "Multiverse definition", 
    y = "Original category level" # Sentence case
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # X-axis labels are visible and rotated
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 5)),
    
    # Place the consolidated legend at the bottom
    legend.position = "bottom",
    legend.title = element_text(face = "plain", size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 10, 10, 10) # Reduced top margin
  )

#############################
# 3. Combine Plots (using patchwork)
#############################

# Stack the two plots vertically, controlling their relative heights
final_plot <- proportion_plot / decision_plot +
  plot_layout(heights = c(2, 1)) # Set proportion plot taller than decision plot

# Display the plot
print(final_plot)


################################################################
# GAPS BETWEEN SES LEVELS
################################################################
##############
#Function to calculate gaps
##############

calculate_gap_stats_survey <- function(svy_design, ses_col_name, pv_var_name) {
  
  if (!(ses_col_name %in% names(svy_design$variables)) ||
      !(pv_var_name %in% names(svy_design$variables))) {
    warning(paste("Missing variables in survey design for SES:", ses_col_name, "or PV:", pv_var_name))
    return(data.frame(
      Gap_Type = c("High-Low", "Medium-Low", "High-Medium"),
      Gap_Value = as.numeric(NA),
      Std_Error = as.numeric(NA)
    ))
  }
  
  tryCatch({
    # Calculate means by SES group
    mean_by_ses <- svyby(
      formula = as.formula(paste0("~", pv_var_name)),
      by = as.formula(paste0("~", ses_col_name)),
      design = svy_design,
      FUN = svymean,
      na.rm = TRUE
    )
    
    # Check if we have all three SES levels
    if (nrow(mean_by_ses) != 3) {
      warning(paste("Expected 3 SES levels, but got", nrow(mean_by_ses)))
      return(data.frame(
        Gap_Type = c("High-Low", "Medium-Low", "High-Medium"),
        Gap_Value = as.numeric(NA),
        Std_Error = as.numeric(NA)
      ))
    }
    
    # Extract means and standard errors directly from svyby result
    # The svyby result should have rows ordered as: Low, Medium, High
    ses_levels <- rownames(mean_by_ses)
    
    # Find the indices for each SES level
    low_idx <- which(ses_levels == "Low")
    medium_idx <- which(ses_levels == "Medium") 
    high_idx <- which(ses_levels == "High")
    
    if (length(low_idx) != 1 || length(medium_idx) != 1 || length(high_idx) != 1) {
      warning("Could not find all SES levels (Low, Medium, High)")
      return(data.frame(
        Gap_Type = c("High-Low", "Medium-Low", "High-Medium"),
        Gap_Value = as.numeric(NA),
        Std_Error = as.numeric(NA)
      ))
    }
    
    # Extract means and SEs
    low_mean <- mean_by_ses[low_idx, pv_var_name]
    medium_mean <- mean_by_ses[medium_idx, pv_var_name]
    high_mean <- mean_by_ses[high_idx, pv_var_name]
    
    low_se <- SE(mean_by_ses)[low_idx]
    medium_se <- SE(mean_by_ses)[medium_idx]
    high_se <- SE(mean_by_ses)[high_idx]
    
    # Calculate gaps
    high_low_gap <- high_mean - low_mean
    medium_low_gap <- medium_mean - low_mean
    high_medium_gap <- high_mean - medium_mean
    
    # Calculate standard errors for the differences
    # SE(A - B) = sqrt(SE(A)^2 + SE(B)^2) assuming independence
    high_low_se <- sqrt(high_se^2 + low_se^2)
    medium_low_se <- sqrt(medium_se^2 + low_se^2)
    high_medium_se <- sqrt(high_se^2 + medium_se^2)
    
    gap_stats <- data.frame(
      Gap_Type = c("High-Low", "Medium-Low", "High-Medium"),
      Gap_Value = c(high_low_gap, medium_low_gap, high_medium_gap),
      Std_Error = c(high_low_se, medium_low_se, high_medium_se)
    )
    
    return(gap_stats)
    
  }, error = function(e) {
    warning(paste("Failed to calculate gap stats. Error:", e$message))
    return(data.frame(
      Gap_Type = c("High-Low", "Medium-Low", "High-Medium"),
      Gap_Value = as.numeric(NA),
      Std_Error = as.numeric(NA)
    ))
  })
}



########################
# --- Calculate Gaps
########################

plausible_value_vars <- paste0("BSSSCI0", 1:5)
all_gap_results <- list()

for (i in seq_along(DataImp)) {
  current_df <- DataImp[[i]]
  pv_var_name <- plausible_value_vars[i]
  
  for (rule in multiverse_rules_cluster1) {
    ses_col_name <- paste0("SES_", rule$name)
    if (ses_col_name %in% names(current_df)) {
      current_df[[ses_col_name]] <- factor(current_df[[ses_col_name]], levels = c("Low", "Medium", "High"))
      
      svy_design <- svydesign(
        ids = ~IDSCHOOL,
        strata = ~JKZONE,
        weights = ~TOTWGT,
        data = current_df,
        nest = TRUE
      )
      
      gap_stats <- calculate_gap_stats_survey(svy_design, ses_col_name, pv_var_name)
      gap_stats$Imputation_ID <- i
      gap_stats$Multiverse_ID <- rule$name
      all_gap_results[[length(all_gap_results) + 1]] <- gap_stats
    }
  }
}


########################
# Pool gaps
#######################

# Manual Rubin's Rules Implementation (Recommended)
manual_rubins_pooling <- function(estimates, std_errors) {
  m <- length(estimates)  # Number of imputations
  
  # Pooled estimate (Q_bar)
  pooled_est <- mean(estimates)
  
  # Within-imputation variance (U_bar)
  within_var <- mean(std_errors^2)
  
  # Between-imputation variance (B)
  between_var <- var(estimates)
  
  # Total variance (T)
  total_var <- within_var + (1 + 1/m) * between_var
  
  # Pooled standard error
  pooled_se <- sqrt(total_var)
  
  # Degrees of freedom (optional, for t-tests)
  if (between_var > 0) {
    lambda <- (1 + 1/m) * between_var / total_var
    df <- (m - 1) / lambda^2
  } else {
    df <- Inf
  }
  
  return(list(
    estimate = pooled_est,
    se = pooled_se,
    df = df,
    within_var = within_var,
    between_var = between_var,
    total_var = total_var
  ))
}

# Apply manual pooling to your gap results
if (length(all_gap_results) > 0) {
  raw_gap_data <- do.call(rbind, all_gap_results)
  
  # Pool using manual Rubin's rules
  pooled_gap_results <- raw_gap_data %>%
    group_by(Multiverse_ID, Gap_Type) %>%
    summarise(
      pooled_result = list(manual_rubins_pooling(Gap_Value, Std_Error)),
      .groups = 'drop'
    ) %>%
    mutate(
      pooled_mean_diff = purrr::map_dbl(pooled_result, ~.x$estimate),
      pooled_se = purrr::map_dbl(pooled_result, ~.x$se),
      pooled_df = purrr::map_dbl(pooled_result, ~.x$df),
      within_var = purrr::map_dbl(pooled_result, ~.x$within_var),
      between_var = purrr::map_dbl(pooled_result, ~.x$between_var)
    ) %>%
    select(Multiverse_ID, Gap_Type, pooled_mean_diff, pooled_se, pooled_df, within_var, between_var)
  
  print("Successfully calculated pooled gap statistics!")
  print(head(pooled_gap_results, 10))
  
  # Save the results
  # write.csv(pooled_gap_results, "pooled_gap_results.csv", row.names = FALSE)
  
} else {
  print("No gap results found!")
}

#############################
# Plot Gaps
#############################


create_label_df <- function(multiverse_rules_cluster1) {
  map_dfr(multiverse_rules_cluster1, function(rule) {
    prefix <- case_when(
      grepl("^BSBG04", rule$name) ~ "B",
      grepl("^BSBG06C", rule$name) ~ "Ed", 
      grepl("^BSBG05_Score", rule$name) ~ "P"
    )
    
    # NEW: Simple ID for x-axis (e.g., B_MV1)
    simple_id <- paste0(prefix, "_", sub(".*(MV[0-9]+)", "\\1", rule$name))
    
    data.frame(
      Multiverse_ID = rule$name, 
      Simple_ID = simple_id, # Use this for plotting X-axis
      Variable_Type = case_when(
        grepl("^BSBG04", rule$name) ~ "Books at Home",
        grepl("^BSBG06C", rule$name) ~ "Parent Education",
        grepl("^BSBG05_Score", rule$name) ~ "Home Possessions"
      ),
      # ADDED: Raw Variable Name for Color_Key creation
      Raw_Variable_Name = case_when( 
        grepl("^BSBG04", rule$name) ~ "BSBG04",
        grepl("^BSBG06C", rule$name) ~ "BSBG06C",
        grepl("^BSBG05_Score", rule$name) ~ "BSBG05_Score"
      )
    )
  })
}

# 1.1 Create Main Plot Data
create_gap_plot_data <- function(pooled_gap_results, multiverse_rules_cluster1) {
  
  multiverse_order <- sapply(multiverse_rules_cluster1, `[[`, "name")
  descriptive_labels_df <- create_label_df(multiverse_rules_cluster1)
  
  # Prepare plotting data
  plot_data <- pooled_gap_results %>%
    left_join(descriptive_labels_df, by = "Multiverse_ID") %>%
    mutate(
      # Create confidence intervals
      ci_lower = pooled_mean_diff - 1.96 * pooled_se,
      ci_upper = pooled_mean_diff + 1.96 * pooled_se,
      
      # Order multiverses
      Multiverse_ID = factor(Multiverse_ID, levels = multiverse_order),
      # CHANGED: Use Simple_ID for X-axis ordering
      Simple_ID = factor(Simple_ID, levels = descriptive_labels_df$Simple_ID),
      
      # Order gap types
      Gap_Type = factor(Gap_Type, levels = c("High-Low", "Medium-Low", "High-Medium")),
      
      # Order variable types
      Variable_Type = factor(Variable_Type, levels = c("Books at Home", "Parent Education", "Home Possessions"))
    ) %>%
    arrange(Multiverse_ID, Gap_Type)
  
  # Return Simple_ID levels in labels_df for use in decision data
  return(list(plot_data = plot_data, labels_df = descriptive_labels_df))
}


# 1.2 UPDATED: Create Decision Table Data to generate Color_Key
create_decision_data <- function(multiverse_rules_cluster1, labels_df) {
  
  # 1. Process rules into long format
  decision_data_raw <- map_dfr(multiverse_rules_cluster1, function(rule) {
    
    levels_list <- list(
      data.frame(Category_Level = rule$low_levels, SES_Group = "Low"),
      data.frame(Category_Level = rule$medium_levels, SES_Group = "Medium"),
      data.frame(Category_Level = rule$high_levels, SES_Group = "High")
    )
    
    # Combine levels and add Multiverse ID
    bind_rows(levels_list) %>%
      mutate(Multiverse_ID = rule$name)
  })
  
  # 2. Join with descriptive labels (which now include Raw_Variable_Name) and apply factors
  decision_data <- decision_data_raw %>%
    left_join(labels_df, by = "Multiverse_ID") %>%
    mutate(
      # CHANGED: Use Simple_ID for the X-axis factor
      Simple_ID = factor(Simple_ID, levels = labels_df$Simple_ID),
      Variable_Type = factor(Variable_Type, levels = c("Books at Home", "Parent Education", "Home Possessions")),
      
      # Order SES groups for consistent coloring/tiling
      SES_Group = factor(SES_Group, levels = c("High", "Medium", "Low")), 
      
      # Category Level must be a factor for proper plotting
      Category_Level = factor(Category_Level),
      
      # CRITICAL: Create the complex Color Key for the 9-color palette (e.g., High_BSBG04)
      Color_Key = paste0(SES_Group, "_", Raw_Variable_Name)
    ) %>%
    # Ensure Color_Key factor levels match the palette definition for scale_fill_manual
    mutate(Color_Key = factor(Color_Key, levels = names(color_palette_cluster1))) %>%
    arrange(Simple_ID, Category_Level)
  
  return(decision_data)
}


#############################
# 2. Create Plot Objects
#############################

# Assume pooled_gap_results and multiverse_rules_cluster1 are defined
plot_list <- create_gap_plot_data(pooled_gap_results, multiverse_rules_cluster1)
plot_data <- plot_list$plot_data
labels_df <- plot_list$labels_df
decision_plot_data <- create_decision_data(multiverse_rules_cluster1, labels_df)
variable_colors <- c(
  "Books at Home" = "#2E86AB",
  "Parent Education" = "#A23B72", 
  "Home Possessions" = "#2E8B57"
)

# 2.1 Main Gap Plot (Modified to remove x-axis labels)

multiverse_gap_plot_apa <- ggplot(plot_data, aes(x = Simple_ID, y = pooled_mean_diff, color = Variable_Type)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3, alpha = 0.7) +
  
  facet_wrap(~Gap_Type, ncol = 1, scales = "fixed", 
             labeller = labeller(Gap_Type = c(
               "High-Low" = "Achievement gap (High SES vs. low SES)",
               "Medium-Low" = "Achievement gap (Medium SES vs. low SES)", 
               "High-Medium" = "Achievement gap (High SES vs. medium SES)"
             ))) +
  
  scale_color_manual(values = variable_colors, name = "SES variable") +
  
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, # X-AXIS LABEL REMOVED
    y = "Mean difference in achievement points (95% CI)"
  ) +
  
  coord_cartesian(ylim = c(-100, 150)) + 
  
  theme_minimal(base_size = 12) + 
  theme(
    # REMOVED X-AXIS TEXT AND TICKS for the main plot
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    
    strip.text = element_text(face = "plain", size = 10, hjust = 0),
    plot.title = element_text(hjust = 0, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0, face = "italic", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "plain"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 0, 10) # Reduced bottom margin
  )


# 2.2 Decision Table Plot (geom_tile)
decision_plot <- ggplot(decision_plot_data, aes(x = Simple_ID, y = Category_Level, fill = Color_Key)) + # CRITICAL: Changed fill aesthetic to Color_Key
  
  # Use geom_tile to create the visual map of cutpoints
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Facet by variable type to align with the grouping of points above
  facet_grid(~Variable_Type, scales = "free_x", space = "free_x") +
  
  # CRITICAL: Use the 9-color gradient. Manually control legend appearance.
  scale_fill_manual(
    values = color_palette_cluster1, 
    name = "Assigned SES Group",
    # Set breaks to only show High, Medium, Low using a single variable's color gradient (BSBG04)
    breaks = c("High_BSBG04", "Medium_BSBG04", "Low_BSBG04"),
    labels = c("High", "Medium", "Low")
  ) +
  
  labs(
    # SIMPLIFIED X-AXIS LABEL
    x = "Multiverse definition", 
    y = "Original Category Level"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # X-axis labels are visible and rotated for readability
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 5)),
    
    # Hide the variable type strip/title which is redundant with the main plot's coloring
    strip.text = element_blank(), 
    panel.spacing = unit(0.1, "lines"), # Very small spacing between facets
    
    # Position the legend to the right, separate from the main plot's legend
    legend.position = "bottom", 
    legend.title = element_text(face = "plain"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 10, 10, 10) # Reduced top margin
  )

#############################
# 3. Combine Plots (using patchwork)
#############################

# The `multiverse_gap_plot_apa` is above, and `decision_plot` is below.
# Use `plot_layout` to control the relative height of the panels.
final_plot <- multiverse_gap_plot_apa / decision_plot +
  plot_layout(heights = c(3, 1))

# Display the plot
print(final_plot)





# Optional: Save the plot
# ggsave("multiverse_achievement_gaps.png", multiverse_gap_plot, 
#        width = 16, height = 12, dpi = 300, units = "in")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
gap_summary <- plot_data %>%
  group_by(Gap_Type, Variable_Type) %>%
  summarise(
    n_multiverses = n(),
    min_gap = min(pooled_mean_diff),
    max_gap = max(pooled_mean_diff),
    range = max_gap - min_gap,
    mean_gap = mean(pooled_mean_diff),
    .groups = 'drop'
  )

print(gap_summary)

# Show range of gaps by type
cat("\n=== RANGE OF GAPS BY TYPE ===\n")
overall_range <- plot_data %>%
  group_by(Gap_Type) %>%
  summarise(
    min_gap = min(pooled_mean_diff),
    max_gap = max(pooled_mean_diff),
    range = max_gap - min_gap,
    .groups = 'drop'
  )

print(overall_range)

# Main objects: 
#### Proportiongs of respondents per level 
final_pooled_results
# pooled_results_cluster1


################################################################
# R square
################################################################
########################
# Function to estimate R2
########################

calculate_survey_r2_simple <- function(svy_design, ses_col_name, pv_var_name) {
  
  # Check if variables exist
  if (!(ses_col_name %in% names(svy_design$variables)) ||
      !(pv_var_name %in% names(svy_design$variables))) {
    return(data.frame(
      R_squared = NA_real_,
      F_statistic = NA_real_,
      p_value = NA_real_
    ))
  }
  
  tryCatch({
    # Fit the survey regression model
    model_formula <- as.formula(paste(pv_var_name, "~", ses_col_name))
    svy_model <- svyglm(model_formula, design = svy_design, family = gaussian())
    
    # Extract R² using a simple method that works reliably
    # Method: Compare null model vs full model deviances
    
    # Fit null model (intercept only)
    null_formula <- as.formula(paste(pv_var_name, "~ 1"))
    null_model <- svyglm(null_formula, design = svy_design, family = gaussian())
    
    # Calculate pseudo-R² using deviance
    null_deviance <- null_model$deviance
    model_deviance <- svy_model$deviance
    
    # R² = 1 - (model deviance / null deviance)
    if (!is.null(null_deviance) && !is.null(model_deviance) && 
        null_deviance > 0) {
      r_squared <- 1 - (model_deviance / null_deviance)
      r_squared <- pmax(0, pmin(1, r_squared))  # Bound between 0 and 1
    } else {
      r_squared <- NA_real_
    }
    
    # Get F-statistic and p-value from anova if possible
    f_stat <- NA_real_
    p_val <- NA_real_
    
    tryCatch({
      anova_result <- anova(svy_model, null_model)
      if (!is.null(anova_result) && nrow(anova_result) >= 2) {
        f_stat <- anova_result$F[2]
        p_val <- anova_result$`Pr(>F)`[2]
      }
    }, error = function(e) {
      # If anova fails, try alternative
      tryCatch({
        summary_result <- summary(svy_model)
        if (!is.null(summary_result$fstatistic)) {
          f_stat <- summary_result$fstatistic[1]
          p_val <- pf(f_stat, summary_result$fstatistic[2], 
                      summary_result$fstatistic[3], lower.tail = FALSE)
        }
      }, error = function(e2) {
        # Leave as NA
      })
    })
    
    return(data.frame(
      R_squared = r_squared,
      F_statistic = f_stat,
      p_value = p_val
    ))
    
  }, error = function(e) {
    return(data.frame(
      R_squared = NA_real_,
      F_statistic = NA_real_,
      p_value = NA_real_
    ))
  })
}

########################
# Pooling function
########################

simple_r2_pooling <- function(r_squared_values, f_stats, p_values) {
  # Remove NA values
  valid_r2 <- r_squared_values[!is.na(r_squared_values)]
  valid_f <- f_stats[!is.na(f_stats)]
  valid_p <- p_values[!is.na(p_values)]
  
  if (length(valid_r2) == 0) {
    return(list(
      r_squared = NA_real_,
      r_squared_se = NA_real_,
      f_statistic = NA_real_,
      p_value = NA_real_
    ))
  }
  
  # Pool R² values
  pooled_r2 <- mean(valid_r2)
  r2_se <- sqrt(var(valid_r2) * (1 + 1/length(valid_r2)))
  
  # Pool other statistics
  pooled_f <- if (length(valid_f) > 0) mean(valid_f) else NA_real_
  pooled_p <- if (length(valid_p) > 0) mean(valid_p) else NA_real_
  
  return(list(
    r_squared = pooled_r2,
    r_squared_se = r2_se,
    f_statistic = pooled_f,
    p_value = pooled_p
  ))
}

########################
# Applying R2 function to each imputed dataset
########################

plausible_value_vars <- paste0("BSSSCI0", 1:5)
all_r2_results <- list()

cat("Calculating R² values with simple method...\n")

for (i in seq_along(DataImp)) {
  current_df <- DataImp[[i]]
  pv_var_name <- plausible_value_vars[i]
  
  cat(paste("Processing imputation", i, "with", pv_var_name, "\n"))
  
  for (rule in multiverse_rules_cluster1) {
    ses_col_name <- paste0("SES_", rule$name)
    if (ses_col_name %in% names(current_df)) {
      current_df[[ses_col_name]] <- factor(current_df[[ses_col_name]], levels = c("Low", "Medium", "High"))
      
      # Check if we have all three levels
      level_counts <- table(current_df[[ses_col_name]], useNA = "ifany")
      if (length(level_counts) < 3 || any(level_counts < 5)) {
        # Skip this multiverse if insufficient data
        next
      }
      
      svy_design <- svydesign(
        ids = ~IDSCHOOL,
        strata = ~JKZONE,
        weights = ~TOTWGT,
        data = current_df,
        nest = TRUE
      )
      
      r2_stats <- calculate_survey_r2_simple(svy_design, ses_col_name, pv_var_name)
      r2_stats$Imputation_ID <- i
      r2_stats$Multiverse_ID <- rule$name
      all_r2_results[[length(all_r2_results) + 1]] <- r2_stats
    }
  }
}

########################
# Pool the r2 results
########################

if (length(all_r2_results) > 0) {
  raw_r2_data <- do.call(rbind, all_r2_results)
  cat("Successfully calculated", nrow(raw_r2_data), "R² estimates\n")
  
  # Pool results
  pooled_r2_results <- raw_r2_data %>%
    group_by(Multiverse_ID) %>%
    summarise(
      pooled_result = list(simple_r2_pooling(R_squared, F_statistic, p_value)),
      .groups = 'drop'
    ) %>%
    mutate(
      R_squared = purrr::map_dbl(pooled_result, ~.x$r_squared),
      R_squared_SE = purrr::map_dbl(pooled_result, ~.x$r_squared_se),
      F_statistic = purrr::map_dbl(pooled_result, ~.x$f_statistic),
      p_value = purrr::map_dbl(pooled_result, ~.x$p_value)
    ) %>%
    select(Multiverse_ID, R_squared, R_squared_SE, F_statistic, p_value)
  
  cat("Pooled R² results:\n")
  print(head(pooled_r2_results, 10))
  
  # Quick check of results
  cat("\nSummary statistics:\n")
  cat("Range of R²:", round(range(pooled_r2_results$R_squared, na.rm = TRUE), 4), "\n")
  cat("Mean R²:", round(mean(pooled_r2_results$R_squared, na.rm = TRUE), 4), "\n")
  cat("Number with valid R²:", sum(!is.na(pooled_r2_results$R_squared)), "\n")
  
} else {
  cat("No R² results calculated - check your data\n")
}

pooled_r2_results
########################
# Plot
########################

#############################################################
# second option
######################################################################




if (exists("pooled_r2_results") && nrow(pooled_r2_results) > 0) {
  
  # Step 1: Create the custom order for the multiverses
  multiverse_order <- c(
    # BSBG04 (Books at Home)
    "BSBG04_MV1", "BSBG04_MV2", "BSBG04_MV3", "BSBG04_MV4", "BSBG04_MV5", "BSBG04_MV6",
    # BSBG06C (Parent Education)
    "BSBG06C_MV1", "BSBG06C_MV2", "BSBG06C_MV3", "BSBG06C_MV4", "BSBG06C_MV5", "BSBG06C_MV6",
    "BSBG06C_MV7", "BSBG06C_MV8", "BSBG06C_MV9", "BSBG06C_MV10", "BSBG06C_MV11", "BSBG06C_MV12",
    "BSBG06C_MV13", "BSBG06C_MV14", "BSBG06C_MV15",
    # BSBG05_Score (Home Possessions)
    "BSBG05_Score_MV1", "BSBG05_Score_MV2", "BSBG05_Score_MV3", "BSBG05_Score_MV4", "BSBG05_Score_MV5",
    "BSBG05_Score_MV6", "BSBG05_Score_MV7", "BSBG05_Score_MV8", "BSBG05_Score_MV9", "BSBG05_Score_MV10",
    "BSBG05_Score_MV11", "BSBG05_Score_MV12", "BSBG05_Score_MV13", "BSBG05_Score_MV14", "BSBG05_Score_MV15",
    "BSBG05_Score_MV16", "BSBG05_Score_MV17", "BSBG05_Score_MV18", "BSBG05_Score_MV19", "BSBG05_Score_MV20",
    "BSBG05_Score_MV21"
  )
  
  # Step 2: Create the plot data and set the factor levels
  plot_data <- pooled_r2_results %>%
    mutate(
      Variable_Type = case_when(
        grepl("^BSBG04", Multiverse_ID) ~ "Books at Home",
        grepl("^BSBG06C", Multiverse_ID) ~ "Parent Education",
        grepl("^BSBG05_Score", Multiverse_ID) ~ "Home Possessions"
      ),
      R_squared_pct = R_squared * 100,
      ci_lower = pmax(0, R_squared - 1.96 * R_squared_SE) * 100,
      ci_upper = pmin(1, R_squared + 1.96 * R_squared_SE) * 100,
      is_significant = p_value < 0.05,
      # Order the Multiverse_ID factor using your custom order vector
      Multiverse_ID = factor(Multiverse_ID, levels = multiverse_order)
    )
  
  # Step 3: Create the plot using facet_grid
  r2_plot <- ggplot(plot_data, aes(x = Multiverse_ID, y = R_squared_pct)) +
    geom_point(aes(color = Variable_Type), size = 2.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = Variable_Type), 
                  width = 0.3, alpha = 0.6) +
    scale_color_manual(values = c("Books at Home" = "#2E86AB",
                                  "Parent Education" = "#A23B72", 
                                  "Home Possessions" = "#2E8B57")) +
    labs(
      title = "Norway's R² Across SES Multiverse Definitions",
      y = "R² (% Variance Explained)",
      x = "Multiverse ID"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position="inside",
      legend.position.inside = c(0.9,0.9),
      strip.text = element_text(size = 12, face = "bold") # Make facet labels more readable
    )
  
  print(r2_plot)
}
####################################################################
if (exists("pooled_r2_results") && nrow(pooled_r2_results) > 0) {
  
  # Step 1: Create descriptive labels based on your multiverse rules
  # You'll need to replace this with your actual Chile multiverse_rules list
  # For now, I'm showing the pattern - you should use your actual Chile multiverse rules
  
  descriptive_labels_df <- map_dfr(multiverse_rules_cluster1, function(rule) {
    prefix <- case_when(
      grepl("^BSBG04", rule$name) ~ "B",
      grepl("^BSBG06C", rule$name) ~ "Ed", 
      grepl("^BSBG05_Score", rule$name) ~ "P"
    )
    
    low_label <- paste(rule$low_levels, collapse = ",")
    medium_label <- paste(rule$medium_levels, collapse = ",")
    high_label <- paste(rule$high_levels, collapse = ",")
    
    full_label <- paste0(
      prefix, "_", 
      sub(".*(MV[0-9]+)", "\\1", rule$name),
      "\nL=", low_label, 
      " M=", medium_label, 
      " H=", high_label
    )
    
    data.frame(
      Multiverse_ID = rule$name, 
      Descriptive_Label = full_label
    )
  })
  
  # Step 2: Create ordering (same as before)
  multiverse_order <- c(
    # BSBG04 (Books at Home)
    "BSBG04_MV1", "BSBG04_MV2", "BSBG04_MV3", "BSBG04_MV4", "BSBG04_MV5", "BSBG04_MV6",
    # BSBG06C (Parent Education)
    "BSBG06C_MV1", "BSBG06C_MV2", "BSBG06C_MV3", "BSBG06C_MV4", "BSBG06C_MV5", "BSBG06C_MV6",
    "BSBG06C_MV7", "BSBG06C_MV8", "BSBG06C_MV9", "BSBG06C_MV10", "BSBG06C_MV11", "BSBG06C_MV12",
    "BSBG06C_MV13", "BSBG06C_MV14", "BSBG06C_MV15",
    # BSBG05_Score (Home Possessions)
    "BSBG05_Score_MV1", "BSBG05_Score_MV2", "BSBG05_Score_MV3", "BSBG05_Score_MV4", "BSBG05_Score_MV5",
    "BSBG05_Score_MV6", "BSBG05_Score_MV7", "BSBG05_Score_MV8", "BSBG05_Score_MV9", "BSBG05_Score_MV10",
    "BSBG05_Score_MV11", "BSBG05_Score_MV12", "BSBG05_Score_MV13", "BSBG05_Score_MV14", "BSBG05_Score_MV15",
    "BSBG05_Score_MV16", "BSBG05_Score_MV17", "BSBG05_Score_MV18", "BSBG05_Score_MV19", "BSBG05_Score_MV20",
    "BSBG05_Score_MV21"
  )
  
  # Filter to existing IDs
  actual_multiverse_ids <- unique(pooled_r2_results$Multiverse_ID)
  multiverse_order_filtered <- multiverse_order[multiverse_order %in% actual_multiverse_ids]
  
  # Step 3: Create plot data with descriptive labels
  plot_data <- pooled_r2_results %>%
    left_join(descriptive_labels_df, by = "Multiverse_ID") %>%
    mutate(
      Variable_Type = case_when(
        grepl("^BSBG04", Multiverse_ID) ~ "Books at Home",
        grepl("^BSBG06C", Multiverse_ID) ~ "Parent Education", 
        grepl("^BSBG05_Score", Multiverse_ID) ~ "Home Possessions"
      ),
      R_squared_pct = R_squared * 100,
      ci_lower = pmax(0, R_squared - 1.96 * R_squared_SE) * 100,
      ci_upper = pmin(1, R_squared + 1.96 * R_squared_SE) * 100,
      is_significant = p_value < 0.05,
      
      # Order both the IDs and labels consistently
      Multiverse_ID = factor(Multiverse_ID, levels = multiverse_order_filtered),
      Descriptive_Label = factor(Descriptive_Label, levels = descriptive_labels_df$Descriptive_Label[match(multiverse_order_filtered, descriptive_labels_df$Multiverse_ID)])
    )
  
  # Step 4: Create the plot with descriptive labels
  r2_plot <- ggplot(plot_data, aes(x = Descriptive_Label, y = R_squared_pct)) +
    geom_point(aes(color = Variable_Type), size = 2.5) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = Variable_Type), 
                  width = 0.3, alpha = 0.6) +
    scale_color_manual(values = c("Books at Home" = "#2E86AB",
                                  "Parent Education" = "#A23B72", 
                                  "Home Possessions" = "#2E8B57")) +
    labs(
      title = NULL,
      subtitle = NULL,
      y = "R² (% Variance Accounted For)",
      x = "Multiverse Definition"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.9, 0.9),
      strip.text = element_blank()
    )
  
  print(r2_plot)
}

#adding plot
final_plot_r2 <- r2_plot / decision_plot +
  plot_layout(heights = c(3, 1))

# Display the plot
print(final_plot_r2)


# it do be like that