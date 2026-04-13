if (file.exists("questionnaire/section_utils.R")) {
	source("questionnaire/section_utils.R")
} else {
	source("section_utils.R")
}

questionnaire_data <- get_questionnaire_data()

cols <- c(
	"pid",
	"activity_mod_days_1_1", "activity_mod_days_2_1", "activity_mod_mins_1_1", "activity_mod_mins_2_1",
	"activity_stairs_1_1", "activity_transport_1_m", "activity_type_1_m", "activity_type_diy_heavy_1_1",
	"activity_type_diy_heavy_dur_1_1", "activity_type_diy_light_1_1", "activity_type_diy_light_dur_1_1",
	"activity_type_exercise_1_1", "activity_type_exercise_dur_1_1", "activity_type_stren_1_1", "activity_type_stren_dur_1_1",
	"activity_type_walk_1_1", "activity_type_walk_dur_1_1", "activity_vig_days_1_1", "activity_vig_days_2_1",
	"activity_vig_mins_1_1", "activity_vig_mins_2_1", "activity_walk_days_1_1", "activity_walk_days_2_1",
	"activity_walk_mins_1_1", "activity_walk_mins_2_1", "activity_walk_pace_1_1",
	"alcohol_beer_mth_1_1", "alcohol_beer_mth_2_1", "alcohol_beer_wk_1_1", "alcohol_chg_1_1",
	"alcohol_chg_abst_reason_1_1", "alcohol_chg_abst_reason_2_m", "alcohol_chg_reduce_reason_1_1",
	"alcohol_chg_reduce_reason_2_m", "alcohol_curr_1_1", "alcohol_food_1_1", "alcohol_other_mth_1_1",
	"alcohol_other_mth_2_1", "alcohol_other_wk_1_1", "alcohol_prev_1_1", "alcohol_spirits_mth_1_1",
	"alcohol_spirits_mth_2_1", "alcohol_spirits_wk_1_1", "alcohol_wine_fort_mth_1_1", "alcohol_wine_fort_mth_2_1",
	"alcohol_wine_fort_wk_1_1", "alcohol_wine_red_mth_1_1", "alcohol_wine_red_mth_2_1", "alcohol_wine_red_wk_1_1",
	"alcohol_wine_white_mth_1_1", "alcohol_wine_white_mth_2_1", "alcohol_wine_white_wk_1_1",
	"lifestyle_drive_hrs_1_1", "lifestyle_outdoor_sum_hrs_1_1", "lifestyle_outdoor_win_hrs_1_1",
	"lifestyle_screen_pc_hrs_1_1", "lifestyle_screen_pc_hrs_2_1", "lifestyle_screen_tv_hrs_1_1", "lifestyle_screen_tv_hrs_2_1",
	"lifestyle_social_rec_1_m", "lifestyle_social_visits_1_1", "sleep_chronotype_1_1", "sleep_daytime_1_1",
	"sleep_hrs_1_1", "sleep_napping_1_1", "sleep_snoring_1_1", "sleep_trouble_1_1", "sleep_waking_1_1",
	"smoke_100_times_1_1", "smoke_100_times_2_1", "smoke_avg_1_1", "smoke_avg_2_1", "smoke_avg_prev_1_1",
	"smoke_chg_1_1", "smoke_chg_2_1", "smoke_chg_abst_1_1", "smoke_chg_abst_2_1", "smoke_chg_abst_reason_1_m",
	"smoke_chg_reduce_abst_1_1", "smoke_chg_reduce_abst_reason_1_m", "smoke_chg_reduce_reason_1_m", "smoke_chg_reduce_reason_2_m",
	"smoke_expose_house_hrs_1_1", "smoke_expose_outside_hrs_1_1", "smoke_exposure_1_1", "smoke_exposure_hrs_1_1",
	"smoke_first_age_1_1", "smoke_first_age_2_1", "smoke_house_1_1", "smoke_prev_age_1_1", "smoke_prev_age_2_1",
	"smoke_prev_reduce_reason_1_m", "smoke_prev_reduce_reason_2_m", "smoke_prev_reg_1_1", "smoke_prev_reg_2_1",
	"smoke_prev_type_1_1", "smoke_reg_1_m", "smoke_reg_day_1_1", "smoke_reg_day_2_1", "smoke_reg_first_age_1_1",
	"smoke_reg_first_age_2_1", "smoke_reg_type_1_1", "smoke_reg_type_2_1", "smoke_status_1_1", "smoke_status_2_1",
	"smoke_tobacco_age_1_1", "smoke_tobacco_prev_1_1", "smoke_tobacco_type_1_m", "smoke_vape_1_1", "smoke_vape_age_1_1",
	"smoke_vape_age_2_1", "smoke_vape_avg_1_1", "smoke_vape_avg_2_1", "smoke_vape_type_1_m", "smoke_vape_type_2_m",
	"smoke_vape_use_1_1", "smoke_vape_use_2_1"
)

questionnaire_data <- ensure_columns(questionnaire_data, cols)
questionnaire_data <- fill_defaults_for_columns(questionnaire_data, cols)
section_df <- questionnaire_data[, cols, drop = FALSE]

n <- nrow(section_df)

# Determine v1/v2 index split.
if ("questionnaire_version" %in% names(questionnaire_data)) {
	idx_v1 <- which(questionnaire_data$questionnaire_version == 1)
	idx_v2 <- which(questionnaire_data$questionnaire_version == 2)
} else {
	idx_v1 <- sample(seq_len(n), size = max(1L, round(0.05 * n)))
	idx_v2 <- setdiff(seq_len(n), idx_v1)
}

# Columns with no verified reference-generation logic must remain NA.
na_cols <- c(
	"lifestyle_drive_hrs_1_1", "lifestyle_outdoor_sum_hrs_1_1", "lifestyle_outdoor_win_hrs_1_1",
	"sleep_hrs_1_1", "smoke_100_times_1_1", "smoke_100_times_2_1",
	"smoke_reg_first_age_1_1", "smoke_reg_first_age_2_1"
)
for (col in na_cols) section_df[[col]] <- rep(NA_character_, n)

# ===========================================================================
# ACTIVITY
# ===========================================================================

section_df$activity_walk_days_1_1 <- rep(NA_integer_, n)
section_df$activity_walk_days_2_1 <- rep(NA_integer_, n)
section_df$activity_mod_days_1_1 <- rep(NA_integer_, n)
section_df$activity_mod_days_2_1 <- rep(NA_integer_, n)
section_df$activity_vig_days_1_1 <- rep(NA_integer_, n)
section_df$activity_vig_days_2_1 <- rep(NA_integer_, n)

if (length(idx_v1) > 0) {
	section_df$activity_walk_days_1_1[idx_v1] <- sample(0:7, length(idx_v1), replace = TRUE, prob = c(0.10, 0.06, 0.08, 0.10, 0.14, 0.18, 0.18, 0.16))
	section_df$activity_mod_days_1_1[idx_v1] <- sample(0:7, length(idx_v1), replace = TRUE, prob = c(0.22, 0.14, 0.15, 0.15, 0.13, 0.10, 0.07, 0.04))
	section_df$activity_vig_days_1_1[idx_v1] <- sample(0:7, length(idx_v1), replace = TRUE, prob = c(0.35, 0.18, 0.16, 0.12, 0.08, 0.06, 0.03, 0.02))
}
if (length(idx_v2) > 0) {
	section_df$activity_walk_days_2_1[idx_v2] <- sample(0:7, length(idx_v2), replace = TRUE, prob = c(0.10, 0.06, 0.08, 0.10, 0.14, 0.18, 0.18, 0.16))
	section_df$activity_mod_days_2_1[idx_v2] <- sample(0:7, length(idx_v2), replace = TRUE, prob = c(0.22, 0.14, 0.15, 0.15, 0.13, 0.10, 0.07, 0.04))
	section_df$activity_vig_days_2_1[idx_v2] <- sample(0:7, length(idx_v2), replace = TRUE, prob = c(0.35, 0.18, 0.16, 0.12, 0.08, 0.06, 0.03, 0.02))
}

section_df$activity_walk_mins_1_1 <- rep(NA_integer_, n)
section_df$activity_walk_mins_2_1 <- rep(NA_integer_, n)
section_df$activity_mod_mins_1_1 <- rep(NA_integer_, n)
section_df$activity_mod_mins_2_1 <- rep(NA_integer_, n)
section_df$activity_vig_mins_1_1 <- rep(NA_integer_, n)
section_df$activity_vig_mins_2_1 <- rep(NA_integer_, n)

if (length(idx_v1) > 0) {
	section_df$activity_walk_mins_1_1[idx_v1] <- round(pmax(0, section_df$activity_walk_days_1_1[idx_v1] * rnorm(length(idx_v1), mean = 28, sd = 12)))
	section_df$activity_mod_mins_1_1[idx_v1] <- round(pmax(0, section_df$activity_mod_days_1_1[idx_v1] * rnorm(length(idx_v1), mean = 35, sd = 16)))
	section_df$activity_vig_mins_1_1[idx_v1] <- round(pmax(0, section_df$activity_vig_days_1_1[idx_v1] * rnorm(length(idx_v1), mean = 24, sd = 14)))
}
if (length(idx_v2) > 0) {
	section_df$activity_walk_mins_2_1[idx_v2] <- round(pmax(0, section_df$activity_walk_days_2_1[idx_v2] * rnorm(length(idx_v2), mean = 28, sd = 12)))
	section_df$activity_mod_mins_2_1[idx_v2] <- round(pmax(0, section_df$activity_mod_days_2_1[idx_v2] * rnorm(length(idx_v2), mean = 35, sd = 16)))
	section_df$activity_vig_mins_2_1[idx_v2] <- round(pmax(0, section_df$activity_vig_days_2_1[idx_v2] * rnorm(length(idx_v2), mean = 24, sd = 14)))
}

section_df$activity_walk_pace_1_1 <- sample(
	c("Slow pace", "Steady average pace", "Brisk pace", "Fast pace", "Do not know", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.12, 0.48, 0.25, 0.08, 0.03, 0.04)
)
section_df$activity_stairs_1_1 <- sample(
	c("None", "1-5 times a day", "6-10 times a day", "11-15 times a day", "16-20 times a day", "More than 20 times a day", "Do not know", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.14, 0.35, 0.22, 0.12, 0.07, 0.04, 0.03, 0.03)
)
section_df$activity_transport_1_m <- sample(
	c("Car or motor vehicle", "Public transport", "Walk", "Cycle", "Mixed modes", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.52, 0.10, 0.15, 0.05, 0.14, 0.04)
)

activity_primary <- sample(
	c("None of the above", "Walking for pleasure (not as a means of transport)", "Other exercises (e.g., swimming, cycling, keep fit, bowling)", "Strenuous sports", "Light DIY (e.g., DIY that does not require a lot of physical effort)", "Heavy DIY (e.g., DIY that requires a lot of physical effort)", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.14, 0.24, 0.21, 0.10, 0.14, 0.10, 0.07)
)
activity_secondary <- sample(
	c("Walking for pleasure (not as a means of transport)", "Other exercises (e.g., swimming, cycling, keep fit, bowling)", "Strenuous sports", "Light DIY (e.g., DIY that does not require a lot of physical effort)", "Heavy DIY (e.g., DIY that requires a lot of physical effort)"),
	n, replace = TRUE
)
add_second_activity <- runif(n) < 0.22

section_df$activity_type_1_m <- activity_primary
for (i in seq_len(n)) {
	if (isTRUE(add_second_activity[i]) &&
		!activity_primary[i] %in% c("None of the above", "Prefer not to answer") &&
		activity_primary[i] != activity_secondary[i]) {
		section_df$activity_type_1_m[i] <- paste(activity_primary[i], activity_secondary[i], sep = "; ")
	}
}

activity_freq_levels <- c("Once in the last 4 weeks", "2-3 times in the last 4 weeks", "Once a week", "2-3 times a week", "4-5 times a week", "Every day", "Do not know", "Prefer not to answer")
activity_dur_levels <- c("Less than 15 minutes", "Between 15 and 30 minutes", "Between 30 minutes and 1 hour", "Between 1 hour and 1.5 hours", "Between 1.5 hours and 2 hours", "Between 2 and 3 hours", "Over 3 hours", "Do not know", "Prefer not to answer")

walk_idx <- which(grepl("Walking for pleasure", section_df$activity_type_1_m, fixed = TRUE))
exercise_idx <- which(grepl("Other exercises", section_df$activity_type_1_m, fixed = TRUE))
stren_idx <- which(grepl("Strenuous sports", section_df$activity_type_1_m, fixed = TRUE))
light_diy_idx <- which(grepl("Light DIY", section_df$activity_type_1_m, fixed = TRUE))
heavy_diy_idx <- which(grepl("Heavy DIY", section_df$activity_type_1_m, fixed = TRUE))

section_df$activity_type_walk_1_1 <- rep(NA_character_, n)
section_df$activity_type_walk_dur_1_1 <- rep(NA_character_, n)
section_df$activity_type_exercise_1_1 <- rep(NA_character_, n)
section_df$activity_type_exercise_dur_1_1 <- rep(NA_character_, n)
section_df$activity_type_stren_1_1 <- rep(NA_character_, n)
section_df$activity_type_stren_dur_1_1 <- rep(NA_character_, n)
section_df$activity_type_diy_light_1_1 <- rep(NA_character_, n)
section_df$activity_type_diy_light_dur_1_1 <- rep(NA_character_, n)
section_df$activity_type_diy_heavy_1_1 <- rep(NA_character_, n)
section_df$activity_type_diy_heavy_dur_1_1 <- rep(NA_character_, n)

if (length(walk_idx) > 0) {
	section_df$activity_type_walk_1_1[walk_idx] <- sample(activity_freq_levels, length(walk_idx), replace = TRUE, prob = c(0.18, 0.20, 0.20, 0.20, 0.10, 0.05, 0.03, 0.04))
	section_df$activity_type_walk_dur_1_1[walk_idx] <- sample(activity_dur_levels, length(walk_idx), replace = TRUE, prob = c(0.10, 0.22, 0.30, 0.18, 0.08, 0.05, 0.03, 0.02, 0.02))
}
if (length(exercise_idx) > 0) {
	section_df$activity_type_exercise_1_1[exercise_idx] <- sample(activity_freq_levels, length(exercise_idx), replace = TRUE, prob = c(0.20, 0.22, 0.20, 0.18, 0.10, 0.05, 0.02, 0.03))
	section_df$activity_type_exercise_dur_1_1[exercise_idx] <- sample(activity_dur_levels, length(exercise_idx), replace = TRUE, prob = c(0.08, 0.18, 0.30, 0.20, 0.10, 0.07, 0.03, 0.02, 0.02))
}
if (length(stren_idx) > 0) {
	section_df$activity_type_stren_1_1[stren_idx] <- sample(activity_freq_levels, length(stren_idx), replace = TRUE, prob = c(0.24, 0.22, 0.18, 0.16, 0.08, 0.04, 0.03, 0.05))
	section_df$activity_type_stren_dur_1_1[stren_idx] <- sample(activity_dur_levels, length(stren_idx), replace = TRUE, prob = c(0.12, 0.22, 0.28, 0.16, 0.08, 0.06, 0.03, 0.03, 0.02))
}
if (length(light_diy_idx) > 0) {
	section_df$activity_type_diy_light_1_1[light_diy_idx] <- sample(activity_freq_levels, length(light_diy_idx), replace = TRUE, prob = c(0.16, 0.18, 0.20, 0.20, 0.12, 0.08, 0.03, 0.03))
	section_df$activity_type_diy_light_dur_1_1[light_diy_idx] <- sample(activity_dur_levels, length(light_diy_idx), replace = TRUE, prob = c(0.08, 0.18, 0.30, 0.20, 0.10, 0.07, 0.04, 0.02, 0.01))
}
if (length(heavy_diy_idx) > 0) {
	section_df$activity_type_diy_heavy_1_1[heavy_diy_idx] <- sample(activity_freq_levels, length(heavy_diy_idx), replace = TRUE, prob = c(0.20, 0.22, 0.20, 0.17, 0.09, 0.05, 0.03, 0.04))
	section_df$activity_type_diy_heavy_dur_1_1[heavy_diy_idx] <- sample(activity_dur_levels, length(heavy_diy_idx), replace = TRUE, prob = c(0.10, 0.20, 0.28, 0.18, 0.10, 0.08, 0.03, 0.02, 0.01))
}

# ===========================================================================
# SMOKING
# ===========================================================================

smoke_levels <- c("Yes, every day", "Yes, some days", "Yes, but rarely", "No, not at all", "Prefer not to answer")
section_df$smoke_status_1_1 <- rep(NA_character_, n)
section_df$smoke_status_2_1 <- rep(NA_character_, n)
if (length(idx_v1) > 0)
	section_df$smoke_status_1_1[idx_v1] <- sample(smoke_levels, length(idx_v1), replace = TRUE, prob = c(0.12, 0.08, 0.08, 0.68, 0.04))
if (length(idx_v2) > 0)
	section_df$smoke_status_2_1[idx_v2] <- sample(smoke_levels, length(idx_v2), replace = TRUE, prob = c(0.12, 0.08, 0.08, 0.68, 0.04))

all_smoke_status <- section_df$smoke_status_1_1
all_smoke_status[is.na(all_smoke_status)] <- section_df$smoke_status_2_1[is.na(all_smoke_status)]
current_smokers <- which(all_smoke_status %in% c("Yes, every day", "Yes, some days", "Yes, but rarely"))

section_df$smoke_prev_reg_1_1 <- rep(NA_character_, n)
section_df$smoke_prev_reg_2_1 <- rep(NA_character_, n)
if (length(idx_v1) > 0)
	section_df$smoke_prev_reg_1_1[idx_v1] <- sample(c("Yes", "No", "Prefer not to answer"), length(idx_v1), replace = TRUE, prob = c(0.28, 0.66, 0.06))
if (length(idx_v2) > 0)
	section_df$smoke_prev_reg_2_1[idx_v2] <- sample(c("Yes", "No", "Prefer not to answer"), length(idx_v2), replace = TRUE, prob = c(0.28, 0.66, 0.06))

if (length(current_smokers) > 0) {
	section_df$smoke_prev_reg_1_1[intersect(current_smokers, idx_v1)] <- "Yes"
	section_df$smoke_prev_reg_2_1[intersect(current_smokers, idx_v2)] <- "Yes"
}

all_prev_reg <- section_df$smoke_prev_reg_1_1
all_prev_reg[is.na(all_prev_reg)] <- section_df$smoke_prev_reg_2_1[is.na(all_prev_reg)]
ever_smokers <- which(all_prev_reg == "Yes" | all_smoke_status %in% c("Yes, every day", "Yes, some days", "Yes, but rarely"))
former_smokers <- setdiff(which(all_prev_reg == "Yes"), current_smokers)

section_df$smoke_tobacco_prev_1_1 <- sample(
	c("Smoked on most or all days", "Smoked occasionally", "Just tried once or twice", "I have never smoked", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.14, 0.16, 0.12, 0.53, 0.05)
)
if (length(ever_smokers) > 0) {
	section_df$smoke_tobacco_prev_1_1[ever_smokers] <- sample(
		c("Smoked on most or all days", "Smoked occasionally", "Just tried once or twice"),
		length(ever_smokers), replace = TRUE, prob = c(0.42, 0.34, 0.24)
	)
}

section_df$smoke_first_age_1_1 <- rep(NA_integer_, n)
section_df$smoke_first_age_2_1 <- rep(NA_integer_, n)
section_df$smoke_avg_1_1 <- rep(NA_integer_, n)
section_df$smoke_avg_2_1 <- rep(NA_integer_, n)
section_df$smoke_tobacco_age_1_1 <- rep(NA_integer_, n)
if (length(ever_smokers) > 0) {
	e1 <- intersect(ever_smokers, idx_v1)
	e2 <- intersect(ever_smokers, idx_v2)
	if (length(e1) > 0) {
		section_df$smoke_first_age_1_1[e1] <- pmin(45, pmax(8, round(rnorm(length(e1), mean = 17, sd = 4))))
		section_df$smoke_avg_1_1[e1] <- pmin(60, pmax(0, round(rlnorm(length(e1), meanlog = log(6), sdlog = 0.7))))
	}
	if (length(e2) > 0) {
		section_df$smoke_first_age_2_1[e2] <- pmin(45, pmax(8, round(rnorm(length(e2), mean = 17, sd = 4))))
		section_df$smoke_avg_2_1[e2] <- pmin(60, pmax(0, round(rlnorm(length(e2), meanlog = log(6), sdlog = 0.7))))
	}
	section_df$smoke_tobacco_age_1_1[ever_smokers] <- pmin(45, pmax(8, round(rnorm(length(ever_smokers), mean = 17, sd = 4))))
}

section_df$smoke_reg_type_1_1 <- rep(NA_character_, n)
section_df$smoke_reg_type_2_1 <- rep(NA_character_, n)
section_df$smoke_reg_day_1_1 <- rep(NA_integer_, n)
section_df$smoke_reg_day_2_1 <- rep(NA_integer_, n)
section_df$smoke_reg_1_m <- rep(NA_character_, n)
section_df$smoke_tobacco_type_1_m <- rep(NA_character_, n)
if (length(current_smokers) > 0) {
	c1 <- intersect(current_smokers, idx_v1)
	c2 <- intersect(current_smokers, idx_v2)
	if (length(c1) > 0) {
		section_df$smoke_reg_type_1_1[c1] <- sample(c("Manufactured cigarette", "Hand-rolled cigarettes", "None of the above", "Prefer not to answer"), length(c1), replace = TRUE, prob = c(0.54, 0.34, 0.05, 0.07))
		section_df$smoke_reg_day_1_1[c1] <- pmin(60, pmax(0, round(rlnorm(length(c1), meanlog = log(8), sdlog = 0.7))))
	}
	if (length(c2) > 0) {
		section_df$smoke_reg_type_2_1[c2] <- sample(c("Manufactured cigarette", "Hand-rolled cigarettes", "None of the above", "Prefer not to answer"), length(c2), replace = TRUE, prob = c(0.54, 0.34, 0.05, 0.07))
		section_df$smoke_reg_day_2_1[c2] <- pmin(60, pmax(0, round(rlnorm(length(c2), meanlog = log(8), sdlog = 0.7))))
	}
	section_df$smoke_reg_1_m[current_smokers] <- sample(c("Current regular smoker", "Current occasional smoker", "Prefer not to answer"), length(current_smokers), replace = TRUE, prob = c(0.66, 0.28, 0.06))
	section_df$smoke_tobacco_type_1_m[current_smokers] <- sample(c("Cigarettes", "Cigar", "Pipe", "Shisha", "Smokeless tobacco", "Prefer not to answer"), length(current_smokers), replace = TRUE, prob = c(0.72, 0.06, 0.05, 0.04, 0.05, 0.08))
}

section_df$smoke_prev_age_1_1 <- rep(NA_integer_, n)
section_df$smoke_prev_age_2_1 <- rep(NA_integer_, n)
if (length(former_smokers) > 0) {
	f1 <- intersect(former_smokers, idx_v1)
	f2 <- intersect(former_smokers, idx_v2)
	if (length(f1) > 0)
		section_df$smoke_prev_age_1_1[f1] <- pmin(85, pmax(12, round(rnorm(length(f1), mean = 39, sd = 12))))
	if (length(f2) > 0)
		section_df$smoke_prev_age_2_1[f2] <- pmin(85, pmax(12, round(rnorm(length(f2), mean = 39, sd = 12))))
}

chg_levels <- c("More nowadays?", "About the same?", "Less nowadays?", "Prefer not to answer")
section_df$smoke_chg_1_1 <- rep(NA_character_, n)
section_df$smoke_chg_2_1 <- rep(NA_character_, n)
section_df$smoke_chg_abst_1_1 <- rep(NA_character_, n)
section_df$smoke_chg_abst_2_1 <- rep(NA_character_, n)
if (length(ever_smokers) > 0) {
	ec1 <- intersect(ever_smokers, idx_v1)
	ec2 <- intersect(ever_smokers, idx_v2)
	if (length(ec1) > 0) {
		section_df$smoke_chg_1_1[ec1] <- sample(chg_levels, length(ec1), replace = TRUE, prob = c(0.10, 0.52, 0.30, 0.08))
		section_df$smoke_chg_abst_1_1[ec1] <- sample(c("Yes", "No", "Do not know", "Prefer not to answer"), length(ec1), replace = TRUE, prob = c(0.35, 0.48, 0.08, 0.09))
	}
	if (length(ec2) > 0) {
		section_df$smoke_chg_2_1[ec2] <- sample(chg_levels, length(ec2), replace = TRUE, prob = c(0.10, 0.52, 0.30, 0.08))
		section_df$smoke_chg_abst_2_1[ec2] <- sample(c("Yes", "No", "Do not know", "Prefer not to answer"), length(ec2), replace = TRUE, prob = c(0.35, 0.48, 0.08, 0.09))
	}
}

all_smoke_chg <- section_df$smoke_chg_1_1
all_smoke_chg[is.na(all_smoke_chg)] <- section_df$smoke_chg_2_1[is.na(all_smoke_chg)]
reduce_smoke_idx <- which(all_smoke_chg == "Less nowadays?")

section_df$smoke_prev_reduce_reason_1_m <- rep(NA_character_, n)
section_df$smoke_prev_reduce_reason_2_m <- rep(NA_character_, n)
section_df$smoke_chg_reduce_reason_1_m <- rep(NA_character_, n)
section_df$smoke_chg_reduce_reason_2_m <- rep(NA_character_, n)
section_df$smoke_chg_reduce_abst_1_1 <- rep(NA_character_, n)
section_df$smoke_chg_reduce_abst_reason_1_m <- rep(NA_character_, n)
if (length(reduce_smoke_idx) > 0) {
	r1 <- intersect(reduce_smoke_idx, idx_v1)
	r2 <- intersect(reduce_smoke_idx, idx_v2)
	if (length(r1) > 0) {
		section_df$smoke_prev_reduce_reason_1_m[r1] <- sample(c("Health reasons", "Cost", "Family/children", "Workplace restrictions", "Other"), length(r1), replace = TRUE, prob = c(0.48, 0.15, 0.10, 0.12, 0.15))
		section_df$smoke_chg_reduce_reason_1_m[r1] <- sample(c("Doctor advice", "Illness", "Pregnancy/fertility", "Cost", "Other"), length(r1), replace = TRUE, prob = c(0.28, 0.24, 0.06, 0.20, 0.22))
	}
	if (length(r2) > 0) {
		section_df$smoke_prev_reduce_reason_2_m[r2] <- sample(c("Health reasons", "Cost", "Family/children", "Workplace restrictions", "Other"), length(r2), replace = TRUE, prob = c(0.48, 0.15, 0.10, 0.12, 0.15))
		section_df$smoke_chg_reduce_reason_2_m[r2] <- sample(c("Doctor advice", "Illness", "Pregnancy/fertility", "Cost", "Other"), length(r2), replace = TRUE, prob = c(0.28, 0.24, 0.06, 0.20, 0.22))
	}
	section_df$smoke_chg_reduce_abst_1_1[reduce_smoke_idx] <- sample(c("Yes", "No", "Prefer not to answer"), length(reduce_smoke_idx), replace = TRUE, prob = c(0.34, 0.58, 0.08))
	abst_yes <- which(section_df$smoke_chg_reduce_abst_1_1 == "Yes")
	if (length(abst_yes) > 0) {
		section_df$smoke_chg_reduce_abst_reason_1_m[abst_yes] <- sample(c("Health concerns", "Family", "Financial", "Social reasons", "Other"), length(abst_yes), replace = TRUE, prob = c(0.45, 0.14, 0.12, 0.12, 0.17))
	}
}

abst_reason_idx <- which(section_df$smoke_chg_abst_1_1 == "Yes" | section_df$smoke_chg_abst_2_1 == "Yes")
section_df$smoke_chg_abst_reason_1_m <- rep(NA_character_, n)
if (length(abst_reason_idx) > 0) {
	section_df$smoke_chg_abst_reason_1_m[abst_reason_idx] <- sample(c("Health", "Pregnancy", "Financial", "Family influence", "Other"), length(abst_reason_idx), replace = TRUE, prob = c(0.48, 0.05, 0.16, 0.10, 0.21))
}

section_df$smoke_house_1_1 <- sample(
	c("Yes, one household member smokes", "Yes, more than one household member smokes", "No", "Do not know", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.18, 0.10, 0.64, 0.04, 0.04)
)
section_df$smoke_exposure_hrs_1_1 <- sample(
	c("Less than 1 hour per day", "1 to 2 hours per day", "3 to 5 hours per day", "6 to 9 hours per day", "10 to 15 hours per day", "More than 15 hours per day", "Prefer not to answer"),
	n, replace = TRUE, prob = c(0.36, 0.31, 0.19, 0.08, 0.03, 0.01, 0.02)
)
section_df$smoke_expose_outside_hrs_1_1 <- pmin(60, pmax(0, round(rlnorm(n, meanlog = log(3), sdlog = 1.0))))

section_df$smoke_vape_1_1 <- sample(c("Yes", "No", "Prefer not to answer"), n, replace = TRUE, prob = c(0.24, 0.71, 0.05))
vape_yes_idx <- which(section_df$smoke_vape_1_1 == "Yes")
section_df$smoke_vape_age_1_1 <- rep(NA_integer_, n)
section_df$smoke_vape_age_2_1 <- rep(NA_integer_, n)
section_df$smoke_vape_use_1_1 <- rep(NA_character_, n)
section_df$smoke_vape_use_2_1 <- rep(NA_character_, n)
section_df$smoke_vape_avg_1_1 <- rep(NA_character_, n)
section_df$smoke_vape_avg_2_1 <- rep(NA_character_, n)
section_df$smoke_vape_type_1_m <- rep(NA_character_, n)
section_df$smoke_vape_type_2_m <- rep(NA_character_, n)
if (length(vape_yes_idx) > 0) {
	v1 <- intersect(vape_yes_idx, idx_v1)
	v2 <- intersect(vape_yes_idx, idx_v2)
	if (length(v1) > 0) {
		section_df$smoke_vape_age_1_1[v1] <- pmin(80, pmax(10, round(rnorm(length(v1), mean = 31, sd = 11))))
		section_df$smoke_vape_use_1_1[v1] <- sample(c("I had never smoked tobacco cigarettes", "I was a current smoker of tobacco cigarettes and had no plans to quit", "I was a current smoker of tobacco cigarettes and was planning to quit", "I was a current smoker of tobacco cigarettes and was planning to", "I had stopped smoking tobacco cigarettes", "Prefer not to answer"), length(v1), replace = TRUE, prob = c(0.11, 0.23, 0.27, 0.12, 0.20, 0.07))
		section_df$smoke_vape_avg_1_1[v1] <- sample(c("Never", "Less than 1 time/mo", "2-3 times/mo", "1-2 times /week", "3-6 times /week", "1-4 times /day", "5-14 times /day", "15-24 times /day", "25-34 times /day", "35-44 times /day", "More than 45 times /day", "Prefer not to answer"), length(v1), replace = TRUE, prob = c(0.10, 0.08, 0.09, 0.18, 0.17, 0.16, 0.09, 0.05, 0.03, 0.02, 0.01, 0.02))
		section_df$smoke_vape_type_1_m[v1] <- sample(c("Nicotine e-liquid", "Non-nicotine e-liquid", "Both nicotine and non-nicotine", "Do not know", "Prefer not to answer"), length(v1), replace = TRUE, prob = c(0.48, 0.16, 0.23, 0.08, 0.05))
	}
	if (length(v2) > 0) {
		section_df$smoke_vape_age_2_1[v2] <- pmin(80, pmax(10, round(rnorm(length(v2), mean = 31, sd = 11))))
		section_df$smoke_vape_use_2_1[v2] <- sample(c("I had never smoked tobacco cigarettes", "I was a current smoker of tobacco cigarettes and had no plans to quit", "I was a current smoker of tobacco cigarettes and was planning to quit", "I was a current smoker of tobacco cigarettes and was planning to", "I had stopped smoking tobacco cigarettes", "Prefer not to answer"), length(v2), replace = TRUE, prob = c(0.11, 0.23, 0.27, 0.12, 0.20, 0.07))
		section_df$smoke_vape_avg_2_1[v2] <- sample(c("Never", "Less than 1 time/mo", "2-3 times/mo", "1-2 times /week", "3-6 times /week", "1-4 times /day", "5-14 times /day", "15-24 times /day", "25-34 times /day", "35-44 times /day", "More than 45 times /day", "Prefer not to answer"), length(v2), replace = TRUE, prob = c(0.10, 0.08, 0.09, 0.18, 0.17, 0.16, 0.09, 0.05, 0.03, 0.02, 0.01, 0.02))
		section_df$smoke_vape_type_2_m[v2] <- sample(c("Nicotine e-liquid", "Non-nicotine e-liquid", "Both nicotine and non-nicotine", "Do not know", "Prefer not to answer"), length(v2), replace = TRUE, prob = c(0.48, 0.16, 0.23, 0.08, 0.05))
	}
}

# Additional smoking columns generated in later reference pass.
section_df$smoke_avg_prev_1_1 <- rep(NA_real_, n)
if (length(ever_smokers) > 0) {
	n_ever <- length(ever_smokers)
	base_vals <- pmin(60, pmax(0, round(rlnorm(n_ever, meanlog = log(10), sdlog = 0.8))))
	outlier_n <- max(1, round(0.015 * n_ever))
	outlier_idx <- sample(seq_len(n_ever), outlier_n)
	base_vals[outlier_idx] <- sample(61:65, outlier_n, replace = TRUE)
	section_df$smoke_avg_prev_1_1[ever_smokers] <- base_vals
}

smoke_exposure_levels <- c(
	"Prefer not to answer", "Never", "Every day", "Most days of the week",
	"One day per week", "One day per month", "A few days per year"
)
section_df$smoke_exposure_1_1 <- sample(
	smoke_exposure_levels, n, replace = TRUE,
	prob = c(0.03, 0.34, 0.18, 0.17, 0.11, 0.07, 0.10)
)

section_df$smoke_expose_house_hrs_1_1 <- rep(NA_real_, n)
exposed_idx <- which(section_df$smoke_exposure_1_1 != "Never")
if (length(exposed_idx) > 0) {
	section_df$smoke_expose_house_hrs_1_1[exposed_idx] <- round(
		pmin(24, pmax(0, rgamma(length(exposed_idx), shape = 2.2, scale = 1.35))), 1
	)
}

section_df$smoke_prev_type_1_1 <- rep(NA_character_, n)
if (length(ever_smokers) > 0) {
	section_df$smoke_prev_type_1_1[ever_smokers] <- sample(
		c("Prefer not to answer", "None of the above", "Manufactured cigarettes", "Hand-rolled cigarettes", "Cigars or pipes"),
		length(ever_smokers), replace = TRUE, prob = c(0.04, 0.10, 0.48, 0.30, 0.08)
	)
}

# ===========================================================================
# ALCOHOL
# ===========================================================================

current_drinkers <- which(!is.na(section_df$alcohol_curr_1_1) & !section_df$alcohol_curr_1_1 %in% c("Never", "Prefer not to answer"))
never_drinkers <- which(section_df$alcohol_curr_1_1 == "Never")

monthly_light <- which(section_df$alcohol_curr_1_1 %in% c("Special occasions only", "One to three times a month"))
weekly_moderate <- which(section_df$alcohol_curr_1_1 %in% c("Once or twice a week", "Three or four times a week"))
daily_heavy <- which(section_df$alcohol_curr_1_1 == "Daily or almost daily")
weekly_drinkers <- sort(unique(c(weekly_moderate, daily_heavy)))

section_df$alcohol_prev_1_1 <- rep(NA_character_, n)
if (length(never_drinkers) > 0) {
	section_df$alcohol_prev_1_1[never_drinkers] <- sample(
		c("Yes", "No", "Prefer not to answer", NA),
		length(never_drinkers), replace = TRUE, prob = c(0.35, 0.50, 0.08, 0.07)
	)
}
if (length(current_drinkers) > 0) {
	section_df$alcohol_prev_1_1[current_drinkers] <- "No"
}

for (cn in c(
	"alcohol_wine_red_mth_1_1", "alcohol_wine_red_mth_2_1", "alcohol_wine_white_mth_1_1", "alcohol_wine_white_mth_2_1",
	"alcohol_beer_mth_1_1", "alcohol_beer_mth_2_1", "alcohol_spirits_mth_1_1", "alcohol_spirits_mth_2_1",
	"alcohol_wine_fort_mth_1_1", "alcohol_wine_fort_mth_2_1", "alcohol_other_mth_1_1", "alcohol_other_mth_2_1",
	"alcohol_wine_red_wk_1_1", "alcohol_wine_white_wk_1_1", "alcohol_beer_wk_1_1", "alcohol_spirits_wk_1_1",
	"alcohol_wine_fort_wk_1_1", "alcohol_other_wk_1_1"
)) section_df[[cn]] <- rep(NA_integer_, n)

fill_monthly <- function(indices, suffix) {
	if (length(indices) == 0) return(invisible(NULL))

	light_idx <- intersect(indices, monthly_light)
	moderate_idx <- intersect(indices, weekly_moderate)
	heavy_idx <- intersect(indices, daily_heavy)

	set_monthly <- function(target_idx, lambda_scale) {
		if (length(target_idx) == 0) return(invisible(NULL))
		section_df[[paste0("alcohol_wine_red_mth_", suffix)]][target_idx] <<- pmax(0, pmin(30, round(rpois(length(target_idx), lambda = 0.9 * lambda_scale))))
		section_df[[paste0("alcohol_wine_white_mth_", suffix)]][target_idx] <<- pmax(0, pmin(30, round(rpois(length(target_idx), lambda = 0.9 * lambda_scale))))
		section_df[[paste0("alcohol_beer_mth_", suffix)]][target_idx] <<- pmax(0, pmin(45, round(rpois(length(target_idx), lambda = 1.4 * lambda_scale))))
		section_df[[paste0("alcohol_spirits_mth_", suffix)]][target_idx] <<- pmax(0, pmin(25, round(rpois(length(target_idx), lambda = 0.7 * lambda_scale))))
		section_df[[paste0("alcohol_wine_fort_mth_", suffix)]][target_idx] <<- pmax(0, pmin(20, round(rpois(length(target_idx), lambda = 0.3 * lambda_scale))))
		section_df[[paste0("alcohol_other_mth_", suffix)]][target_idx] <<- pmax(0, pmin(20, round(rpois(length(target_idx), lambda = 0.4 * lambda_scale))))
	}

	set_monthly(light_idx, 1.0)
	set_monthly(moderate_idx, 2.3)
	set_monthly(heavy_idx, 3.8)
}

fill_monthly(intersect(current_drinkers, idx_v1), "1_1")
fill_monthly(intersect(current_drinkers, idx_v2), "2_1")

if (length(weekly_drinkers) > 0) {
	wk_moderate <- intersect(weekly_drinkers, weekly_moderate)
	wk_heavy <- intersect(weekly_drinkers, daily_heavy)

	if (length(wk_moderate) > 0) {
		section_df$alcohol_wine_red_wk_1_1[wk_moderate] <- pmax(0, pmin(25, round(rpois(length(wk_moderate), lambda = 2.6))))
		section_df$alcohol_wine_white_wk_1_1[wk_moderate] <- pmax(0, pmin(25, round(rpois(length(wk_moderate), lambda = 2.4))))
		section_df$alcohol_beer_wk_1_1[wk_moderate] <- pmax(0, pmin(35, round(rpois(length(wk_moderate), lambda = 3.9))))
		section_df$alcohol_spirits_wk_1_1[wk_moderate] <- pmax(0, pmin(20, round(rpois(length(wk_moderate), lambda = 1.8))))
		section_df$alcohol_wine_fort_wk_1_1[wk_moderate] <- pmax(0, pmin(15, round(rpois(length(wk_moderate), lambda = 0.6))))
		section_df$alcohol_other_wk_1_1[wk_moderate] <- pmax(0, pmin(15, round(rpois(length(wk_moderate), lambda = 0.8))))
	}

	if (length(wk_heavy) > 0) {
		section_df$alcohol_wine_red_wk_1_1[wk_heavy] <- pmax(0, pmin(40, round(rpois(length(wk_heavy), lambda = 4.3))))
		section_df$alcohol_wine_white_wk_1_1[wk_heavy] <- pmax(0, pmin(40, round(rpois(length(wk_heavy), lambda = 4.0))))
		section_df$alcohol_beer_wk_1_1[wk_heavy] <- pmax(0, pmin(55, round(rpois(length(wk_heavy), lambda = 6.2))))
		section_df$alcohol_spirits_wk_1_1[wk_heavy] <- pmax(0, pmin(35, round(rpois(length(wk_heavy), lambda = 2.7))))
		section_df$alcohol_wine_fort_wk_1_1[wk_heavy] <- pmax(0, pmin(20, round(rpois(length(wk_heavy), lambda = 1.1))))
		section_df$alcohol_other_wk_1_1[wk_heavy] <- pmax(0, pmin(20, round(rpois(length(wk_heavy), lambda = 1.2))))
	}
}

section_df$alcohol_food_1_1 <- rep(NA_character_, n)
if (length(current_drinkers) > 0) {
	food_weekly <- intersect(current_drinkers, weekly_drinkers)
	food_monthly <- setdiff(current_drinkers, weekly_drinkers)

	if (length(food_monthly) > 0) {
		section_df$alcohol_food_1_1[food_monthly] <- sample(
			c("Never", "Sometimes", "Usually", "Always", "Prefer not to answer"),
			length(food_monthly), replace = TRUE, prob = c(0.20, 0.50, 0.22, 0.04, 0.04)
		)
	}
	if (length(food_weekly) > 0) {
		section_df$alcohol_food_1_1[food_weekly] <- sample(
			c("Never", "Sometimes", "Usually", "Always", "Prefer not to answer"),
			length(food_weekly), replace = TRUE, prob = c(0.08, 0.34, 0.36, 0.17, 0.05)
		)
	}
}

section_df$alcohol_chg_1_1 <- rep(NA_character_, n)
ever_drinkers <- sort(unique(c(current_drinkers, which(section_df$alcohol_prev_1_1 == "Yes"))))
if (length(ever_drinkers) > 0) {
	section_df$alcohol_chg_1_1[ever_drinkers] <- sample(
		c("Drink about the same", "Drink less than before", "Drink more than before", "Stopped drinking", "Prefer not to answer"),
		length(ever_drinkers), replace = TRUE, prob = c(0.58, 0.20, 0.11, 0.07, 0.04)
	)
}

section_df$alcohol_chg_reduce_reason_1_1 <- rep(NA_character_, n)
section_df$alcohol_chg_reduce_reason_2_m <- rep(NA_character_, n)
reduce_idx <- which(section_df$alcohol_chg_1_1 == "Drink less than before")
if (length(reduce_idx) > 0) {
	section_df$alcohol_chg_reduce_reason_1_1[reduce_idx] <- sample(
		c("Health reasons", "Cost reasons", "Family/work responsibilities", "Other", "Prefer not to answer"),
		length(reduce_idx), replace = TRUE, prob = c(0.50, 0.16, 0.18, 0.11, 0.05)
	)
	section_df$alcohol_chg_reduce_reason_2_m[reduce_idx] <- sample(
		c("Health advice", "Medication interaction", "Training/fitness", "Pregnancy/fertility", "Religious/cultural", "Other"),
		length(reduce_idx), replace = TRUE, prob = c(0.37, 0.16, 0.18, 0.08, 0.07, 0.14)
	)
}

section_df$alcohol_chg_abst_reason_1_1 <- rep(NA_character_, n)
section_df$alcohol_chg_abst_reason_2_m <- rep(NA_character_, n)
abst_idx <- which(section_df$alcohol_chg_1_1 == "Stopped drinking")
if (length(abst_idx) > 0) {
	section_df$alcohol_chg_abst_reason_1_1[abst_idx] <- sample(
		c("Health reasons", "Personal choice", "Previous dependence concerns", "Religious/cultural", "Other", "Prefer not to answer"),
		length(abst_idx), replace = TRUE, prob = c(0.42, 0.24, 0.12, 0.07, 0.10, 0.05)
	)
	section_df$alcohol_chg_abst_reason_2_m[abst_idx] <- sample(
		c("Medical condition", "Medication interaction", "Mental wellbeing", "Family reasons", "Religious/cultural", "Other"),
		length(abst_idx), replace = TRUE, prob = c(0.32, 0.18, 0.16, 0.14, 0.07, 0.13)
	)
}

# ===========================================================================
# SOCIAL, SCREEN TIME, SLEEP
# ===========================================================================

section_df$lifestyle_social_visits_1_1 <- sample(
	c("Almost daily", "2-4 times a week", "Once a week", "1-3 times a month", "Less than once a month", "Never", "Prefer not to answer", NA),
	n, replace = TRUE, prob = c(0.10, 0.23, 0.22, 0.22, 0.14, 0.05, 0.02, 0.02)
)

social_primary <- sample(
	c("Pub or social club", "Sports club or gym", "Religious group", "Volunteering group", "Arts/music group", "No regular group", "Prefer not to answer", NA),
	n, replace = TRUE, prob = c(0.15, 0.23, 0.09, 0.09, 0.11, 0.27, 0.03, 0.03)
)
social_secondary <- sample(
	c("Pub or social club", "Sports club or gym", "Religious group", "Volunteering group", "Arts/music group"),
	n, replace = TRUE, prob = c(0.20, 0.24, 0.17, 0.19, 0.20)
)
has_second_social <- runif(n) < 0.18
section_df$lifestyle_social_rec_1_m <- social_primary
for (i in seq_len(n)) {
	if (isTRUE(has_second_social[i]) && !is.na(social_primary[i]) &&
		!social_primary[i] %in% c("No regular group", "Prefer not to answer") &&
		social_primary[i] != social_secondary[i]) {
		section_df$lifestyle_social_rec_1_m[i] <- paste(social_primary[i], social_secondary[i], sep = "; ")
	}
}

section_df$lifestyle_screen_tv_hrs_1_1 <- rep(NA_real_, n)
section_df$lifestyle_screen_tv_hrs_2_1 <- rep(NA_real_, n)
section_df$lifestyle_screen_pc_hrs_1_1 <- rep(NA_real_, n)
section_df$lifestyle_screen_pc_hrs_2_1 <- rep(NA_real_, n)
if (length(idx_v1) > 0) {
	section_df$lifestyle_screen_tv_hrs_1_1[idx_v1] <- pmin(14, pmax(0, round(rnorm(length(idx_v1), mean = 2.5, sd = 1.6), 1)))
	section_df$lifestyle_screen_pc_hrs_1_1[idx_v1] <- pmin(14, pmax(0, round(rnorm(length(idx_v1), mean = 2.9, sd = 1.9), 1)))
}
if (length(idx_v2) > 0) {
	section_df$lifestyle_screen_tv_hrs_2_1[idx_v2] <- pmin(14, pmax(0, round(rnorm(length(idx_v2), mean = 2.5, sd = 1.6), 1)))
	section_df$lifestyle_screen_pc_hrs_2_1[idx_v2] <- pmin(14, pmax(0, round(rnorm(length(idx_v2), mean = 2.9, sd = 1.9), 1)))
}

section_df$sleep_chronotype_1_1 <- rep(NA_character_, n)
section_df$sleep_daytime_1_1 <- rep(NA_character_, n)
section_df$sleep_napping_1_1 <- rep(NA_character_, n)
section_df$sleep_snoring_1_1 <- rep(NA_character_, n)
section_df$sleep_trouble_1_1 <- rep(NA_character_, n)
section_df$sleep_waking_1_1 <- rep(NA_character_, n)
if (length(idx_v2) > 0) {
	section_df$sleep_chronotype_1_1[idx_v2] <- sample(c("Definitely a 'morning' person", "More a 'morning' than 'evening' person", "More an 'evening' than a 'morning' person", "Definitely an 'evening' person", "Do not know", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.20, 0.30, 0.25, 0.12, 0.03, 0.03, 0.07))
	section_df$sleep_daytime_1_1[idx_v2] <- sample(c("Never/rarely", "Sometimes", "Often", "Do not know", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.55, 0.28, 0.08, 0.02, 0.02, 0.05))
	section_df$sleep_napping_1_1[idx_v2] <- sample(c("Never/rarely", "Sometimes", "Usually", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.50, 0.30, 0.12, 0.03, 0.05))
	section_df$sleep_snoring_1_1[idx_v2] <- sample(c("Yes", "No", "Do not know", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.25, 0.60, 0.05, 0.03, 0.07))
	section_df$sleep_trouble_1_1[idx_v2] <- sample(c("Never/rarely", "Sometimes", "Usually", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.50, 0.30, 0.12, 0.03, 0.05))
	section_df$sleep_waking_1_1[idx_v2] <- sample(c("Not easy at all", "Not very easy", "Fairly easy", "Very easy", "Do not know", "Prefer not to answer", NA), length(idx_v2), replace = TRUE, prob = c(0.08, 0.18, 0.40, 0.25, 0.02, 0.02, 0.05))
}

section_df <- apply_pdf_value_catalog(section_df, questionnaire_data, cols)

if ("activity_stairs_1_1" %in% names(section_df)) {
	stairs <- trimws(as.character(section_df$activity_stairs_1_1))
	stairs[stairs == "1-5"] <- "1-5 times a day"
	stairs[stairs == "6-10"] <- "6-10 times a day"
	stairs[stairs == "11-15"] <- "11-15 times a day"
	stairs[stairs == "16-20"] <- "16-20 times a day"
	stairs[stairs == "More than 20"] <- "More than 20 times a day"
	section_df$activity_stairs_1_1 <- stairs
}

# Smoking main-question rule:
# - v2: everyone answers smoke_tobacco_type_1_m
# - v1: everyone answers smoke_status_1_1
# If v2 main answer is "I have not used any of these tobacco products" (or NA),
# then all other smoking detail columns are blank.
if ("smoke_tobacco_type_1_m" %in% names(section_df)) {
	info <- get_pdf_column_values("smoke_tobacco_type_1_m")
	if (length(idx_v2) > 0) {
		non_use_label <- "I have not used any of these tobacco products"
		fallback_opts <- c(non_use_label, "Cigarettes", "Cigar", "Pipe", "Shisha", "Smokeless tobacco", "Prefer not to answer")
		opts <- character(0)
		if (!is.null(info)) {
			opts <- sanitize_pdf_values(if (length(info$v2) > 0) info$v2 else info$all)
		}
		opts <- unique(c(opts, fallback_opts))
		other_opts <- setdiff(opts, non_use_label)
		probs <- rep(0, length(opts))
		names(probs) <- opts
		probs[non_use_label] <- 0.62
		if (length(other_opts) > 0) {
			probs[other_opts] <- 0.38 / length(other_opts)
		}
		section_df$smoke_tobacco_type_1_m[idx_v2] <- sample(opts, length(idx_v2), replace = TRUE, prob = probs)
	}
	if (length(idx_v1) > 0) section_df$smoke_tobacco_type_1_m[idx_v1] <- NA_character_
}

non_smoker_v2_idx <- integer(0)
if ("smoke_tobacco_type_1_m" %in% names(section_df)) {
	v2_main <- trimws(as.character(section_df$smoke_tobacco_type_1_m[idx_v2]))
	non_smoker_v2_idx <- idx_v2[
		is.na(v2_main) |
		v2_main == "" |
		startsWith(v2_main, "I have not used any of these tobacco products")
	]
}
non_smoker_v1_idx <- integer(0)
if ("smoke_status_1_1" %in% names(section_df)) {
	non_smoker_v1_idx <- idx_v1[section_df$smoke_status_1_1[idx_v1] == "No, not at all"]
}
non_smoker_idx <- sort(unique(c(non_smoker_v1_idx, non_smoker_v2_idx)))

if (length(non_smoker_idx) > 0) {
	smoke_detail_cols <- setdiff(grep("^smoke_", names(section_df), value = TRUE), c(
		"smoke_status_1_1", "smoke_status_2_1", "smoke_tobacco_type_1_m"
	))
	for (cn in smoke_detail_cols) section_df[[cn]][non_smoker_idx] <- NA_character_
}

write_section(section_df, "your_lifestyle")