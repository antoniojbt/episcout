# ###
# # Try with ggplot2:
# # Function to convert survfit object to data frame:
# # TO DO: move to episcout
# # TO DO: double check this function and summary(surv_fit_point) give the same output
# surv_summary <- function(survfit_obj, time_var) {
#     data.frame(
#         time = survfit_obj$time,
#         n.risk = survfit_obj$n.risk,
#         n.event = survfit_obj$n.event,
#         n.censor = survfit_obj$n.censor,
#         surv = survfit_obj$surv,
#         strata = rep(names(survfit_obj$strata), times = survfit_obj$strata)
#     )
# }
#
#
# # Convert survfit object to data frame:
# # TO DO: move to episcout
# # Re-run the surv object and fit:
# surv_fit_point <- survival::survfit(Surv(days_to_death, death) ~ intervention + time_cuts,
#                                     data = data_f
# )
# str(surv_fit_point)
# surv_fit_point$n
# surv_fit_point$time
#
#
# surv_data <- surv_summary(surv_fit_point, "time")
# epi_head_and_tail(surv_data, cols = 6)
# summary(surv_data)
#
# # Extract hospital and time_cuts from strata:
# surv_data <- surv_data %>%
#     mutate(
#         intervention = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("intervention=", "", x[1]))),
#         time_cuts = sapply(strsplit(as.character(strata), ","), function(x) trimws(sub("time_cuts=", "", x[2])))
#     )
# epi_head_and_tail(surv_data, cols = 8)
# surv_data$time_cuts
# surv_data$intervention
# # Trimmed of whitespaces now
#
# # Set order for time-cuts:
# surv_data$time_cuts <- factor(surv_data$time_cuts,
#                               levels = c('pre-T0', 'T0', 'gap_T0_T1', 'T1',
#                                          'gap_T1_T2', 'T2', 'post-T2'),
#                               ordered = TRUE)
# summary(surv_data$time_cuts)
#
# # Plot with ggplot2 and faceting:
# # TO DO: save plot
# ggplot(surv_data, aes(x = time, y = surv, color = intervention)) +
#     geom_step() +
#     facet_wrap(~time_cuts, scales = "free_y") +
#     labs(
#         title = "Kaplan-Meier Survival Curves by Intervention and Study Point",
#         x = "Time (days)",
#         y = "Survival Probability"
#     ) +
#     theme_minimal() +
#     theme(legend.title = element_blank())
#
# surv_data
# ###
