# # Create function for saving RData objects:
#
# # Parameters
# # Objects to save:
# objects_to_save <- (c('input_data')) # as char
# # Filename to save current R session, data and objects at the end:
# suffix <- paste0('_col_classes', '.RData')
# save_session <- epi_output_name(input_name, suffix)
# # Comression:
# compress <- 'gzip'
#
# save(list = objects_to_save,
#      file = save_session,
#      compress = compress)
#
