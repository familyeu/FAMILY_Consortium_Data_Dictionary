family_color_palette <- c('#64BED2', '#422C7E','#E8AF0C','#A9899C',"#81A594","#00628B", "#C3C3E5", "#FFCC00")


##############################   IMPORT DATABASE   ##############################

# format overview families sheet
meta_db_overview_families <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Cohort overview - families')
colnames(meta_db_overview_families) <- meta_db_overview_families[1, ]
meta_db_overview_families <- meta_db_overview_families[-1, ]
names(meta_db_overview_families)[1] <- "cohort"
for (col in colnames(meta_db_overview_families[, -1])) {
  meta_db_overview_families[[col]] <- ifelse(meta_db_overview_families[[col]] %in% c("TBD", "<NA>"), NA, as.numeric(as.character(meta_db_overview_families[[col]])))
}

meta_db_overview_families <- meta_db_overview_families %>%
  plyr::mutate(across(-1, as.numeric))
colnames(meta_db_overview_families) <- gsub(" ", "_", colnames(meta_db_overview_families))


# format overview offspring sheet
meta_db_overview_offspring <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Cohort overview - offspring')
identify_and_rename <- function(column_name) {
  if (any(sapply(c('SzO', 'BdO', 'MddO', 'Controls'), function(pattern) grepl(pattern, column_name)))) {
    return(column_name)
  } else {
    return(NULL)
  }
}
column_names <- colnames(meta_db_overview_offspring)
identified_columns <- sapply(column_names, identify_and_rename)

for (col in identified_columns[!sapply(identified_columns, is.null)]) {
  prefix <- gsub('\\..*', '', col)
  suffix <- gsub('.*\\.(\\d+)$', '\\1', col)
  col_index <- which(column_names == col)
  
  for (i in 0:4) {
    new_col_index <- col_index + i
    if (new_col_index <= length(column_names)) {
      existing_col_name <- meta_db_overview_offspring[1, new_col_index]
      existing_col_name <- gsub(" ", "_", existing_col_name)
      new_col_name <- paste0(prefix, '_', suffix, '_', existing_col_name)
      colnames(meta_db_overview_offspring)[new_col_index] <- new_col_name
       }
  }
}
colnames(meta_db_overview_offspring)[2:5] <- paste0("overall_",  gsub(" ", "_", meta_db_overview_offspring[1, 2:5]))
colnames(meta_db_overview_offspring)[6:9] <- paste0("overall_genetic_",  gsub(" ", "_", meta_db_overview_offspring[1, 6:9]))
colnames(meta_db_overview_offspring)[10:13] <- paste0("overall_imaging_",  gsub(" ", "_", meta_db_overview_offspring[1, 10:13]))
meta_db_overview_offspring <- meta_db_overview_offspring[-1, ]
names(meta_db_overview_offspring)[1] <- "Cohort"
for (col in colnames(meta_db_overview_offspring[, -1])) {
  if (grepl("age", col, ignore.case = TRUE)) {
    meta_db_overview_offspring[[col]] <- ifelse(meta_db_overview_offspring[[col]] %in% c("TBD", "<NA>"), NA, meta_db_overview_offspring[[col]])
  } else {
    meta_db_overview_offspring[[col]] <- ifelse(meta_db_overview_offspring[[col]] %in% c("TBD", "<NA>"), NA, as.numeric(as.character(meta_db_overview_offspring[[col]])))
  }
}

# format overview parents sheet
meta_db_overview_parents <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Cohort overview - parents')
identify_and_rename_parents <- function(column_name) {
  if (any(sapply(c('Affected.schizophrenia', 'Non-affected.schizophrenia', 'Affected.bipolar.disorder', 'Non-affected.bipolar.disorder',
                   'Affected.major.depressive.disorder', 'Non-affected.major.depressive.disorder', 'Control'), 
                 function(pattern) grepl(pattern, column_name)))) {
    return(column_name)
  } else {
    return(NULL)
  }
}
column_names_parents <- colnames(meta_db_overview_parents)
identified_columns_parents <- sapply(column_names_parents, identify_and_rename_parents)
for (col in identified_columns_parents[!sapply(identified_columns_parents, is.null)]) {
  prefix <- gsub('\\.[^.]*$', '', col)
  col_index <- which(column_names_parents == col)
  
  for (i in 0:4) {
    new_col_index <- col_index + i
    if (new_col_index <= length(column_names_parents)) {
      existing_col_name <- meta_db_overview_parents[1, new_col_index]
      existing_col_name <- gsub(" ", "_", existing_col_name)
      new_col_name <- paste0(prefix, '_', existing_col_name)
      colnames(meta_db_overview_parents)[new_col_index] <- new_col_name
    }
  }
}
meta_db_overview_parents <- meta_db_overview_parents[-1, ]
names(meta_db_overview_parents)[1] <- "Cohort"
for (col in colnames(meta_db_overview_parents[, -1])) {
  if (grepl("age", col, ignore.case = TRUE)) {
    meta_db_overview_parents[[col]] <- ifelse(meta_db_overview_parents[[col]] %in% c("TBD", "<NA>"), NA, meta_db_overview_parents[[col]])
  } else {
    meta_db_overview_parents[[col]] <- ifelse(meta_db_overview_parents[[col]] %in% c("TBD", "<NA>"), NA, as.numeric(as.character(meta_db_overview_parents[[col]])))
  }
}

meta_db_clinical <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Metadata Clinical')
meta_db_imaging <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Metadata Imaging')
meta_db_genetic <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Metadata Genetic')
meta_db_other <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Metadata Other')
meta_db_publications <- read.xlsx('FAMILY_metadata_database.xlsx', sheet = 'Metadata Other')



##############################   OVERVIEW FAMILIES   ##################################

pie_chart_plotly <- function(category) {
  selected_category_data <- reactive({
    selected_columns <- grep(gsub(" ", "_", category), names(meta_db_overview_families), value = TRUE)
    meta_db_overview_families[, c('cohort', selected_columns), drop = FALSE]
  })
  
  counts <- selected_category_data() %>%
    group_by(cohort) %>%
    summarise(across(where(is.numeric), ~sum(as.numeric(.), na.rm = TRUE)))
  
  counts_long <- counts %>%
    pivot_longer(cols = -cohort, names_to = "label", values_to = "value")
  
  counts_long$label <- gsub("_", " ", counts_long$label)
  counts_long$label <- gsub(paste0("\\(", category, "\\)"), "", counts_long$label)
  counts_long <- counts_long[complete.cases(counts_long) & counts_long$value != 0, ]
  
  total_count_per_label <- counts_long %>%
    group_by(label) %>%
    summarise(total_count = sum(value))
  
  label_total <- if (category == 'Control') {
    "N families (Controls)"
  } else {
    "N families "
  }
  
  n_families_count <- total_count_per_label$total_count[total_count_per_label$label == label_total]
  
  total_count_per_label <- total_count_per_label %>%
    dplyr::filter(label != label_total)
  counts_long <- counts_long %>%
    dplyr::filter(label != label_total)
  
  total_count_per_label <- total_count_per_label %>%
    rename(Total = total_count)
  
  table_count <- formattable(
    head(total_count_per_label),
    list(
      Total = color_bar("#C3C3E5")
    ),
    align = "l",
    col.names = c("", "Total")
  )


  
  cohort_color_mapping <- setNames(family_color_palette[1:length(na.omit(unique(counts_long$cohort)))],
                                   na.omit(unique(counts_long$cohort)))
  counts_long$cohort <- factor(counts_long$cohort, levels = names(cohort_color_mapping))
  counts_long$label <- sapply(counts_long$label, function(x) paste(strwrap(x, width = 20), collapse = "\n"))
  
  counts_long$cohort <- factor(counts_long$cohort, levels = names(cohort_color_mapping))

  sectorPlot <-  plot_ly(
    data = counts_long,
    type = 'bar',
    x = ~label,
    y = ~value,
    color = ~cohort,
    colors = cohort_color_mapping,
    showlegend = TRUE
  ) %>%
    plotly::layout(
      barmode = 'stack', 
      xaxis = list(
        title = '',  
        tickfont = list(size = 16) 
      ),
      yaxis = list(
        title = '',  
        tickfont = list(size = 16) 
      ),
      legend = list(
        font = list(size = 16) 
      )
    )
  
  list(table_count_ = table_count, sectorPlot_ = sectorPlot, count_box_ = n_families_count)
}

##############################   OVERVIEW OFFSPRING   ###########################

n_table <- function(n){
  timepoint <- n
  
  meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]] <- as.numeric( meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]])
  meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]] <- as.numeric( meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]])
  meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]] <- as.numeric( meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]])
  meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]] <- as.numeric( meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]])
  
  
  summed_data <- meta_db_overview_offspring %>%
    group_by(Cohort) %>%
    dplyr::summarise(
      SzO = sum(get(paste0('SzO_', timepoint, '_n'))),
      BdO =  sum(get(paste0('BdO_', timepoint, '_n'))),
      MddO = sum(get(paste0('MddO_', timepoint, '_n'))),
      HC = sum(get(paste0(paste0('Controls_', timepoint, '_n')))),
    )
  data_long <- gather(summed_data, key = "Variable", value = "Value", -Cohort)
  
  mapping <- c("SzO" = "Schizophrenia offspring",
               "BdO" = "Bipolar disorder offspring",
               "MddO" = "Major depressive disorder offspring",
               "HC" = "Controls")
  data_long <- data_long %>%
    plyr::mutate(Variable = dplyr::recode(Variable, !!!mapping))
  
  # Calculate pooled statistics
  # SzO
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_offspring[[paste0('SzO_', timepoint, '_Age:_mean(sd;_range)')]]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_offspring[[paste0('SzO_', timepoint, '_Age:_mean(sd;_range)')]]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_offspring[[paste0('SzO_', timepoint, '_%female')]]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]], na.rm = TRUE)
  pooled_mean_age_sz <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]] - 1), na.rm = TRUE) / (sum(meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]] , na.rm = TRUE) -1))  
  pooled_sd_sz <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('SzO_', timepoint, '_n')]], na.rm = TRUE)  
  pooled_percentage_female_sz <- paste('%females:',round(pooled_percentage_female, 2))
  
  #Bd
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_offspring[[paste0('BdO_', timepoint, '_Age:_mean(sd;_range)')]]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_offspring[[paste0('BdO_', timepoint, '_Age:_mean(sd;_range)')]]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_offspring[[paste0('BdO_', timepoint, '_%female')]]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]], na.rm = TRUE)
  pooled_mean_age_bd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]] - 1), na.rm = TRUE) / (sum(meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]] , na.rm = TRUE) -1))  
  pooled_sd_bd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('BdO_', timepoint, '_n')]], na.rm = TRUE)  
  pooled_percentage_female_bd <- paste('%females:',round(pooled_percentage_female, 2))
  
  #MddO
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_offspring[[paste0('MddO_', timepoint, '_Age:_mean(sd;_range)')]]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_offspring[[paste0('MddO_', timepoint, '_Age:_mean(sd;_range)')]]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_offspring[[paste0('MddO_', timepoint, '_%female')]]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]], na.rm = TRUE)
  pooled_mean_age_mdd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]] - 1), na.rm = TRUE) / (sum(meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]] , na.rm = TRUE) -1))  
  pooled_sd_mdd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('MddO_', timepoint, '_n')]], na.rm = TRUE)  
  pooled_percentage_female_mdd<- paste('%females:',round(pooled_percentage_female, 2))
  
  # HC
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_offspring[[paste0('Controls_', timepoint, '_Age:_mean(sd;_range)')]]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_offspring[[paste0('Controls_', timepoint, '_Age:_mean(sd;_range)')]]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_offspring[[paste0('Controls_', timepoint, '_%female')]]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]], na.rm = TRUE)
  pooled_mean_age_hc <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]] - 1), na.rm = TRUE) / (sum(meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]] , na.rm = TRUE) -1))  
  pooled_sd_hc <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]], na.rm = TRUE) / sum(meta_db_overview_offspring[[paste0('Controls_', timepoint, '_n')]], na.rm = TRUE)  
  pooled_percentage_female_hc <- paste('%females:',round(pooled_percentage_female, 2))

  desired_order <- c('Bipolar disorder offspring', 'Schizophrenia offspring', 'Major depressive disorder offspring', 'Controls')
  data_long$Variable <- factor(data_long$Variable, levels = desired_order)
  
  n_graph <- ggplot(data_long, aes(x = Variable, y = Value, fill = Cohort)) +
    geom_bar(stat = "identity") +
    labs(x ='', y = "N", fill = "Cohort") +
    scale_y_continuous(expand = c(0, 70)) +  # Format y-axis as percentages
    theme_minimal() +
    theme(
      plot.margin = margin(20, 0.01, 20, 0.01),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),  # Increase size to 12
      axis.text.y = element_text(size = 16),  # Increase size to 12
      axis.title.x = element_text(size = 14),  # Increase size to 14
      axis.title.y = element_text(size = 14),   # Increase size to 14
      legend.text = element_text(size = 16),  # Increase size of legend text to 12
      legend.title = element_text(size = 16)   # Increase size of legend title to 14
    ) +
    scale_fill_manual(values = c("#64BED2", "#422C7E", "#E8AF0C", "#A9899C","#81A594","#00628B", "#C3C3E5", "#FFCC00" ))+
    annotate(
      "text",
      x = 2,  # Adjust the x-position as needed
      y = sum(data_long[data_long$Variable == 'Schizophrenia offspring',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_sz, "\n", pooled_sd_sz, "\n", pooled_percentage_female_sz),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 3,  # Adjust the x-position as needed
      y = sum(data_long[data_long$Variable == 'Major depressive disorder offspring',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_mdd, "\n", pooled_sd_mdd, "\n", pooled_percentage_female_mdd),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 4,  # Adjust the x-position as needed
      y = sum(data_long[data_long$Variable == 'Controls',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_hc, "\n", pooled_sd_hc, "\n", pooled_percentage_female_hc),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 1,  # Adjust the x-position as needed
      y = sum(data_long[data_long$Variable == 'Bipolar disorder offspring',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_bd, "\n", pooled_sd_bd, "\n", pooled_percentage_female_bd),
      size = 6,
      hjust = 0.5
    )

  return(n_graph)
}


##############################   OVERVIEW PARENTS   ###########################

n_table_parents <- function(n){
  summed_data <- meta_db_overview_parents %>%
    group_by(Cohort) %>%
    dplyr::summarise(
      A_SzP = sum(get('Affected.schizophrenia_n'), na.rm = TRUE),
      NA_SzP = sum(get('Non-affected.schizophrenia_n'), na.rm = TRUE),
      A_BdP = sum(get('Affected.bipolar.disorder_n'), na.rm = TRUE),
      NA_BdP = sum(get('Non-affected.bipolar.disorder_n'), na.rm = TRUE),
      A_MddP = sum(get('Affected.major.depressive.disorder_n'), na.rm = TRUE),
      NA_MddP = sum(get('Non-affected.major.depressive.disorder_n'), na.rm = TRUE),
      ControlP = sum(get('Control_n'), na.rm = TRUE)
    )
  data_long <- gather(summed_data, key = "Variable", value = "Value", -Cohort)
  
  mapping <- c("A_SzP" = "Schizophrenia - affected parent",
               "NA_SzP" = "Schizophrenia - non-affected parent",
               "A_BdP" = "Bipolar disorder - affected parent",
               "NA_BdP" = "Bipolar disorder - non-affected parent",
               "A_MddP" = "Major depressive disorder - affected parent",
               "NA_MddP" = "Major depressive disorder - non-affected parent",
               "ControlP" = "Control parent")
  data_long <- data_long %>%
    plyr::mutate(Variable = dplyr::recode(Variable, !!!mapping))
  
  # Calculate pooled statistics
  # A_SzP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Affected.schizophrenia_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Affected.schizophrenia_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Affected.schizophrenia_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Affected.schizophrenia_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.schizophrenia_n']], na.rm = TRUE)
  pooled_mean_age_sz <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Affected.schizophrenia_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Affected.schizophrenia_n']] , na.rm = TRUE) -1))  
  pooled_sd_sz <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Affected.schizophrenia_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.schizophrenia_n']], na.rm = TRUE)  
  pooled_percentage_female_sz <- paste('%females:',round(pooled_percentage_female, 2))
  
  # NA_SzP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Non-affected.schizophrenia_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Non-affected.schizophrenia_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Non-affected.schizophrenia_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Non-affected.schizophrenia_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.schizophrenia_n']], na.rm = TRUE)
  pooled_mean_age_na_sz <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Non-affected.schizophrenia_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Non-affected.schizophrenia_n']] , na.rm = TRUE) -1))  
  pooled_sd_na_sz <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Non-affected.schizophrenia_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.schizophrenia_n']], na.rm = TRUE)  
  pooled_percentage_female_na_sz <- paste('%females:',round(pooled_percentage_female, 2))
  
  # A_BdP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Affected.bipolar.disorder_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Affected.bipolar.disorder_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Affected.bipolar.disorder_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Affected.bipolar.disorder_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.bipolar.disorder_n']], na.rm = TRUE)
  pooled_mean_age_bd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Affected.bipolar.disorder_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Affected.bipolar.disorder_n']] , na.rm = TRUE) -1))  
  pooled_sd_bd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Affected.bipolar.disorder_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.bipolar.disorder_n']], na.rm = TRUE)  
  pooled_percentage_female_bd <- paste('%females:',round(pooled_percentage_female, 2))
  
  # NA_BdP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Non-affected.bipolar.disorder_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Non-affected.bipolar.disorder_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Non-affected.bipolar.disorder_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Non-affected.bipolar.disorder_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.bipolar.disorder_n']], na.rm = TRUE)
  pooled_mean_age_na_bd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Non-affected.bipolar.disorder_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Non-affected.bipolar.disorder_n']] , na.rm = TRUE) -1))  
  pooled_sd_na_bd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Non-affected.bipolar.disorder_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.bipolar.disorder_n']], na.rm = TRUE)  
  pooled_percentage_female_na_bd <- paste('%females:',round(pooled_percentage_female, 2))
  
  # A_MddP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Affected.major.depressive.disorder_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Affected.major.depressive.disorder_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Affected.major.depressive.disorder_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Affected.major.depressive.disorder_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.major.depressive.disorder_n']], na.rm = TRUE)
  pooled_mean_age_mdd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Affected.major.depressive.disorder_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Affected.major.depressive.disorder_n']] , na.rm = TRUE) -1))  
  pooled_sd_mdd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Affected.major.depressive.disorder_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Affected.major.depressive.disorder_n']], na.rm = TRUE)  
  pooled_percentage_female_mdd <- paste('%females:',round(pooled_percentage_female, 2))
  
  # NA_MddP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Non-affected.major.depressive.disorder_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Non-affected.major.depressive.disorder_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Non-affected.major.depressive.disorder_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Non-affected.major.depressive.disorder_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.major.depressive.disorder_n']], na.rm = TRUE)
  pooled_mean_age_na_mdd <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Non-affected.major.depressive.disorder_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Non-affected.major.depressive.disorder_n']] , na.rm = TRUE) -1))  
  pooled_sd_na_mdd <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Non-affected.major.depressive.disorder_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Non-affected.major.depressive.disorder_n']], na.rm = TRUE)  
  pooled_percentage_female_na_mdd <- paste('%females:',round(pooled_percentage_female, 2))
  

  # ControlP
  mean_age_values <- as.numeric(gsub("^(\\d+).*", "\\1", meta_db_overview_parents[['Control_Age:_mean(sd;_range)']]))
  sd_values <- as.numeric(gsub(".*\\((.*?);.*", "\\1", meta_db_overview_parents[['Control_Age:_mean(sd;_range)']]))
  percentage_female_values <- as.numeric(gsub("%", "", meta_db_overview_parents[['Control_%female']]))
  pooled_mean_age <- sum(mean_age_values * meta_db_overview_parents[['Control_n']], na.rm = TRUE) / sum(meta_db_overview_parents[['Control_n']], na.rm = TRUE)
  pooled_mean_age_hc <- paste('Mean age:', round(pooled_mean_age,2))
  pooled_sd <- sqrt(sum(sd_values^2 * (meta_db_overview_parents[['Control_n']] - 1), na.rm = TRUE) / (sum(meta_db_overview_parents[['Control_n']] , na.rm = TRUE) -1))  
  pooled_sd_hc <- paste('SD pooled:', round(pooled_sd, 2))
  pooled_percentage_female<- sum(percentage_female_values * meta_db_overview_parents[[paste0('Control_n')]], na.rm = TRUE) / sum(meta_db_overview_parents[['Control_n']], na.rm = TRUE)  
  pooled_percentage_female_hc <- paste('%females:',round(pooled_percentage_female, 2))
 
  desired_order <- c('Bipolar disorder - affected parent', 'Bipolar disorder - non-affected parent',
                     'Schizophrenia - affected parent', 'Schizophrenia - non-affected parent',
                     'Major depressive disorder - affected parent', 'Major depressive disorder - non-affected parent',
                     'Control parent')
  data_long$Variable <- factor(data_long$Variable, levels = desired_order)
  
  n_graph <- ggplot(data_long, aes(x = Variable, y = Value, fill = Cohort)) +
    geom_bar(stat = "identity") +
    labs(x ='', y = "N", fill = "Cohort") +
    theme_minimal() +
    theme(
      plot.margin = margin(20, 0.01, 20, 0.01),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16), 
      axis.text.y = element_text(size = 16),  
      axis.title.x = element_text(size = 14),  
      axis.title.y = element_text(size = 14),  
      legend.text = element_text(size = 16),  
      legend.title = element_text(size = 16)  
    ) +
    scale_x_discrete(labels = function(labels) str_wrap(labels, width = 0.5))+
    scale_fill_manual(values = c("#64BED2", "#422C7E", "#E8AF0C", "#A9899C","#81A594","#00628B", "#C3C3E5", "#FFCC00" ))+
    annotate(
      "text",
      x = 3, 
      y = sum(data_long[data_long$Variable == 'Schizophrenia - affected parent',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_sz, "\n", pooled_sd_sz, "\n", pooled_percentage_female_sz),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 4,  
      y = sum(data_long[data_long$Variable == 'Schizophrenia - non-affected parent',]$Value, na.rm = TRUE) + 70,  # Adjust the y-position as needed
      label = paste(pooled_mean_age_na_sz, "\n", pooled_sd_na_sz, "\n", pooled_percentage_female_na_sz),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 1,  
      y = sum(data_long[data_long$Variable == 'Bipolar disorder - affected parent',]$Value, na.rm = TRUE) + 70, 
      label = paste(pooled_mean_age_bd, "\n", pooled_sd_bd, "\n", pooled_percentage_female_bd),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 2, 
      y = sum(data_long[data_long$Variable == 'Bipolar disorder - non-affected parent',]$Value, na.rm = TRUE) + 70, 
      label = paste(pooled_mean_age_na_bd, "\n", pooled_sd_na_bd, "\n", pooled_percentage_female_na_bd),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 5,  
      y = sum(data_long[data_long$Variable == 'Major depressive disorder - affected parent',]$Value, na.rm = TRUE) + 70,  
      label = paste(pooled_mean_age_mdd, "\n", pooled_sd_bd, "\n", pooled_percentage_female_mdd),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 6, 
      y = sum(data_long[data_long$Variable == 'Major depressive disorder - non-affected parent',]$Value, na.rm = TRUE) + 70,  
      label = paste(pooled_mean_age_na_mdd, "\n", pooled_sd_na_mdd, "\n", pooled_percentage_female_na_mdd),
      size = 6,
      hjust = 0.5
    )+
    annotate(
      "text",
      x = 7, 
      y = sum(data_long[data_long$Variable == 'Control parent',]$Value, na.rm = TRUE) + 70,  
      label = paste(pooled_mean_age_hc, "\n", pooled_sd_hc, "\n", pooled_percentage_female_hc),
      size = 6,
      hjust = 0.5
    )
  return(n_graph)
}

##############################   IMAGING METADATA   ############################

colnames(meta_db_imaging) <- meta_db_imaging[1, ]
meta_db_imaging <- meta_db_imaging[-1, ]

meta_db_imaging_t1 <- meta_db_imaging[meta_db_imaging$Modality == "T1w", ]
meta_db_imaging_t1 <- meta_db_imaging_t1[, !(names(meta_db_imaging_t1) %in% c("Modality", "Number of difusion directions", "Number of B0s", "Bvalue(s)", "TR", "TE", "Number of repetitions", "Eyes open/closed"))]
meta_db_imaging_dwi<- meta_db_imaging[meta_db_imaging$Modality == "DWI", ]
meta_db_imaging_dwi <- meta_db_imaging_dwi[, !(names(meta_db_imaging_dwi) %in% c("Modality", "TR", "TE" ,"Number of repetitions", "Eyes open/closed"))]
meta_db_imaging_rs <- meta_db_imaging[meta_db_imaging$Modality == "Rs-fMRI", ]
meta_db_imaging_rs <- meta_db_imaging_rs[, !(names(meta_db_imaging_rs) %in% c("Modality", "Number of difusion directions", "Number of B0s", "Bvalue(s)"))]

pie_chart_imaging <- function(input, output) {
  library(dplyr)
  selected_category_data_img <- reactive({
    selected_columns_img <- grep(ifelse(input$time == "At least one timepoint", "overall", input$time), names(meta_db_overview_offspring), value = TRUE)
    selected_columns_img <- grep('Imaging', selected_columns_img, value = TRUE, ignore.case = TRUE)
    meta_db_overview_offspring[, selected_columns_img, drop = FALSE]
  })
  
  output$sectorPlot_imaging_offspring <- renderPlotly({
  counts_img <- selected_category_data_img() %>%
    plotly::summarise(across(where(is.numeric), ~sum(as.numeric(.), na.rm = TRUE)))
  
  counts_df_img <- data.frame(label = names(counts_img), value = as.numeric(counts_img))
  
  counts_df_img$label <- sub("^.*(SzO|BdO|MddO|Controls).*", "\\1", counts_df_img$label)

  plot_ly(data = counts_df_img, labels = ~label, values = ~value, type = 'pie', marker = list(colors = family_color_palette),
          textinfo='value', insidetextfont = list(size = 30))
  })
}



selected_columns_parents <- grep('Imaging', names(meta_db_overview_parents), value = TRUE)
selected_columns_parents <- grep('Imaging', selected_columns_parents, value = TRUE, ignore.case = TRUE)
selected_category_data_parents <- meta_db_overview_parents[, selected_columns_parents, drop = FALSE]
counts_parents <- selected_category_data_parents %>%
  plotly::summarise(across(where(is.numeric), ~sum(as.numeric(.), na.rm = TRUE)))

counts_df_parents <- data.frame(label = names(counts_parents), value = as.numeric(counts_parents))

mapping_img <- c("Affected.schizophrenia_n_Imaging" = "Schizophrenia - affected parent",
                 "Non-affected.schizophrenia_n_Imaging" = "Schizophrenia - non-affected parent",
                 "Affected.bipolar.disorder_n_Imaging" = "Bipolar disorder - affected parent",
                 "Non-affected.bipolar.disorder_n_Imaging" = "Bipolar disorder - non-affected parent",
                 "Affected.major.depressive.disorder_n_Imaging" = "Major depressive disorder - affected parent",
                 "Non-affected.major.depressive.disorder_n_Imaging" = "Major depressive disorder - non-affected parent",
                 "Control_n_Imaging" = "Control parent")
counts_df_parents <- counts_df_parents %>%
  plyr::mutate(label = dplyr::recode(label, !!!mapping_img))
  
sectorPlot_imaging_parents <-  plot_ly(data = counts_df_parents, labels = ~label, 
                                      values = ~value, type = 'pie', marker = list(colors = family_color_palette),
                                      textinfo='value', insidetextfont = list(size = 30))
colnames(meta_db_genetic) <- gsub("\\.", " ", colnames(meta_db_genetic))


##############################   GENETICS METADATA   ############################

selected_genetics_offspring <- grep('_genetic_', names(meta_db_overview_offspring), value = TRUE)
selected_genetics_data_offspring <- meta_db_overview_offspring[, selected_genetics_offspring, drop = FALSE]
counts_offspring_genetics <- selected_genetics_data_offspring %>%
  plotly::summarise(across(where(is.numeric), ~sum(as.numeric(.), na.rm = TRUE)))

counts_df_offspring_genetics <- data.frame(label = names(counts_offspring_genetics), value = as.numeric(counts_offspring_genetics))

mapping_gen_offspring <- c("overall_genetic_n_SzO" = "Schizophrenia offspring",
                           "overall_genetic_n_BdO" = "Bipolar disorder offspring",
                           "overall_genetic_n_MddO" = "Major depressive disorder offspring",
                           "overall_genetic_n_Controls" = "Control offspring")
counts_df_offspring_genetics <- counts_df_offspring_genetics %>%
  plyr::mutate(label = dplyr::recode(label, !!!mapping_gen_offspring))

sectorPlot_genetics_offspring <-  plot_ly(data = counts_df_offspring_genetics, labels = ~label, 
                                          values = ~value, type = 'pie', marker = list(colors = family_color_palette),
                                          textinfo='value', insidetextfont = list(size = 30))

selected_genetics_parents <- grep('Genetics', names(meta_db_overview_parents), value = TRUE)
selected_genetics_data_parents <- meta_db_overview_parents[, selected_genetics_parents, drop = FALSE]
counts_parents_genetics <- selected_genetics_data_parents %>%
  plotly::summarise(across(where(is.numeric), ~sum(as.numeric(.), na.rm = TRUE)))

counts_df_parents_genetics <- data.frame(label = names(counts_parents_genetics), value = as.numeric(counts_parents_genetics))

mapping_gen <- c("Affected.schizophrenia_n_Genetics" = "Schizophrenia - affected parent",
                 "Non-affected.schizophrenia_n_Genetics" = "Schizophrenia - non-affected parent",
                 "Affected.bipolar.disorder_n_Genetics" = "Bipolar disorder - affected parent",
                 "Non-affected.bipolar.disorder_n_Genetics" = "Bipolar disorder - non-affected parent",
                 "Affected.major.depressive.disorder_n_Genetics" = "Major depressive disorder - affected parent",
                 "Non-affected.major.depressive.disorder_n_Genetics" = "Major depressive disorder - non-affected parent",
                 "Control_n_Genetics" = "Control parent")
counts_df_parents_genetics <- counts_df_parents_genetics %>%
  plyr::mutate(label = dplyr::recode(label, !!!mapping_gen))

sectorPlot_genetics_parents <-  plot_ly(data = counts_df_parents_genetics, labels = ~label, 
                                        values = ~value, type = 'pie', marker = list(colors = family_color_palette),
                                        textinfo='value', insidetextfont = list(size = 30))

##############################   CLINICAL METADATA  ################################

names(meta_db_clinical)[names(meta_db_clinical) == "Study/cohort"] <- "Cohort"
names(meta_db_clinical)[names(meta_db_clinical) == "timepoint"] <- "No. of timepoints"
names(meta_db_clinical)[names(meta_db_clinical) == "informant.(who.filled.the.instrument.in)"] <- "Informant"
names(meta_db_clinical)[names(meta_db_clinical) == "Informee.(who.the.instrument.is.about)"] <- "Informee"
columns_to_concat <- c("Category.1", "Category.2", "Category.3", "Category.4", "Suggested.categories")

meta_db_clinical_new <- meta_db_clinical %>%
  unite("CombinedCategories", all_of(columns_to_concat), sep = ", ", na.rm = TRUE) %>%
  plyr::mutate(
    CombinedCategories = str_replace_all(CombinedCategories, "None", "") %>%
      str_replace_all("[[:space:],]+$", "")
  )
names(meta_db_clinical_new)[names(meta_db_clinical_new) == "CombinedCategories"] <- "Assessed categories"

selected_columns_clinical <- c("Instrument", "Assessed categories", "Informant", "Informee", "Cohort", "No. of timepoints")

summary_table_clinical <-  meta_db_clinical_new[,selected_columns_clinical] %>%
  group_by(Instrument) 



category_finder <- function(input, time){
  rows <- meta_db_clinical_new[grep(input, meta_db_clinical_new$"Assessed categories", ignore.case = TRUE),]
  category <- rows %>%
    group_by(Instrument) %>%
    dplyr::summarize(across(c("Informant", "Informee", "Cohort", "No. of timepoints"),
                            ~toString(unique(na.omit(.x)))), .groups = 'drop') 
  
  category <- category %>%
    plyr::mutate(`No. of timepoints` = sapply(strsplit(`No. of timepoints`, ','), function(x) toString(sort(unique(na.omit(as.numeric(x)))))))
  
  category <- category[grep(time, category$"No. of timepoints"),]
}


##############################  GET SAMPLE SIZE  ################################

sample_calculator <- function(input_1, input_2, input_3, input_4, output){
  if ( "Triad" %in% input_2 | "Dyad" %in% input_2) {
  if (any(is.na(c(input_1, input_2, input_3)))) {
    columns <- meta_db_overview_families
  } else {
  input_1 <- gsub(" ", "_", input_1)
  input_2 <- gsub(" ", "_", input_2)
  columns <- meta_db_overview_families[, grepl(paste(input_1, collapse = "|"), colnames(meta_db_overview_families)), drop = FALSE]
  columns <- columns[, grepl(paste(input_2, collapse = "|"), colnames(columns)), drop = FALSE]
  if (!is.na(input_3)){
    columns <- columns[, ifelse(input_3 == 'Yes' & grepl('and', colnames(columns)), FALSE, TRUE), drop = FALSE]
  } 
  columns <- cbind(meta_db_overview_families[, 1, drop = FALSE], columns)
  }
  df_long <- gather(columns, key = "category", value = "count", -cohort)
  df_long$category <- gsub("_", " ", df_long$category)
  df_long_filtered <- df_long[complete.cases(df_long) & df_long$count != 0,]
  df_long_filtered$percentage <- df_long_filtered$count / tapply(df_long_filtered$count, df_long_filtered$category, sum)[df_long_filtered$category] * 100
  names(df_long_filtered)[1] <- "Cohort"
  } else {
    df_long_filtered <- NA
  }
  if ("Offspring" %in% input_2) {
    if (any(is.na(c(input_1, input_3)))) {
      columns_2 <- meta_db_overview_offspring
    } else {
      input_1 <- gsub(" ", "_", input_1)
      input_2 <- gsub(" ", "_", input_2)
      mapping <- c("Schizophrenia" = "SzO",
                   "Bipolar_disorder" = "BdO",
                   "Major_depressive_disorder_offspring" = "MddO",
                   "Controls" = "HC")
      input_new <- dplyr::recode(input_1, !!!mapping)
      columns_2 <- meta_db_overview_offspring[, grepl(paste(input_new, collapse = "|"), colnames(meta_db_overview_offspring)), drop = FALSE]
      columns_2 <- columns_2[, grepl(paste('overall_n', collapse = "|"), colnames(columns_2)), drop = FALSE]
      if (!is.na(input_3)){
        columns_2 <- columns_2[, ifelse(input_3 == 'Yes' & grepl('and', colnames(columns_2)), FALSE, TRUE), drop = FALSE]
      } 
      columns_2 <- cbind(meta_db_overview_offspring[, 1, drop = FALSE], columns_2)
    }
    df_long_2 <- gather(columns_2, key = "category", value = "count", -Cohort)
    df_long_2$category <- gsub("_", " ", df_long_2$category)
    df_long_filtered_2 <- df_long_2[complete.cases(df_long_2) & df_long_2$count != 0,]
    df_long_filtered_2$percentage <- df_long_filtered_2$count / tapply(df_long_filtered_2$count, df_long_filtered_2$category, sum)[df_long_filtered_2$category] * 100
    
  } else {
    df_long_filtered_2 <- NA
  }
  if("Parents" %in% input_2) {
    if (any(is.na(c(input_1, input_3)))) {
      columns_3 <- meta_db_overview_parents
    } else {
      input_1 <- gsub(" ", "_", input_1)
      input_2 <- gsub(" ", "_", input_2)
      mapping <- c("Schizophrenia" = "schizophrenia",
                   "Bipolar_disorder" = "bipolar",
                   "Major_depressive_disorder" = "depressive",
                   "Controls" = "Control")
      input_new_2 <- dplyr::recode(input_1, !!!mapping)
      columns_3 <- meta_db_overview_parents[, grepl(paste(input_new_2, collapse = "|"), colnames(meta_db_overview_parents)), drop = FALSE]
      columns_3 <- columns_3[, grepl(paste('_n', collapse = "|"), colnames(columns_3)) & 
                               !grepl(paste('_n_', collapse = "|"), colnames(columns_3)), drop = FALSE]
      if (!is.na(input_3)){
        columns_3 <- columns_3[, ifelse(input_3 == 'Yes' & grepl('and', colnames(columns_3)), FALSE, TRUE), drop = FALSE]
      } 
      columns_3 <- cbind(meta_db_overview_parents[, 1, drop = FALSE], columns_3)
    }
    df_long_3 <- gather(columns_3, key = "category", value = "count", -Cohort)
    df_long_3$category <- gsub("_", " ", df_long_3$category)
    df_long_filtered_3 <- df_long_3[complete.cases(df_long_3) & df_long_3$count != 0,]
    df_long_filtered_3$percentage <- df_long_filtered_3$count / tapply(df_long_filtered_3$count, df_long_filtered_3$category, sum)[df_long_filtered_3$category] * 100
  } else {
    df_long_filtered_3 <- NA
  }
  
  columns_all <- rbind(df_long_filtered, df_long_filtered_2, df_long_filtered_3)
  columns_all <- columns_all[complete.cases(columns_all),]
  
  columns_all <- subset(columns_all, Cohort %in% input_4)
  
  if (!is_empty(columns_all)){
    mapping <- c("Affected.bipolar.disorder n" = "Affected parent (Bd)",
                 "Non-affected.bipolar.disorder n" = "Non-affected parent (Bd)",
                 "Affected.schizophrenia n" = "Affected parent (Sz)",
                 "Non-affected.schizophrenia n" = "Non-affected parent (Sz)",
                 "Affected.major.depressive.disorder n" = "Affected parent (Mdd)",
                 "Non-affected.major.depressive.disorder n" = "Non-affected parent (Mdd)",
                 "Control n " = "Control parent",
                "Triad (Controls)" = "Control triad",
                 "Triad one affected parent (Bipolar disorder)" = "Triad one affected parent (Bd)",
                 "Triad two affected parents (Bipolar disorder)" = "Triad two affected parents (Bd)",
                 "Triad one affected parent (Schizophrenia)" = "Triad one affected parent (Sz)",
                 "Triad two affected parents (Schizophrenia)" = "Triad two affected parents (Sz)",
                 "Triad two affected parents (Schizophrenia and Bipolar disorder)" = "Triad two affected parents (Sz and Bd)",
                 "Triad two affected parents (Schizophrenia and Major depressive disorder)" = "Triad two affected parents (Sz and Mdd)",
                 "Triad two affected parents (Bipolar disorder and Major depressive disorder)" = "Triad two affected parents (Bd and Mdd)",
                "Dyad non-affected parent (Bipolar disorder)" =  "Dyad non-affected parent (Bd)",
                "Dyad affected parent (Bipolar disorder)" =  "Dyad affected parent (Bd)",
                "Dyad non-affected parent (Schizophrenia)" =  "Dyad non-affected parent (Sz)",
                "Dyad affected parent (Schizophrenia)" =  "Dyad affected parent (Sz)",
                "Dyad non-affected parent (Major depressive disorder)" =  "Dyad non-affected parent (Mdd)",
                "Dyad affected parent (Major depressive disorder)" =  "Dyad affected parent (Mdd)",
                "overall n BdO" = 'BdO',
                 "overall n SzO" = 'SzO',
                 "overall n MddO" = 'MddO',
                 "overall n Controls" = 'Control Offspring'
                 )
    columns_all$category <- dplyr::recode( columns_all$category , !!!mapping)
    
    total_count_per_category <- columns_all %>%
      group_by(category) %>%
      summarise(total_count = sum(count))
    
    ggplot(columns_all, aes(x = percentage, y = category, fill = Cohort)) +
      geom_bar(stat = "identity", width = 0.4) +
      annotate("text", x = 110, y = total_count_per_category$category, label = total_count_per_category$total_count, size = 9.5, color = "black") +
      labs(y = "") +
      scale_fill_manual(values = family_color_palette) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16))
  } 
}




###################################   SERVER  ##################################


server = function(input, output) {
  plots <- reactive(pie_chart_plotly(input$category))
  output$table_count <- renderFormattable({
    plots()$table_count_
  })
  output$sectorPlot <- renderPlotly({
    plots()$sectorPlot_
  })
  output$FamilyCountsBox <- renderInfoBox({
    count_box <- valueBox(
      HTML('<span style="font-size: 18px;font-weight: bold;">Total number of families</span>'),
      HTML(plots()$count_box_),
      icon = icon("users"),
      color = "purple"
    )
  })
  output$plot_n_1 <- renderPlot({
    n_table(1)
  })
  
  output$plot_n_2 <- renderPlot({
    n_table(2)
  })
  
  output$plot_n_3 <- renderPlot({
    n_table(3)
  })
  output$plot_n_4 <- renderPlot({
    n_table(4)
  })
  output$plot_n_parents <- renderPlot({
    n_table_parents(0)
  })    
  output$values <- renderTable({
    category_finder(input$text, input$slider)})
  output$table <-shiny::renderDataTable(summary_table_clinical)
  output$sample_size <-  renderPlot({
    sample_calculator(input$input_1, input$input_2, input$input_3, input$input_4)})
  output$sectorPlot_gen_offspring <- renderPlotly({
    sectorPlot_genetics_offspring
  })
  output$sectorPlot_gen_parents <- renderPlotly({
    sectorPlot_genetics_parents
  })    
  output$table_gen<-DT::renderDataTable({
    datatable(meta_db_genetic)  
  }) 
  output$sectorPlot_imaging_offspring <- renderPlot({
    pie_chart_imaging(input, output)
  })
  output$sectorPlot_img_parents <- renderPlotly({
    sectorPlot_imaging_parents
  })
  output$table_imaging_t1 <-DT::renderDataTable({
    datatable(meta_db_imaging_t1)  
  })
  output$table_imaging_dwi <-DT::renderDataTable({
    datatable(meta_db_imaging_dwi)  
  })
  output$table_imaging_rs <-DT::renderDataTable({
    datatable(meta_db_imaging_rs)  
  })
}

