analyze_statin_prescription_gaps <- function(atc_codes, filepath = "/Users/padraicdonoghue/Desktop/GitHub/DIAMOND/Prescription intervals/Code/merged_output.csv") {
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  
  # Read and prep data
  raw_data <- read.csv(filepath)
  raw_data$script_date <- as.Date(as.character(raw_data$script_date), format = "%d%b%Y")
  
  for (code in atc_codes) {
    cat("\n--- Processing ATC:", code, "---\n")
    
    # Filter for current ATC
    statins <- raw_data %>%
      filter(str_detect(atc_final, code)) %>%
      mutate(
        prescription_month = month(script_date, label = TRUE, abbr = FALSE),
        prescription_year = year(script_date),
        age_bracket = case_when(
          age >= 60 & age <= 64 ~ "60–64",
          age >= 65 & age <= 69 ~ "65–69",
          age >= 70 & age <= 74 ~ "70–74",
          age >= 75 & age <= 79 ~ "75–79",
          age >= 80 & age <= 84 ~ "80–84",
          age >= 85 & age <= 90 ~ "85–90",
          age > 90 ~ "90+",
          TRUE ~ NA_character_
        ),
        sex_label = case_when(
          sex == 1 ~ "Male",
          sex == 2 ~ "Female",
          TRUE ~ "Unknown"
        )
      ) %>%
      arrange(UniquePatientID, script_date) %>%
      group_by(UniquePatientID) %>%
      mutate(gap_days = abs(as.numeric(difftime(lead(script_date), script_date, units = "days")))) %>%
      ungroup()
    
    ## 1. 30-day and 90-day prescription histograms
      list("30" = 1, "90" = 3) %>%
      purrr::iwalk(function(issue_val, label) {
        df <- statins %>% filter(numberofissues == issue_val)
        if (nrow(df) > 0) {
          ggplot(df, aes(x = gap_days, y = ..density..)) +
            geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
            labs(
              title = paste("Density of Gaps Between", label, "Day Prescriptions | ATC:", code),
              x = "Days Since Last Prescription",
              y = "Density"
            ) +
            scale_x_continuous(breaks = seq(0, 365, 30)) +
            coord_cartesian(xlim = c(0, 365)) +
            theme_minimal() %>% print()
        }
      })
    
    ## 2. Overall density
    overall <- ggplot(statins, aes(x = gap_days, y = ..density..)) +
      geom_histogram(binwidth = 5, fill = "darkorange", colour = "black") +
      labs(
        title = paste("Density of Gaps Between Prescriptions | ATC:", code),
        x = "Days Since Last Prescription",
        y = "Density"
      ) +
      scale_x_continuous(breaks = seq(0, 365, 30)) +
      coord_cartesian(xlim = c(0, 365)) +
      theme_minimal()
    plot(overall)
    
    ## 3. Gap density by ATC
   atcden <-  ggplot(statins, aes(x = gap_days, y = ..density..)) +
      geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
      labs(
        title = paste("Density of Gaps Between Prescriptions (Grouped by ATC) | ATC:", code),
        x = "Days Since Last Prescription",
        y = "Density"
      ) +
      scale_x_continuous(breaks = seq(0, 365, 30)) +
      coord_cartesian(xlim = c(0, 365)) +
      theme_minimal() 
   plot(atcden)
    
    ## 4. Boxplot by Age Bracket and Sex (faceted by Scheme)
    ggplot(statins %>% filter(!is.na(gap_days)),
           aes(x = age_bracket, y = gap_days, fill = sex_label, color = sex_label)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~ scheme) +
      scale_y_continuous(breaks = seq(0, 365, 60)) +
      labs(
        title = paste("Gap Days by Age Bracket and Sex | ATC:", code),
        x = "Age Bracket", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot(atcden)
    
    ## 5. Month Scatterplot
   monthscatter <- statins$prescription_month <- factor(statins$prescription_month, levels = month.name, ordered = TRUE)
    ggplot(statins %>% filter(!is.na(gap_days)),
           aes(x = prescription_month, y = gap_days)) +
      geom_jitter(width = 0.2, alpha = 0.4, color = "steelblue") +
      scale_y_continuous(limits = c(0, 365), breaks = seq(0, 360, 30)) +
      labs(
        title = paste("Prescription Gap Days by Month | ATC:", code),
        subtitle = "Each point represents one prescription interval",
        x = "Month", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot(monthscatter)
    
    ## 6. Month Boxplot
   monthbox <- ggplot(statins %>% filter(!is.na(gap_days)),
           aes(x = prescription_month, y = gap_days)) +
      geom_boxplot(aes(fill = prescription_month), alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.15, alpha = 0.3, size = 1, color = "steelblue") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      scale_fill_viridis_d(option = "plasma", guide = "none") +
      scale_y_continuous(limits = c(0, 365), breaks = seq(0, 365, 30)) +
      labs(
        title = paste("Gap Days by Month | ATC:", code),
        subtitle = "Red diamonds = monthly means | Boxes = medians & IQRs",
        x = "Month", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
   plot(monthbox)
    
    ## 7. Sex Boxplot
   sexbox <- ggplot(statins %>% filter(!is.na(gap_days)),
           aes(x = sex_label, y = gap_days, fill = sex_label)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.15, alpha = 0.3, size = 1, color = "steelblue") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e")) +
      scale_y_continuous(limits = c(0, 365), breaks = seq(0, 365, 30)) +
      labs(
        title = paste("Statin Prescription Intervals by Sex | ATC:", code),
        subtitle = "Red diamonds = means | Boxes = medians & IQRs",
        x = "Sex", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(legend.position = "none") 
   print(sexbox)
    
    ## 8. Scheme Boxplot
    schemebox <- ggplot(statins %>% filter(!is.na(gap_days)),
           aes(x = scheme, y = gap_days, fill = scheme)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "steelblue") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      scale_fill_brewer(palette = "Pastel1") +
      scale_y_continuous(limits = c(0, 365), breaks = seq(0, 365, 30)) +
      labs(
        title = paste("Statin Prescription Intervals by Scheme | ATC:", code),
        subtitle = "Red diamonds = means | Jitter = individuals",
        x = "Scheme", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(legend.position = "none") 
    plot(schemebox)
    
    ## 9. Age Boxplot
    agebox <- ggplot(statins %>% filter(!is.na(gap_days)),
     aes(x = age_bracket, y = gap_days, fill = age_bracket)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "steelblue") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
      scale_y_continuous(limits = c(0, 365), breaks = seq(0, 365, 30)) +
      labs(
        title = paste("Statin Prescription Intervals by Age Group | ATC:", code),
        subtitle = "Red diamonds = means | Jitter = individuals",
        x = "Age Bracket", y = "Gap Days"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    plot(agebox)
  }
}
analyze_statin_prescription_gaps(c("C10AA01", "N02AA01", "A02BC03"))

