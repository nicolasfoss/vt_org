###_____________________________________________________________________________
### I show an example of how an organization
### can apply the Vicarious Trauma Organizational Readiness Guide
### from the assessment provided by the Office of Justice Programs.  
###_____________________________________________________________________________
### The assessment can be found here:
### https://ovc.ojp.gov/sites/g/files/xyckuh226/files/media/document/os_vt-org_victim_services-508.pdf
###_____________________________________________________________________________
### The Office of Justice Programs provides a website full of resources that help
### interpret the VT-ORG measure and use it to improve an organization.
### An explanation of vicarious trauma:
### https://ovc.ojp.gov/program/vtt/what-is-vicarious-trauma
###_____________________________________________________________________________ 
### The website for the VT-ORG tool and plenty of resoures on how to use it!
### https://ovc.ojp.gov/program/vtt/vt-org-and-compendium
###_____________________________________________________________________________
### In my following script, I create a dataset from real responses
### on the survey and show my analyses and visualizations of the raw data.
### Feel free to give me feedback and enjoy!
###_____________________________________________________________________________

###_____________________________________________________________________________
# Install and load appropriate packages.
###_____________________________________________________________________________

install.packages(c("tidyverse", "assertive"))
library(tidyverse)
library(readxl)
library(assertive)
library(scales)
library(magrittr)
library(ggthemes)

#__________________________________________________
### Create functions to make analyses faster!
#__________________________________________________

# Function to read sheets.

read_my_sheets <- function(sheet) {
  assert_is_character(sheet)
  path <- file.path("C:/Users/ADDS 5/Desktop/R Projects/VT-ORG/vt_org.xlsx") # you can change the file.path call to use your own working directory
  read_excel(path, sheet = sheet) # I provided the file in the Github repo for this code.
  
}

# Function to take data.frame and pivot longer for plotting and further analyses.

pivot_my_df <- function(df_name, first_col, last_col, header) {

new_df_pivot <- df_name %>%
  mutate(Respondent = respondents, .before = first_col) %>%
  pivot_longer(cols = first_col:last_col, 
               names_to = "question",
               values_to = "value") %>%
  mutate(question = factor(question, 
                           levels = names(header)
  )
  )
}

# Function to get contingency table.

my_cont_tbl <- function(df) {
  
  assert_is_data.frame(df)
  
  new_cont_tbl <- table(df$question, df$value)
  
}

# Function to make plotable prop.table.

prop_plotable <- function(cont_tbl) {
  
  assert_is_table(cont_tbl)
  
  new_prop_tbl <- cont_tbl %>%
    prop.table(1) %>%
    multiply_by(100) %>%
    as.data.frame()
}

# Function to summarize to get average scores.

pivot_summary <- function(pivot_df) {
  
  assert_is_data.frame(pivot_df)
  
  new_df_summary <- pivot_df %>%
    group_by(question) %>%
    summarize(Average = mean(value, na.rm = TRUE))  
}

# Function to plot averages in colored bar charts.

plot_summary_df <- function(summary_df, subtitle, range_of_questions, plot_name, width = 7, height = 5) {
  
  assert_is_data.frame(summary_df)
  assert_is_numeric(range_of_questions)
  
  path <- file.path("C:/Users/ADDS 5/Desktop/R Projects/VT-ORG/")
  
  numbers <- range_of_questions
  
  labels = paste0("Q", numbers)
  
  new_colored_bar <-
    ggplot(summary_df, aes(question, Average, fill = question)) +
    geom_col(position = position_dodge(width = 0.5),
             color = "black",
             width = 0.75
             ) +
    labs(x = "Question",
         y = "Average Score",
         title = "Vicarious Trauma - Organizational Readiness Guide for Victim Services",
         subtitle = subtitle,
         caption = "***Total of 8 surveys collected",
         fill = "Question") + 
    scale_x_discrete(labels = labels)
  
  ggsave(filename = plot_name, plot = last_plot(), path = path, width = width, height = height, units = "in")
  
  return(new_colored_bar)
  
}

# Function for getting the average score across each dataset,
# turn into data.frame, useful for plotting.

get_averages <- function(data1, data2, data3, data4, data5) {
  
  mean1 <- mean(colMeans(data1, na.rm = TRUE))
  mean2 <- mean(colMeans(data2, na.rm = TRUE))
  mean3 <- mean(colMeans(data3, na.rm = TRUE))
  mean4 <- mean(colMeans(data4, na.rm = TRUE))
  mean5 <- mean(colMeans(data5, na.rm = TRUE))
  
  fac <- c("Leadership and Mission", 
           "Management and Supervision", 
           "Employee Empowerment and Work Environment", 
           "Training and Professional Development",
           "Staff Health and Wellness"
           )
  
  average_df <- data.frame(Data_Sets = factor(fac, levels = fac),
                           Averages = c(mean1, mean2, mean3, mean4, mean5)
                           )
}

# Function to plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram <- function(pivot_tbl, subtitle, fill = fill, plot_name, width = 7, height = 5) {

  assert_is_data.frame(pivot_tbl)
  assert_is_character(subtitle)
  assert_is_character(fill)
  assert_is_character(plot_name)
  
  path <- file.path("C:/Users/ADDS 5/Desktop/R Projects/VT-ORG/")

new_plot <- 
  ggplot(pivot_tbl, aes(value)) + 
  geom_histogram(fill = fill,
                 color = "black") + 
  facet_wrap(~ question) + 
  labs(title = "Vicarious Trauma - Organizational Readiness Guide for Victim Services",
       subtitle = subtitle,
       x = "Value",
       y = "Count",
       caption = paste0("***Total of 8 surveys collected\n", "***", sum(is.na(pivot_tbl)), " NA values detected")
       ) + 
  theme(plot.caption = element_text(hjust = 0))

ggsave(filename = plot_name, plot = last_plot(), path = path, width = width, height = height, units = "in")

return(new_plot)

}

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar <- function(prop_tbl, pivot_tbl, subtitle, range_of_questions, palette, plot_name, width = 7, height = 5) {

  assert_is_data.frame(prop_tbl)
  assert_is_data.frame(pivot_tbl)
  assert_is_character(subtitle)
  assert_is_numeric(range_of_questions)
  assert_is_character(palette)
  assert_is_character(plot_name)
  
  numbers <- range_of_questions
  labels <- paste0("Q", numbers)
  
path <- file.path("C:/Users/ADDS 5/Desktop/R Projects/VT-ORG/")
  
new_stacked_bar <- 
  ggplot(prop_tbl, aes(x = Var1, y = Freq, fill = Var2, palette = palette)) + 
  geom_col(position = "fill",
           color = "black") + 
  labs(x = "Survey Section",
       y = "Proportion",
       title = "Vicarious Trauma - Organizational Readiness Guide for Victim Services",
       subtitle = subtitle,
       caption = paste0("***Total of 8 surveys collected\n","***", sum(is.na(pivot_tbl)), " NA values detected"),
       fill = "Score") + 
  scale_fill_brewer(palette = palette) + 
  theme(plot.caption = element_text(hjust = 0)
  ) + 
  scale_x_discrete(labels = labels) + 
  theme_clean()

ggsave(filename = plot_name, plot = last_plot(), path = path, width = width, height = height, units = "in")

return(new_stacked_bar)

}

#__________________________________________________
### Get file headers.
### I took the section names of the assessment
### itself and made each section a column
### on an excel file to save time later.
### I shortened the names, so please refer to the
### assessment itself if the names are not clear.
#__________________________________________________

# There are 8 respondents to the survey.

respondents <- c(1:8)

# Get sheets and use the columns for new file headers.

leadership_mission_header <- read_my_sheets("leadership_mission")

management_supervision_header <- read_my_sheets("management_supervision")

employee_empowerment_work_env_header <- read_my_sheets("employee_empowerment_work_env")

training_professional_develpmnt_header <- read_my_sheets("training_professional_develpmnt")

staff_health_wellness_header <- read_my_sheets("staff_health_wellness")

#____________________________________________________________
### Set values for these datasets to set up analyses.
#____________________________________________________________

#____________________________________________________________
#Leadership / Mission section.
#____________________________________________________________

leadership_mission <- data.frame(one = c(4,5,5,4,4,4,5,5),
                                 two = c(4,5,4,4,4,4,4,5),
                                 three = c(4,5,4,3,5,4,4,5),
                                 four = c(4,4,5,3,2,3,4,3),
                                 five = c(3,5,5,4,5,4,5,5),
                                 six = c(4,4,5,3,3,3,3,3),
                                 seven = c(3,5,5,5,5,5,5,4),
                                 eight= c(3,5,5,5,5,4,5,4),
                                 nine = c(4,5,5,5,2,4,5,4),
                                 ten = c(3,5,5,5,3,4,5,4)
                                 )
# Better names.

names(leadership_mission) <- names(leadership_mission_header)

# Create a longer data.frame to perform calculations.

leadership_mission_pivot <- 
  pivot_my_df(leadership_mission, 
            "clear_decisive_vision", 
            "no_tolerance_discrim", 
            leadership_mission_header)

# Contingency table for leadership / mission.

leadership_mission_cont_tbl <- my_cont_tbl(leadership_mission_pivot)

# Plotable prop.table.

leadership_mission_prop_tbl <- prop_plotable(leadership_mission_cont_tbl)

# Summarize to get average scores.

leadership_mission_summary <- pivot_summary(leadership_mission_pivot)

#Plot averages across scores in dataset.

plot_summary_df(leadership_mission_summary, 
                subtitle = "Leadership Mission and Mission Results", 
                range_of_questions = 1:10, 
                plot_name = "leadership_mission_averages.png")

# Plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram(leadership_mission_pivot, 
                subtitle = "Leadership and Mission Results", 
                fill = "chartreuse", 
                plot_name = "leadership_mission_hist.png")

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar(leadership_mission_prop_tbl,
                  leadership_mission_pivot,
                  subtitle = "Leadership and Mission Results", 
                  range_of_questions = 1:10, 
                  palette = "PuOr", 
                  plot_name = "leadership_mission_stacked_bar.png")

#____________________________________________________________
# Management / supervision data.frame.
#____________________________________________________________

management_supervision <- data.frame(one = c(4,5,5,2,4,3,4,3),
                                     two = c(NA,5,5,2,4,3,4,3),
                                     three = c(4,4,5,2,3,3,2,3),
                                     four = c(4,4,5,2,4,3,4,3),
                                     five = c(4,NA,5,3,5,5,5,4),
                                     six = c(5,5,5,NA,4,4,2,5),
                                     seven = c(5,5,5,1,3,4,2,5),
                                     eight = c(4,5,5,2,3,3,2,5),
                                     nine = c(5,5,5,3,4,5,3,5),
                                     ten = c(5,5,5,2,4,5,3,4),
                                     eleven = c(4,4,5,3,3,4,4,5),
                                     twelve = c(4,4,5,2,3,3,2,4),
                                     thirteen = c(5,5,NA,3,4,4,3,4),
                                     fourteen = c(2,5,3,3,3,3,2,4),
                                     fifteen = c(4,5,5,4,5,5,5,5),
                                     sixteen = c(5,5,5,2,4,5,4,5),
                                     seventeen = c(5,NA,5,2,4,NA,2,4),
                                     eighteen = c(5,NA,5,2,4,5,2,4)
                                     )
# Better names.

names(management_supervision) <- names(management_supervision_header)

# Create a longer data.frame to perform calculations.

management_supervision_pivot <- 
  pivot_my_df(management_supervision, 
              "protocol__client_trauma", 
              "perf_eval_pos_contrb", 
              management_supervision_header)

# Contingency table for management / supervision.

management_supervision_cont_tbl <- my_cont_tbl(management_supervision_pivot)

# Plotable prop.table.

management_supervision_prop_tbl <- prop_plotable(management_supervision_cont_tbl)

# Summarize to get average scores.

management_supervision_summary <- pivot_summary(management_supervision_pivot)

#Plot averages across scores in dataset.

plot_summary_df(management_supervision_summary, 
                subtitle = "Management and Supervision Results", 
                range_of_questions = 1:18, 
                width = 8.5,
                height = 6.5,
                plot_name = "management_supervision_averages.png"
                )

# Plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram(management_supervision_pivot, 
                subtitle = "Management and Supervision Results",
                fill = "midnightblue",
                plot_name = "management_supervisoin_hist.png",
                width = 10,
                height = 6.5
                )

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar(management_supervision_prop_tbl,
                  management_supervision_pivot,
                  subtitle = "Management and Supervision Results",
                  range_of_questions = 1:18,
                  palette = "PuOr",
                  plot_name = "management_supervision_stacked_bar.png",
                  width = 8.5,
                  height = 6.5
                  )

#____________________________________________________________
# Employee Empowerment and Work Environment data.frame.
#____________________________________________________________

employee_empowerment_work_env <- data.frame(one = c(4,4,3,4,3,4,3,5),
                                            two = c(4,4,3,4,3,4,3,5),
                                            three = c(5,4,4,3,4,5,3,5),
                                            four = c(4,3,3,4,3,4,3,5),
                                            five = c(5,4,4,4,5,4,4,5),
                                            six = c(5,4,4,4,5,5,3,5),
                                            seven = c(5,5,4,4,3,5,3,5),
                                            eight = c(5,5,4,NA,5,4,3,4),
                                            nine = c(5,4,4,4,5,4,4,4),
                                            ten = c(4,3,4,4,3,4,4,4),
                                            eleven = c(5,3,3,NA,3,4,4,4), 
                                            twelve = c(5,3,2,NA,2,3,4,4),
                                            thirteen = c(5,4,3,4,5,4,4,5),
                                            fourteen = c(5,4,4,5,4,4,4,5),
                                            fifteen = c(5,4,3,5,5,4,4,4)
                                            )

# Better names.

names(employee_empowerment_work_env) <- names(employee_empowerment_work_env_header)

# Create a longer data.frame to perform calculations.

employee_empowerment_work_env_pivot <- pivot_my_df(employee_empowerment_work_env,
                                                   "staff_input_dev_policy",
                                                   "org_provid_diverse_tasks",
                                                   employee_empowerment_work_env_header)

# Contingency table for employee empowerment and work environment.

employee_empowerment_work_env_cont_tbl <- my_cont_tbl(employee_empowerment_work_env_pivot)

# Plotable prop.table.

employee_empowerment_work_env_prop_tbl <- prop_plotable(employee_empowerment_work_env_cont_tbl)

# Summarize to get average scores.

employee_empowerment_work_env_summary <- pivot_summary(employee_empowerment_work_env_pivot)

#Plot averages across scores in dataset.

plot_summary_df(employee_empowerment_work_env_summary,
                subtitle = "Employee Empowerment and Work Environment Results",
                range_of_questions = 1:15,
                plot_name = "employee_empowerment_work_env_averages.png",
                width = 8,
                height = 6
                )

# Plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram(employee_empowerment_work_env_pivot,
                subtitle = "Employee Empowerment and Work Environment Results",
                fill = "firebrick",
                plot_name = "employee_empowerment_work_env_hist.png",
                width = 8,
                height = 6)

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar(employee_empowerment_work_env_prop_tbl,
                  employee_empowerment_work_env_pivot,
                  subtitle = "Employee Empowerment and Work Environment Results",
                  range_of_questions = 1:15,
                  palette = "PuOr",
                  plot_name = "employee_empowerment_work_env_stacked_bar.png",
                  width = 8,
                  height = 6
                  )

#____________________________________________________________
# Training and Professional Development data.frame.
#____________________________________________________________

training_professional_develpmnt <- data.frame(one = c(3,5,5,5,5,5,4,5), 
                                              two = c(4,4,3,3,5,4,4,4), 
                                              three = c(4,3,3,3,5,4,4,4), 
                                              four = c(5,4,4,5,4,4,4,5), 
                                              five = c(4,3,4,5,4,4,4,5), 
                                              six = c(4,4,3,5,4,3,5,5), 
                                              seven = c(5,4,4,5,5,4,3,5),
                                              eight = c(5,3,3,5,4,4,4,5)
                                              )
# Better names.

names(training_professional_develpmnt) <- names(training_professional_develpmnt_header)

# Create a longer data.frame to perform calculations.

training_professional_develpmnt_pivot <- pivot_my_df(training_professional_develpmnt,
                                                     "org_orient_new_staff_to_job",
                                                     "encourage_networking_collab",
                                                     training_professional_develpmnt_header)

# Contingency table for training and professional development.

training_professional_develpmnt_cont_tbl <- my_cont_tbl(training_professional_develpmnt_pivot)

# Plotable prop.table.

training_professional_develpmnt_prop_tbl <- prop_plotable(training_professional_develpmnt_cont_tbl)

# Summarize to get average scores.

training_professional_develpmnt_summary <- pivot_summary(training_professional_develpmnt_pivot)

#Plot averages across scores in dataset.

plot_summary_df(training_professional_develpmnt_summary,
                subtitle = "Training and Professional Development Results",
                range_of_questions = 1:8,
                plot_name = "training_professional_develpmnt_averages.png"
                )

# Plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram(training_professional_develpmnt_pivot,
                subtitle = "Training and Professional Develoment Results",
                fill = "forestgreen",
                plot_name = "training_professional_develpmnt_hist.png"
                )

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar(training_professional_develpmnt_prop_tbl,
                  training_professional_develpmnt_pivot,
                  subtitle = "Training and Professional Development Results",
                  range_of_questions = 1:8,
                  palette = "PuOr",
                  plot_name = "training_professional_develpmnt_stacked_bar.png",
                  width = 7.5
                  )

#____________________________________________________________
# Staff Health and Wellness data.frame.
#____________________________________________________________

staff_health_wellness <- data.frame(one = c(4,4,3,4,2,4,4,NA), 
                                    two = c(4,3,2,4,3,3,4,4), 
                                    three = c(4,5,4,4,5,5,4,5), 
                                    four = c(5,4,4,4,5,5,4,5), 
                                    five = c(5,2,4,NA,5,NA,NA,5), 
                                    six = c(5,3,3,3,5,4,3,5), 
                                    seven = c(5,4,4,4,4,4,4,5),
                                    eight = c(5,3,4,4,4,4,4,5),
                                    nine = c(2,NA,2,NA,1,1,2,4),
                                    ten = c(3,2,4,4,5,1,3,4),
                                    eleven = c(5,4,2,5,2,5,4,4),
                                    twelve = c(5,2,2,NA,4,NA,3,3)
                                    )

# Better names.

names(staff_health_wellness) <- names(staff_health_wellness_header)

# Create a longer data.frame to perform calculations.

staff_health_wellness_pivot <- pivot_my_df(staff_health_wellness,
                                           "ask_job_applicants_coping",
                                           "org_plcy_support_worker_fam",
                                           staff_health_wellness_header)

# Contingency table for staff health and wellness.

staff_health_wellness_cont_tbl <- my_cont_tbl(staff_health_wellness_pivot)

# Plotable prop.table.

staff_health_wellness_prop_tbl <- prop_plotable(staff_health_wellness_cont_tbl)

# Summarize to get average scores.

staff_health_wellness_summary <- pivot_summary(staff_health_wellness_pivot)

#Plot averages across scores in dataset.

plot_summary_df(staff_health_wellness_summary,
                subtitle = "Staff Health and Wellness Results",
                range_of_questions = 1:12,
                plot_name = "staff_health_wellness_averages.png",
                width = 8,
                height = 6)

# Plot histogram to visualize distribution of scores for Leadership/Mission Section.

vtorg_histogram(staff_health_wellness_pivot,
                subtitle = "Staff Health and Wellness Results",
                fill = "deeppink",
                plot_name = "staff_health_wellness_hist.png",
                width = 8,
                height = 6)

# Stacked bar chart to show proportions for each section.

vtorg_stacked_bar(staff_health_wellness_prop_tbl,
                  staff_health_wellness_pivot,
                  subtitle = "Staff Health and Wellness Results",
                  range_of_questions = 1:12,
                  palette = "PuOr",
                  plot_name = "staff_health_wellness_stacked_bar.png",
                  width = 8,
                  height = 6
                  )

#____________________________________________________________
### Get dataset averages to understand where to best
### expand leadership team energy.
#____________________________________________________________

# Use custom function to create a data.frame of means to plot.

VT_ORG_means <- 
  get_averages(leadership_mission, 
             management_supervision, 
             employee_empowerment_work_env, 
             training_professional_develpmnt, 
             staff_health_wellness
             )

# Plot the means data.frame.

ggplot(VT_ORG_means, aes(x = factor(Data_Sets, levels = rev(levels(Data_Sets))), 
                         y = round(Averages, digits = 1), 
                         fill = Data_Sets, 
                         group = Data_Sets, 
                         label = round(Averages, digits = 1)
                         )
       ) + 
  geom_col(color = "black",
           position = position_dodge(width = 0.5),
           width = 0.5
           ) +
  geom_text(aes(x = factor(Data_Sets, levels = rev(levels(Data_Sets))),
                y = Averages),
            color = "white",
            size = 5,
            fontface = "bold",
            hjust = c(-0.15,-0.15,-0.15,-0.25,-0.15)
            ) + 
  coord_flip() + 
  labs(x = NULL,
       y = "Average Scores of Each Data Set",
       title = "Average Scores Across VT-ORG Data Sets",
       subtitle = "Summary Scores of 5 Data Sets Containing 8 Responses Each",
       caption = paste0("***Detection of a total of ", sum(sum(is.na(leadership_mission)),
                                                           sum(is.na(management_supervision)),
                                                           sum(is.na(employee_empowerment_work_env)),
                                                           sum(is.na(training_professional_develpmnt)),
                                                           sum(is.na(staff_health_wellness))
                                                           ),
                        " NA values across all data sets."
                        ),
       fill = "Data Set"
       ) + 
  theme_dark() +
  scale_fill_brewer(palette = "RdYlBu")

ggsave(filename = "vt_org_means.png", plot = last_plot(), width = 15, height = 6, unit = "in")
