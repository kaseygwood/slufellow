library(tidyverse)
library(readxl)
course_reg_full <- read_excel("course_reg_14_23.xlsx")

course_reg_data <- course_reg_full |> filter(Subject == "MATH" | 
                                               Subject == "STAT" | 
                                               Subject == "DATA" | 
                                               Subject == "CS") |>
  select(`Acad Year Term`, `Reporting Year`, `Course ID`, `Course Name`, 
         `Subject`, `Course Number`, `Course Title`, `Enrolled`, 
         `xlist Primary Course`, `xlist Section1`, `xlist Section2`, 
         `xlist Section3`, `xlist Section4`) |> arrange(desc(`Reporting Year`))
course_reg_data <- course_reg_data |> 
  group_by(`Course Title`, `Acad Year Term`) |> 
  summarize(Enrolled = sum(Enrolled),
            `Reporting Year` = first(`Reporting Year`),
            `Course Title` = first(`Course Title`)) |> 
  rename("course" = "Course Title") |>
  rename("term" = "Acad Year Term") |>
  rename("reporting_year" = "Reporting Year") |>
  filter(Enrolled >= 4) |> group_by(course, reporting_year) |>
  arrange(reporting_year) |>
  separate(col = term, into = c("year", "semester"), sep = -2) |>
  pivot_wider(names_from = semester, values_from= Enrolled)

course_reg_data <- course_reg_data |> select(-year) |> select(-SU)

course_reg_data <- course_reg_data %>%
  group_by(course, reporting_year) %>%
  summarize(
    reporting_year = first(reporting_year),
    yearly_enrolled = sum(FA + SP, na.rm = TRUE),
    FA = sum(FA, na.rm = TRUE),
    SP = sum(SP, na.rm = TRUE)
  )
course_reg_data <- course_reg_data |> group_by(course, reporting_year) |>
  summarize(yearly_enrolled = sum(FA + SP, na.rm = TRUE),
            FA = first(FA),
            SP = first(SP))

write_csv(course_reg_data, "course_reg_data.csv")

#course_reg_data_yearly <- course_reg_data |> group_by(course, reporting_year) |>
 # summarize(Enrolled = sum(Enrolled),
   #         course = first(course)) |>
 # arrange(reporting_year)

#course_reg_data_fall <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |> arrange(reporting_year) |> filter(semester == "FA")

#course_reg_data_spring <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |>
  #arrange(reporting_year) |> filter(semester == "SP")





