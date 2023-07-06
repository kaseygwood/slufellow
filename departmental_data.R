library(tidyverse)

library(readxl)

course_reg_full <- read_excel("course_reg_14_23.xlsx")


course_capacity <- read_excel("Course Capacity.xlsx")
course_capacity <- course_capacity |> rename("Course Section Name" = "Section Name") |> 
  rename("Course Section ID" = "Section ID") |> select(-"Section Number") |> select(-"...5")

course_reg_full <- left_join(course_reg_full, course_capacity, by = "Course Section ID")
course_reg_data <- course_reg_full |> filter(Subject == "MATH" | 
                                               Subject == "STAT" | 
                                               Subject == "DATA" | 
                                               Subject == "CS") |>
  select(`Acad Year Term`, `Reporting Year`, `Course ID`, `Course Name`, 
         `Subject`, `Course Number`, `Course Title`, `Enrolled`, 
         `xlist Primary Course`, `xlist Section1`, `xlist Section2`, 
         `xlist Section3`, `xlist Section4`, `Section Number`, `Section Capacity`) |> arrange(desc(`Reporting Year`))
course_reg_data <- course_reg_data |> separate(col = `Acad Year Term`, into = c("reporting_year", "semester"), sep = -2) 


#course_reg_data <- course_reg_data |> unite("course", c(`Course Name`, `Course Title`), sep = ": ") |> group_by(course, semester, `Reporting Year`) |>
  #mutate(semester_capacity = sum(`Section Capacity`, na.rm = TRUE))

#course_reg_fall <- course_reg_data |> unite("course", c(`Course Name`, `Course Title`), sep = ": ") |> 
 # filter(semester == "FA") |>
  #group_by(course, `Reporting Year`) |>
  #mutate(semester_capacity = sum(`Section Capacity`, na.rm = TRUE))

#course_reg_spring <- course_reg_data |> unite("course", c(`Course Name`, `Course Title`), sep = ": ") |>
 # filter(semester == "SP") |>
  #group_by(course, `Reporting Year`) |>
  #mutate(semester_capacity = sum(`Section Capacity`, na.rm = TRUE))

#course_reg_data <- bind_rows(course_reg_fall, course_reg_spring)

course_reg_data <- course_reg_data |> unite("section",c(semester, `Section Number`)) 
View(course_reg_data)
# fixing cross-listed courses
course_reg_xlist <- course_reg_data |> drop_na(`xlist Section1`)
course_reg_xlist <- course_reg_xlist |> group_by(`Course Title`, section, `Reporting Year`) |> mutate(enrolled = sum(Enrolled)) |> select(-Enrolled) |> rename("Enrolled" = "enrolled") |>
  unite("course", c(`Course Name`, `Course Title`), sep = ": ")
course_reg_noxlist <- course_reg_data |> filter(is.na(`xlist Section1`)) |> unite("course", c(`Course Name`, `Course Title`), sep = ": ")
course_reg_data <- full_join(course_reg_xlist, course_reg_noxlist)
View(course_reg_data)
course_reg_data <- course_reg_data |> group_by(course, `Reporting Year`) |>
  mutate(yearly_capacity = sum(`Section Capacity`, na.rm = TRUE))

course_reg_wide <-course_reg_data |>
  pivot_wider(names_from = section, values_from = `Section Capacity`)
course_reg_wide <- course_reg_wide |> 
  group_by(course, `Reporting Year`) |>
  summarise(FA_01 = sum(FA_01, na.rm = TRUE),
            FA_02 = sum(FA_02, na.rm = TRUE),
            FA_03 = sum(FA_03, na.rm = TRUE),
            FA_04 = sum(FA_04, na.rm = TRUE),
            FA_05 = sum(FA_05, na.rm = TRUE),
            FA_06 = sum(FA_06, na.rm = TRUE),
            FA_07 = sum(FA_07, na.rm = TRUE),
            FA_08 = sum(FA_08, na.rm = TRUE),
            FA_09 = sum(FA_09, na.rm = TRUE),
            FA_10 = sum(FA_10, na.rm = TRUE),
            SP_01 = sum(SP_01, na.rm = TRUE),
            SP_02 = sum(SP_02, na.rm = TRUE),
            SP_03 = sum(SP_03, na.rm = TRUE),
            SP_04 = sum(SP_04, na.rm = TRUE),
            SP_05 = sum(SP_05, na.rm = TRUE),
            SP_06 = sum(SP_06, na.rm = TRUE),
            SP_07 = sum(SP_07, na.rm = TRUE),
            SP_08 = sum(SP_08, na.rm = TRUE),
            SP_09 = sum(SP_09, na.rm = TRUE),
            SP_11 = sum(SP_11, na.rm = TRUE),
            SP_12 = sum(SP_12, na.rm = TRUE),
            SP_13 = sum(SP_13, na.rm = TRUE),
            SP_14 = sum(SP_14, na.rm = TRUE),
            SP_15 = sum(SP_15, na.rm = TRUE),
            SP_16 = sum(SP_16, na.rm = TRUE),
            SP_18 = sum(SP_18, na.rm = TRUE),
            SP_19 = sum(SP_19, na.rm = TRUE),
            FA_11 = sum(FA_11, na.rm = TRUE),
            FA_12 = sum(FA_12, na.rm = TRUE),
            FA_13 = sum(FA_13, na.rm = TRUE),
            FA_14 = sum(FA_14, na.rm = TRUE),
            FA_15 = sum(FA_15, na.rm = TRUE),
            FA_16 = sum(FA_16, na.rm = TRUE),
            FA_17 = sum(FA_17, na.rm = TRUE),
            FA_20 = sum(FA_20, na.rm = TRUE),
            FA_21 = sum(FA_21, na.rm = TRUE),
            FA_22 = sum(FA_22, na.rm = TRUE),
            FA_23 = sum(FA_23, na.rm = TRUE))


course_reg_capacity <- course_reg_wide |>
  group_by(course, `Reporting Year`) |>
  summarize(fall_capacity = sum(FA_01 + FA_02 + FA_03 + FA_04 + FA_05 + FA_06 + FA_07 + FA_08 + FA_09 + FA_10 + FA_11 + FA_12 + FA_13 + FA_14 + FA_15 +
                                  FA_16 + FA_17 + FA_20 + FA_21 + FA_22 + FA_23, na.rm = TRUE),
            spring_capacity = sum(SP_01 + SP_02 + SP_03 + SP_04 + SP_05 + SP_06 + SP_07 + SP_08 + SP_09 + SP_11 + SP_12 + SP_13 + SP_14 + SP_15 + SP_16 +
                                    SP_18 + SP_19, na.rm = TRUE)) |> rename("reporting_year" = "Reporting Year")

View(course_reg_capacity)


course_reg_data <- course_reg_data |> group_by(course, `Reporting Year`) |> 
  pivot_wider(names_from = section, values_from = Enrolled) |>
  select(-reporting_year) |>
  rename("reporting_year" = "Reporting Year")
View(course_reg_data)
course_reg_data <- course_reg_data |>
  group_by(course, reporting_year) |>
  summarize(yearly_capacity = first(yearly_capacity),
            FA_01 = sum(FA_01, na.rm = TRUE),
            FA_02 = sum(FA_02, na.rm = TRUE),
            FA_03 = sum(FA_03, na.rm = TRUE),
            FA_04 = sum(FA_04, na.rm = TRUE),
            FA_05 = sum(FA_05, na.rm = TRUE),
            FA_06 = sum(FA_06, na.rm = TRUE),
            FA_07 = sum(FA_07, na.rm = TRUE),
            FA_08 = sum(FA_08, na.rm = TRUE),
            FA_09 = sum(FA_09, na.rm = TRUE),
            FA_10 = sum(FA_10, na.rm = TRUE),
            SP_01 = sum(SP_01, na.rm = TRUE),
            SP_02 = sum(SP_02, na.rm = TRUE),
            SP_03 = sum(SP_03, na.rm = TRUE),
            SP_04 = sum(SP_04, na.rm = TRUE),
            SP_05 = sum(SP_05, na.rm = TRUE),
            SP_06 = sum(SP_06, na.rm = TRUE),
            SP_07 = sum(SP_07, na.rm = TRUE),
            SP_08 = sum(SP_08, na.rm = TRUE),
            SP_09 = sum(SP_09, na.rm = TRUE),
            SP_11 = sum(SP_11, na.rm = TRUE),
            SP_12 = sum(SP_12, na.rm = TRUE),
            SP_13 = sum(SP_13, na.rm = TRUE),
            SP_14 = sum(SP_14, na.rm = TRUE),
            SP_15 = sum(SP_15, na.rm = TRUE),
            SP_16 = sum(SP_16, na.rm = TRUE),
            SP_18 = sum(SP_18, na.rm = TRUE),
            SP_19 = sum(SP_19, na.rm = TRUE),
            FA_11 = sum(FA_11, na.rm = TRUE),
            FA_12 = sum(FA_12, na.rm = TRUE),
            FA_13 = sum(FA_13, na.rm = TRUE),
            FA_14 = sum(FA_14, na.rm = TRUE),
            FA_15 = sum(FA_15, na.rm = TRUE),
            FA_16 = sum(FA_16, na.rm = TRUE),
            FA_17 = sum(FA_17, na.rm = TRUE),
            FA_20 = sum(FA_20, na.rm = TRUE),
            FA_21 = sum(FA_21, na.rm = TRUE),
            FA_22 = sum(FA_22, na.rm = TRUE),
            FA_23 = sum(FA_23, na.rm = TRUE),
            `Course Number` = first(`Course Number`),
            Subject = first(Subject))
course_reg_data <- course_reg_data |>
  group_by(course, reporting_year) |>
  summarize(yearly_enrolled = sum(FA_01 + SP_01 + FA_02 + SP_02 + FA_03 + SP_03 +
                                  FA_04 + SP_04 + FA_05 + SP_05 + FA_06 + SP_06 +
                                    FA_07 + SP_07 + FA_08 + SP_08 + FA_09 + SP_09 + 
                                    FA_10 + FA_11 + FA_12 + FA_13 + FA_14 + FA_15 +
                                    FA_16 + FA_17 + FA_20 + FA_21 + FA_22 + FA_23 +
                                    SP_11 + SP_12 + SP_13 + SP_14 + SP_15 + SP_16 +
                                    SP_18 + SP_19, na.rm = TRUE),
            fall_enrolled = sum(FA_01 + FA_02 + FA_03 + FA_04 + FA_05 + FA_06 + FA_07 + FA_08 + FA_09 + FA_10 + FA_11 + FA_12 + FA_13 + FA_14 + FA_15 +
                                  FA_16 + FA_17 + FA_20 + FA_21 + FA_22 + FA_23, na.rm = TRUE),
            spring_enrolled = sum(SP_01 + SP_02 + SP_03 + SP_04 + SP_05 + SP_06 + SP_07 + SP_08 + SP_09 + SP_11 + SP_12 + SP_13 + SP_14 + SP_15 + SP_16 +
                                    SP_18 + SP_19, na.rm = TRUE),
            FA_01 = first(FA_01),
            FA_02 = first(FA_02),
            FA_03 = first(FA_03),
            FA_04 = first(FA_04),
            FA_05 = first(FA_05),
            FA_06 = first(FA_06),
            FA_07 = first(FA_07),
            FA_08 = first(FA_08),
            FA_09 = first(FA_09),
            FA_10 = first(FA_10),
            SP_01 = first(SP_01),
            SP_02 = first(SP_02),
            SP_03 = first(SP_03),
            SP_04 = first(SP_04),
            SP_05 = first(SP_05),
            SP_06 = first(SP_06),
            SP_07 = first(SP_07),
            SP_08 = first(SP_08),
            SP_09 = first(SP_09),
            SP_11 = first(SP_11),
            SP_12 = first(SP_12),
            SP_13 = first(SP_13),
            SP_14 = first(SP_14),
            SP_15 = first(SP_15),
            SP_16 = first(SP_16),
            SP_18 = first(SP_18),
            SP_19 = first(SP_19),
            FA_11 = first(FA_11),
            FA_12 = first(FA_12),
            FA_13 = first(FA_13),
            FA_14 = first(FA_14),
            FA_15 = first(FA_15),
            FA_16 = first(FA_16),
            FA_17 = first(FA_17),
            FA_20 = first(FA_20),
            FA_21 = first(FA_21),
            FA_22 = first(FA_22),
            FA_23 = first(FA_23),
            `Course Number` = first(`Course Number`),
            Subject = first(Subject),
            yearly_capacity = first(yearly_capacity)) 

course_reg_data <- left_join(course_reg_data, course_reg_capacity, by=c("course", "reporting_year"))
View(course_reg_data)
write_csv(course_reg_data, "course_reg_data.csv")
View(course_reg_data)
#course_reg_data_yearly <- course_reg_data |> group_by(course, reporting_year) |>
 # summarize(Enrolled = sum(Enrolled),
   #         course = first(course)) |>
 # arrange(reporting_year)

#course_reg_data_fall <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |> arrange(reporting_year) |> filter(semester == "FA")

#course_reg_data_spring <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |>
  #arrange(reporting_year) |> filter(semester == "SP")

## TRY COMBINING SEMESTER AND SECTION BEFORESUMMINGTHESECTION



