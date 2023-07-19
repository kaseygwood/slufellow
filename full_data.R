library(tidyverse)

library(readxl)

course_reg_full <- read_excel("course_reg_14_23.xlsx")
course_capacity <- read_excel("Course Capacity.xlsx")
course_capacity <- course_capacity |> rename("Course Section Name" = "Section Name") |> 
  rename("Course Section ID" = "Section ID") |> select(-"Section Number") |> select(-"...5")

course_reg_full <- left_join(course_reg_full, course_capacity, by = "Course Section ID")
course_reg_data <- course_reg_full |>
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
# fixing cross-listed courses

course_reg_xlist <- course_reg_data |> drop_na(`xlist Section1`)
course_reg_xlist <- course_reg_xlist |> group_by(`Course Title`, section, `Reporting Year`) |> mutate(enrolled = sum(Enrolled)) |> select(-Enrolled) |> rename("Enrolled" = "enrolled") |>
  unite("course", c(`Course Name`, `Course Title`), sep = ": ")
course_reg_noxlist <- course_reg_data |> filter(is.na(`xlist Section1`)) |> unite("course", c(`Course Name`, `Course Title`), sep = ": ")
course_reg_data <- full_join(course_reg_xlist, course_reg_noxlist)
course_reg_data <- course_reg_data |> group_by(course, `Reporting Year`) |>
  mutate(yearly_capacity = sum(`Section Capacity`, na.rm = TRUE))
View(course_reg_data)
course_reg_wide <-course_reg_data |>
  pivot_wider(names_from = section, values_from = `Section Capacity`,
              values_fn = list)


course_reg_wide <- course_reg_wide |>
  group_by(course, `Reporting Year`) |>
  summarise(across(starts_with("FA_"), ~sum(unlist(.), na.rm = TRUE)),
            across(starts_with("SP_"), ~sum(unlist(.), na.rm = TRUE)))


course_reg_capacity <- course_reg_wide |>
  group_by(course, `Reporting Year`) |>
  summarize(
    fall_capacity = sum(FA_01 + FA_02 + FA_03 + FA_04 + FA_05 + FA_06 + FA_07 + FA_08 + FA_09 + FA_10 + FA_11 + FA_12 + FA_13 + FA_14 + FA_15 +
                                  FA_16 + FA_17 + FA_20 + FA_21 + FA_22 + FA_23, na.rm = TRUE),
            spring_capacity = sum(SP_01 + SP_02 + SP_03 + SP_04 + SP_05 + SP_06 + SP_07 + SP_08 + SP_09 + SP_11 + SP_12 + SP_13 + SP_14 + SP_15 + SP_16 +
                                    SP_18 + SP_19, na.rm = TRUE)) |> rename("reporting_year" = "Reporting Year")


course_reg_data <- course_reg_data |> group_by(course, `Reporting Year`) |> 
  pivot_wider(names_from = section, values_from = Enrolled) |>
  rename("year" = "reporting_year") |>
  rename("reporting_year" = "Reporting Year")

course_reg_data <- course_reg_data |>
  group_by(course, reporting_year) |>
  summarise(across(starts_with("FA_"), ~sum(unlist(.), na.rm = TRUE)),
            across(starts_with("SP_"), ~sum(unlist(.), na.rm = TRUE)),
            `Course Number` = first(`Course Number`),
            Subject = first(Subject),
            yearly_capacity = first(yearly_capacity))

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

full_course_reg_data <- left_join(course_reg_data, course_reg_capacity, by=c("course", "reporting_year"))
View(full_course_reg_data)
write_csv(full_course_reg_data, "full_course_reg_data.csv")
#course_reg_data_yearly <- course_reg_data |> group_by(course, reporting_year) |>
# summarize(Enrolled = sum(Enrolled),
#         course = first(course)) |>
# arrange(reporting_year)

#course_reg_data_fall <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |> arrange(reporting_year) |> filter(semester == "FA")

#course_reg_data_spring <- course_reg_data |> separate(col = term, into = c("year", "semester"), sep = -2) |>
#arrange(reporting_year) |> filter(semester == "SP")

## TRY COMBINING SEMESTER AND SECTION BEFORESUMMINGTHESECTION



