# Clean data and transform from long to wide

source("../project_support.r")

# Load data
related_questions <- fread("./input/question_dictionary.csv")
v6_questions <- fread("./input/drh_v6_poll.csv") %>% rename(quest = Question, quest_desc = `Question description`)
data <- fread("./input/drh.csv") 
analysis_questions <- fread("./input/analysis_questions.csv")

# Convert date ranges to numeric & Question ID to integer for joining and change year 0 to 1
data <- data[, start_date := str_extract(`Date range`, "[^-]+")][
  , start_year := as.numeric(gsub("([0-9]+).*$", "\\1", start_date))][
  , start_year := if_else(grepl("B", start_date), -start_year, start_year)][
  , start_year := if_else(start_year == 0, 1, start_year)][
  , end_date := sub(".*-", "", `Date range`)][
  , end_year := as.numeric(gsub("([0-9]+).*$", "\\1", end_date))][
  , end_year := if_else(grepl("B", end_date), -end_year, end_year)][
  , end_year := if_else(end_year == 0, 1, end_year)][
  , c("start_date", "end_date") := NULL][
  , `Question ID` := as.integer(`Question ID`)]

# Convert questions from other polls into equivalent v6 poll questions
data_stand <- related_questions[data, on = "Question ID", allow.cartesian=TRUE][
  , `Question ID` := if_else(Poll != "Religious Group (v6)" & !is.na(`Related question ID`), `Related question ID`, `Question ID`)]
data_stand <- v6_questions[data_stand, on = "Question ID"][
  , Question := quest][
  , `Parent question` := `Parent Question`][
  , c("Poll", "quest", "quest_desc", "Parent Question", "Related question ID") := NULL][
  !is.na(Question)]
data_stand <- unique(data_stand)

# Remove Field doesn't know and I don't know answers
data_known <- data_stand[Answers != "Field doesn't know" & Answers != "field doesn't know" & Answers != "I don't know" & Answers != "I don't know" & Answers != "Field doesn't know; I don't know"]

# One-hot encode nominal data which allows users to select all from list of answers

# 4765 Where is iconography present [select all that apply]:
# Correct entry 997 where answers are split into separate rows
icon <- data_known[`Question ID` == "4765"][
  , Answers := ifelse(`Entry ID` == "997", "On persons; Some public spaces", Answers)][
  , `4765_1` := ifelse(grepl("On persons", Answers), "1", "0")][
  , `4765_2` := ifelse(grepl("At home", Answers), "1", "0")][
  , `4765_3` := ifelse(grepl("Only religious public space", Answers), "1", "0")][
  , `4765_4` := ifelse(grepl("Some public spaces", Answers), "1", "0")][
  , `4765_5` := ifelse(grepl("All public spaces", Answers), "1", "0")][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
icon <- melt(icon, measure.vars = c("4765_1", "4765_2", "4765_3", "4765_4", "4765_5"),
             variable.name = "Question ID", value.name = "Answer values")
icon <- unique(icon)
icon <- setcolorder(icon, colnames(data_known))

# 5087 Moral norms apply to
# Correct entry 843 where answers are split into separate rows
moral <- data_known[`Question ID` == "5087"][
  , Answers := ifelse(`Entry ID` == "843", "All individuals within society; Only specialized religious class", Answers)][
  , `5087_1` := ifelse(grepl("Only specialized religious class", Answers), "1", "0")][
  , `5087_2` := ifelse(grepl("Only one class of society", Answers), "1", "0")][
  , `5087_3` := ifelse(grepl("Only one gender", Answers), "1", "0")][
  , `5087_4` := ifelse(grepl("All individuals within society \\(excepting slaves, aliens\\)", Answers), "1", "0")][
  , `5087_5` := ifelse(grepl("All individuals within society", Answers), "1", "0")][
  , `5087_6` := ifelse(grepl("All individuals within contemporary world", Answers), "1", "0")][
  , `5087_7` := ifelse(grepl("All individuals \\(any time period\\)", Answers), "1", "0")][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
moral <- melt(moral, measure.vars = c("5087_1", "5087_2", "5087_3", "5087_4", "5087_5", "5087_6", "5087_7"),
              variable.name = "Question ID", value.name = "Answer values")
moral <- unique(moral)
moral <- setcolorder(moral, colnames(data_known))

# 5227 Does the religious group in question provide food for themselves Please characterize the forms/level of food production [choose all that apply]:
# Answers of Other [specify in comments] are excluded
# Correct entry 977 where answers are split into separate rows
self_food <- data_known[`Question ID` == "5227"][
  , Answers := ifelse(`Entry ID` == "977", "Large-scale agriculture (e.g., monocropping, organized irrigation systems); Other [specify in comments]", Answers)][
  , `5227_1` := ifelse(grepl("Gathering", Answers), "1", "0")][
  , `5227_2` := ifelse(grepl("Hunting \\(including marine animals\\)", Answers), "1", "0")][
  , `5227_3` := ifelse(grepl("Fishing", Answers), "1", "0")][
  , `5227_4` := ifelse(grepl("Pastoralism", Answers), "1", "0")][
  , `5227_5` := ifelse(grepl("Large-scale agriculture \\[organized irrigation systems, etc.\\]", Answers), "1", "0")][
  , `5227_6` := ifelse(grepl("Small-scale agriculture \\/ horticultural gardens or orchards", Answers), "1", "0")][
  , `5227_7` := ifelse(grepl("Large-scale agriculture \\(e.g., monocropping, organized irrigation systems\\)", Answers), "1", "0")][
  , `5227_8` := ifelse(grepl("Cannibalism", Answers), "1", "0")][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
self_food <- melt(self_food, measure.vars = c("5227_1", "5227_2", "5227_3", "5227_4", "5227_5", "5227_6", "5227_7", "5227_8"),
                  variable.name = "Question ID", value.name = "Answer values")
self_food <- unique(self_food)
self_food <- setcolorder(self_food, colnames(data_known))

# 5229 Is food provided to the group’s adherents by an institution(s) other than the religious group in question Please characterize the forms/level of food production [choose all that apply]:
# Answers of Other [specify in comments] are excluded
other_food <- data_known[`Question ID` == "5229"][
  , `Answer values` := ifelse(`Entry ID` == "977", "7; 0", `Answer values`)][
  , `5229_1` := ifelse(grepl("Gathering", Answers), "1", "0")][
  , `5229_2` := ifelse(grepl("Hunting \\(including marine animals\\)", Answers), "1", "0")][
  , `5229_3` := ifelse(grepl("Fishing", Answers), "1", "0")][
  , `5229_4` := ifelse(grepl("Pastoralism", Answers), "1", "0")][
  , `5229_5` := ifelse(grepl("Large-scale agriculture \\[organized irrigation systems, etc.\\]", Answers), "1", "0")][
  , `5229_6` := ifelse(grepl("Small-scale agriculture \\/ horticultural gardens or orchards", Answers), "1", "0")][
  , `5229_7` := ifelse(grepl("Large-scale agriculture \\(e.g., monocropping, organized irrigation systems\\)", Answers), "1", "0")][
  , `5229_8` := ifelse(grepl("Cannibalism", Answers), "1", "0")][
  , c("Question ID","Answer values"):=NULL][
  , c("Answers","Note") := ""]
other_food <- melt(other_food, measure.vars = c("5229_1", "5229_2", "5229_3", "5229_4", "5229_5", "5229_6", "5229_7", "5229_8"),
                   variable.name = "Question ID", value.name = "Answer values")
other_food <- unique(other_food)
other_food <- setcolorder(other_food, colnames(data_known))

# Remove qualitative (references) and quantitative data 
data_nominal <- data_known[`Data Type` != "Nominal - Multiple" & `Data Type` != "Qualitative" & `Data Type` != "Continuous" & `Data Type` != "Discrete"]
data_nominal <- rbind(data_nominal, icon, moral, self_food, other_food)

# Create multiple answers questions answers
data_multi <- data_nominal[, Answers := ifelse(`Question ID` == "4765_1" & `Answer values` == 1, "On persons - Present", 
                                  ifelse(`Question ID` == "4765_1" & `Answer values` == 0, "On persons - Absent", 
                                  ifelse(`Question ID` == "4765_2" & `Answer values` == 1, "At home - Present",
                                  ifelse(`Question ID` == "4765_2" & `Answer values` == 0, "At home - Absent", 
                                  ifelse(`Question ID` == "4765_3" & `Answer values` == 1, "Only religious public space - Present", 
                                  ifelse(`Question ID` == "4765_3" & `Answer values` == 0, "Only religious public space - Absent", 
                                  ifelse(`Question ID` == "4765_4" & `Answer values` == 1, "Some public spaces - Present", 
                                  ifelse(`Question ID` == "4765_4" & `Answer values` == 0, "Some public spaces - Absent",
                                  ifelse(`Question ID` == "4765_5" & `Answer values` == 1, "All public spaces - Present", 
                                  ifelse(`Question ID` == "4765_5" & `Answer values` == 0, "All public spaces - Absent",
                                  ifelse(`Question ID` == "5087_1" & `Answer values` == 1, "Only specialized religious class - Present", 
                                  ifelse(`Question ID` == "5087_1" & `Answer values` == 0, "Only specialized religious class - Absent", 
                                  ifelse(`Question ID` == "5087_2" & `Answer values` == 1, "Only one class of society - Present", 
                                  ifelse(`Question ID` == "5087_2" & `Answer values` == 0, "Only one class of society - Absent", 
                                  ifelse(`Question ID` == "5087_3" & `Answer values` == 1, "Only one gender - Present", 
                                  ifelse(`Question ID` == "5087_3" & `Answer values` == 0, "Only one gender - Absent", 
                                  ifelse(`Question ID` == "5087_4" & `Answer values` == 1, "All individuals within society (excepting slaves, aliens) - Present", 
                                  ifelse(`Question ID` == "5087_4" & `Answer values` == 0, "All individuals within society (excepting slaves, aliens) - Absent",
                                  ifelse(`Question ID` == "5087_5" & `Answer values` == 1, "All individuals within society - Present", 
                                  ifelse(`Question ID` == "5087_5" & `Answer values` == 0, "All individuals within society - Absent",
                                  ifelse(`Question ID` == "5087_6" & `Answer values` == 1, "All individuals within contemporary world - Present", 
                                  ifelse(`Question ID` == "5087_6" & `Answer values` == 0, "All individuals within contemporary world - Absent",
                                  ifelse(`Question ID` == "5087_7" & `Answer values` == 1, "All individuals (any time period) - Present", 
                                  ifelse(`Question ID` == "5087_7" & `Answer values` == 0, "All individuals (any time period) - Absent",
                                  ifelse(`Question ID` == "5227_1" & `Answer values` == 1, "Gathering - Present", 
                                  ifelse(`Question ID` == "5227_1" & `Answer values` == 0, "Gathering - Absent",
                                  ifelse(`Question ID` == "5227_2" & `Answer values` == 1, "Hunting (including marine animals) - Present", 
                                  ifelse(`Question ID` == "5227_2" & `Answer values` == 0, "Hunting (including marine animals) - Absent", 
                                  ifelse(`Question ID` == "5227_3" & `Answer values` == 1, "Fishing - Present", 
                                  ifelse(`Question ID` == "5227_3" & `Answer values` == 0, "Fishing - Absent", 
                                  ifelse(`Question ID` == "5227_4" & `Answer values` == 1, "Pastoralism - Present", 
                                  ifelse(`Question ID` == "5227_4" & `Answer values` == 0, "Pastoralism - Absent", 
                                  ifelse(`Question ID` == "5227_5" & `Answer values` == 1, "Large-scale agriculture [organized irrigation systems, etc.] - Present", 
                                  ifelse(`Question ID` == "5227_5" & `Answer values` == 0, "Large-scale agriculture [organized irrigation systems, etc.] - Absent", 
                                  ifelse(`Question ID` == "5227_6" & `Answer values` == 1, "Small-scale agriculture / horticultural gardens or orchards - Present", 
                                  ifelse(`Question ID` == "5227_6" & `Answer values` == 0, "Small-scale agriculture / horticultural gardens or orchards - Absent",
                                  ifelse(`Question ID` == "5227_7" & `Answer values` == 1, "Large-scale agriculture (e.g., monocropping, organized irrigation systems) - Present", 
                                  ifelse(`Question ID` == "5227_7" & `Answer values` == 0, "Large-scale agriculture (e.g., monocropping, organized irrigation systems) - Absent",
                                  ifelse(`Question ID` == "5227_8" & `Answer values` == 1, "Cannibalism - Present", 
                                  ifelse(`Question ID` == "5227_8" & `Answer values` == 0, "Cannibalism - Absent", 
                                  ifelse(`Question ID` == "5229_1" & `Answer values` == 1, "Gathering - Present", 
                                  ifelse(`Question ID` == "5229_1" & `Answer values` == 0, "Gathering - Absent", 
                                  ifelse(`Question ID` == "5229_2" & `Answer values` == 1, "Hunting (including marine animals) - Present", 
                                  ifelse(`Question ID` == "5229_2" & `Answer values` == 0, "Hunting (including marine animals) - Absent", 
                                  ifelse(`Question ID` == "5229_3" & `Answer values` == 1, "Fishing - Present", 
                                  ifelse(`Question ID` == "5229_3" & `Answer values` == 0, "Fishing - Absent",
                                  ifelse(`Question ID` == "5229_4" & `Answer values` == 1, "Pastoralism - Present", 
                                  ifelse(`Question ID` == "5229_4" & `Answer values` == 0, "Pastoralism - Absent", Answers))))))))))))))))))))))))))))))))))))))))))))))))][, 
                       Answers := ifelse(`Question ID` == "5229_5" & `Answer values` == 1, "Large-scale agriculture [organized irrigation systems, etc.] - Present", 
                                  ifelse(`Question ID` == "5229_5" & `Answer values` == 0, "Large-scale agriculture [organized irrigation systems, etc.] - Absent", 
                                  ifelse(`Question ID` == "5229_6" & `Answer values` == 1, "Small-scale agriculture / horticultural gardens or orchards - Present", 
                                  ifelse(`Question ID` == "5229_6" & `Answer values` == 0, "Small-scale agriculture / horticultural gardens or orchards - Absent", 
                                  ifelse(`Question ID` == "5229_7" & `Answer values` == 1, "Large-scale agriculture (e.g., monocropping, organized irrigation systems) - Present", 
                                  ifelse(`Question ID` == "5229_7" & `Answer values` == 0, "Large-scale agriculture (e.g., monocropping, organized irrigation systems) - Absent", 
                                  ifelse(`Question ID` == "5229_8" & `Answer values` == 1, "Cannibalism - Present", 
                                  ifelse(`Question ID` == "5229_8" & `Answer values` == 0, "Cannibalism - Absent", Answers))))))))]
                                                                                                                
# Remove questions with [specify] answers
data_non_specify <- data_multi[!grepl("\\[specify\\]", Question)] 

# Remove Other [specify in comments], Other [specify] and Specify: answers
data_non_specify <- data_non_specify[!grepl("\\[specify in comments\\]", Answers) & !grepl("Other \\[specify\\]", Answers)  & !grepl("specify:", Answers)]

# Remove questions with Yes [specify]: answers
yes_specify_quest <- as_tibble(data_non_specify) %>%
  filter(grepl("Yes \\[specify\\]", Answers)) %>%
  select(Question, `Question ID`) %>%
  distinct()
data_non_specify <- data_non_specify[!`Question ID` %in% yes_specify_quest$`Question ID`] 

# Correct answers/answer values of question 4763 How strict is pilgrimage:
drh_4763_cor <- data_non_specify %>%
  mutate(Answers = ifelse(`Question ID` == 4763 & grepl("optional \\(rare\\)", Answers), "Optional (rare)",
                   ifelse(`Question ID` == 4763 & grepl("optional \\(common\\)", Answers), "Optional (common)",
                   ifelse(`Question ID` == 4763 & grepl("obligatory for some", Answers), "Obligatory for some", Answers)))) %>%
  mutate(`Answer values`= ifelse(`Question ID` == 4763 & grepl("Optional \\(rare\\)", Answers), 0,
                   ifelse(`Question ID` == 4763 & grepl("Optional \\(common\\)", Answers), 1,
                   ifelse(`Question ID` == 4763 & grepl("Obligatory for some", Answers), 2,`Answer values`))))

# Split multiple answer values into multiple rows
# Paul the Apostle (355ER) The supreme high god can see you everywhere (in the dark, at home): - Answer values 1;-1 (Yes; Field doesn't know)
# Qumran Movement (176R) Originated from divine or semi-divine human beings: - Answer values 1; 0 (Yes; No)
# Qumran Movement (176R) Originated from non-divine human being: - Answer values 1; 0 (Yes; No)
data_multi_ans_row <- drh_4763_cor[
  , `Answer values` := gsub("1; -1", "1,, -1", `Answer values`)][
  , `Answer values` := gsub("1; 0", "1,, 0", `Answer values`)]
data_multi_ans_row_cor <- cSplit(data_multi_ans_row, "Answer values", ",,", "long")
data_multi_ans_row_cor <- data_multi_ans_row_cor[
  , `Answer values` := gsub('c\\(\\"1\\"', "1", `Answer values`)][
  , `Answer values` := gsub('\\" -1\\"\\)', "-1", `Answer values`)][
  , `Answer values` := gsub('\\" 0\\"\\)', "0", `Answer values`)]
data_multi_ans_row_cor <- unique(data_multi_ans_row_cor)  

# Split data by branching question answers
data_branch <- data_multi_ans_row_cor[
  , `Branching question` := gsub("\\(", "", `Branching question`)][
  , `Branching question` := gsub("\\)", "", `Branching question`)][
  , `Branching question` := gsub("common people, general populace", "", `Branching question`)][
  , `Branching question` := gsub("Status of Readership: ", "", `Branching question`)][
  , `Branching question` := gsub("Status of Participants: ", "", `Branching question`)][
  , `Branching question` := gsub("Non-elite", "N", `Branching question`)][
  , `Branching question` := gsub("Elite", "E", `Branching question`)][
  , `Branching question` := gsub("Religious Specialists", "R", `Branching question`)][
  , `Branching question` := gsub(" ", "", `Branching question`, fixed = TRUE)][
  , `Branching question` := strsplit(`Branching question`, ",")][
  , list(`Branching question` = as.character(unlist(`Branching question`))), by = setdiff(names(data_multi_ans_row_cor), "Branching question")]

# If an entry has multiple overlapping time periods but a single answer, combine these into 1 time period
data_comb_date <- as_tibble(data_branch) %>%
  group_by(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, `Question ID`, `Answer values`) %>%
  mutate(other_start_year = lag(start_year), other_end_year = lag(end_year)) %>%
  mutate(other_start_year = ifelse(is.na(other_start_year), lead(start_year), NA), other_end_year = ifelse(is.na(other_end_year), lead(end_year), NA)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year & other_end_year >= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year & other_start_year <= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, Question, `Question ID`, `Answer values`, Answers, `Parent question`, start_year, end_year) %>%
  distinct() %>%
  mutate(other_start_year_lead = lead(start_year), other_end_year_lead = lead(end_year)) %>%
  mutate(other_start_year_lag = lag(start_year), other_end_year_lag = lag(end_year)) %>%
  mutate(other_start_year = other_start_year_lead, other_end_year = other_end_year_lead) %>%
  mutate(other_start_year = ifelse(is.na(other_start_year), other_start_year_lag, other_start_year), other_end_year = ifelse(is.na(other_end_year), other_end_year_lag, other_end_year)) %>%
  mutate(new_start_year = ifelse(other_start_year < start_year & other_start_year <= end_year, other_start_year, start_year)) %>%
  mutate(new_end_year = ifelse(other_end_year > end_year & other_end_year >= start_year, other_end_year, end_year)) %>%
  mutate(start_year = ifelse(!is.na(new_start_year), new_start_year, start_year), end_year = ifelse(!is.na(new_end_year), new_end_year, end_year)) %>%
  distinct() 

# Recombine entries where the different branching question answers (Elite, Non-elite and religious specialist) have the same answers for all questions
data_group_comb <- as_tibble(data_comb_date) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, `Question ID`, Question, `Answer values`, Answers, `Parent question`, start_year, end_year) %>%
  # Remove additional white space from branching questions
  mutate(`Branching question` = str_trim(`Branching question`)) %>%
  arrange(`Branching question`) %>%
  group_by(across(c(-`Branching question`))) %>%
  summarise(`Branching question` = paste(unique(`Branching question`), collapse=","), .groups = "keep") %>% 
  ungroup() %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, everything()) %>%
  arrange(start_year) %>%
  arrange(`Entry ID`)

# Fill in missing questions
data_all_questions <- data_group_comb %>%
  complete(nesting(`Entry ID`, `Entry name`,`Branching question`, `Region ID`, `Region name`, start_year, end_year),
           nesting(Question, `Question ID`, `Parent question`),
           fill = list(value=0)) %>%
  filter(!is.na(`Question ID`)) %>%
  # remove duplicate rows
  distinct()

# Extract unanswered child questions 
questions <- v6_questions %>% 
  mutate(`Question ID` = as.factor(`Question ID`))
data_unans <- data_all_questions %>%
  left_join(questions) %>%
  filter(is.na(`Answer values`) & `Parent Question` != "") %>%
  select(-quest, -quest_desc, -`Data Type`, -`Parent Question`) %>% 
  distinct()

# Extract Place questions that should not be filled in
place_remove <- data %>% 
  filter(Poll == "Religious Place (v1)" | Poll == "Religious Place (v1.1)" | Poll ==  "Religious Place (v1.1)") %>% 
  select(Question, `Question ID`, `Parent question`) %>% 
  distinct() %>%
  left_join(related_questions) %>% 
  rename(Q_ID = `Question ID`, `Question ID` = `Related question ID`) %>% 
  left_join(v6_questions) %>% 
  filter(!is.na(quest)) %>%
  mutate(`Parent Question` = ifelse(`Parent Question` == "", NA, `Parent Question`)) %>%
  filter(!is.na(`Parent Question`) & `Parent question` == "")

# Add poll to each entry and select only religious group questions
data_poll <- as_tibble(data) %>%
  select(Poll, `Entry ID`) %>%
  distinct()
data_unans <- data_unans %>%
  left_join(data_poll)

# Split unanswered questions by poll type and remove place and text questions that should not be included
data_unans_group <- data_unans %>%
  filter(Poll == "Religious Group (v5)" | Poll == "Religious Group (v6)")
data_unans_place <- data_unans %>%
  filter(Poll == "Religious Place (v1)" | Poll == "Religious Place (v1.1)" | Poll == "Religious Place (v1.2)") %>%
  filter(!`Question ID` %in% place_remove$`Question ID`)

# Recombine
data_unans <- bind_rows(data_unans_group, data_unans_place) %>%
  select(-Poll)

# Extract parent questions with No answers
parent_question <- data_unans %>% 
  select(`Parent question`) %>% 
  distinct() %>% 
  filter(`Parent question` != "") %>%
  rename(quest = `Parent question`) %>%
  left_join(v6_questions)

# Extract first order parent questions
first_order_quest <- parent_question %>%
  filter(`Parent Question` == "")
second_order_quest <- parent_question %>%
  filter(`Parent Question` %in% first_order_quest$quest)
third_order_quest <- parent_question %>%
  filter(`Parent Question` %in% second_order_quest$quest)

expect_equal(nrow(parent_question), (nrow(first_order_quest) + nrow(second_order_quest) + nrow(third_order_quest)))

# If parent question has a no answers replace the answer of the corresponding child question
data_first_order <- data_group_comb %>%
  filter(Question %in% first_order_quest$quest) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, Question, `Answer values`, Answers) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`, panswers = Answers) %>%
  filter(panswervalue == 0) %>%
  right_join(data_unans) %>% 
  mutate(`Answer values` = ifelse(is.na(`Answer values`), panswervalue, `Answer values`)) %>%
  mutate(Answers = ifelse(is.na(Answers), panswers, Answers)) %>%
  filter(!is.na(panswers)) %>%
  select(-panswervalue, -panswers) %>%
  distinct() 
data_second_order <- data_group_comb %>%
  filter(Question %in% second_order_quest$quest) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, Question, `Answer values`, Answers) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`, panswers = Answers) 
filled_second_order <- data_first_order %>%
  filter(Question %in% second_order_quest$quest) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, Question, `Answer values`, Answers) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`, panswers = Answers)
data_second_order <- bind_rows(data_second_order, filled_second_order) %>%
  distinct() %>%
  filter(panswervalue == 0) %>%
  right_join(data_unans) %>% 
  mutate(`Answer values` = ifelse(is.na(`Answer values`), panswervalue, `Answer values`)) %>%
  mutate(Answers = ifelse(is.na(Answers), panswers, Answers)) %>%
  filter(!is.na(panswers)) %>%
  select(-panswervalue, -panswers) %>%
  distinct() 
data_third_order <- data_group_comb %>%
  filter(Question %in% third_order_quest$quest) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, Question, `Answer values`, Answers) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`, panswers = Answers) 
filled_third_order <- data_second_order %>%
  filter(Question %in% third_order_quest$quest) %>%
  select(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, Question, `Answer values`, Answers) %>%
  rename(`Parent question` = Question, panswervalue = `Answer values`, panswers = Answers) 
data_third_order <- bind_rows(data_third_order, filled_third_order) %>%
  distinct() %>%
  filter(panswervalue == 0) %>%
  right_join(data_unans) %>% 
  mutate(`Answer values` = ifelse(is.na(`Answer values`), panswervalue, `Answer values`)) %>%
  mutate(Answers = ifelse(is.na(Answers), panswers, Answers)) %>%
  filter(!is.na(panswers)) %>%
  select(-panswervalue, -panswers) %>%
  distinct() 

# Join filled question/answers
data_filled <- bind_rows(data_first_order, data_second_order, data_third_order) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, `Question ID`, Question, `Answer values`, Answers, `Parent question`)

# Rejoin with answered data
data_all <- bind_rows(data_group_comb, data_filled)
  
# Find questions with multiple disparate answers in the same entry & time period
data_multi_ans <- data_all %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year, `Question ID`, `Answer values`) %>%
  distinct() %>%
  group_by(`Entry ID`, `Branching question`, `Region ID`, start_year, end_year, `Question ID`) %>%
  tally() %>%
  filter(n > 1) %>%
  left_join(data_all)

# Recombine answers of Yes and No
data_multi_ans_cor <- data_multi_ans %>%
  ungroup() %>%
  mutate(`Answer values` = gsub(",", "", `Answer values`)) %>%
  filter(`Answer values` != "-1") %>%
  # Yes and No treated as Yes or No 
  mutate(`Answer values` = ifelse(Answers == "Yes" & lead(Answers) == "No" | Answers == "No" & lead(Answers) == "Yes" | Answers == "Yes" & lag(Answers) == "No" | Answers == "No" & lag(Answers) == "Yes" | Answers == "Yes; No", "2", `Answer values`)) %>%
  mutate(Answers = ifelse(Answers == "Yes" & lead(Answers) == "No" | Answers == "No" & lead(Answers) == "Yes" | Answers == "Yes" & lag(Answers) == "No" | Answers == "No" & lag(Answers) == "Yes", "Yes; No", Answers)) %>%
  mutate(Answers = ifelse(Answers == "Yes; Field doesn't know", "Yes", Answers)) %>%
  # Remove questions with please choose 1 answers
  # 5174 The society to which the religious group belongs is best characterized as (please choose one):
  # 4698 Nature of religious group [please select one]:
  filter(`Question ID` != 5174 & `Question ID` != 4698) %>%
  distinct() %>%
  select(-n) %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, everything())
  
# Extract questions with single answers
data_single_ans <- anti_join(data_all, data_multi_ans)

# Recombine
data_all_ans <- bind_rows(data_single_ans, data_multi_ans_cor)

expect_equal((nrow(data_single_ans) + nrow(data_multi_ans)), nrow(data_all))

# Remove multiple answers
data_no_multi <- data_all_ans %>%
  filter(Answers != "Yes; No")

# Select only questions of interest
data_qoi <- data_no_multi %>%
  filter(`Question ID` %in% analysis_questions$`Question ID`)

# Transpose question and answer data
data_t <- data_qoi %>%
  ungroup() %>%
  select(-Answers, -Question, -`Parent question`) %>%
  distinct() %>%
  pivot_wider(names_from = `Question ID`, values_from = `Answer values`)

# Extract regions used for analysis
regions <- drh_regions(data_qoi) 

# Join with Entry data
region_id <- data_qoi %>%
  select(`Region ID`, `Region name`) %>%
  distinct()
regions_all <- data_qoi %>%
  select(`Entry ID`, `Entry name`, `Branching question`, `Region ID`, `Region name`, start_year, end_year) %>%
  distinct() %>%
  left_join(region_id) %>%
  rename(Name = `Region name`) %>%
  left_join(regions)

# Create output directory
make.dir("./output")

# Save wide, long  and region data
write_csv(data_t, "./output/data_wide.csv")
write_csv(data_all_ans, "./output/data_long.csv")
saveRDS(regions_all, file = "./output/drh_regions.rds")
