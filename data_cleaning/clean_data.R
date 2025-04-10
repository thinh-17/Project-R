library(dplyr)
library(tidyr)
library(stringr)

data <- read.csv("../data_set/Life Expectancy Data.csv")

# Thay khoảng trắng giữa tên quốc gia thành "_"
data <- data %>%
  mutate(Country = str_replace_all(Country, " ", "_"))

# Các cột chứa dòng có dữ liệu NA của mỗi quốc gia sẽ chuyển thành giá trị trung vị của cột đó ứng với từng quốc gia
data_grouped <- data %>%
  group_by(Country) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(.x, na.rm = TRUE), .x))) %>%
  ungroup()

# Gộp các cột liên quan thành chỉ số chung
data_grouped <- data_grouped %>%
  rowwise() %>%
  mutate(
    vaccination_rate = median(c_across(all_of(c("Hepatitis.B", "Polio", "Diphtheria"))), na.rm = TRUE),
    thinness = mean(c_across(all_of(c("thinness..1.19.years", "thinness.5.9.years"))), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(
    -c(Hepatitis.B, Polio, Diphtheria,
       thinness..1.19.years, thinness.5.9.years)
  )

# Xóa các dòng có dữ liệu NA còn lại
data_grouped <- data_grouped %>%
  drop_na()

write.csv(data_grouped, "../data_set/data_cleaned.csv", row.names = FALSE)
print("Đã xuất file clean")