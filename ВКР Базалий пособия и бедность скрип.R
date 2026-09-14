rm(list = ls())  # очистить все объекты
gc()             # освободить память

library(haven)
library(dplyr)
library(ggplot2)
library(flextable)

# =============================================================================
# 1. ЗАГРУЗКА ДАННЫХ
# =============================================================================

data_raw <- read_dta("/Users/darabazalij/Downloads/Диплом/HH+IND.dta")
living_wage_raw <- read_dta("/Users/darabazalij/Downloads/Диплом/2025_reg.dta")

# =============================================================================
# 2. ФИЛЬТРАЦИЯ: 2014–2024, домохозяйства с детьми
# =============================================================================

data <- data_raw %>%
  filter(year >= 2014, year <= 2024) %>%
  filter(num_kidz0_16 >= 1)

living_wage <- living_wage_raw %>%
  filter(year >= 2014, year <= 2024)

rm(data_raw)
gc()

# =============================================================================
# 3. СОЗДАНИЕ ПЕРЕМЕННЫХ
# =============================================================================

data_clean <- data %>%
  mutate(
    # --- Зависимые переменные ---
    poor_abs        = as.integer(a_regpoor_n == 1),          # абсолютная бедность
    poor_cons     = a_expendpoor_n,                         # потребительская 
    r_poor_eu     = r_poor_eu,                              # относительная 60% 
    r_poor_oecd   = r_poor_oecd,                             # относительная 50%
    
    # --- Основная объясняющая переменная ---
    # child_benefit уже дефлирован к 2020 и скорректирован на регион
    
    child_benefit_k = child_benefit / 1000,
    
    # --- Охват пособиями (для описательной статистики) ---
    benefit_d_fixed = as.integer(child_benefit > 0),
    
    # --- Структура домохозяйства ---
    large_fam       = as.integer(num_kidz0_16 >= 3),         # многодетная семья
    child_u1        = as.integer(num_kidz0_1 > 0),           # ребёнок до 1 года
    child_share     = num_kidz0_16 / nfm,                    # доля детей
    pension_share   = num_pens / nfm,                        # доля пенсионеров
    
    
    # --- Глава домохозяйства ---
    female_head     = as.integer(fh == 1 | rfh == 1),
    pension_head    = as.integer(rmh == 1 | rfh == 1),
    
    # --- Место проживания ---
    settlement_type = case_when(
      status == 1 ~ 0L,           # областной центр (база)
      status == 2 ~ 1L,           # город
      status %in% c(3, 4) ~ 2L   # ПГТ + село
    ),
    settlement_city  = as.integer(status == 2),        # город (база = областной центр)
    settlement_rural = as.integer(status %in% c(3, 4)), # ПГТ + село (база = областной центр)
    rural           = as.integer(status == 4),
    moscow_spb      = as.integer(region %in% c(138, 141)),
    
    # --- Занятость ---
    employed_prop = employedsum / nfm,
    
    # --- Наличие родителей в д/х ---
    has_mother = as.integer(mother_yes == 1 | mother_l_yes == 1),
    has_father = as.integer(father_yes == 1 | father_l_yes == 1),
    single_parent = as.integer(has_father == 0 | has_mother == 0),
    
    # --- Алименты ---
    alimony         = as.integer(!is.na(child_aliments) & child_aliments > 0),
    alimony_x_single = alimony * single_parent,
    alimony_k = ifelse(is.na(child_aliments), 0, child_aliments / 1000),
    
    # --- Здоровье родителей (бинарные: 0=хорошее, 1=среднее/плохое) ---
    # Кодировка: 1=очень плохое, 2=плохое, 3=среднее, 4=хорошее, 5=очень хорошее
    m_sah1          = case_when(
      m_sah %in% c(4, 5) ~ 0L,
      m_sah %in% c(1, 2, 3) ~ 1L,
      TRUE ~ NA_integer_
    ),
    f_sah1          = case_when(
      f_sah %in% c(4, 5) ~ 0L,
      f_sah %in% c(1, 2, 3) ~ 1L,
      TRUE ~ NA_integer_
    ),
    
    # --- Переменные родителей × дамми наличия ---
    m_highedu_x   = ifelse(has_mother == 1, m_highedu,   0),
    m_employed_x  = ifelse(has_mother == 1, m_employed,  0),
    m_age_x       = ifelse(has_mother == 1, m_age,       0),  
    m_sah1_x      = ifelse(has_mother == 1, m_sah1,      0),
    m_alcoabuse_x = ifelse(has_mother == 1, m_alcoabuse, 0),
    
    f_highedu_x   = ifelse(has_father == 1, f_highedu,   0),
    f_employed_x  = ifelse(has_father == 1, f_employed,  0),
    f_age_x       = ifelse(has_father == 1, f_age,       0),  
    f_sah1_x      = ifelse(has_father == 1, f_sah1,      0),
    f_alcoabuse_x = ifelse(has_father == 1, f_alcoabuse, 0),
    
    # --- Факторные переменные для моделей ---
    year_f          = factor(year),
    id_h_f          = factor(id_h)
  ) %>%
  filter(
    !is.na(poor_abs),
    !is.na(child_benefit_k),
    !is.na(num_kidz0_16),
    !is.na(nfm),
    !is.na(female_head),
    !is.na(pension_head),
    !is.na(employed_prop),
    !is.na(settlement_type),
    !is.na(ownhome),
    !is.na(large_fam),
    !is.na(child_u1),
    !is.na(child_share),
    !is.na(pension_share),
    !is.na(moscow_spb),
    !(has_mother == 1 & is.na(m_highedu)),
    !(has_mother == 1 & is.na(m_employed)),
    !(has_father == 1 & is.na(f_highedu)),
    !(has_father == 1 & is.na(f_employed)),
  )

cat("Финальная выборка:", nrow(data_clean), "наблюдений,",
    n_distinct(data_clean$id_h), "домохозяйств\n")

# rm(data)
# gc()



# =============================================================================
# 4. ОПИСАТЕЛЬНАЯ СТАТИСТИКА
# =============================================================================

library(dplyr)
library(ggplot2)

# Введение

# Многодетные (large_fam == 1)
mean(data_clean$poor_abs[data_clean$large_fam == 1], na.rm=TRUE) * 100

# Неполные семьи (single_parent == 1)
mean(data_clean$poor_abs[data_clean$single_parent == 1], na.rm=TRUE) * 100

# Сельские (rural == 1)
mean(data_clean$poor_abs[data_clean$rural == 1], na.rm=TRUE) * 100

# Доля неполных среди бедных
mean(data_clean$single_parent[data_clean$poor_abs == 1], na.rm=TRUE) * 100

# Доля сельских среди бедных
mean(data_clean$rural[data_clean$poor_abs == 1], na.rm=TRUE) * 100

# --- Переменная охвата (correct) + типы д/х ---
data_clean <- data_clean %>%
  mutate(
    received_clean = case_when(
      child_benefit_eligible == 1 & child_benefit_recived == 1 ~ 1,
      child_benefit_eligible == 1 & child_benefit_recived == 0 ~ 0,
      child_benefit_eligible == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    hh_type4 = case_when(
      single_parent == 0 & large_fam == 0 ~ "Полная, 1–2 ребёнка",
      single_parent == 0 & large_fam == 1 ~ "Полная, 3+ детей",
      single_parent == 1 & large_fam == 0 ~ "Неполная, 1–2 ребёнка",
      single_parent == 1 & large_fam == 1 ~ "Неполная, 3+ детей"
    )
  )
# =============================================================================
# ТАБЛИЦЫ
# =============================================================================

# ТАБЛИЦА 1: Сводка по всем переменным


all_vars <- c(
  # Зависимые
  "poor_abs", "poor_cons", "r_poor_eu", "r_poor_oecd",
  # Основная
  "child_benefit", "received_clean",
  # Д/х
  "nfm", "num_kidz0_16", "large_fam", "single_parent",
  "female_head", "pension_head", "child_u1",
  "child_share", "pension_share", "employed_prop",
  "settlement_city", "settlement_rural", "moscow_spb",
  "ownhome", "rural",
  # Родители
  "has_mother", "has_father",
  "m_highedu_x", "m_employed_x", "m_age",
  "f_highedu_x", "f_employed_x", "f_age",
  # Доп контроли
  "m_sah1_x", "f_sah1_x",
  "m_alcoabuse_x", "f_alcoabuse_x",
  "alimony"
)

table_all <- data.frame(
  mean = sapply(all_vars, function(v) round(mean(data_clean[[v]], na.rm = TRUE), 3)),
  sd   = sapply(all_vars, function(v) round(sd(data_clean[[v]], na.rm = TRUE), 3)),
  n    = sapply(all_vars, function(v) sum(!is.na(data_clean[[v]])))
)

print(table_all)

# ТАБЛИЦА 2: Динамика по годам


table_year <- data_clean %>%
  group_by(year) %>%
  summarise(
    poor_abs_pct   = round(mean(poor_abs,        na.rm = TRUE) * 100, 1),
    poor_cons_pct  = round(mean(poor_cons,        na.rm = TRUE) * 100, 1),
    poor_rel60_pct = round(mean(r_poor_eu,        na.rm = TRUE) * 100, 1),
    poor_rel50_pct = round(mean(r_poor_oecd,      na.rm = TRUE) * 100, 1),
    benefit_all    = round(mean(child_benefit, na.rm=TRUE)/1000, 2),
    benefit_recv   = round(mean(child_benefit[child_benefit > 0], na.rm=TRUE)/1000, 2),
    coverage_pct   = round(mean(received_clean,   na.rm = TRUE) * 100, 1),
    n              = n()
  )

print(table_year, width = Inf)



# ТАБЛИЦА 3: Бедные vs небедные (с тестами)


# Непрерывные переменные — t-тест
cont_vars <- c("child_benefit", "nfm", "num_kidz0_16",
               "child_share", "pension_share", "employed_prop",
               "m_age", "f_age")


# Бинарные переменные — хи-квадрат 

bin_vars <- c("large_fam", "single_parent", "female_head","pension_head",
              "child_u1", "ownhome", "rural", "moscow_spb",
              "settlement_city", "settlement_rural",
              "has_mother", "has_father",
              "m_highedu_x", "f_highedu_x",
              "m_employed_x", "f_employed_x",
              "m_sah1_x", "f_sah1_x",
              "m_alcoabuse_x", "f_alcoabuse_x",
              "alimony", "received_clean")

# t-тест для непрерывных
ttest_results <- lapply(cont_vars, function(v) {
  poor1 <- data_clean[[v]][data_clean$poor_abs == 1]
  poor0 <- data_clean[[v]][data_clean$poor_abs == 0]
  tt <- t.test(poor1, poor0)
  data.frame(
    variable = v,
    mean_poor    = round(mean(poor1, na.rm = TRUE), 3),
    mean_nonpoor = round(mean(poor0, na.rm = TRUE), 3),
    p_value      = round(tt$p.value, 4),
    test         = "t-тест"
  )
}) %>% bind_rows()

# хи-квадрат для бинарных
chi_results <- lapply(bin_vars, function(v) {
  tbl <- table(data_clean$poor_abs, data_clean[[v]])
  if (nrow(tbl) < 2 | ncol(tbl) < 2) return(NULL)
  ch <- chisq.test(tbl)
  data.frame(
    variable = v,
    mean_poor    = round(mean(data_clean[[v]][data_clean$poor_abs == 1], na.rm = TRUE), 3),
    mean_nonpoor = round(mean(data_clean[[v]][data_clean$poor_abs == 0], na.rm = TRUE), 3),
    p_value      = round(ch$p.value, 4),
    test         = "χ²"
  )
}) %>% bind_rows()

table_poor <- bind_rows(ttest_results, chi_results) %>%
  mutate(significant = ifelse(p_value < 0.01, "***",
                              ifelse(p_value < 0.05, "**",
                                     ifelse(p_value < 0.10, "*", ""))))

print(table_poor)


# ТАБЛИЦА 4: По 4 типам домохозяйств

table_type <- data_clean %>%
  group_by(hh_type4) %>%
  summarise(
    poor_abs_pct   = round(mean(poor_abs,      na.rm = TRUE) * 100, 1),
    poor_cons_pct  = round(mean(poor_cons,      na.rm = TRUE) * 100, 1),
    poor_rel60_pct = round(mean(r_poor_eu,      na.rm = TRUE) * 100, 1),
    poor_rel50_pct = round(mean(r_poor_oecd,    na.rm = TRUE) * 100, 1),
    benefit_mean   = round(mean(child_benefit,  na.rm = TRUE), 0),
    coverage_pct   = round(mean(received_clean, na.rm = TRUE) * 100, 1),
    employed_mean  = round(mean(employed_prop,  na.rm = TRUE) * 100, 1),
    hh_size_mean   = round(mean(nfm,            na.rm = TRUE), 2),
    n              = n()
  )

print(table_type, width = Inf)

# ANOVA по типам домохозяйств
aov_poor <- aov(poor_abs ~ hh_type4, data = data_clean)
summary(aov_poor)
TukeyHSD(aov_poor)

aov_benefit <- aov(child_benefit ~ hh_type4, data = data_clean)
summary(aov_benefit)
TukeyHSD(aov_benefit)


# ТАБЛИЦА 5: По институциональным периодам

table_period <- data_clean %>%
  mutate(period = case_when(
    year <= 2017                ~ "2014–2017",
    year >= 2018 & year <= 2022 ~ "2018–2022",
    year >= 2023                ~ "2023–2024"
  )) %>%
  group_by(period) %>%
  summarise(
    poor_abs_pct   = round(mean(poor_abs,      na.rm = TRUE) * 100, 1),
    poor_cons_pct  = round(mean(poor_cons,      na.rm = TRUE) * 100, 1),
    poor_rel60_pct = round(mean(r_poor_eu,      na.rm = TRUE) * 100, 1),
    poor_rel50_pct = round(mean(r_poor_oecd,    na.rm = TRUE) * 100, 1),
    benefit_mean   = round(mean(child_benefit,  na.rm = TRUE), 0),
    coverage_pct   = round(mean(received_clean, na.rm = TRUE) * 100, 1),
    employed_mean  = round(mean(employed_prop,  na.rm = TRUE) * 100, 1),
    n              = n()
  )

print(table_period,width = Inf)

data_clean %>%
  mutate(subperiod = case_when(
    year <= 2017 ~ "2014–2017",
    year >= 2018 & year <= 2019 ~ "2018–2019",
    year >= 2020 & year <= 2022 ~ "2020–2022",
    year >= 2023 ~ "2023–2024"
  )) %>%
  group_by(subperiod) %>%
  summarise(
    poor_abs_pct  = round(mean(poor_abs, na.rm=TRUE)*100, 1),
    poor_cons_pct = round(mean(poor_cons, na.rm=TRUE)*100, 1),
    benefit_mean  = round(mean(child_benefit, na.rm=TRUE)/1000, 2),
    coverage_pct  = round(mean(received_clean, na.rm=TRUE)*100, 1),
    employed_mean = round(mean(employed_prop, na.rm=TRUE)*100, 1),
    n = n()
  )

# =============================================================================
# ГРАФИКИ
# =============================================================================

# --- График 1: Динамика размера пособий по годам ---
plot_data <- data_clean %>%
  group_by(year) %>%
  summarise(
    all_hh    = mean(child_benefit, na.rm = TRUE),
    receivers = mean(child_benefit[child_benefit > 0], na.rm = TRUE)
  )

reforms <- data.frame(
  x = c(2016, 2018, 2020, 2023),
  label = c(
    "Регионализация (доход < 1 ПМ)",
    "Федер. выплаты до 1.5 лет (доход < 1.5 ПМ)",
    "Расширение: до 3 лет (доход < 2 ПМ)",
    "Единое пособие до 17 лет (доход < 1 ПМ)"
  )
)
ggplot() +
  geom_vline(data = reforms, aes(xintercept = x, colour = label),
             linetype = "dashed", linewidth = 0.9) +
  geom_line(data = plot_data, aes(x = year, y = all_hh, colour = "Все д/х с детьми"),
            linewidth = 1.2) +
  geom_point(data = plot_data, aes(x = year, y = all_hh, colour = "Все д/х с детьми"),
             size = 2.5) +
  geom_line(data = plot_data, aes(x = year, y = receivers, colour = "Только получатели"),
            linewidth = 1.2) +
  geom_point(data = plot_data, aes(x = year, y = receivers, colour = "Только получатели"),
             size = 2.5) +
  # Невидимые точки только для легенды
  geom_point(data = reforms, aes(x = x, y = -Inf, colour = label),
             size = 3, shape = 16, alpha = 0) +
  scale_colour_manual(
    values = c(
      "Все д/х с детьми"                            = "#1F3864",
      "Только получатели"                           = "#C0392B",
      "Регионализация (доход < 1 ПМ)"               = "#A569BD",
      "Федер. выплаты до 1.5 лет (доход < 1.5 ПМ)" = "#27AE60",
      "Расширение: до 3 лет (доход < 2 ПМ)"        = "#1ABFC8",
      "Единое пособие до 17 лет (доход < 1 ПМ)"    = "#F39C12"
    ),
    breaks = c(
      "Все д/х с детьми",
      "Только получатели",
      "Регионализация (доход < 1 ПМ)",
      "Федер. выплаты до 1.5 лет (доход < 1.5 ПМ)",
      "Расширение: до 3 лет (доход < 2 ПМ)",
      "Единое пособие до 17 лет (доход < 1 ПМ)"
    )
  ) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
  labs(title = "Средний размер детских пособий, 2014–2024",
       x = NULL, y = "Руб./мес. (цены 2020 г.)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(hjust = 0.5),
    legend.position      = "bottom",
    legend.justification = "left",
    legend.box           = "vertical",
    legend.text          = element_text(size = 8),
    legend.key.width     = unit(1, "cm")
  ) +
  
  guides(colour = guide_legend(ncol = 1, override.aes = list(
    linetype = c("solid", "solid", "blank", "blank", "blank", "blank"),
    shape    = c(16, 16, 16, 16, 16, 16),
    size     = c(2.5, 2.5, 3, 3, 3, 3),
    alpha    = c(1, 1, 1, 1, 1, 1)
  )))
  
ggsave("plot1_benefit.png", 
       width = 20, height = 16, units = "cm", dpi = 300,
       bg = "white")

# --- График 2: Динамика бедности по всем мерам ---
data_clean %>%
  group_by(year) %>%
  summarise(
    `Абсолютная`        = mean(poor_abs,    na.rm=TRUE)*100,
    `Потребительская`   = mean(poor_cons,   na.rm=TRUE)*100,
    `Относительная 60%` = mean(r_poor_eu,   na.rm=TRUE)*100,
    `Относительная 50%` = mean(r_poor_oecd, na.rm=TRUE)*100
  ) %>%
  tidyr::pivot_longer(-year, names_to = "Тип бедности", values_to = "pct") %>%
  mutate(`Тип бедности` = factor(`Тип бедности`, 
                                 levels = c("Абсолютная", "Потребительская", "Относительная 60%", "Относительная 50%"))) %>%
  ggplot(aes(x = year, y = pct, colour = `Тип бедности`)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(2016, 2018, 2020, 2023),
             linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c(
    "Абсолютная"        = "#1F3864",
    "Потребительская"   = "#C0392B",
    "Относительная 60%" = "#27AE60",
    "Относительная 50%" = "#F39C12"
  )) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(title = "Уровень бедности домохозяйств с детьми, 2014–2024",
       x = NULL, y = "%", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(hjust = 0.5, margin = margin(b = 15)),
    legend.position      = "bottom",
    legend.justification = "left",
    legend.text          = element_text(size = 9)
  )
ggsave("plot2_poverty.png", 
       width = 22, height = 14, units = "cm", dpi = 300,
       bg = "white")

# --- График 3: корреляция абс бедность VS размер пособий---

install.packages("ggrepel")
library(ggrepel)

data_clean %>%
  group_by(year) %>%
  summarise(
    poor_abs_pct = mean(poor_abs, na.rm = TRUE) * 100,
    benefit_mean = mean(child_benefit[child_benefit > 0], na.rm = TRUE)
  ) %>%
  ggplot(aes(x = benefit_mean, y = poor_abs_pct)) +
  geom_smooth(method = "lm", se = TRUE, colour = "#C0392B",
              linetype = "dashed", alpha = 0.15) +
  geom_point(size = 2, colour = "#1F3864") +
  ggrepel::geom_text_repel(aes(label = year), size = 2.5, colour = "grey30",
                           nudge_y = 0.3, min.segment.length = 0) +
  scale_x_continuous(labels = scales::comma_format(big.mark = " "))+
  labs(title = "Размер пособий и уровень абсолютной бедности, 2014–2024",
       x = "Средний размер пособий среди получателей, руб.",
       y = "Доля абсолютно бедных д/х, %") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot3_scatter.png", 
       width = 18, height = 16, units = "cm", dpi = 300,
       bg = "white")

# --- График 4: столбчатая диаграмма по типам домохозяйств ---

# table_type %>%
#   ggplot(aes(x = reorder(hh_type4, poor_abs_pct), y = poor_abs_pct, fill = hh_type4)) +
#   geom_col(width = 0.6) +
#   geom_text(aes(label = paste0(poor_abs_pct, "%")), 
#             hjust = -0.2, size = 4) +
#   coord_flip() +
#   scale_fill_manual(values = c(
#     "Полная, 1–2 ребёнка"   = "#1F3864",
#     "Полная, 3+ детей"      = "#2E86C1",
#     "Неполная, 1–2 ребёнка" = "#C0392B",
#     "Неполная, 3+ детей"    = "#E74C3C"
#   )) +
#   scale_y_continuous(limits = c(0, 55)) +
#   labs(title = "Уровень абсолютной бедности по типам домохозяйств, 2014–2024",
#        x = NULL, y = "%") +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title      = element_text(hjust = 0.5),
#     legend.position = "none"
#   )

table_type %>%
  mutate(
    family_type = ifelse(grepl("Неполная", hh_type4), "Неполная семья", "Полная семья"),
    children    = ifelse(grepl("3\\+", hh_type4), "3+ детей", "1–2 ребёнка")
  ) %>%
  ggplot(aes(x = family_type, y = poor_abs_pct, fill = children)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(poor_abs_pct, "%")),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(
    "1–2 ребёнка" = "#1F3864",
    "3+ детей"    = "#C0392B"
  )) +
  scale_y_continuous(limits = c(0, 55)) +
  labs(title = "Уровень абсолютной бедности по типам домохозяйств\n(в среднем за 2014–2024, %)",
       x = NULL, y = "%", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("plot4_types_hh.png", 
       width = 20, height = 15, units = "cm", dpi = 300,
       bg = "white")

# --- График 5: Круговая диаграмма бедность × получение пособий ---
pie_data <- data_clean %>%
  mutate(
    group = case_when(
      poor_abs == 1 & received_clean == 1 ~ "Бедные, получают пособия",
      poor_abs == 1 & received_clean == 0 ~ "Бедные, не получают пособия",
      poor_abs == 0 & received_clean == 1 ~ "Небедные, получают пособия",
      poor_abs == 0 & received_clean == 0 ~ "Небедные, не получают пособия"
    )
  ) %>%
  filter(!is.na(group)) %>%
  count(group) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(pie_data, aes(x = "", y = pct, fill = group)) +
  geom_col(width = 1, colour = "white") +
  coord_polar("y") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Бедные, получают пособия"      = "#E8A0A0",
    "Бедные, не получают пособия"   = "#C0392B",
    "Небедные, получают пособия"    = "#87CEEB",
    "Небедные, не получают пособия" = "#2E86C1"
  ))+
  labs(title = "Абсолютная бедность и получение пособий\n(в среднем за 2014–2024, %)",
       fill = NULL) +
  theme_void(base_size = 10) +
  theme(
    plot.title      = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.text     = element_text(size = 9)
  )

ggsave("plot5_no_yes_poor.png", 
       width = 18, height = 12, units = "cm", dpi = 300,
       bg = "white")


#--- График 6: Охват по пособиям ---

data_clean %>%
  mutate(settlement = ifelse(settlement_rural == 1, 
                             "Сельская местность", 
                             "Городская местность")) %>%
  group_by(year, settlement) %>%
  summarise(coverage = mean(received_clean, na.rm = TRUE) * 100, .groups = "drop") %>%
  bind_rows(
    data_clean %>%
      group_by(year) %>%
      summarise(coverage = mean(received_clean, na.rm = TRUE) * 100, .groups = "drop") %>%
      mutate(settlement = "Все д/х с детьми")
  ) %>%
  ggplot(aes(x = year, y = coverage, colour = settlement)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = c(2016, 2018,2020, 2023),
             linetype = "dashed", colour = "grey70") +
  scale_colour_manual(values = c(
    "Все д/х с детьми"   = "black",
    "Городская местность" = "#2E86C1",
    "Сельская местность"  = "#C0392B"
  )) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2)) +
  labs(title = "Охват пособиями по типу населённого пункта, 2014–2024",
       x = NULL, y = "%", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(hjust = 0.5),
    legend.position      = "bottom",
    legend.justification = "left"
  )
ggsave("plot6_settlement.png", 
       width = 20, height = 12, units = "cm", dpi = 300,
       bg = "white")



# # =============================================================================
# # 5. ПРОСТАЯ МОДЕЛЬ (КРОСС-СЕКЦИЯ 2014 и 2024)
# # =============================================================================
# 
# # Кросс-секция 2014
# data_2014 <- data_clean %>% filter(year == 2014)
# probit_2014 <- glm(
#   poor_abs ~ child_benefit_k + large_fam + single_parent + child_u1 +
#     nfm + num_pens + pension_head +
#     settlement_type + ownhome + moscow_spb + employed_pct,
#   data   = data_2014,
#   family = binomial(link = "probit")
# )
# summary(probit_2014)
# 
# 
# # Кросс-секция 2024
# data_2024 <- data_clean %>% filter(year == 2024)
# probit_2024 <- glm(
#   poor_abs ~ hild_benefit_k + large_fam + single_parent + child_u1 +
#     nfm + num_pens + pension_head +
#     settlement_type + ownhome + moscow_spb + employed_pct,
#   data   = data_2024,
#   family = binomial(link = "probit")
# )
# summary(probit_2024)

# =============================================================================
# КРОСС-СЕКЦИОННЫЕ МОДЕЛИ 
# =============================================================================

#парные по годам

years <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

results <- lapply(years, function(y) {
  m <- glm(poor_abs ~ child_benefit_k,
           data = data_clean %>% filter(year == y),
           family = binomial(link = "probit"))
  c(year = y,
    coef = coef(m)["child_benefit_k"],
    pval = summary(m)$coefficients["child_benefit_k", 4])
})

do.call(rbind, results)

#2014 и 2024

# Модель 1: только основная объясняющая переменная
data_2014 <- data_clean %>% filter(year == 2014)
m1_2014 <- glm(poor_abs ~ child_benefit_k,
               data = data_2014, family = binomial(link = "probit"))
summary(m1_2014)

# Модель 2: + структура домохозяйства
m2_2014 <- glm(poor_abs ~ child_benefit_k +
                 # структура д/х
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share,
               data = data_2014, family = binomial(link = "probit"))
summary(m2_2014)

# Модель 3: + место проживания, жильё, занятость, глава д/х
m3_2014 <- glm(poor_abs ~ child_benefit_k +
                 # структура д/х
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                 # место проживания и жильё
                 settlement_city + settlement_rural + ownhome + moscow_spb +
                 # занятость и глава д/х
                 employed_pct + pension_head,
               data = data_2014, family = binomial(link = "probit"))
summary(m3_2014)

# Модель 4: + характеристики родителей
m4_2014 <- glm(poor_abs ~ child_benefit_k +
                 # структура д/х
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                 # место проживания и жильё
                 settlement_city + settlement_rural + ownhome + moscow_spb +
                 # занятость и глава д/х
                 employed_pct + pension_head +
                 # наличие родителей
                 m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
                 # характеристики отца × наличие отца
                 f_highedu_x +  f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x,
               data = data_2014, family = binomial(link = "probit"))
summary(m4_2014)

# # Модель 5: + алименты
# m5_2014 <- glm(poor_abs ~ child_benefit_k +
#                  # структура д/х
#                  large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
#                  # место проживания и жильё
#                  settlement_city + settlement_rural + ownhome + moscow_spb +
#                  # занятость и глава д/х
#                  employed_pct + pension_head +
#                  # характеристики матери × наличие матери
#                  m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
#                  # характеристики отца × наличие отца
#                  f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x +
#                  # алименты × неполная семья
#                  alimony_x_single,
#                data = data_2014, family = binomial(link = "probit"))
# summary(m5_2014)
# 
# m5b_2014 <- glm(poor_abs ~ child_benefit_k +
#                   large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
#                   settlement_city + settlement_rural + ownhome + moscow_spb +
#                   employed_pct + pension_head +
#                   m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
#                   f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x +
#                   alimony,
#                 data = data_2014, family = binomial(link = "probit"))
# summary(m5b_2014)

m5c_2014 <- glm(poor_abs ~ child_benefit_k +
                  large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                  settlement_city + settlement_rural + ownhome + moscow_spb +
                  employed_pct + pension_head +
                  m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
                  f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x +
                  + alimony_k,
                data = data_2014, family = binomial(link = "probit"))
summary(m5c_2014)

# --- То же самое для 2024 ---
data_2024 <- data_clean %>% filter(year == 2024)
m1_2024 <- glm(poor_abs ~ child_benefit_k,
               data = data_2024, family = binomial(link = "probit"))
summary(m1_2024)

m2_2024 <- glm(poor_abs ~ child_benefit_k +
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share,
               data = data_2024, family = binomial(link = "probit"))
summary(m2_2024)

m3_2024 <- glm(poor_abs ~ child_benefit_k +
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                 settlement_city + settlement_rural + ownhome + moscow_spb +
                 employed_pct + pension_head,
               data = data_2024, family = binomial(link = "probit"))
summary(m3_2024)

m4_2024 <- glm(poor_abs ~ child_benefit_k +
                 large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                 settlement_city + settlement_rural + ownhome + moscow_spb +
                 employed_pct + pension_head +
                 m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
                 f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x,
               data = data_2024, family = binomial(link = "probit"))
summary(m4_2024)

# m5_2024 <- glm(poor_abs ~ child_benefit_k +
#                  large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
#                  settlement_city + settlement_rural + ownhome + moscow_spb +
#                  employed_pct + pension_head +
#                  m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
#                  f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x +
#                  alimony_x_single,
#                data = data_2024, family = binomial(link = "probit"))
# summary(m5_2024)

m5c_2024 <- glm(poor_abs ~ child_benefit_k +
                  large_fam + single_parent + child_u1 + log(nfm) + child_share + pension_share +
                  settlement_city + settlement_rural + ownhome + moscow_spb +
                  employed_pct + pension_head +
                  m_highedu_x + m_employed_x + m_age_x + m_sah1_x + m_alcoabuse_x +
                  f_highedu_x + f_employed_x + f_age_x + f_sah1_x + f_alcoabuse_x +
                  + alimony_k,
                data = data_2024, family = binomial(link = "probit"))
summary(m5c_2024)

library(pscl)
library(lmtest)

models_2014 <- list(m1_2014, m2_2014, m3_2014, m4_2014, m5c_2014)
models_2024 <- list(m1_2024, m2_2024, m3_2024, m4_2024, m5c_2024)

r2_2014 <- sapply(models_2014, function(m) round(pR2(m)["McFadden"], 3))
r2_2024 <- sapply(models_2024, function(m) round(pR2(m)["McFadden"], 3))

cat("McFadden R2 2014:", r2_2014, "\n")
cat("McFadden R2 2024:", r2_2024, "\n")


# LR-тест только между моделями одинакового размера
lr_chi_2014 <- c(NA, 
                 round(lrtest(m1_2014, m2_2014)$Chisq[2], 2),
                 round(lrtest(m2_2014, m3_2014)$Chisq[2], 2),
                 NA,  # нельзя сравнивать м3 и м4 - разный размер
                 round(lrtest(m4_2014,m5c_2014)$Chisq[2], 2))

lr_p_2014 <- c(NA,
               round(lrtest(m1_2014, m2_2014)$`Pr(>Chisq)`[2], 4),
               round(lrtest(m2_2014, m3_2014)$`Pr(>Chisq)`[2], 4),
               NA,
               round(lrtest(m4_2014, m5c_2014)$`Pr(>Chisq)`[2], 4))

lr_chi_2024 <- c(NA,
                 round(lrtest(m1_2024, m2_2024)$Chisq[2], 2),
                 round(lrtest(m2_2024, m3_2024)$Chisq[2], 2),
                 NA,
                 round(lrtest(m4_2024, m5c_2024)$Chisq[2], 2))

lr_p_2024 <- c(NA,
               round(lrtest(m1_2024, m2_2024)$`Pr(>Chisq)`[2], 4),
               round(lrtest(m2_2024, m3_2024)$`Pr(>Chisq)`[2], 4),
               NA,
               round(lrtest(m4_2024, m5c_2024)$`Pr(>Chisq)`[2], 4))

cat("LR χ² 2014:", lr_chi_2014, "\n")
cat("LR p   2014:", lr_p_2014, "\n")
cat("LR χ² 2024:", lr_chi_2024, "\n")
cat("LR p   2024:", lr_p_2024, "\n")

library(modelsummary)
library(pandoc)



# --- Сводная таблица ---
modelsummary(
  list(
    "2014 (1)" = m1_2014, "2014 (2)" = m2_2014,
    "2014 (3)" = m3_2014, "2014 (4)" = m4_2014, "2014 (5)" = m5c_2014,
    "2024 (1)" = m1_2024, "2024 (2)" = m2_2024,
    "2024 (3)" = m3_2024, "2024 (4)" = m4_2024, "2024 (5)" = m5c_2024
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c("nobs", "aic", "bic", "r2.mcfadden"),
  output = "result_crosssection.docx"
)


# =============================================================================
# 6. ПАНЕЛЬНЫЕ МОДЕЛИ
# =============================================================================

library(lme4)
library(survival)

library(GLMMadaptive)

data_clean <- data_clean %>%
  mutate(ln_nfm = log(nfm))

# Модель 0 — только child_benefit_k + год
re_m0 <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial(link = "probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(re_m0)

# Модель 1 — + характеристики д/х
re_m1 <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + factor(year) +
    female_head + large_fam + child_u1 +
    ln_nfm + child_share + pension_share +
    settlement_city + settlement_rural + moscow_spb +
    employed_prop + pension_head,
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial(link = "probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(re_m1)

#ОСНОВНАЯ МОДЕЛЬ 2 + характеристики родителей

# --- RE probit-marginal

re_probit <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + female_head + large_fam + child_u1 +
    ln_nfm + child_share + pension_share +
    settlement_city + settlement_rural + moscow_spb +
    employed_prop + pension_head +
    has_mother + m_highedu_x + m_age_x +
    has_father + f_highedu_x + f_age_x +
    factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial(link = "probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(re_probit)

#Модель 3 + алименты

re_m3 <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + factor(year) +
    female_head + large_fam + child_u1 +
    ln_nfm + child_share + pension_share +
    settlement_city + settlement_rural + moscow_spb +
    employed_prop + pension_head +
    has_mother + m_highedu_x + m_employed_x + m_age_x +
    has_father + f_highedu_x + f_employed_x + f_age_x +
    alimony,
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial(link = "probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(re_m3)

# Модель 4 + др характеристики д/х

re_m4 <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + factor(year) +
    female_head + large_fam + child_u1 +
    ln_nfm + child_share + pension_share +
    settlement_city + settlement_rural + moscow_spb +
    employed_prop + pension_head +
    has_mother + m_highedu_x + m_employed_x + m_age_x +
    has_father + f_highedu_x + f_employed_x + f_age_x +
    alimony + m_sah1_x + f_sah1_x + m_alcoabuse_x + f_alcoabuse_x,
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial(link = "probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(re_m4)

#AME

pred0 <- fitted(re_m1)
ame_m0 <- fixef(re_m1) * mean(dnorm(qnorm(pred0)))

pred1 <- fitted(re_m2)
ame_m1 <- fixef(re_m2) * mean(dnorm(qnorm(pred1)))

pred2 <- fitted(re_probit)
ame_m2 <- fixef(re_probit) * mean(dnorm(qnorm(pred2)))

pred3 <- fitted(re_m3)
ame_m3 <- fixef(re_m3) * mean(dnorm(qnorm(pred3)))

pred4 <- fitted(re_m4)
ame_m4 <- fixef(re_m4) * mean(dnorm(qnorm(pred3)))

cat("AME child_benefit_k:\n")
cat("Модель 0:", round(ame_m0["child_benefit_k"], 5), "\n")
cat("Модель 1:", round(ame_m1["child_benefit_k"], 5), "\n")
cat("Модель ОСНОВНАЯ (2):", round(ame_m2["child_benefit_k"], 5), "\n")
cat("Модель 3:", round(ame_m3["child_benefit_k"], 5), "\n")
cat("Модель 4:", round(ame_m3["child_benefit_k"], 5), "\n")

# Полные AME для всех моделей
print(round(ame_m0, 5))
print(round(ame_m1, 5))
print(round(ame_m2, 5))
print(round(ame_m3, 5))
print(round(ame_m4, 5))

# SE для AME через дельта-метод

sf0 <- mean(dnorm(qnorm(fitted(re_m1))))
sf1 <- mean(dnorm(qnorm(fitted(re_m2))))
sf2 <- mean(dnorm(qnorm(fitted(re_probit))))
sf3 <- mean(dnorm(qnorm(fitted(re_m3))))
sf4 <- mean(dnorm(qnorm(fitted(re_m4))))

se_ame_m0 <- sqrt(diag(vcov(re_m1))) * sf0
se_ame_m1 <- sqrt(diag(vcov(re_m2))) * sf1
se_ame_m2 <- sqrt(diag(vcov(re_probit))) * sf2
se_ame_m3 <- sqrt(diag(vcov(re_m3))) * sf3
se_ame_m4 <- sqrt(diag(vcov(re_m3))) * sf4

print(round(se_ame_m0, 5))
print(round(se_ame_m1, 5))
print(round(se_ame_m2, 5))
print(round(se_ame_m3, 5))
print(round(se_ame_m4, 5))

# Проверяем child_benefit_k
cat("SE AME child_benefit_k:\n")
cat("Модель 0:", round(se_ame_m0["child_benefit_k"], 5), "\n")
cat("Модель 1:", round(se_ame_m1["child_benefit_k"], 5), "\n")
cat("Модель 2:", round(se_ame_m2["child_benefit_k"], 5), "\n")
cat("Модель 3:", round(se_ame_m3["child_benefit_k"], 5), "\n")



library(bife)

fe_probit <- bife(
  poor_abs ~ child_benefit_k + female_head + large_fam + child_u1 +
    ln_nfm + child_share + pension_share +
    employed_prop + pension_head +
    has_mother + m_highedu_x + m_employed_x + m_age_x +
    has_father + f_highedu_x + f_employed_x + f_age_x +
    factor(year) | id_h,
  data = data_clean
)

summary(fe_probit)



# AME для FE probit
apes_fe <- get_APEs(fe_probit)
summary(apes_fe)

#ТЕСТЫ RE VS FE

# Хаусман для пробита

# Ковариационные матрицы
b_re <- fixef(re_probit)
V_re <- vcov(re_probit)[common_vars, common_vars]
V_fe <- vcov(fe_probit)
rownames(V_fe) <- names(b_fe)
colnames(V_fe) <- names(b_fe)

V_fe_c <- V_fe[common_vars, common_vars]
V_re_c <- V_re[common_vars, common_vars]

# Статистика Хаусмана
diff <- b_fe_c - b_re_c
V_diff <- V_fe_c - V_re_c
H <- as.numeric(t(diff) %*% solve(V_diff) %*% diff)
df <- length(common_vars)
p_val <- pchisq(H, df, lower.tail = FALSE)

cat("Hausman test statistic:", round(H, 3), "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", round(p_val, 4), "\n")

library(plm)

# Создаём панельный датафрейм
pdata <- pdata.frame(data_clean, index = c("id_h", "year"))

# Линейная модель для теста (plm требует lm)
plm_model <- plm(poor_abs ~ child_benefit_k + female_head + large_fam + child_u1 +
                   ln_nfm + child_share + pension_share +
                   settlement_city + settlement_rural + moscow_spb +
                   employed_prop + pension_head +
                   has_mother + m_highedu_x + m_employed_x + m_age_x +
                   has_father + f_highedu_x + f_employed_x + f_age_x,
                 data = pdata,
                 model = "pooling")

# Тест Вулдриджа на серийную корреляцию
pbgtest(plm_model)

#===============================================
#7. ПРОВЕРКА УСТЙЧИВОСТИ
#===============================================

# Потребительская бедность
m_cons <- mixed_model(
  fixed = poor_cons ~ child_benefit_k + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_cons)

# Относительная 60%
m_eu <- mixed_model(
  fixed = r_poor_eu ~ child_benefit_k + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_eu)

# Относительная 50%
m_oecd <- mixed_model(
  fixed = r_poor_oecd ~ child_benefit_k + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
  
)

summary(m_oecd )

# Все AME для трёх моделей
ame_all_cons <- m_cons$coefficients * sf_cons
ame_all_eu   <- m_eu$coefficients   * sf_eu
ame_all_oecd <- m_oecd$coefficients * sf_oecd

se_all_cons <- sqrt(diag(vcov(m_cons))) * sf_cons
se_all_eu   <- sqrt(diag(vcov(m_eu)))   * sf_eu
se_all_oecd <- sqrt(diag(vcov(m_oecd))) * sf_oecd

# Сводная таблица по основным переменным 
vars_main <- c("child_benefit_k", "female_head", "large_fam", "child_u1",
               "ln_nfm", "child_share", "pension_share", "settlement_city",
               "settlement_rural", "moscow_spb", "employed_prop", "pension_head",
               "has_mother", "m_highedu_x", "m_employed_x", "m_age_x",
               "has_father", "f_highedu_x", "f_employed_x", "f_age_x")

result <- data.frame(
  var     = vars_main,
  ame_cons = round(ame_all_cons[vars_main], 5),
  se_cons  = round(se_all_cons[vars_main], 5),
  ame_eu   = round(ame_all_eu[vars_main], 5),
  se_eu    = round(se_all_eu[vars_main], 5),
  ame_oecd = round(ame_all_oecd[vars_main], 5),
  se_oecd  = round(se_all_oecd[vars_main], 5)
)

print(result, row.names = FALSE)

#===============================================
#8. ОЦЕНКА С ИНСТРУМЕНТАЛЬНОЙ ПЕРЕМЕННОЙ
#===============================================

data_clean <- data_clean %>%
  left_join(living_wage %>% select(region, year, rpm_child, rpm_all), 
            by = c("region", "year"))

# Проверяем
head(data_clean[, c("region", "year", "rpm_child", "rpm_all")])
sum(is.na(data_clean$rpm_child))

# С rpm_child
first_stage_child <- lm(child_benefit_k ~ rpm_child + female_head + large_fam + 
                          child_u1 + ln_nfm + child_share + pension_share + 
                          settlement_city + settlement_rural + moscow_spb + 
                          employed_prop + pension_head + has_mother + m_highedu_x + 
                          m_employed_x + m_age_x + has_father + f_highedu_x + 
                          f_employed_x + f_age_x + factor(year),
                        data = data_clean)

# С rpm_all
first_stage_all <- lm(child_benefit_k ~ rpm_all + female_head + large_fam + 
                        child_u1 + ln_nfm + child_share + pension_share + 
                        settlement_city + settlement_rural + moscow_spb + 
                        employed_prop + pension_head + has_mother + m_highedu_x + 
                        m_employed_x + m_age_x + has_father + f_highedu_x + 
                        f_employed_x + f_age_x + factor(year),
                      data = data_clean)

# Сравниваем F-статистику инструмента
library(lmtest)
library(sandwich)
cat("rpm_child:\n")
coeftest(first_stage_child, vcov = vcovHC(first_stage_child, type = "HC1"))["rpm_child",]
cat("\nrpm_all:\n")
coeftest(first_stage_all, vcov = vcovHC(first_stage_all, type = "HC1"))["rpm_all",]

library(car)
# F-статистика для rpm_all
linearHypothesis(first_stage_all, "rpm_all = 0", 
                 vcov = vcovHC(first_stage_all, type = "HC1"))

linearHypothesis(first_stage_child, "rpm_child = 0", 
                 vcov = vcovHC(first_stage_child, type = "HC1"))

# Оба инструмента вместе
first_stage_both <- lm(child_benefit_k ~ rpm_child + rpm_all + female_head + large_fam + 
                         child_u1 + ln_nfm + child_share + pension_share + 
                         settlement_city + settlement_rural + moscow_spb + 
                         employed_prop + pension_head + has_mother + m_highedu_x + 
                         m_employed_x + m_age_x + has_father + f_highedu_x + 
                         f_employed_x + f_age_x + factor(year),
                       data = data_clean)

linearHypothesis(first_stage_both, c("rpm_child = 0", "rpm_all = 0"),
                 vcov = vcovHC(first_stage_both, type = "HC1"))
library(AER)

# IV для r_poor_eu (60%)
iv_eu <- ivreg(r_poor_eu ~ child_benefit_k + female_head + large_fam + 
                 child_u1 + ln_nfm + child_share + pension_share + 
                 settlement_city + settlement_rural + moscow_spb + 
                 employed_prop + pension_head + has_mother + m_highedu_x + 
                 m_employed_x + m_age_x + has_father + f_highedu_x + 
                 f_employed_x + f_age_x + factor(year) |
                 rpm_all + female_head + large_fam + 
                 child_u1 + ln_nfm + child_share + pension_share + 
                 settlement_city + settlement_rural + moscow_spb + 
                 employed_prop + pension_head + has_mother + m_highedu_x + 
                 m_employed_x + m_age_x + has_father + f_highedu_x + 
                 f_employed_x + f_age_x + factor(year),
               data = data_clean)

# IV для r_poor_oecd (50%)
iv_oecd <- ivreg(r_poor_oecd ~ child_benefit_k + female_head + large_fam + 
                   child_u1 + ln_nfm + child_share + pension_share + 
                   settlement_city + settlement_rural + moscow_spb + 
                   employed_prop + pension_head + has_mother + m_highedu_x + 
                   m_employed_x + m_age_x + has_father + f_highedu_x + 
                   f_employed_x + f_age_x + factor(year) |
                   rpm_all + female_head + large_fam + 
                   child_u1 + ln_nfm + child_share + pension_share + 
                   settlement_city + settlement_rural + moscow_spb + 
                   employed_prop + pension_head + has_mother + m_highedu_x + 
                   m_employed_x + m_age_x + has_father + f_highedu_x + 
                   f_employed_x + f_age_x + factor(year),
                 data = data_clean)

summary(iv_eu, diagnostics = TRUE)
summary(iv_oecd, diagnostics = TRUE)

# Первая стадия 
first_stage_k <- lm(child_benefit_k ~ rpm_all + female_head + large_fam + 
                      child_u1 + ln_nfm + child_share + pension_share + 
                      settlement_city + settlement_rural + moscow_spb + 
                      employed_prop + pension_head + has_mother + m_highedu_x + 
                      m_employed_x + m_age_x + has_father + f_highedu_x + 
                      f_employed_x + f_age_x + factor(year),
                    data = data_clean)

# Предсказанные значения
data_clean$child_benefit_hat_k <- fitted(first_stage_k)

# RE probit второй стадии
m_cf_eu <- mixed_model(
  fixed = r_poor_eu ~ child_benefit_hat_k + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_cf_eu)

m_cf_oecd <- mixed_model(
  fixed = r_poor_oecd ~ child_benefit_hat_k + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_cf_oecd)

# Первая стадия с обоими инструментами
first_stage_both_k <- lm(child_benefit_k ~ rpm_child + rpm_all + female_head + large_fam + 
                           child_u1 + ln_nfm + child_share + pension_share + 
                           settlement_city + settlement_rural + moscow_spb + 
                           employed_prop + pension_head + has_mother + m_highedu_x + 
                           m_employed_x + m_age_x + has_father + f_highedu_x + 
                           f_employed_x + f_age_x + factor(year),
                         data = data_clean)

data_clean$child_benefit_hat_both <- fitted(first_stage_both_k)

m_cf_eu_both <- mixed_model(
  fixed = r_poor_eu ~ child_benefit_hat_both + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_cf_eu_both)

# AME для IV моделей (control function approach)
# Для rpm_all
sf_cf_eu <- mean(dnorm(qnorm(fitted(m_cf_eu))))
sf_cf_oecd <- mean(dnorm(qnorm(fitted(m_cf_oecd))))
sf_cf_both <- mean(dnorm(qnorm(fitted(m_cf_eu_both))))

ame_cf_eu <- m_cf_eu$coefficients["child_benefit_hat_k"] * sf_cf_eu
se_cf_eu <- sqrt(vcov(m_cf_eu)["child_benefit_hat_k","child_benefit_hat_k"]) * sf_cf_eu

ame_cf_oecd <- m_cf_oecd$coefficients["child_benefit_hat_k"] * sf_cf_oecd
se_cf_oecd <- sqrt(vcov(m_cf_oecd)["child_benefit_hat_k","child_benefit_hat_k"]) * sf_cf_oecd

ame_cf_both <- m_cf_eu_both$coefficients["child_benefit_hat_both"] * sf_cf_both
se_cf_both <- sqrt(vcov(m_cf_eu_both)["child_benefit_hat_both","child_benefit_hat_both"]) * sf_cf_both

cat("IV (rpm_all), EU 60%:", round(ame_cf_eu, 5), "(", round(se_cf_eu, 5), ")\n")
cat("IV (rpm_all), OECD 50%:", round(ame_cf_oecd, 5), "(", round(se_cf_oecd, 5), ")\n")
cat("IV (оба), EU 60%:", round(ame_cf_both, 5), "(", round(se_cf_both, 5), ")\n")

m_cf_oecd_both <- mixed_model(
  fixed = r_poor_oecd ~ child_benefit_hat_both + female_head + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

sf_cf_oecd_both <- mean(dnorm(qnorm(fitted(m_cf_oecd_both))))
ame_cf_oecd_both <- m_cf_oecd_both$coefficients["child_benefit_hat_both"] * sf_cf_oecd_both
se_cf_oecd_both <- sqrt(vcov(m_cf_oecd_both)["child_benefit_hat_both","child_benefit_hat_both"]) * sf_cf_oecd_both

cat("IV (оба), OECD 50%:", round(ame_cf_oecd_both, 5), "(", round(se_cf_oecd_both, 5), ")\n")

#===================================
#9.ПРОВЕРКА ГИПОТЕЗ ПРО РЕФОРМЫ
#===================================

# Создаём переменную периода
data_clean <- data_clean %>%
  mutate(period = case_when(
    year <= 2017 ~ 0,  # базовый
    year <= 2022 ~ 1,  # ФЗ-418
    TRUE ~ 2           # единое пособие
  ))

data_clean <- data_clean %>%
  mutate(
    period1 = as.integer(year >= 2018 & year <= 2022),  # ФЗ-418
    period2 = as.integer(year >= 2023)                   # единое пособие
  )

#RE периоды - дамми

m_period <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + period1 + period2 +
    female_head + large_fam + child_u1 + ln_nfm + child_share + 
    pension_share + settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x,
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_period) 

sf_period <- mean(dnorm(qnorm(fitted(m_period))))

ame_period <- m_period$coefficients * sf_period
se_period  <- sqrt(diag(vcov(m_period))) * sf_period

# Ключевые переменные
vars_show <- c("child_benefit_k", "period1", "period2")
data.frame(
  var = vars_show,
  AME = round(ame_period[vars_show], 5),
  SE  = round(se_period[vars_show], 5)
)

# с разделением 2020

data_clean <- data_clean %>%
  mutate(
    period1 = as.integer(year >= 2018 & year <= 2019),
    period2 = as.integer(year >= 2020 & year <= 2022),
    period3 = as.integer(year >= 2023)
  )

m_period4 <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + period1 + period2 + period3 +
    female_head + large_fam + child_u1 + ln_nfm + child_share + 
    pension_share + settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + has_father + f_highedu_x + 
    f_employed_x + f_age_x,
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_period4)

sf_period4 <- mean(dnorm(qnorm(fitted(m_period4))))

ame_period4 <- m_period4$coefficients * sf_period4
se_period4  <- sqrt(diag(vcov(m_period4))) * sf_period4

# Ключевые переменные
vars_show <- c("child_benefit_k", "period1", "period2", "period3")
data.frame(
  var = vars_show,
  AME = round(ame_period4[vars_show], 5),
  SE  = round(se_period4[vars_show], 5)
)


#Проверка неполных семей на потр бедности без харак-тик отца

m_single_abs <- mixed_model(
  fixed = poor_abs ~ child_benefit_k + single_parent + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_single_abs)

#на потребительской

m_single <- mixed_model(
  fixed = poor_cons ~ child_benefit_k + single_parent + large_fam + 
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_single)

m_single_large <- mixed_model(
  fixed = poor_cons ~ child_benefit_k + single_parent:large_fam + single_parent + large_fam +
    child_u1 + ln_nfm + child_share + pension_share + 
    settlement_city + settlement_rural + moscow_spb + 
    employed_prop + pension_head + has_mother + m_highedu_x + 
    m_employed_x + m_age_x + factor(year),
  random = ~ 1 | id_h,
  data = data_clean,
  family = binomial("probit"),
  nAGQ = 12,
  control = list(iter_EM = 0)
)

summary(m_single_large)



# AME
ame_all_abs  <- m_single_abs$coefficients   * sf_abs
ame_all_cons <- m_single$coefficients       * sf_cons
ame_all_int  <- m_single_large$coefficients * sf_int

# Стандартные ошибки
se_all_abs  <- sqrt(diag(vcov(m_single_abs)))   * sf_abs
se_all_cons <- sqrt(diag(vcov(m_single)))       * sf_cons
se_all_int  <- sqrt(diag(vcov(m_single_large))) * sf_int

# Переменные для обычных моделей
vars_base <- c(
  "child_benefit_k",
  "single_parent",
  "large_fam",
  "child_u1",
  "ln_nfm",
  "child_share",
  "pension_share",
  "settlement_city",
  "settlement_rural",
  "moscow_spb",
  "employed_prop",
  "pension_head",
  "has_mother",
  "m_highedu_x",
  "m_employed_x",
  "m_age_x"
)

# Переменные для модели с взаимодействием
vars_int <- c(vars_base, "single_parent:large_fam")

# Таблица
result <- data.frame(
  var = vars_int,
  
  ame_abs = c(round(ame_all_abs[vars_base], 5), NA),
  se_abs  = c(round(se_all_abs[vars_base], 5), NA),
  
  ame_cons = c(round(ame_all_cons[vars_base], 5), NA),
  se_cons  = c(round(se_all_cons[vars_base], 5), NA),
  
  ame_int = round(ame_all_int[vars_int], 5),
  se_int  = round(se_all_int[vars_int], 5)
)

rownames(result) <- NULL

print(result)






cat("Абс single_parent:", round(m_single_abs$coefficients["single_parent"] * sf_abs, 5), "\n")
cat("Потреб single_parent:", round(m_single$coefficients["single_parent"] * sf_cons, 5), "\n")
cat("Потреб пересечение:", round(m_single_large$coefficients["single_parent:large_fam"] * sf_int, 5), "\n")