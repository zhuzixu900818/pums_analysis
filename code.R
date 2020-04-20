library(dplyr)
library(googlesheets4)

occ_risk_sheet <- "https://docs.google.com/spreadsheets/d/18dmgZC_sQZOc9AxETwqs7Wc9P6QmCG62y40wlFxXMMk"
occ_risk_xwalk <- occ_risk_sheet %>% 
  read_sheet(col_types = "ld__c", col_names = c("risk_group", "occ", "ind"), skip = 1) %>%
  select(risk_group,occ,ind='...3')

hh <- read.csv("psam_h08.csv")
pp <- read.csv("psam_p08.csv")

pp_clean <- pp %>%
  left_join(occ_risk_xwalk, by = c('OCCP'='occ')) %>%
  mutate(
    risk_group = if_else(is.na(WAGP) | WAGP==0, NA, risk_group),
    risk_group_lab = if_else(risk_group, "More vulnerable", "Less vulnerable"),
    risk_wages = if_else(risk_group, ADJINC/1000000*WAGP, NA_real_),
    
    raceth = case_when(
      HISP != 1 ~ "Hispanic or Latino",
      RAC1P == 1 ~ "White",
      RAC1P == 2 ~ "Black",
      RAC1P %in% c(3,4,5) ~ "Native",
      RAC1P == 6 ~ "Asian",
      RAC1P == 7 ~ "Islander",
      RAC1P %in% c(8,9) ~ "Others"
    ),
    
    inc_grp = cut(
      risk_wages,
      c(-Inf, 7.5e3, 15e3, 25e3, 38e3, 56e3, 75e3, 120e3, Inf),
      c("< $7.5K","$7.5K ~ $15K","$15K ~ $25K","$25K ~ $38K","$38K ~ $56K","$56K ~ $75K","$75K ~ $120K",">= $120K")
    )
  )

hh_clean <- hh %>%
  filter(TYPE == 1) %>%
  mutate(
    hh_inc = if_else(HINCP<0,0,ADJINC/1000000*HINCP),
    hh_inc_group = cut(
      hh_inc,
      c(-Inf, 15e3, 30e3, 50e3, 75e3, 112.5e3, 150e3, Inf),
      c("< $15k", "$15k - 30k", "$30k - $50k", "$50k - $75k", "$75k - $112.5k", "$112.5k - $150k", ">= $150k"),
      include.lowest = TRUE,
      ordered_result = TRUE
    ),
    
    is_renter = (TEN==3),
    gross_rent = if_else(is_renter, ADJHSG/1000000*GRNTP, NA_real_),
    gross_rent_group = cut(
      gross_rent,
      c(-Inf, 600, 1000, 1400, 1800, 2200, Inf),
      c("< $600", "$600 - 1,000", "$1,000 - $1,400", "$1,400 - $1,800", "$1,800 - $2,200", ">= $2,200"),
      include.lowest = TRUE, 
      ordered_result = TRUE
    ),
    is_rent_burdened = (GRNTP > (HINCP/12*0.3)),
    is_rent_burdened_sev = (GRNTP > (HINCP/12*0.5)),
    is_rent_burdened_mod = (is_rent_burdened) & (!is_rent_burdened_sev)
  )

hp <- hh_clean %>%
  inner_join(pp_clean, by = "SERIALNO") %>%
  group_by(SERIALNO) %>%
  mutate(
    hh_any_risk = case_when(
      all(is.na(risk_group)) ~ NA, # no wage earners
      any(risk_group, na.rm = TRUE) ~ TRUE, # any wage earners are at risk
      all(!risk_group, na.rm = TRUE) ~ FALSE # all wage earners are at NOT at risk
    ),
    
    hh_all_risk = case_when(
      all(is.na(risk_group)) ~ NA, # no wage earners
      all(risk_group, na.rm = TRUE) ~ TRUE, # all wage earners are at risk
      any(!risk_group, na.rm = TRUE) ~ FALSE # not all wage earners are at risk
    ),
    
    hh_risk_wages = sum(risk_wages, na.rm = TRUE),
    hh_risk_wages_pct = sum(risk_wages, na.rm = TRUE) / na_if(hh_inc, 0)
  ) %>%
  ungroup()


library(srvyr)
library(ggplot2)
library(plotly)
library(ggthemes)

# Households with at least one member employed in a more vulnerable occupation by household income
p_hh <- hp %>%
  filter(
    SPORDER == 1,
    !is.na(hh_any_risk)
  ) %>%
  as_survey_design(weights = WGTP) %>%
  group_by(hh_inc_group, .drop = FALSE) %>%
  summarise(
    risk_pct = survey_mean(hh_any_risk, vartype = 'ci', level = 0.90),
    hh_eff = survey_total(hh_any_risk, vartype = 'ci', level = 0.9)
  )

plot1 <- ggplot(p_hh, aes(x=hh_inc_group)) +
  geom_col(aes(y = risk_pct), fill = "blue") +
  geom_point(aes(y = hh_eff*0.6/150000), color = "red", size = 5) +
  labs(
    title = "Households in Colorado with at least one member employed \n in a vulnerable occupation due to pandemic by household income",
    x = "Households Income Group",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release"
  ) +
  scale_y_continuous(
    breaks = seq(0,0.6,by=0.1), labels = scales::percent_format(accuracy = 1), limits = c(0,0.6), name = "Share of Households",
    sec.axis = sec_axis(~ . *150000/0.6, name = 'Number of Households', labels = scales::unit_format(unit = 'K', scale = 1e-3), breaks = seq(0,150000,by=25000))
  ) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file = "Households in Colorado with at least one member employed in a vulnerable occupation due to pandemic by household income.jpg", plot = plot1)

#Renter households with at least one member employed in a more vulnerable occupation by household income
p_rt <- hp %>%
  filter(
    SPORDER == 1,
    hh_any_risk
  ) %>%
  as_survey_design(weights = WGTP) %>%
  group_by(hh_inc_group, .drop = FALSE) %>%
  summarise(
    renter_pct = survey_mean(is_renter, vartype = "ci", level = 0.90),
    renter = survey_total(is_renter, vartype = "ci", level = 0.90)
  )

plot2 <- ggplot(p_rt, aes(x=hh_inc_group)) +
  geom_col(aes(y = renter_pct), fill = "blue") +
  geom_point(aes(y = renter*0.8/72000), color = "red", size = 5) +
  labs(
    title = "Renter households in Colorado with at least one member employed \n in a vulnerable occupation due to pandemic by household income",
    x = "Households Income Group",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release"
  ) +
  scale_y_continuous(
    breaks = seq(0,0.8,by=0.1), labels = scales::percent_format(accuracy = 1), limits = c(0,0.8), name = "Share of Households",
    sec.axis = sec_axis(~ . *72000/0.8, name = 'Number of Households', labels = scales::unit_format(unit = 'K', scale = 1e-3, accuracy = 1), breaks = seq(0,72000,by=9000))
  ) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file = "Renter households in Colorado with at least one member employed in a vulnerable occupation due to pandemic by household income.jpg",plot = plot2)

#wage earners employed in more vulnerable occupations by race/ethnicity
p_re <- pp_clean %>%
  filter(
    !is.na(risk_group)
  ) %>%
  as_survey_design(weights = PWGTP) %>%
  group_by(raceth, .drop = FALSE) %>%
  summarise(
    pop_pct = survey_mean(risk_group, vartype = "ci", level = 0.90),
    pop = survey_total(risk_group, vartype = "ci", level = 0.90)
  )

plot3 <- ggplot(p_re, aes(x=raceth)) +
  geom_col(aes(y = pop_pct), fill = "blue") +
  geom_point(aes(y = pop*0.5/600000), color = "red", size = 5) +
  labs(
    title = "Wage earners in Colorado employed in vulnerable occupations due to pandemic by race/ethnicity",
    x = "Race Ethnicity Group",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release"
  ) +
  scale_y_continuous(
    breaks = seq(0,0.5,by=0.1), labels = scales::percent_format(accuracy = 1), limits = c(0,0.5), name = "Share of Workers",
    sec.axis = sec_axis(~ . *600000/0.5, name = 'Number of Workers', labels = scales::unit_format(unit = 'K', scale = 1e-3, accuracy = 1), breaks = seq(0,600000,by=120000))
  ) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file = "wage earners in Colorado employed in vulnerable occupations by race ethnicity.jpg",plot = plot3)

#renter households by level of housing cost burden
p_rtbur <- hh_clean %>%
  filter(
    is_renter
  ) %>%
  mutate(
    burden_lv = if_else(is_rent_burdened_mod, "Moderate:\nrent/income 30%~50%", if_else(is_rent_burdened_sev, "Severe:\nrent/income >50%", "Mild:\nrent/income <30%"))
  ) %>%
  as_survey_design(weights = WGTP) %>%
  group_by(burden_lv, .drop = FALSE) %>%
  summarise(
    bur = survey_total(vartype = "ci", level = 0.90)
  )

plot4 <- ggplot(p_rtbur, aes(x="", y = bur, fill = burden_lv)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y=sum(bur)-cumsum(bur) + 0.5*bur,label = burden_lv), color = "white", size = 6) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Renter households in Colorado with at least one member employed \nin a vulnerable occupation due to pandemic by cost burden.jpg",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file="Renter Pie.jpg", plot=plot4)

# $loss by age group
p_dlage <- pp_clean %>%
  filter(
    risk_group
  ) %>%
  mutate(
    age_group = cut(
      AGEP,
      c(-Inf, 17, 24, 34, 44, 54, 64, Inf),
      c("under18", "age18to24", "age25to34", "age35to44", "age45to54", "age55to64", "over65"),
      include.lowest = TRUE,
      ordered_result = TRUE
    ),
  ) %>%
  as_survey_design(weights = PWGTP) %>%
  group_by(age_group, .drop=FALSE) %>%
  summarise(
    dollar_loss = survey_total(risk_wages, vartype = "ci", level=0.9)
  )

plot5 <- ggplot(p_dlage,aes(x=age_group, y=dollar_loss/12)) +
  geom_col(aes(y=dollar_loss/12), fill = "blue") +
  geom_errorbar(aes(ymin=dollar_loss_low/12,ymax=dollar_loss_upp/12), color = "red") +
  labs(
    title = "Potential Monthly Income Loss by Workers employed in vulnerable occupation due to pandemic in Colorado",
    x = "Age Group",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release\nRed mark represents 90% confidence interval"
  ) +
  scale_y_continuous(
    breaks = seq(0,6e+8,by=1.2e+8), labels = scales::dollar_format(scale = 1e-6, suffix = "M"), limits = c(0,6e+8), name = "Dollars in Millions",
  ) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = "dollars loss.jpg", plot = plot5)

#worker by education attainment
p_edu <- pp_clean %>%
  filter(
    risk_group
  ) %>%
  mutate(
    ed = cut(
      SCHL,
      c(-Inf, 15, 17, 19, 21, Inf),
      c("less than HS", "HS or equivalent", "some college", "Associate or Bachelor", "Advanced"),
      include.lowest = TRUE,
      ordered_result = TRUE
    )
  ) %>%
  as_survey_design(weights = PWGTP) %>%
  group_by(ed, .drop=FALSE) %>%
  summarise(
    workers = survey_total(risk_wages, vartype = "ci", level=0.9)
  )

plot6 <- ggplot(p_edu,aes(x="", y=workers, fill = ed)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y=sum(workers)-cumsum(workers) + 0.5*workers, label = ed), color = "white", size = 6) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "none") +
  labs(
    title = "Renter households in Colorado with at least one member employed \nin a vulnerable occupation due to pandemic by cost burden.jpg",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release"
  ) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file = "dollars loss.jpg", plot = plot5)

#Workers by Industry and Income
p_bb <- pp_clean %>%
  filter(risk_group) %>%
  as_survey_design(weights = PWGTP) %>%
  group_by(inc_grp,ind) %>%
  summarise(
    workers = survey_total(risk_group)
  )

plot6 <- ggplot(p_bb, aes(x=inc_grp, y=workers, color=ind)) +
  geom_point(size = 10, alpha=0.7) +
  labs(
    title = "Workers in Colorado employed in vulnerable occupations due to pandemic",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release",
    x = "Income Group", y = "Number of Workers", color = "Industry"
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = 'K', scale = 1e-3)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.position = 'bottom',
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    axis.title.y=element_text(size=20),
    axis.title.x=element_text(size=20),
    axis.text.y=element_text(size=15),
    axis.text.x=element_text(size=15),
  )
ggsave(file="workers by income.jpg", plot=plot6)

#workers by Industry and Income
p_bb2 <- pp_clean %>%
  filter(risk_group) %>%
  as_survey_design(weights = PWGTP) %>%
  group_by(raceth,ind) %>%
  summarise(
    workers = survey_total(risk_group)
  )

plot7 <- ggplot(p_bb2, aes(x=raceth, y=workers, color=ind)) +
  geom_point(size = 10, alpha=0.7) +
  labs(
    title = "Workers in Colorado employed in vulnerable occupations due to pandemic",
    subtitle = "Author: Shift Research Lab", caption = "Data Source: American Community Survey 5 Year Public Use Microdata Sample, 2018 release",
    x = "Race Ethnicity", y = "Number of Workers", color = "Industry"
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = 'K', scale = 1e-3)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.position = 'bottom',
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    axis.title.y=element_text(size=20),
    axis.title.x=element_text(size=20),
    axis.text.y=element_text(size=15),
    axis.text.x=element_text(size=15),
  )
ggsave(file = "workers by raceth.jpg", plot = plot7)