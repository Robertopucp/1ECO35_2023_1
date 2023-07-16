library (magrittr)

data <- haven::read_dta (
    file = "PorterSerraAEJ.dta"
) %>% dplyr::filter (female == 1)

# Parte 1 -----------------------------------------------------------------

data %>% dplyr::select (
    american,instate,freshman,ACumGPA,
    greek,econ_hs,varsity
) %>% base::as.data.frame () %>% stargazer::stargazer (type = "latex")

# Parte 2 -----------------------------------------------------------------

model1_table3 <- lm (
    data = data, formula = took_year ~ treat2016 + yr_2016 + treatment_class
);model1_table3 %>% base::summary ()

model2_table3 <- lm (
    data = data, formula = tookanother ~ treat2016 + yr_2016 + treatment_class
);model2_table3 %>% base::summary ()

stargazer::stargazer (model1_table3,model2_table3,type = "latex",no.space = TRUE)

model1_table4 <- lm (
    data = data,formula = numeconclass ~ treat2016 + yr_2016 + treatment_class
);model1_table4 %>% base::summary ()
model2_table4 <- lm (
    data = data,formula = econmajor ~ treat2016 + yr_2016 + treatment_class
);model2_table4 %>% base::summary ()

stargazer::stargazer (model1_table4,model2_table4,type = "latex",no.space = TRUE)

model1_table5 <- lm (
    data = data,formula = Major_STEM ~ treat2016 + yr_2016 + treatment_class
);model1_table5 %>% base::summary ()
model2_table5 <- lm (
    data = data,formula = Major_Business ~ treat2016 + yr_2016 + treatment_class
);model2_table5 %>% base::summary ()
model3_table5 <- lm (
    data = data,formula = Major_Finance ~ treat2016 + yr_2016 + treatment_class
);model3_table5 %>% base::summary ()
model4_table5 <- lm (
    data = data,formula = Major_Marketing ~ treat2016 + yr_2016 + treatment_class
);model4_table5 %>% base::summary ()

stargazer::stargazer (
    model1_table5,model2_table5,model3_table5,model4_table5,type = "latex",
    no.space = TRUE,column.sep.width = "-10pt"
)

model1_table6 <- lm (
    data = data,formula = Major_SocSc ~ treat2016 + yr_2016 + treatment_class
);model1_table6 %>% base::summary ()
model2_table6 <- lm (
    data = data,formula = Major_Arts ~ treat2016 + yr_2016 + treatment_class
);model2_table6 %>% base::summary ()
model3_table6 <- lm (
    data = data,formula = Major_Comm ~ treat2016 + yr_2016 + treatment_class
);model3_table6 %>% base::summary ()
model4_table6 <- lm (
    data = data,formula = Major_Hum ~ treat2016 + yr_2016 + treatment_class
);model4_table6 %>% base::summary ()

stargazer::stargazer (
    model1_table6,model2_table6,model3_table6,model4_table6,type = "latex",
    no.space = TRUE,column.sep.width = "-10pt"
)

# Parte 3 -----------------------------------------------------------------

plot1 <- coefplot::coefplot (model = model1_table4)
plot2 <- coefplot::coefplot (model = model2_table4)











