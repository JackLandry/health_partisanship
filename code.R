

library(rio)
library(tidyverse)
library(binsreg)
library(estimatr)
library(glue)
library(RColorBrewer)
library(ggpubr)
library(extrafont)
library(srvyr)
library(surveytools2)
library(tidylog)


cces_raw <- import("~/Dropbox/CCES/Fresh CCES/2012/CCES12_Common_VV.dta")

#Grabbing relevent variables
cces <- cces_raw %>% mutate(age=2012-birthyr,
                        general_health=genhealth,
                        high_blood_pressure=CC326_5,
                        high_cholestoral=CC326_6,
                        heart_disese=CC326_2,
                        diabetes=CC326_1,
                        asthma=CC326_4,
                        smoker=CC325_1,
                        alcohol=CC325_2,
                        healthy_diet=CC325_4,
                        exercise=CC325_3,
                        weight=V103) %>% 
  select(age:exercise,gender,race,hispanic,faminc,pid7,educ,employ,inputstate,weight) %>% 
  as_tibble()


cces <- cces %>% 
  mutate(across(c(high_blood_pressure:exercise),~ifelse(.x==2,0,.x)),
         across(c(gender:faminc,educ:inputstate), as.factor), 
         pid_lean=ifelse(pid7<=3,"Democrat",
                         ifelse(pid7==4,"Indepdent",
                                ifelse(pid7>4 & pid7<8,"Republican",
                                       ifelse(pid7==8,"Not Sure",NA)))))



cces_2_party <- cces %>% filter(pid_lean %in% c("Republican","Democrat"))


cces_2_party_weights <- cces_2_party %>% 
  as_survey_design(weights = weight)

#Weighted means for health variables
health_party <- cces_2_party_weights %>% select(general_health:exercise,pid_lean) %>% 
  group_by(pid_lean) %>% 
  summarise_all(survey_mean, na.rm=TRUE)

#Could try different conditions same graph
health_party_long <- health_party %>% 
  pivot_longer(cols = general_health:exercise_se)

#Need to get SDs separate and then add confidence intervals

health_party_long_mean <- health_party_long %>% 
  filter(!grepl('_se', name))  

health_party_long_se <- health_party_long %>% 
  filter(grepl('_se', name)) %>% 
  mutate(name=str_remove(name, "_se")) %>% 
  rename(se=value)


health_party_long <- left_join(health_party_long_mean,health_party_long_se)

health_party_long <- health_party_long %>% 
  mutate(name=str_replace_all(name, "_","\n"),
         name=str_to_title(name),
         ci_max=value+(se*1.96),
         ci_min=value-(se*1.96)) %>% 
  filter(!name %in% c("Exercise","Alcohol", "General\nHealth", "Healthy\nDiet"))


#Health party graph

health_party_graph <- ggplot(data=health_party_long, aes(x=name, y=value, ymin=ci_min, ymax=ci_max, fill=pid_lean)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5)+
  geom_errorbar(width = .05, position = position_dodge(0.5), color = "gray") +
  labs(title="\nHealth Conditions Among\nRepubicans and Democrats",
       x="", y="Percentage\nwith\nCondition") +
  scale_y_continuous(limits = c(0, .35), breaks = seq(from = 0.0, to = .35, by = .05), 
                     labels = scales::percent_format(accuracy = 5L)) +
  theme_jack() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c('#4d94ff','#cc3300'))  

health_party_graph
# ,
# caption = "Data from Cooperative Campaign Election Study 2012 (Self Reported)\nDemocrats in Blue, Republicans in Red") +

png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/health_party_graph.png", 
    width=6, height=4, units="in", res=1000)
health_party_graph
dev.off()



#Want to do a regression with covariates and then graph gaps

#Covariates
covariate_sets <- list(
  "none" = c(),
  "age" = c("age"),
  "all" = c("age", "race", "gender","faminc","educ","employ","inputstate")
)
#Function for lm_robust later
run_lm_robust <- function(dv_name, treatment_name, covariate_set_name, weight_var_name, dataset_name){
  rhs <- paste(c(treatment_name, covariate_sets[[covariate_set_name]]), collapse = " + ")
  form <- as.formula(glue("{dv_name} ~ {rhs}"))
  lm_robust(form, weights = get(weight_var_name), data = get(dataset_name))
}

#Do all the model variations
model_df <- expand_grid(dv_name = c("high_blood_pressure",
                                    "high_cholestoral","heart_disese","diabetes",
                                    "asthma","smoker"), 
                        treatment_name = c("pid_lean"), 
                        covariate_set_name = c("none", "age", "all"), 
                        weight_var_name = c("weight"), 
                        dataset_name = "cces_2_party")

#Run the models and clean them up
results <- model_df %>% 
  mutate(model = pmap(., run_lm_robust),
         tidy_coefs = map(model, tidy)) %>% 
  unnest(cols = tidy_coefs)  %>% filter(term=="pid_leanRepublican")

#Further cleaning of the names, prepping for graphing
results_clean <- results %>% mutate(dv_name=str_replace_all(dv_name, "_","\n"),
                              dv_name=str_to_title(dv_name),
                              ub=estimate+(1.96*std.error),
                              lb=estimate-(1.96*std.error), #Fucked up by releveling high_cholestoral
                              dv_name=ifelse(dv_name=="Heart\nDisese","Heart\nDisease",dv_name),
                              dv_name=ifelse(dv_name=="High\nCholestoral","High\nCholesterol", dv_name),
                              dv_name=fct_relevel(dv_name, "Asthma","Diabetes","High\nBlood\nPressure",
                                                  "High\nCholesterol", "Heart\nDisease", "Smoker"),
                              covariate_set_name=str_to_title(covariate_set_name),
                              covariate_set_name=as.factor(covariate_set_name),
                              covariate_set_name=fct_relevel(covariate_set_name, 
                                                             "None", "Age", "All"))


health_covariate_adjusted <- ggplot(data=results_clean, aes(x=dv_name, y=estimate, ymin=lb,ymax=ub,fill=covariate_set_name)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5)+
  geom_errorbar(width = .05, position = position_dodge(0.5), color = "gray") +
  labs(title="\nHealth Conditions By Party:\nCovariate Adjusted",
       x="", y="Republicans-\nDemocrats\nWith Condition\n") +
  scale_y_continuous(limits = c(-.1, .05), breaks = seq(from = -.1, to = .05, by = .05),
                     labels = scales::percent_format(accuracy=1)) +
  theme_jack() +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.text.x = element_text(size=10),
        plot.caption = element_text(lineheight = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title="Covariates"))
health_covariate_adjusted

# #,
# caption = "Data from 2012 Cooperative Congresional Election Survey\n
#        Full Set of Covarites include age, gender, race, education, family income, employment status, and state of residence"

png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/health_covariate_adjusted.png", 
    width=7, height=4, units="in", res=1000)
health_covariate_adjusted
dev.off()


#Want to specifically hilight health conditional on age with some nice graphs
age_health <- cces_2_party %>% group_by(age,pid_lean) %>% 
  summarise(across(c(general_health:exercise), ~mean(.x, na.rm=TRUE))) 

#Esample graph, want to do it with all the conditions
ggplot(age_health %>% filter(age<80), aes(x=age, y=high_blood_pressure, color=pid_lean)) +
  geom_point() +
  geom_smooth(method=loess, se=TRUE, fullrange=TRUE) +
  labs(title="",
       x="\nAge", y="Percentage with High Blood Pressure\n") +
  theme_minimal() +
  scale_color_manual(values=c('#4d94ff','#cc3300')) +
  theme(legend.position = "none") 


#First, graph comparing ages
age_party <- cces %>% mutate(republican=ifelse(pid_lean=="Republican",1,0),
                             democrat=ifelse(pid_lean=="Democrat",1,0)) %>% 
  group_by(age) %>% 
  summarise(mean_republican=mean(republican, na.rm=TRUE),
            mean_democrat=mean(democrat, na.rm=TRUE))

age_party_long <- age_party %>% pivot_longer(cols = c(mean_republican:mean_democrat))

age_party_graph <- ggplot(age_party_long %>% filter(age<80), aes(x=age, y=value, group = name, color = name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method=loess, se=TRUE, fullrange=TRUE) + #Color red
  labs(title="\nPartisanship By Age",
       x="\nAge", y="Percentage\nRepublicans/\nDemocrats") +
  theme_jack() +
  scale_y_continuous(limits = c(.2, .6), breaks = seq(from = 0, to = 1, by = .05),
                     labels = scales::percent_format(accuracy=1)) +
  scale_color_manual(values=c('#4d94ff','#cc3300')) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10, 
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size=10),
        legend.position = "none", plot.title = element_text(hjust = 0.5))
age_party_graph


png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/age_party.png", 
    width=7, height=4, units="in", res=1000)
age_party_graph
dev.off()


#Making a function to do a graph by age
graph_func <- function(d_var,d_title,title) {
  d_var <- enquo(d_var)
  ggplot(age_health %>% filter(age<80), aes(x=age, y= !! d_var, color=pid_lean)) +
    geom_point(alpha = 0.5) +
    theme_jack() +
    geom_smooth(method=loess, se=TRUE, fullrange=TRUE) +
    labs(title=title,
         x="", y="") +
    scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
    scale_color_manual(values=c('#4d94ff','#cc3300')) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.title.x = element_text(size=10),
          legend.position = "none", plot.title = element_text(hjust = 0.5)) 
}
#Maybe just a title?
high_blood_pressure <- graph_func(high_blood_pressure,"Percentage\nwith\nHigh\nBlood\nPressure\n","High Blood Pressure") #this does work


asthma <- graph_func(asthma,"","Asthma") #Have to do them individually to get the names nice
diabetes <- graph_func(diabetes,"","Diabetes")
high_blood_pressure  <- graph_func(high_blood_pressure,"","High Blood Pressure")
high_cholestoral <- graph_func(high_cholestoral,"","High Cholesterol")
heart_disese <- graph_func(heart_disese,"","Heart Disease")
smoker <- graph_func(smoker,"","Smoker")
exercise <- graph_func(exercise,"","Exercise")
general_health <- graph_func(general_health,"","General Health")

#Combinings step 1
age_health_figure <- ggarrange(asthma, diabetes,high_blood_pressure,
                    high_cholestoral,heart_disese,smoker,
                    ncol = 3, nrow =2)

#Combinings step 2 with the axis labels
age_health_final_figure <- annotate_figure(age_health_figure,
                top = text_grob("\nHealth Conditions by Party and Age\n", size = 20, family="Cairo"),
                bottom = text_grob("Age\n", family="Cairo"),
                left = text_grob("Percentage\nWith\nCondtion", family="Cairo"))
age_health_final_figure

png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/age_health_final_figure.png", 
    width=7, height=6, units="in", res=1000)
age_health_final_figure
dev.off()



#Want to investigate general health by party, how it responds to actual reported health conditions

lm_robust(general_health ~ pid_lean + high_blood_pressure*pid_lean+high_cholestoral*pid_lean+
            heart_disese*pid_lean+diabetes*pid_lean+asthma*pid_lean+smoker+alcohol*pid_lean+exercise*pid_lean, data=cces_2_party) %>% tidy()


#Undoing reverse coding of general health
cces_2_party <- cces_2_party %>% mutate(general_health=reverse_code(general_health))



lm_robust(general_health ~ high_blood_pressure+high_cholestoral+
            heart_disese+diabetes+asthma+smoker+alcohol+exercise, data=cces_2_party) %>% tidy()

#Also demographic controls
results_reg <- lm_robust(general_health ~ pid_lean + high_blood_pressure*pid_lean+high_cholestoral*pid_lean+
            heart_disese*pid_lean+diabetes*pid_lean+asthma*pid_lean+smoker+alcohol*pid_lean+exercise*pid_lean+
            race+gender+faminc+educ+employ, 
          fixed_effects = age + inputstate, data=cces_2_party) %>% tidy()

republican_diff <- results_reg %>% filter(grepl('pid_lean', term), term!="pid_leanRepublican")  %>% 
  mutate(term=str_remove(term, "pid_leanRepublican:")) %>% rename(r_diff_est=estimate) %>% 
  select(term,r_diff_est)
republican_diff
results_reg

diff_results <- republican_diff %>% left_join(results_reg, by = "term") %>% 
  mutate(r_estimate=estimate-r_diff_est) %>% select(term,r_estimate,estimate) %>% 
  mutate(term=str_replace_all(term, "_","\n"),
         term=str_to_title(term),
         term=ifelse(term=="Alcohol","Drink\nAlcohol", term)) %>% 
  rename(d_estimate=estimate)

diff_results_long <- pivot_longer(diff_results, cols=r_estimate:d_estimate)

# ggplot(data=diff_results_long, aes(x=term, y=value, fill=name)) +
#   geom_bar(stat="identity", position=position_dodge(), width=0.5)+
#   labs(title="",
#        x="", y="Generral\Health\nPenalty\n",
#        caption = "Data from 2012 Cooperative Congresional Election Survey\n
#        Full Set of Covarites include age, gender, race, education, family income, employment status, and state of residence") +
#   theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.text.x = element_text(size=10))

#General health penelty by condition R-Ds
general_health_penalty <- ggplot(data=diff_results_long, aes(x=term, y=value, fill=name)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.5)+
  labs(title="\nGeneral Health Penalty For Different\nHealth Conditions By Party",
       x="", y="General\nHealth\nPenalty\n") +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(from = -1, to = 1, by = .25)) +
  theme_jack() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10), axis.text.x = element_text(size=10),
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")  +
  scale_fill_manual(values=c('#4d94ff','#cc3300'))  

png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/general_health_penalty.png", 
    width=6, height=4, units="in", res=1000)
general_health_penalty
dev.off()

general_health_penalty

#####General Health Graph
health_party_long <- left_join(health_party_long_mean,health_party_long_se)

general_health <- health_party_long %>% 
  mutate(name=str_replace_all(name, "_"," "),
         name=str_to_title(name),
         ci_max=value+(se*1.96),
         ci_min=value-(se*1.96)) %>% 
  filter(name=="General Health")



#Maybe Add a title here rather than the bottom label
#Want to directly label democrat and republican


general_health_mod <- general_health %>% mutate(name=ifelse(pid_lean=="Democrat","Democrat","Republican"))


general_health_graph <- ggplot(data=general_health_mod, aes(x=name, y=value, ymin=ci_min, ymax=ci_max, fill=name)) +
  geom_bar(position = position_dodge(.9), stat="identity", width = .5)  +
  geom_errorbar(position=position_dodge(.9), width = .05, color = "grey") +
  labs(title="\nGeneral Health\nRating",
       x="", y="") +
  theme_jack() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size=10),
        legend.position = "none", plot.title = element_text(hjust = 0.5))  +
  scale_fill_manual(values=c('#4d94ff','#cc3300')) +
  scale_y_continuous(breaks = 1:5, limits = c(0,5), 
                     labels = c("Poor", "Fair", "Good", "Very Good", "Excellent")) +
  coord_cartesian(ylim=c(1,5)) 

general_health_graph

#       caption = "Data from Cooperative Campaign Election Study 2012 (Self Reported)")
png(filename = "/Users/jacklandry/Documents/GitHub/health_partisanship/figures/general_health_graph.png", 
    width=6, height=4, units="in", res=1000)
general_health_graph
dev.off()


#Want to do correlation between conditions democrats vs. republican correlation heatmap


library(widyr)
library(corrplot)

#https://towardsdatascience.com/customizable-correlation-plots-in-r-b1d2856a4b05

# cc = cor(com, method = "spearman")
# 
# 
# gapminder %>%
#   pairwise_cor(country, year, lifeExp, upper = FALSE, sort = TRUE)
# 



# 
# #This is binscatter
# est <- binsreg(cces_2_party$high_blood_pressure, cces_2_party$age, by=cces_2_party$pid_lean, line=c(3,3), cb=c(3,3), 
#                bycolors=c("blue", "red"), bysymbols=c(19,17))

# health_party <- cces_2_party %>% group_by(pid_lean) %>% 
#   summarise(across(all_of(var_list), ~mean(.x, na.rm=TRUE)))
