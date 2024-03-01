### Behind the graduate mental health crisis in science ###
### March 1, 2024 ###

library(tidyverse)
library(ggrepel)
library(ordinal)
library(stats)

my_data <- read.csv("24mar1_deID_gradMH_data.csv", header = T)

### set reference groups for regressions ----
my_data$race2 <- factor(my_data$race2, 
                        levels = c("asian", "black",
                                   "latinx", "other" ,"white"))
my_data$race2 <- relevel(my_data$race2, ref = "white")

my_data$fin.stab2 <- factor(my_data$fin.stab2, 
                            levels = c("not_always", "yes"))
my_data$fin.stab2 <- relevel(my_data$fin.stab2, ref = "yes")

my_data$gender_bin <- factor(my_data$gender_bin, 
                             levels = c("man", "woman.nb"))
my_data$gender_bin <- relevel(my_data$gender_bin, ref = "man")

my_data$firstgen_grad <- factor(my_data$firstgen_grad)
my_data$firstgen_grad <- relevel(my_data$firstgen_grad, ref = "parent_grad")

my_data$time.in.grad <- factor(my_data$time.in.grad, 
                               levels = c("less.than.one", "one", "two", "three", "four",
                                          "five", "six", "seven", "more.than.seven"))
my_data$time.in.grad <- relevel(my_data$time.in.grad, ref = "less.than.one")

my_data$time.grad_bin <- factor(my_data$time.grad_bin)
my_data$time.grad_bin <- relevel(my_data$time.grad_bin, ref = "3+")

my_data$field.of.study2 <- factor(my_data$field.of.study2)
my_data$field.of.study2 <- relevel(my_data$field.of.study2, ref = "bio")

my_data$dep.sev2 <- factor(my_data$dep.sev2, 
                           levels = c("mild", "moderate", "severe"))
my_data$dep.sev2 <- relevel(my_data$dep.sev2, ref = "mild")

my_data$anx.sev2 <- factor(my_data$anx.sev2, 
                           levels = c("mild", "moderate", "severe"))
my_data$anx.sev2 <- relevel(my_data$anx.sev2, ref = "mild")

my_data$age <- factor(my_data$age, 
                      levels = c("<22", "23-27", "28-32", "33-37", "38+"))
my_data$age <- relevel(my_data$age, ref = "<22")

my_data$degree.type <- factor(my_data$degree.type)
my_data$degree.type <- relevel(my_data$degree.type, ref = "thesis.doc")

my_data$started.covid <- factor(my_data$started.covid, 
                                levels = c("before.covid", "during.covid.inperson", "during.covid.online"))
my_data$started.covid <- relevel(my_data$started.covid, ref = "before.covid")

#make frequency factors
my_data <- my_data |>
  mutate_at(c(colnames(my_data[which(str_detect(colnames(my_data), "frequency."))])),factor,
            levels = c("not", "less.than.year", "yearly", "two.x.year", "four.x.year",
                       "monthly", "weekly")) |>
  mutate_at(c(colnames(my_data[which(str_detect(colnames(my_data), "frequency."))])),relevel,
            ref = "not")

#make severity impact factors
my_data <- my_data |>
  mutate_at(c(colnames(my_data[which(str_detect(colnames(my_data), ".worsen.dep"))]), 
              colnames(my_data[which(str_detect(colnames(my_data), ".worsen.anx"))]),
              colnames(my_data[which(str_detect(colnames(my_data), ".better.dep"))]), 
              colnames(my_data[which(str_detect(colnames(my_data), ".better.anx"))])),factor,
            levels = c("not", "slightly", "somewhat", "very", "extremely")) |>
  mutate_at(c(colnames(my_data[which(str_detect(colnames(my_data), ".worsen.dep"))]), 
              colnames(my_data[which(str_detect(colnames(my_data), ".worsen.anx"))]),
              colnames(my_data[which(str_detect(colnames(my_data), ".better.dep"))]), 
              colnames(my_data[which(str_detect(colnames(my_data), ".better.anx"))])),relevel,
            ref = "not")

### demographic tables -----

demo_fxn <- function(x){
  tmp <- data.frame(my_data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)),
                "my_data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo <- arrange(demo, desc(count))
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  demo <- demo %>% select(name, demo, count)
  return(demo)
}

demo_table <- do.call(rbind, lapply(c("gender", "race", "fin.stab",
                                      "lgbtq", "time.in.grad", 
                                      "depression", "dep.sev",
                                      "anxiety", "anx.sev"),
                                    demo_fxn))

grad_demo_table <- do.call(rbind, lapply(c("field.of.study2", "degree.type", "changed.PIs",
                                           "considered.leaving","started.covid","teaching.experience"),
                                         demo_fxn))

dep_demo_table <- do.call(rbind, lapply(c("depression", "depression2", "phq.score",
                                          "dep.sev", "dep.diagnosis", "dep.treatment"),
                                        demo_fxn))

anx_demo_table <- do.call(rbind, lapply(c("anxiety", "anxiety2", "gad.score", "gad.stand.alone",
                                          "anx.sev", "anx.diagnosis", "anx.treatment"),
                                        demo_fxn))




### negative research aspects summary ----

exacerbate_freq <- c("frequency.failure","frequency.struct",
                     "frequency.neg.ref", "frequency.expect",
                     "frequency.compare", "frequency.tech",
                     "frequency.isolated", "frequency.present",
                     "frequency.require")

exacerbate_aspects <- do.call(rbind,
                              lapply(exacerbate_freq,function(i){
                                tmp <- as.data.frame(table(my_data[[i]]))%>%
                                  filter(Var1 != "")
                                mode <- as.data.frame(tmp[tmp$Freq == max(tmp$Freq),])
                                colnames(mode) <- c("mode_freq", "mode_count")
                                mode$aspect <- i
                                mode$aspect <- str_split(mode$aspect, "\\.")[[1]][2]
                                tmp <- tmp |> filter(Var1 != "not")
                                mode$total_resp <- sum(tmp$Freq)
                                mode$sum <- tmp[tmp$Var1 == "four.x.year",]$Freq*4 +
                                  tmp[tmp$Var1 == "less.than.year",]$Freq*.5 +
                                  tmp[tmp$Var1 == "monthly",]$Freq*12 +
                                  tmp[tmp$Var1 == "two.x.year",]$Freq*2 +
                                  tmp[tmp$Var1 == "weekly",]$Freq*52 +
                                  tmp[tmp$Var1 == "yearly",]$Freq*1
                                mode$avg_freq  <- mode$sum/sum(tmp$Freq)
                                return(mode)
                              }))

dep_worse <- c("failure.worsen.dep", "struct.worsen.dep",
               "neg.ref.worsen.dep", "expect.worsen.dep","compare.worsen.dep",
               "tech.worsen.dep", "isolated.worsen.dep", "present.worsen.dep",
               "require.worsen.dep")

dep_exac_severity <- do.call(rbind,
                             lapply(dep_worse,function(i){
                               tmp <- as.data.frame(table(my_data[[i]]))%>%
                                 filter(Var1 != "")
                               tmp2 <- as.data.frame(i)
                               colnames(tmp2) <- c("aspect_full")
                               tmp2$dep_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                 tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                 tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                 tmp[tmp$Var1 == "very",]$Freq*3 + 
                                 tmp[tmp$Var1 == "extremely",]$Freq*4
                               tmp2$dep_avg <- tmp2$dep_sum/sum(tmp$Freq)
                               tmp2$dep_total <- sum(tmp$Freq)
                               tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                               tmp2$aspect_full <- NULL
                               tmp2$impact_not.d <- tmp[tmp$Var1 == "not",]$Freq
                               tmp2$impact_slightly.d <- tmp[tmp$Var1 == "slightly",]$Freq
                               tmp2$impact_somewhat.d <- tmp[tmp$Var1 == "somewhat",]$Freq
                               tmp2$impact_very.d <- tmp[tmp$Var1 == "very",]$Freq
                               tmp2$impact_extremely.d <- tmp[tmp$Var1 == "extremely",]$Freq
                               tmp2$impact_sum.d <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                        tmp2$impact_somewhat, tmp2$impact_very,
                                                        tmp2$impact_extremely)
                               return(tmp2)
                             }))

anx_worse <- c("failure.worsen.anx", "struct.worsen.anx", "neg.ref.worsen.anx",
               "expect.worsen.anx", "compare.worsen.anx", "tech.worsen.anx",
               "isolated.worsen.anx",  "present.worsen.anx", "require.worsen.anx")

anx_exac_severity <- do.call(rbind,
                             lapply(anx_worse,function(i){
                               tmp <- as.data.frame(table(my_data[[i]]))%>%
                                 filter(Var1 != "")
                               tmp2 <- as.data.frame(i)
                               colnames(tmp2) <- c("aspect_full")
                               tmp2$anx_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                 tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                 tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                 tmp[tmp$Var1 == "very",]$Freq*3 + 
                                 tmp[tmp$Var1 == "extremely",]$Freq*4
                               tmp2$anx_avg <- tmp2$anx_sum/sum(tmp$Freq)
                               tmp2$anx_total <- sum(tmp$Freq)
                               tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                               tmp2$aspect_full <- NULL
                               tmp2$impact_not.a <- tmp[tmp$Var1 == "not",]$Freq
                               tmp2$impact_slightly.a <- tmp[tmp$Var1 == "slightly",]$Freq
                               tmp2$impact_somewhat.a <- tmp[tmp$Var1 == "somewhat",]$Freq
                               tmp2$impact_very.a <- tmp[tmp$Var1 == "very",]$Freq
                               tmp2$impact_extremely.a <- tmp[tmp$Var1 == "extremely",]$Freq
                               tmp2$impact_sum.a <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                        tmp2$impact_somewhat, tmp2$impact_very,
                                                        tmp2$impact_extremely)
                               return(tmp2)
                             }))

exacerbate_aspects <- merge(exacerbate_aspects, dep_exac_severity, by = "aspect")
exacerbate_aspects <- merge(exacerbate_aspects, anx_exac_severity, by = "aspect")

exacerbate_aspects <- dplyr::select(exacerbate_aspects, aspect, avg_freq, dep_avg, anx_avg, everything())
### positive research aspects summary ----

better_freq <- c("frequency.tasks","frequency.progress",
                 "frequency.collab", "frequency.passion",
                 "frequency.flex","frequency.emosh")

better_aspects <- do.call(rbind,
                          lapply(better_freq,function(i){
                            tmp <- as.data.frame(table(my_data[[i]]))%>%
                              filter(Var1 != "")
                            mode <- as.data.frame(tmp[tmp$Freq == max(tmp$Freq),])
                            colnames(mode) <- c("mode_freq", "mode_count")
                            mode$aspect <- i
                            mode$aspect <- str_split(mode$aspect, "\\.")[[1]][2]
                            tmp <- tmp |> filter(Var1 != "not")
                            mode$total_resp <- sum(tmp$Freq)
                            mode$sum <- tmp[tmp$Var1 == "four.x.year",]$Freq*4 +
                              tmp[tmp$Var1 == "less.than.year",]$Freq*.5 +
                              tmp[tmp$Var1 == "monthly",]$Freq*12 +
                              tmp[tmp$Var1 == "two.x.year",]$Freq*2 +
                              tmp[tmp$Var1 == "weekly",]$Freq*52 +
                              tmp[tmp$Var1 == "yearly",]$Freq*1
                            mode$avg_freq  <- mode$sum/sum(tmp$Freq)
                            return(mode)
                          }))

dep_better <- c("tasks.better.dep", "progress.better.dep",
                "collab.better.dep", "passion.better.dep","flex.better.dep",
                "emosh.better.dep")

dep_better_severity <- do.call(rbind,
                               lapply(dep_better,function(i){
                                 tmp <- as.data.frame(table(my_data[[i]]))%>%
                                   filter(Var1 != "")
                                 tmp2 <- as.data.frame(i)
                                 colnames(tmp2) <- c("aspect_full")
                                 tmp2$dep_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                   tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                   tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                   tmp[tmp$Var1 == "very",]$Freq*3 + 
                                   tmp[tmp$Var1 == "extremely",]$Freq*4
                                 tmp2$dep_avg <- tmp2$dep_sum/sum(tmp$Freq)
                                 tmp2$dep_total <- sum(tmp$Freq)
                                 tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                 tmp2$aspect_full <- NULL
                                 tmp2$impact_not.d <- tmp[tmp$Var1 == "not",]$Freq
                                 tmp2$impact_slightly.d <- tmp[tmp$Var1 == "slightly",]$Freq
                                 tmp2$impact_somewhat.d <- tmp[tmp$Var1 == "somewhat",]$Freq
                                 tmp2$impact_very.d <- tmp[tmp$Var1 == "very",]$Freq
                                 tmp2$impact_extremely.d <- tmp[tmp$Var1 == "extremely",]$Freq
                                 tmp2$impact_sum.d <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                          tmp2$impact_somewhat, tmp2$impact_very,
                                                          tmp2$impact_extremely)
                                 return(tmp2)
                               }))

anx_better <- c("tasks.better.anx", "progress.better.anx",
                "collab.better.anx", "passion.better.anx","flex.better.anx",
                "emosh.better.anx")

anx_better_severity <- do.call(rbind,
                               lapply(anx_better,function(i){
                                 tmp <- as.data.frame(table(my_data[[i]]))%>%
                                   filter(Var1 != "")
                                 tmp2 <- as.data.frame(i)
                                 colnames(tmp2) <- c("aspect_full")
                                 tmp2$anx_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                   tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                   tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                   tmp[tmp$Var1 == "very",]$Freq*3 + 
                                   tmp[tmp$Var1 == "extremely",]$Freq*4
                                 tmp2$anx_avg <- tmp2$anx_sum/sum(tmp$Freq)
                                 tmp2$anx_total <- sum(tmp$Freq)
                                 tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                 tmp2$aspect_full <- NULL
                                 tmp2$impact_not.a <- tmp[tmp$Var1 == "not",]$Freq
                                 tmp2$impact_slightly.a <- tmp[tmp$Var1 == "slightly",]$Freq
                                 tmp2$impact_somewhat.a <- tmp[tmp$Var1 == "somewhat",]$Freq
                                 tmp2$impact_very.a <- tmp[tmp$Var1 == "very",]$Freq
                                 tmp2$impact_extremely.a <- tmp[tmp$Var1 == "extremely",]$Freq
                                 tmp2$impact_sum.a <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                          tmp2$impact_somewhat, tmp2$impact_very,
                                                          tmp2$impact_extremely)
                                 return(tmp2)
                               }))

better_aspects <- merge(better_aspects, dep_better_severity, by = "aspect")
better_aspects <- merge(better_aspects, anx_better_severity, by = "aspect")

better_aspects <- dplyr::select(better_aspects, aspect, avg_freq, dep_avg, anx_avg, everything())

### negative teaching aspects summary ----

exacerbate_teach_freq <- c("frequency.response","frequency.negT",
                           "frequency.pressure","frequency.lackTT",
                           "frequency.public")     

exacerbate_teach_aspects <- do.call(rbind,
                                    lapply(exacerbate_teach_freq,function(i){
                                      tmp <- as.data.frame(table(my_data[[i]]))%>%
                                        filter(Var1 != "")
                                      mode <- as.data.frame(tmp[tmp$Freq == max(tmp$Freq),])
                                      colnames(mode) <- c("mode_freq", "mode_count")
                                      mode$aspect <- i
                                      mode$aspect <- str_split(mode$aspect, "\\.")[[1]][2]
                                      tmp <- tmp |> filter(Var1 != "not")
                                      mode$total_resp <- sum(tmp$Freq)
                                      mode$sum <- tmp[tmp$Var1 == "four.x.year",]$Freq*4 +
                                        tmp[tmp$Var1 == "less.than.year",]$Freq*.5 +
                                        tmp[tmp$Var1 == "monthly",]$Freq*12 +
                                        tmp[tmp$Var1 == "two.x.year",]$Freq*2 +
                                        tmp[tmp$Var1 == "weekly",]$Freq*52 +
                                        tmp[tmp$Var1 == "yearly",]$Freq*1
                                      mode$avg_freq  <- mode$sum/sum(tmp$Freq)
                                      return(mode)
                                    }))

dep_worsen_teach <- c("response.worsen.dep", "negT.worsen.dep",
                      "pressure.worsen.dep", "lackTT.worsen.dep","public.worsen.dep")

dep_exac_severity_teach <- do.call(rbind,
                                   lapply(dep_worsen_teach,function(i){
                                     tmp <- as.data.frame(table(my_data[[i]]))%>%
                                       filter(Var1 != "")
                                     tmp2 <- as.data.frame(i)
                                     colnames(tmp2) <- c("aspect_full")
                                     tmp2$dep_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                       tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                       tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                       tmp[tmp$Var1 == "very",]$Freq*3 + 
                                       tmp[tmp$Var1 == "extremely",]$Freq*4
                                     tmp2$dep_avg <- tmp2$dep_sum/sum(tmp$Freq)
                                     tmp2$dep_total <- sum(tmp$Freq)
                                     tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                     tmp2$aspect_full <- NULL
                                     tmp2$impact_not.d <- tmp[tmp$Var1 == "not",]$Freq
                                     tmp2$impact_slightly.d <- tmp[tmp$Var1 == "slightly",]$Freq
                                     tmp2$impact_somewhat.d <- tmp[tmp$Var1 == "somewhat",]$Freq
                                     tmp2$impact_very.d <- tmp[tmp$Var1 == "very",]$Freq
                                     tmp2$impact_extremely.d <- tmp[tmp$Var1 == "extremely",]$Freq
                                     tmp2$impact_sum.d <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                              tmp2$impact_somewhat, tmp2$impact_very,
                                                              tmp2$impact_extremely)
                                     return(tmp2)
                                   }))

anx_worsen_teach <- c("response.worsen.anx", "negT.worsen.anx",
                      "pressure.worsen.anx", "lackTT.worsen.anx","public.worsen.anx")

anx_exac_severity_teach <- do.call(rbind,
                                   lapply(anx_worsen_teach,function(i){
                                     tmp <- as.data.frame(table(my_data[[i]]))%>%
                                       filter(Var1 != "")
                                     tmp2 <- as.data.frame(i)
                                     colnames(tmp2) <- c("aspect_full")
                                     tmp2$anx_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                       tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                       tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                       tmp[tmp$Var1 == "very",]$Freq*3 + 
                                       tmp[tmp$Var1 == "extremely",]$Freq*4
                                     tmp2$anx_avg <- tmp2$anx_sum/sum(tmp$Freq)
                                     tmp2$anx_total <- sum(tmp$Freq)
                                     tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                     tmp2$aspect_full <- NULL
                                     tmp2$impact_not.a <- tmp[tmp$Var1 == "not",]$Freq
                                     tmp2$impact_slightly.a <- tmp[tmp$Var1 == "slightly",]$Freq
                                     tmp2$impact_somewhat.a <- tmp[tmp$Var1 == "somewhat",]$Freq
                                     tmp2$impact_very.a <- tmp[tmp$Var1 == "very",]$Freq
                                     tmp2$impact_extremely.a <- tmp[tmp$Var1 == "extremely",]$Freq
                                     tmp2$impact_sum.a <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                              tmp2$impact_somewhat, tmp2$impact_very,
                                                              tmp2$impact_extremely)
                                     return(tmp2)
                                   }))

exacerbate_teach_aspects <- merge(exacerbate_teach_aspects, dep_exac_severity_teach, by = "aspect")
exacerbate_teach_aspects <- merge(exacerbate_teach_aspects, anx_exac_severity_teach, by = "aspect")

exacerbate_teach_aspects <- dplyr::select(exacerbate_teach_aspects, aspect, avg_freq, dep_avg, anx_avg, everything())


### positive teaching aspects summary ----

better_teach_freq <-  c("frequency.pos.ref","frequency.inc.struc",
                        "frequency.passion.T","frequency.distract",
                        "frequency.conf","frequency.relash")

better_teach_aspects <- do.call(rbind,
                                lapply(better_teach_freq,function(i){
                                  tmp <- as.data.frame(table(my_data[[i]]))%>%
                                    filter(Var1 != "")
                                  mode <- as.data.frame(tmp[tmp$Freq == max(tmp$Freq),])
                                  colnames(mode) <- c("mode_freq", "mode_count")
                                  mode$aspect <- i
                                  mode$aspect <- str_split(mode$aspect, "\\.")[[1]][2]
                                  tmp <- tmp |> filter(Var1 != "not")
                                  mode$total_resp <- sum(tmp$Freq)
                                  mode$sum <- tmp[tmp$Var1 == "four.x.year",]$Freq*4 +
                                    tmp[tmp$Var1 == "less.than.year",]$Freq*.5 +
                                    tmp[tmp$Var1 == "monthly",]$Freq*12 +
                                    tmp[tmp$Var1 == "two.x.year",]$Freq*2 +
                                    tmp[tmp$Var1 == "weekly",]$Freq*52 +
                                    tmp[tmp$Var1 == "yearly",]$Freq*1
                                  mode$avg_freq  <- mode$sum/sum(tmp$Freq)
                                  return(mode)
                                }))

dep_better_teach <- c("pos.ref.better.dep", "inc.struc.better.dep",
                      "passion.T.better.dep", "distract.better.dep","conf.better.dep",
                      "relash.better.dep")

dep_better_severity_teach <- do.call(rbind,
                                     lapply(dep_better_teach,function(i){
                                       tmp <- as.data.frame(table(my_data[[i]]))%>%
                                         filter(Var1 != "")
                                       tmp2 <- as.data.frame(i)
                                       colnames(tmp2) <- c("aspect_full")
                                       tmp2$dep_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                         tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                         tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                         tmp[tmp$Var1 == "very",]$Freq*3 + 
                                         tmp[tmp$Var1 == "extremely",]$Freq*4
                                       tmp2$dep_avg <- tmp2$dep_sum/sum(tmp$Freq)
                                       tmp2$dep_total <- sum(tmp$Freq)
                                       tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                       tmp2$aspect_full <- NULL
                                       tmp2$impact_not.d <- tmp[tmp$Var1 == "not",]$Freq
                                       tmp2$impact_slightly.d <- tmp[tmp$Var1 == "slightly",]$Freq
                                       tmp2$impact_somewhat.d <- tmp[tmp$Var1 == "somewhat",]$Freq
                                       tmp2$impact_very.d <- tmp[tmp$Var1 == "very",]$Freq
                                       tmp2$impact_extremely.d <- tmp[tmp$Var1 == "extremely",]$Freq
                                       tmp2$impact_sum.d <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                                tmp2$impact_somewhat, tmp2$impact_very,
                                                                tmp2$impact_extremely)
                                       return(tmp2)
                                     }))

anx_better_teach <- c("pos.ref.better.anx", "inc.struc.better.anx",
                      "passion.T.better.anx", "distract.better.anx","conf.better.anx",
                      "relash.better.anx")

anx_better_severity_teach <- do.call(rbind,
                                     lapply(anx_better_teach,function(i){
                                       tmp <- as.data.frame(table(my_data[[i]]))%>%
                                         filter(Var1 != "")
                                       tmp2 <- as.data.frame(i)
                                       colnames(tmp2) <- c("aspect_full")
                                       tmp2$anx_sum <- tmp[tmp$Var1 == "not",]$Freq*0 +
                                         tmp[tmp$Var1 == "slightly",]$Freq*1 + 
                                         tmp[tmp$Var1 == "somewhat",]$Freq*2 + 
                                         tmp[tmp$Var1 == "very",]$Freq*3 + 
                                         tmp[tmp$Var1 == "extremely",]$Freq*4
                                       tmp2$anx_avg <- tmp2$anx_sum/sum(tmp$Freq)
                                       tmp2$anx_total <- sum(tmp$Freq)
                                       tmp2$aspect <- str_split(tmp2$aspect_full, "\\.")[[1]][1]
                                       tmp2$aspect_full <- NULL
                                       tmp2$impact_not.a <- tmp[tmp$Var1 == "not",]$Freq
                                       tmp2$impact_slightly.a <- tmp[tmp$Var1 == "slightly",]$Freq
                                       tmp2$impact_somewhat.a <- tmp[tmp$Var1 == "somewhat",]$Freq
                                       tmp2$impact_very.a <- tmp[tmp$Var1 == "very",]$Freq
                                       tmp2$impact_extremely.a <- tmp[tmp$Var1 == "extremely",]$Freq
                                       tmp2$impact_sum.a <- sum(tmp2$impact_not, tmp2$impact_slightly,
                                                                tmp2$impact_somewhat, tmp2$impact_very,
                                                                tmp2$impact_extremely)
                                       return(tmp2)
                                     }))

better_teach_aspects <- merge(better_teach_aspects, dep_better_severity_teach, by = "aspect")
better_teach_aspects <- merge(better_teach_aspects, anx_better_severity_teach, by = "aspect")

better_teach_aspects <- dplyr::select(better_teach_aspects, aspect, avg_freq, dep_avg, anx_avg, everything())

### ordinal regression negative research on teaching on depression and anxiety -----

#negative research aspects: "failure", "struct", "neg.ref", "expect", "compare", "tech", "isolated", "present", "require"
#negative teaching aspects: "response", "negT", "pressure", "lackTT", "public"
#.worsen.dep, .worsen.anx

#"failure.worsen.anx","failure.worsen.dep","struct.worsen.anx","struct.worsen.dep","neg.ref.worsen.anx","neg.ref.worsen.dep", "expect.worsen.anx","expect.worsen.dep","compare.worsen.anx","compare.worsen.dep","tech.worsen.anx","tech.worsen.dep", "isolated.worsen.anx","isolated.worsen.dep","present.worsen.anx","present.worsen.dep","require.worsen.anx","require.worsen.dep","response.worsen.anx","response.worsen.dep","negT.worsen.anx","negT.worsen.dep","pressure.worsen.anx","pressure.worsen.dep","lackTT.worsen.anx","lackTT.worsen.dep","public.worsen.anx","public.worsen.dep"

negative_long <- my_data |>
  dplyr::select(ID, anxiety2, depression2, anx.sev2, dep.sev2,failure.worsen.anx,failure.worsen.dep,struct.worsen.anx,struct.worsen.dep,
                neg.ref.worsen.anx,neg.ref.worsen.dep, expect.worsen.anx,expect.worsen.dep,compare.worsen.anx,compare.worsen.dep,
                tech.worsen.anx,tech.worsen.dep, isolated.worsen.anx,isolated.worsen.dep,present.worsen.anx,present.worsen.dep,
                require.worsen.anx,require.worsen.dep,response.worsen.anx,response.worsen.dep,negT.worsen.anx,negT.worsen.dep,
                pressure.worsen.anx,pressure.worsen.dep,lackTT.worsen.anx,lackTT.worsen.dep,public.worsen.anx,public.worsen.dep) |>
  reshape2::melt(id = c("ID", "anxiety2", "depression2", "anx.sev2", "dep.sev2")) |> 
  rename(aspect = variable, impact = value) |>
  reshape2::melt(id = c("ID", "anxiety2", "depression2", "aspect", "impact")) |> 
  filter(!is.na(impact)) |> 
  mutate(condition = ifelse(str_detect(aspect, ".dep") == T, "depression", ifelse(str_detect(aspect, ".anx") == T, "anxiety", NA))) |> 
  as.data.frame()


negative_long <- negative_long |>
  filter((condition == "depression" & str_detect(variable, "dep")) | 
           (condition == "anxiety" & str_detect(variable, "anx")))

negative_long$value <- factor(negative_long$value, 
                              levels = c("mild", "moderate", "severe"))
negative_long$value <- relevel(negative_long$value, ref = "mild")

negative_long$condition <- factor(negative_long$condition, 
                                  levels = c("anxiety", "depression"))
negative_long$condition <- relevel(negative_long$condition, ref = "depression")


negative_long$impact <- factor(negative_long$impact, 
                               levels = c("not", "slightly", "somewhat", "very", "extremely"))
negative_long$impact <- relevel(negative_long$impact, ref = "not")

summary(ordinal::clm(impact ~ condition + value + (1|ID),
                     data = negative_long,
                     Hess = TRUE))

negative_long$context <- NA
negative_long[str_detect(negative_long$aspect, "failure|struct|neg.ref|expect|compare|tech|isolated|present|require"),]$context <- "research"
negative_long[str_detect(negative_long$aspect, "response|negT|pressure|lackTT|public"),]$context <- "teaching"


negative_long$context <- factor(negative_long$context, 
                                levels = c("research", "teaching"))
negative_long$context <- relevel(negative_long$context, ref = "teaching")

summary(ordinal::clm(impact ~ context + value + (1|ID),
                     data = negative_long,
                     Hess = TRUE))

### ordinal regression positive research on teaching on depression and anxiety -----

#positive research aspects: "tasks", "progress", "collab", "passion","flex", "emosh"
#positive teaching aspects: "pos.ref", "inc.struc", "passion.T", "distract","conf", "relash"
#.better.dep, .better.anx

#[1] "tasks.better.anx","tasks.better.dep","progress.better.anx","progress.better.dep","collab.better.anx","collab.better.dep","passion.better.anx","passion.better.dep","flex.better.anx","flex.better.dep","emosh.better.anx","emosh.better.dep","pos.ref.better.anx","pos.ref.better.dep","inc.struc.better.anx","inc.struc.better.dep","passion.T.better.anx" "passion.T.better.dep","distract.better.anx","distract.better.dep","conf.better.anx","conf.better.dep","relash.better.anx","relash.better.dep" 

positive_long <- my_data |>
  dplyr::select(ID, anxiety2, depression2, anx.sev2, dep.sev2,tasks.better.anx,tasks.better.dep,progress.better.anx,progress.better.dep,
                collab.better.anx,collab.better.dep,passion.better.anx,passion.better.dep,flex.better.anx,flex.better.dep,emosh.better.anx,
                emosh.better.dep,pos.ref.better.anx,pos.ref.better.dep,inc.struc.better.anx,inc.struc.better.dep,passion.T.better.anx,
                passion.T.better.dep,distract.better.anx,distract.better.dep,conf.better.anx,conf.better.dep,relash.better.anx,
                relash.better.dep) |>
  reshape2::melt(id = c("ID", "anxiety2", "depression2", "anx.sev2", "dep.sev2")) |> 
  rename(aspect = variable, impact = value) |>
  reshape2::melt(id = c("ID", "anxiety2", "depression2", "aspect", "impact")) |> 
  filter(!is.na(impact)) |> 
  mutate(condition = ifelse(str_detect(aspect, ".dep") == T, "depression", ifelse(str_detect(aspect, ".anx") == T, "anxiety", NA))) |> 
  as.data.frame()


positive_long <- positive_long |>
  filter((condition == "depression" & str_detect(variable, "dep")) | 
           (condition == "anxiety" & str_detect(variable, "anx")))

positive_long$value <- factor(positive_long$value, 
                              levels = c("mild", "moderate", "severe"))
positive_long$value <- relevel(positive_long$value, ref = "mild")

positive_long$condition <- factor(positive_long$condition, 
                                  levels = c("anxiety", "depression"))
positive_long$condition <- relevel(positive_long$condition, ref = "depression")


positive_long$impact <- factor(positive_long$impact, 
                               levels = c("not", "slightly", "somewhat", "very", "extremely"))
positive_long$impact <- relevel(positive_long$impact, ref = "not")

summary(ordinal::clm(impact ~ condition + value + (1|ID),
                     data = positive_long,
                     Hess = TRUE))

positive_long$context <- NA
positive_long[str_detect(positive_long$aspect, "tasks|progress|collab|passion|flex|emosh"),]$context <- "research"
positive_long[str_detect(positive_long$aspect, "pos.ref|inc.struc|passion.T|distract|conf|relash"),]$context <- "teaching"


positive_long$context <- factor(positive_long$context, 
                                levels = c("research", "teaching"))
positive_long$context <- relevel(positive_long$context, ref = "teaching")

summary(ordinal::clm(impact ~ context + value + (1|ID),
                     data = positive_long,
                     Hess = TRUE))



### grad research worsen depression & anxiety ordinal regressions STUDENT ----

res_neg_mods <- do.call(rbind, lapply(c("failure", "struct",
                                        "neg.ref", "expect","compare",
                                        "tech", "isolated", "present",
                                        "require"),
                                      function(x){
                                        mod_out_dep <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x,".worsen.dep ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_dep$aspect <- x
                                        mod_out_dep$predictor <- rownames(mod_out_dep)
                                        mod_out_dep$condition <- "depression"
                                        mod_out_anx <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x, ".worsen.anx ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_anx$aspect <- x
                                        mod_out_anx$predictor <- rownames(mod_out_anx)
                                        mod_out_anx$condition <- "anxiety"
                                        mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                        return(mod_out_combo)
                                      }))


res_neg_mods <- res_neg_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(res_neg_mods) <- NULL
res_neg_mods$or <- exp(res_neg_mods$est)
res_neg_mods <- res_neg_mods |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
res_neg_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")
#for depression
for(i in 1:length(predictors)){
  res_neg_mods[res_neg_mods$predictor == predictors[i] & res_neg_mods$condition == "depression",]$qval<-p.adjust(p = res_neg_mods[res_neg_mods$predictor == predictors[i] & res_neg_mods$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  res_neg_mods[res_neg_mods$predictor == predictors[i] & res_neg_mods$condition == "anxiety",]$qval<-p.adjust(p = res_neg_mods[res_neg_mods$predictor == predictors[i] & res_neg_mods$condition == "anxiety",]$pval, method = "fdr")
}

res_neg_mods |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                       & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")



### grad teaching worsen depression & anxiety ordinal regressions STUDENT ----
tea_neg_mods <- do.call(rbind, lapply(c("response","negT",
                                        "pressure","lackTT",
                                        "public"),
                                      function(x){
                                        mod_out_dep <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x,".worsen.dep ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_dep$aspect <- x
                                        mod_out_dep$predictor <- rownames(mod_out_dep)
                                        mod_out_dep$condition <- "depression"
                                        mod_out_anx <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x, ".worsen.anx ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_anx$aspect <- x
                                        mod_out_anx$predictor <- rownames(mod_out_anx)
                                        mod_out_anx$condition <- "anxiety"
                                        mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                        return(mod_out_combo)
                                      }))


tea_neg_mods <- tea_neg_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(tea_neg_mods) <- NULL
tea_neg_mods$or <- exp(tea_neg_mods$est)
tea_neg_mods <- tea_neg_mods |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
tea_neg_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")
#for depression
for(i in 1:length(predictors)){
  tea_neg_mods[tea_neg_mods$predictor == predictors[i] & tea_neg_mods$condition == "depression",]$qval<-p.adjust(p = tea_neg_mods[tea_neg_mods$predictor == predictors[i] & tea_neg_mods$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  tea_neg_mods[tea_neg_mods$predictor == predictors[i] & tea_neg_mods$condition == "anxiety",]$qval<-p.adjust(p = tea_neg_mods[tea_neg_mods$predictor == predictors[i] & tea_neg_mods$condition == "anxiety",]$pval, method = "fdr")
}

tea_neg_mods |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                       & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")

### grad research better depression & anxiety ordinal regressions STUDENT ----

res_pos_mods <- do.call(rbind, lapply(c("tasks", "progress",
                                        "passion", "flex","emosh",
                                        "collab"),
                                      function(x){
                                        mod_out_dep <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x,".better.dep ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_dep$aspect <- x
                                        mod_out_dep$predictor <- rownames(mod_out_dep)
                                        mod_out_dep$condition <- "depression"
                                        mod_out_anx <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x, ".better.anx ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_anx$aspect <- x
                                        mod_out_anx$predictor <- rownames(mod_out_anx)
                                        mod_out_anx$condition <- "anxiety"
                                        mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                        return(mod_out_combo)
                                      }))


res_pos_mods <- res_pos_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(res_pos_mods) <- NULL
res_pos_mods$or <- exp(res_pos_mods$est)
res_pos_mods <- res_pos_mods |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
res_pos_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")
#for depression
for(i in 1:length(predictors)){
  res_pos_mods[res_pos_mods$predictor == predictors[i] & res_pos_mods$condition == "depression",]$qval<-p.adjust(p = res_pos_mods[res_pos_mods$predictor == predictors[i] & res_pos_mods$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  res_pos_mods[res_pos_mods$predictor == predictors[i] & res_pos_mods$condition == "anxiety",]$qval<-p.adjust(p = res_pos_mods[res_pos_mods$predictor == predictors[i] & res_pos_mods$condition == "anxiety",]$pval, method = "fdr")
}

res_pos_mods |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                       & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")


### grad teaching better depression & anxiety ordinal regressions STUDENT ----

tea_pos_mods <- do.call(rbind, lapply(c("pos.ref","inc.struc",
                                        "relash","passion.T",
                                        "distract", "conf"),
                                      function(x){
                                        mod_out_dep <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x,".better.dep ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_dep$aspect <- x
                                        mod_out_dep$predictor <- rownames(mod_out_dep)
                                        mod_out_dep$condition <- "depression"
                                        mod_out_anx <- as.data.frame(summary(
                                          ordinal::clm(as.formula(paste0(x, ".better.anx ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                       data = my_data,
                                                       Hess = TRUE))$coefficients)
                                        mod_out_anx$aspect <- x
                                        mod_out_anx$predictor <- rownames(mod_out_anx)
                                        mod_out_anx$condition <- "anxiety"
                                        mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                        return(mod_out_combo)
                                      }))


tea_pos_mods <- tea_pos_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(tea_pos_mods) <- NULL
tea_pos_mods$or <- exp(tea_pos_mods$est)
tea_pos_mods <- tea_pos_mods |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
tea_pos_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")
#for depression
for(i in 1:length(predictors)){
  tea_pos_mods[tea_pos_mods$predictor == predictors[i] & tea_pos_mods$condition == "depression",]$qval<-p.adjust(p = tea_pos_mods[tea_pos_mods$predictor == predictors[i] & tea_pos_mods$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  tea_pos_mods[tea_pos_mods$predictor == predictors[i] & tea_pos_mods$condition == "anxiety",]$qval<-p.adjust(p = tea_pos_mods[tea_pos_mods$predictor == predictors[i] & tea_pos_mods$condition == "anxiety",]$pval, method = "fdr")
}

tea_pos_mods |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                       & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "depression")
### mediation analyses for impact of research and teaching on MH -----

#list of demographics that sig predict severity of depression AND the impact of an aspect on MH
summary(glm(phq.score ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin, data = my_data)) #use bc of M
#dep sig: gender, race (black), lgbtq, fin.stab, time grad

#make binary black column for mediation
my_data$black <- NA
my_data[my_data$race2 == "black" & !is.na(my_data$race2),]$black <- "yes"
my_data[my_data$race2 == "asian" & !is.na(my_data$race2),]$black <- "no"
my_data[my_data$race2 == "white" & !is.na(my_data$race2),]$black <- "no"
my_data[my_data$race2 == "latinx" & !is.na(my_data$race2),]$black <- "no"
my_data[my_data$race2 == "other" & !is.na(my_data$race2),]$black <- "no"

#depression-related mediations
med_dep <- do.call(rbind, lapply(c("failure.worsen.dep", "struct.worsen.dep",
                                   "neg.ref.worsen.dep", "expect.worsen.dep","compare.worsen.dep",
                                   "tech.worsen.dep", "isolated.worsen.dep", "present.worsen.dep",
                                   "require.worsen.dep",
                                   "tasks.better.dep", "progress.better.dep",
                                   "passion.better.dep", "flex.better.dep","emosh.better.dep",
                                   "collab.better.dep",
                                   "response.worsen.dep","negT.worsen.dep",
                                   "pressure.worsen.dep","lackTT.worsen.dep",
                                   "public.worsen.dep",
                                   "pos.ref.better.dep","inc.struc.better.dep",
                                   "relash.better.dep","passion.T.better.dep",
                                   "distract.better.dep", "conf.better.dep"),function(y) {
                                     do.call(rbind, lapply(c("gender_bin", "black", "lgbtq", "fin.stab2", "time.grad_bin"),
                                                           function(x){
                                                             df <- data.frame(my_data$ID)
                                                             df$M <- my_data[["phq.score"]]
                                                             df$X <- my_data[[x]] #demographic, predictor
                                                             df$Y <- my_data[[y]] #aspect, outcome
                                                             model.one <- ordinal::clm(Y~X, data = df, Hess = T)
                                                             mod.one <- as.data.frame(summary(model.one)$coefficients)[,-3]
                                                             mod.one <- mod.one |> rename(est = Estimate,
                                                                                          se = `Std. Error`, 
                                                                                          pval = `Pr(>|z|)`) |> as.data.frame()
                                                             mod.one$predictor <- rownames(mod.one)
                                                             rownames(mod.one) <- NULL
                                                             mod.one <- mod.one |> filter(str_detect(predictor, "X"))
                                                             mod.one$X <- x #"gender_bin"
                                                             mod.one$Y <- y #"failure.worsen.dep"
                                                             mod.one$M <- "phq.score"
                                                             mod.one$model <- "one"
                                                             model.two <- ordinal::clm(Y~X+M, data = df, Hess = T)
                                                             mod.two <- as.data.frame(summary(model.two)$coefficients)[,-3]
                                                             mod.two <- mod.two |> rename(est = Estimate,
                                                                                          se = `Std. Error`, 
                                                                                          pval = `Pr(>|z|)`) |> as.data.frame()
                                                             mod.two$predictor <- rownames(mod.two)
                                                             rownames(mod.two) <- NULL
                                                             mod.two <- mod.two |> filter(str_detect(predictor, "X")| str_detect(predictor, "M"))
                                                             mod.two$X <- x #"gender_bin" 
                                                             mod.two$Y <- y #"failure.worsen.dep"
                                                             mod.two$M <- "phq.score"
                                                             mod.two$model <- "two"
                                                             model.three <- lm(M~X, data = df)
                                                             mod.three <- as.data.frame(summary(model.three)$coefficients)[,-3]
                                                             mod.three <- mod.three |> rename(est = Estimate,
                                                                                              se = `Std. Error`, 
                                                                                              pval = `Pr(>|t|)`) |> as.data.frame()
                                                             mod.three$predictor <- rownames(mod.three)
                                                             rownames(mod.three) <- NULL
                                                             mod.three <- mod.three |> filter(str_detect(predictor, "X"))
                                                             mod.three$X <- x #"gender_bin" 
                                                             mod.three$Y <- y #"failure.worsen.dep"
                                                             mod.three$M <- "phq.score"
                                                             mod.three$model <- "three"
                                                             full_mod <- rbind(mod.one, mod.two, mod.three)
                                                             full_mod$a <- mod.three$est[1]
                                                             full_mod$b <- mod.two$est[2]
                                                             full_mod$c <- mod.one$est[1]
                                                             full_mod$cp <- mod.two$est[1]
                                                             full_mod$ab <-full_mod$a*full_mod$b #indirect effect
                                                             full_mod$prop_med <- full_mod$ab/full_mod$c #proportion mediated effect
                                                             
                                                             return(full_mod)
                                                           }))}))

med_dep$formula <- paste0(med_dep$Y, "~", med_dep$X, "+", med_dep$M)

nsig_x <- med_dep[(med_dep$model == "one" & med_dep$pval > .05) | 
                    (med_dep$model == "three" & med_dep$pval > .05),]

med_dep <- med_dep |>
  filter(!(formula %in% nsig_x$formula))

rm(nsig_x)


#list of demographics that sig predict severity of anxiety AND the impact of an aspect on MH
summary(glm(gad.score ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin, data = my_data)) #use bc of M
#anx sig: gender, race (asian), lgbtq, fin.stab

my_data$asian <- NA
my_data[my_data$race2 == "asian" & !is.na(my_data$race2),]$asian <- "yes"
my_data[my_data$race2 == "latinx" & !is.na(my_data$race2),]$asian <- "no"
my_data[my_data$race2 == "white" & !is.na(my_data$race2),]$asian <- "no"
my_data[my_data$race2 == "black" & !is.na(my_data$race2),]$asian <- "no"
my_data[my_data$race2 == "other" & !is.na(my_data$race2),]$asian <- "no"

med_anx <- do.call(rbind, lapply(c("failure.worsen.anx", "struct.worsen.anx",
                                   "neg.ref.worsen.anx", "expect.worsen.anx","compare.worsen.anx",
                                   "tech.worsen.anx", "isolated.worsen.anx", "present.worsen.anx",
                                   "require.worsen.anx",
                                   "tasks.better.anx", "progress.better.anx",
                                   "passion.better.anx", "flex.better.anx","emosh.better.anx",
                                   "collab.better.anx",
                                   "response.worsen.anx","negT.worsen.anx",
                                   "pressure.worsen.anx","lackTT.worsen.anx",
                                   "public.worsen.anx",
                                   "pos.ref.better.anx","inc.struc.better.anx",
                                   "relash.better.anx","passion.T.better.anx",
                                   "distract.better.anx", "conf.better.anx"),function(y) {
                                     do.call(rbind, lapply(c("gender_bin", "asian", "lgbtq", "fin.stab2"),
                                                           function(x){
                                                             df <- data.frame(my_data$ID)
                                                             df$M <- my_data[["gad.score"]]
                                                             df$X <- my_data[[x]] #demographic, predictor
                                                             df$Y <- my_data[[y]] #aspect, outcome
                                                             model.one <- ordinal::clm(Y~X, data = df, Hess = T)
                                                             mod.one <- as.data.frame(summary(model.one)$coefficients)[,-3]
                                                             mod.one <- mod.one |> rename(est = Estimate,
                                                                                          se = `Std. Error`, 
                                                                                          pval = `Pr(>|z|)`) |> as.data.frame()
                                                             mod.one$predictor <- rownames(mod.one)
                                                             rownames(mod.one) <- NULL
                                                             mod.one <- mod.one |> filter(str_detect(predictor, "X"))
                                                             mod.one$X <- x #"gender_bin"
                                                             mod.one$Y <- y #"failure.worsen.anx"
                                                             mod.one$M <- "gad.score"
                                                             mod.one$model <- "one"
                                                             model.two <- ordinal::clm(Y~X+M, data = df, Hess = T)
                                                             mod.two <- as.data.frame(summary(model.two)$coefficients)[,-3]
                                                             mod.two <- mod.two |> rename(est = Estimate,
                                                                                          se = `Std. Error`, 
                                                                                          pval = `Pr(>|z|)`) |> as.data.frame()
                                                             mod.two$predictor <- rownames(mod.two)
                                                             rownames(mod.two) <- NULL
                                                             mod.two <- mod.two |> filter(str_detect(predictor, "X")| str_detect(predictor, "M"))
                                                             mod.two$X <- x #"gender_bin" 
                                                             mod.two$Y <- y #"failure.worsen.anx"
                                                             mod.two$M <- "gad.score"
                                                             mod.two$model <- "two"
                                                             model.three <- lm(M~X, data = df)
                                                             mod.three <- as.data.frame(summary(model.three)$coefficients)[,-3]
                                                             mod.three <- mod.three |> rename(est = Estimate,
                                                                                              se = `Std. Error`, 
                                                                                              pval = `Pr(>|t|)`) |> as.data.frame()
                                                             mod.three$predictor <- rownames(mod.three)
                                                             rownames(mod.three) <- NULL
                                                             mod.three <- mod.three |> filter(str_detect(predictor, "X"))
                                                             mod.three$X <- x #"gender_bin" 
                                                             mod.three$Y <- y #"failure.worsen.anx"
                                                             mod.three$M <- "gad.score"
                                                             mod.three$model <- "three"
                                                             full_mod <- rbind(mod.one, mod.two, mod.three)
                                                             full_mod$a <- mod.three$est[1]
                                                             full_mod$b <- mod.two$est[2]
                                                             full_mod$c <- mod.one$est[1]
                                                             full_mod$cp <- mod.two$est[1]
                                                             full_mod$ab <-full_mod$a*full_mod$b #indirect effect
                                                             full_mod$prop_med <- full_mod$ab/full_mod$c #proportion mediated effect
                                                             
                                                             return(full_mod)
                                                           }))}))

med_anx$formula <- paste0(med_anx$Y, "~", med_anx$X, "+", med_anx$M)

nsig_x <- med_anx[(med_anx$model == "one" & med_anx$pval > .05) | 
                    (med_anx$model == "three" & med_anx$pval > .05),]

med_anx <- med_anx |>
  filter(!(formula %in% nsig_x$formula))

rm(nsig_x)


### grad research worsen depression & anxiety ordinal regressions PROGRAM ----

res_neg_mods_prog <- do.call(rbind, lapply(c("failure", "struct",
                                             "neg.ref", "expect","compare",
                                             "tech", "isolated", "present",
                                             "require"),
                                           function(x){
                                             mod_out_dep <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x,".worsen.dep ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_dep$aspect <- x
                                             mod_out_dep$predictor <- rownames(mod_out_dep)
                                             mod_out_dep$condition <- "depression"
                                             mod_out_anx <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x, ".worsen.anx ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_anx$aspect <- x
                                             mod_out_anx$predictor <- rownames(mod_out_anx)
                                             mod_out_anx$condition <- "anxiety"
                                             mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                             return(mod_out_combo)
                                           }))


res_neg_mods_prog <- res_neg_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(res_neg_mods_prog) <- NULL
res_neg_mods_prog$or <- exp(res_neg_mods_prog$est)
res_neg_mods_prog <- res_neg_mods_prog |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
res_neg_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")
#for depression
for(i in 1:length(predictors)){
  res_neg_mods_prog[res_neg_mods_prog$predictor == predictors[i] & res_neg_mods_prog$condition == "depression",]$qval<-p.adjust(p = res_neg_mods_prog[res_neg_mods_prog$predictor == predictors[i] & res_neg_mods_prog$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  res_neg_mods_prog[res_neg_mods_prog$predictor == predictors[i] & res_neg_mods_prog$condition == "anxiety",]$qval<-p.adjust(p = res_neg_mods_prog[res_neg_mods_prog$predictor == predictors[i] & res_neg_mods_prog$condition == "anxiety",]$pval, method = "fdr")
}

res_neg_mods_prog |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                            & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")
### grad teaching worsen depression & anxiety ordinal regressions PROGRAM ----

tea_neg_mods_prog <- do.call(rbind, lapply(c("response","negT",
                                             "pressure","lackTT",
                                             "public"),
                                           function(x){
                                             mod_out_dep <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x,".worsen.dep ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_dep$aspect <- x
                                             mod_out_dep$predictor <- rownames(mod_out_dep)
                                             mod_out_dep$condition <- "depression"
                                             mod_out_anx <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x, ".worsen.anx ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_anx$aspect <- x
                                             mod_out_anx$predictor <- rownames(mod_out_anx)
                                             mod_out_anx$condition <- "anxiety"
                                             mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                             return(mod_out_combo)
                                           }))


tea_neg_mods_prog <- tea_neg_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(tea_neg_mods_prog) <- NULL
tea_neg_mods_prog$or <- exp(tea_neg_mods_prog$est)
tea_neg_mods_prog <- tea_neg_mods_prog |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
tea_neg_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")
#for depression
for(i in 1:length(predictors)){
  tea_neg_mods_prog[tea_neg_mods_prog$predictor == predictors[i] & tea_neg_mods_prog$condition == "depression",]$qval<-p.adjust(p = tea_neg_mods_prog[tea_neg_mods_prog$predictor == predictors[i] & tea_neg_mods_prog$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  tea_neg_mods_prog[tea_neg_mods_prog$predictor == predictors[i] & tea_neg_mods_prog$condition == "anxiety",]$qval<-p.adjust(p = tea_neg_mods_prog[tea_neg_mods_prog$predictor == predictors[i] & tea_neg_mods_prog$condition == "anxiety",]$pval, method = "fdr")
}

tea_neg_mods_prog |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                            & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")


### grad research better depression & anxiety ordinal regressions PROGRAM ----

res_pos_mods_prog <- do.call(rbind, lapply(c("tasks", "progress",
                                             "passion", "flex","emosh",
                                             "collab"),
                                           function(x){
                                             mod_out_dep <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x,".better.dep ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_dep$aspect <- x
                                             mod_out_dep$predictor <- rownames(mod_out_dep)
                                             mod_out_dep$condition <- "depression"
                                             mod_out_anx <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x, ".better.anx ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_anx$aspect <- x
                                             mod_out_anx$predictor <- rownames(mod_out_anx)
                                             mod_out_anx$condition <- "anxiety"
                                             mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                             return(mod_out_combo)
                                           }))


res_pos_mods_prog <- res_pos_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(res_pos_mods_prog) <- NULL
res_pos_mods_prog$or <- exp(res_pos_mods_prog$est)
res_pos_mods_prog <- res_pos_mods_prog |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
res_pos_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")
#for depression
for(i in 1:length(predictors)){
  res_pos_mods_prog[res_pos_mods_prog$predictor == predictors[i] & res_pos_mods_prog$condition == "depression",]$qval<-p.adjust(p = res_pos_mods_prog[res_pos_mods_prog$predictor == predictors[i] & res_pos_mods_prog$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  res_pos_mods_prog[res_pos_mods_prog$predictor == predictors[i] & res_pos_mods_prog$condition == "anxiety",]$qval<-p.adjust(p = res_pos_mods_prog[res_pos_mods_prog$predictor == predictors[i] & res_pos_mods_prog$condition == "anxiety",]$pval, method = "fdr")
}

res_pos_mods_prog |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                            & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")

### grad teaching better depression & anxiety ordinal regressions PROGRAM ----

tea_pos_mods_prog <- do.call(rbind, lapply(c("pos.ref","inc.struc",
                                             "relash","passion.T",
                                             "distract", "conf"),
                                           function(x){
                                             mod_out_dep <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x,".better.dep ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_dep$aspect <- x
                                             mod_out_dep$predictor <- rownames(mod_out_dep)
                                             mod_out_dep$condition <- "depression"
                                             mod_out_anx <- as.data.frame(summary(
                                               ordinal::clm(as.formula(paste0(x, ".better.anx ~ degree.type + field.of.study2")),
                                                            data = my_data,
                                                            Hess = TRUE))$coefficients)
                                             mod_out_anx$aspect <- x
                                             mod_out_anx$predictor <- rownames(mod_out_anx)
                                             mod_out_anx$condition <- "anxiety"
                                             mod_out_combo <- rbind(mod_out_dep, mod_out_anx)
                                             return(mod_out_combo)
                                           }))


tea_pos_mods_prog <- tea_pos_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(tea_pos_mods_prog) <- NULL
tea_pos_mods_prog$or <- exp(tea_pos_mods_prog$est)
tea_pos_mods_prog <- tea_pos_mods_prog |> dplyr::select(aspect, condition, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
tea_pos_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")
#for depression
for(i in 1:length(predictors)){
  tea_pos_mods_prog[tea_pos_mods_prog$predictor == predictors[i] & tea_pos_mods_prog$condition == "depression",]$qval<-p.adjust(p = tea_pos_mods_prog[tea_pos_mods_prog$predictor == predictors[i] & tea_pos_mods_prog$condition == "depression",]$pval, method = "fdr")
}

#for anxiety
for(i in 1:length(predictors)){
  tea_pos_mods_prog[tea_pos_mods_prog$predictor == predictors[i] & tea_pos_mods_prog$condition == "anxiety",]$qval<-p.adjust(p = tea_pos_mods_prog[tea_pos_mods_prog$predictor == predictors[i] & tea_pos_mods_prog$condition == "anxiety",]$pval, method = "fdr")
}

tea_pos_mods_prog |> filter(qval < .05 & predictor != "not|slightly" & predictor != "slightly|somewhat"
                            & predictor != "somewhat|very" & predictor != "very|extremely") |> 
  filter(condition == "anxiety")


### table aspects depression/anxiety affect grad research/teaching ----

depanx_fxn <- function(x){
  tmp <- data.frame(my_data[[x]])
  demo <- merge(data.frame(table(tmp)), 
                data.frame(round((table(tmp)/sum(table(tmp)))*100, 2)),
                "my_data..x..")
  colnames(demo)<-c("demo","count","perc")
  demo$denom <- sum(table(tmp))
  demo$`Percent (n)` <- paste0(demo$perc, " (", demo$count, ")")
  demo <- arrange(demo, desc(count))
  demo$demo <- stringr::str_to_title(demo$demo)
  demo$name <- x
  demo <- demo %>% select(name, everything())
  return(demo)
}

depanx_impact <- do.call(rbind, lapply(c("dep.res_lack.motivation", "dep.res_low.selfesteem", "dep.res_focus",
                                         "anx.res_avoid.tasks", "anx.res_no.risks", "anx.res_detail.neg",
                                         "anx.res_detail.pos",
                                         "dep.teach_disconnect","dep.teach_energy",
                                         "dep.teach_understand",
                                         "anx.teach_understand", "anx.teach_unproductive", "anx.teach_detail.neg",
                                         "anx.teach_detail.pos"),
                                       depanx_fxn))

depanx_impact <- depanx_impact |> filter(demo == 1)
### aspects dep affect research and teaching regressions STUDENT ----

dep_aspects_mods <- do.call(rbind, lapply(c("dep.res_lack.motivation",
                                            "dep.res_low.selfesteem", 
                                            "dep.res_focus",
                                            "dep.teach_disconnect",
                                            "dep.teach_energy",
                                            "dep.teach_understand"),
                                          function(x){
                                            mod_out <- as.data.frame(summary(
                                              glm(as.formula(paste0(x,"~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                  data = my_data,
                                                  family = "binomial"))$coefficients)
                                            mod_out$aspect <- x
                                            mod_out$predictor <- rownames(mod_out)
                                            return(mod_out)
                                          }))
dep_aspects_mods <- dep_aspects_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(dep_aspects_mods) <- NULL
dep_aspects_mods$or <- exp(dep_aspects_mods$est)
dep_aspects_mods <- dep_aspects_mods |> dplyr::select(aspect, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
dep_aspects_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")

#research aspects
for(i in 1:length(predictors)){
  dep_aspects_mods[dep_aspects_mods$predictor == predictors[i] & str_detect(dep_aspects_mods$aspect, "res"),]$qval<-p.adjust(p = dep_aspects_mods[dep_aspects_mods$predictor == predictors[i] & str_detect(dep_aspects_mods$aspect, "res"),]$pval, method = "fdr")
}
#teaching aspects
for(i in 1:length(predictors)){
  dep_aspects_mods[dep_aspects_mods$predictor == predictors[i] & str_detect(dep_aspects_mods$aspect, "teach"),]$qval<-p.adjust(p = dep_aspects_mods[dep_aspects_mods$predictor == predictors[i] & str_detect(dep_aspects_mods$aspect, "teach"),]$pval, method = "fdr")
}

dep_aspects_mods |> filter(qval < .05 & predictor != "(Intercept)")


### aspects anx affect research and teaching regressions STUDENT ----

anx_aspects_mods <- do.call(rbind, lapply(c("anx.res_avoid.tasks",
                                            "anx.res_no.risks", 
                                            "anx.res_detail.neg",
                                            "anx.res_detail.pos",
                                            "anx.teach_understand",
                                            "anx.teach_unproductive",
                                            "anx.teach_detail.neg",
                                            "anx.teach_detail.pos"),
                                          function(x){
                                            mod_out <- as.data.frame(summary(
                                              glm(as.formula(paste0(x,"~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin")),
                                                  data = my_data,
                                                  family = "binomial"))$coefficients)
                                            mod_out$aspect <- x
                                            mod_out$predictor <- rownames(mod_out)
                                            return(mod_out)
                                          }))
anx_aspects_mods <- anx_aspects_mods |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(anx_aspects_mods) <- NULL
anx_aspects_mods$or <- exp(anx_aspects_mods$est)
anx_aspects_mods <- anx_aspects_mods |> dplyr::select(aspect, predictor, everything())


# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
anx_aspects_mods$qval <- NA
predictors <- c("gender_binwoman.nb","race2asian","race2black","race2latinx","race2other",
                "lgbtqyes", "fin.stab2not_always","time.grad_bin<3")

#research aspects
for(i in 1:length(predictors)){
  anx_aspects_mods[anx_aspects_mods$predictor == predictors[i] & str_detect(anx_aspects_mods$aspect, "res"),]$qval<-p.adjust(p = anx_aspects_mods[anx_aspects_mods$predictor == predictors[i] & str_detect(anx_aspects_mods$aspect, "res"),]$pval, method = "fdr")
}
#teaching aspects
for(i in 1:length(predictors)){
  anx_aspects_mods[anx_aspects_mods$predictor == predictors[i] & str_detect(anx_aspects_mods$aspect, "teach"),]$qval<-p.adjust(p = anx_aspects_mods[anx_aspects_mods$predictor == predictors[i] & str_detect(anx_aspects_mods$aspect, "teach"),]$pval, method = "fdr")
}
anx_aspects_mods |> filter(qval < .05 & predictor != "(Intercept)")

### mediation analyses for impact of MH on research and teaching -----

#list of demographics that sig predict severity of depression AND the impact of an aspect on MH
summary(glm(phq.score ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin, data = my_data)) #use bc of M
#dep sig: gender, race (black), lgbtq, fin.stab, time grad

#depression-related mediations
med2_dep <- do.call(rbind, lapply(c("dep.res_lack.motivation",
                                    "dep.res_low.selfesteem", 
                                    "dep.res_focus",
                                    "dep.teach_disconnect",
                                    "dep.teach_energy",
                                    "dep.teach_understand"),function(y) {
                                      do.call(rbind, lapply(c("gender_bin", "black", "lgbtq", "fin.stab2", "time.grad_bin"),
                                                            function(x){
                                                              df <- data.frame(my_data$ID)
                                                              df$M <- my_data[["phq.score"]]
                                                              df$X <- my_data[[x]] #demographic, predictor
                                                              df$Y <- my_data[[y]] #aspect, outcome
                                                              model.one <- glm(Y~X, data = df, family = "binomial")
                                                              mod.one <- as.data.frame(summary(model.one)$coefficients)[,-3]
                                                              mod.one <- mod.one |> rename(est = Estimate,
                                                                                           se = `Std. Error`, 
                                                                                           pval = `Pr(>|z|)`) |> as.data.frame()
                                                              mod.one$predictor <- rownames(mod.one)
                                                              rownames(mod.one) <- NULL
                                                              mod.one <- mod.one |> filter(str_detect(predictor, "X"))
                                                              mod.one$X <- x #"gender_bin"
                                                              mod.one$Y <- y #"failure.worsen.dep"
                                                              mod.one$M <- "phq.score"
                                                              mod.one$model <- "one"
                                                              model.two <- glm(Y~X+M, data = df, family = "binomial")
                                                              mod.two <- as.data.frame(summary(model.two)$coefficients)[,-3]
                                                              mod.two <- mod.two |> rename(est = Estimate,
                                                                                           se = `Std. Error`, 
                                                                                           pval = `Pr(>|z|)`) |> as.data.frame()
                                                              mod.two$predictor <- rownames(mod.two)
                                                              rownames(mod.two) <- NULL
                                                              mod.two <- mod.two |> filter(str_detect(predictor, "X")|
                                                                                             str_detect(predictor, "M"))
                                                              mod.two$X <- x #"gender_bin" 
                                                              mod.two$Y <- y #"failure.worsen.dep"
                                                              mod.two$M <- "phq.score"
                                                              mod.two$model <- "two"
                                                              model.three <- lm(M~X, data = df)
                                                              mod.three <- as.data.frame(summary(model.three)$coefficients)[,-3]
                                                              mod.three <- mod.three |> rename(est = Estimate,
                                                                                               se = `Std. Error`, 
                                                                                               pval = `Pr(>|t|)`) |> as.data.frame()
                                                              mod.three$predictor <- rownames(mod.three)
                                                              rownames(mod.three) <- NULL
                                                              mod.three <- mod.three |> filter(str_detect(predictor, "X"))
                                                              mod.three$X <- x #"gender_bin" 
                                                              mod.three$Y <- y #"failure.worsen.dep"
                                                              mod.three$M <- "phq.score"
                                                              mod.three$model <- "three"
                                                              full_mod <- rbind(mod.one, mod.two, mod.three)
                                                              full_mod$a <- mod.three$est[1]
                                                              full_mod$b <- mod.two$est[2]
                                                              full_mod$c <- mod.one$est[1]
                                                              full_mod$cp <- mod.two$est[1]
                                                              full_mod$ab <-full_mod$a*full_mod$b #indirect effect
                                                              full_mod$prop_med <- full_mod$ab/full_mod$c #proportion mediated effect
                                                              
                                                              return(full_mod)
                                                            }))}))

med2_dep$formula <- paste0(med2_dep$Y, "~", med2_dep$X, "+", med2_dep$M)

nsig_x <- med2_dep[(med2_dep$model == "one" & med2_dep$pval > .05) | 
                     (med2_dep$model == "three" & med2_dep$pval > .05),]

med2_dep <- med2_dep |>
  filter(!(formula %in% nsig_x$formula))

rm(nsig_x)


#list of demographics that sig predict severity of anxiety AND the impact of an aspect on MH
summary(glm(gad.score ~ gender_bin + race2 + lgbtq + fin.stab2 + time.grad_bin, data = my_data)) #use bc of M
#anx sig: gender, race (asian), lgbtq, fin.stab

med2_anx <- do.call(rbind, lapply(c("anx.res_avoid.tasks",
                                    "anx.res_no.risks", 
                                    "anx.res_detail.neg",
                                    "anx.res_detail.pos",
                                    "anx.teach_understand",
                                    "anx.teach_unproductive",
                                    "anx.teach_detail.neg",
                                    "anx.teach_detail.pos"),function(y) {
                                      do.call(rbind, lapply(c("gender_bin", "asian", "lgbtq", "fin.stab2"),
                                                            function(x){
                                                              df <- data.frame(my_data$ID)
                                                              df$M <- my_data[["gad.score"]]
                                                              df$X <- my_data[[x]] #demographic, predictor
                                                              df$Y <- my_data[[y]] #aspect, outcome
                                                              model.one <- glm(Y~X, data = df, family = "binomial")
                                                              mod.one <- as.data.frame(summary(model.one)$coefficients)[,-3]
                                                              mod.one <- mod.one |> rename(est = Estimate,
                                                                                           se = `Std. Error`, 
                                                                                           pval = `Pr(>|z|)`) |> as.data.frame()
                                                              mod.one$predictor <- rownames(mod.one)
                                                              rownames(mod.one) <- NULL
                                                              mod.one <- mod.one |> filter(str_detect(predictor, "X"))
                                                              mod.one$X <- x #"gender_bin"
                                                              mod.one$Y <- y #"failure.worsen.anx"
                                                              mod.one$M <- "gad.score"
                                                              mod.one$model <- "one"
                                                              model.two <- glm(Y~X+M, data = df, family = "binomial")
                                                              mod.two <- as.data.frame(summary(model.two)$coefficients)[,-3]
                                                              mod.two <- mod.two |> rename(est = Estimate,
                                                                                           se = `Std. Error`, 
                                                                                           pval = `Pr(>|z|)`) |> as.data.frame()
                                                              mod.two$predictor <- rownames(mod.two)
                                                              rownames(mod.two) <- NULL
                                                              mod.two <- mod.two |> filter(str_detect(predictor, "X")| str_detect(predictor, "M"))
                                                              mod.two$X <- x #"gender_bin" 
                                                              mod.two$Y <- y #"failure.worsen.anx"
                                                              mod.two$M <- "gad.score"
                                                              mod.two$model <- "two"
                                                              model.three <- lm(M~X, data = df)
                                                              mod.three <- as.data.frame(summary(model.three)$coefficients)[,-3]
                                                              mod.three <- mod.three |> rename(est = Estimate,
                                                                                               se = `Std. Error`, 
                                                                                               pval = `Pr(>|t|)`) |> as.data.frame()
                                                              mod.three$predictor <- rownames(mod.three)
                                                              rownames(mod.three) <- NULL
                                                              mod.three <- mod.three |> filter(str_detect(predictor, "X"))
                                                              mod.three$X <- x #"gender_bin" 
                                                              mod.three$Y <- y #"failure.worsen.anx"
                                                              mod.three$M <- "gad.score"
                                                              mod.three$model <- "three"
                                                              full_mod <- rbind(mod.one, mod.two, mod.three)
                                                              full_mod$a <- mod.three$est[1]
                                                              full_mod$b <- mod.two$est[2]
                                                              full_mod$c <- mod.one$est[1]
                                                              full_mod$cp <- mod.two$est[1]
                                                              full_mod$ab <-full_mod$a*full_mod$b #indirect effect
                                                              full_mod$prop_med <- full_mod$ab/full_mod$c #proportion mediated effect
                                                              
                                                              return(full_mod)
                                                            }))}))

med2_anx$formula <- paste0(med2_anx$Y, "~", med2_anx$X, "+", med2_anx$M)

nsig_x <- med2_anx[(med2_anx$model == "one" & med2_anx$pval > .05) | 
                     (med2_anx$model == "three" & med2_anx$pval > .05),]

med2_anx <- med2_anx |>
  filter(!(formula %in% nsig_x$formula))

rm(nsig_x)



### aspects dep affect research and teaching regressions PROGRAM ----

dep_aspects_mods_prog <- do.call(rbind, lapply(c("dep.res_lack.motivation",
                                                 "dep.res_low.selfesteem", 
                                                 "dep.res_focus",
                                                 "dep.teach_disconnect",
                                                 "dep.teach_energy",
                                                 "dep.teach_understand"),
                                               function(x){
                                                 mod_out <- as.data.frame(summary(
                                                   glm(as.formula(paste0(x,"~ degree.type + field.of.study2")),
                                                       data = my_data,
                                                       family = "binomial"))$coefficients)
                                                 mod_out$aspect <- x
                                                 mod_out$predictor <- rownames(mod_out)
                                                 return(mod_out)
                                               }))
dep_aspects_mods_prog <- dep_aspects_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(dep_aspects_mods_prog) <- NULL
dep_aspects_mods_prog$or <- exp(dep_aspects_mods_prog$est)
dep_aspects_mods_prog <- dep_aspects_mods_prog |> dplyr::select(aspect, predictor, everything())


# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
dep_aspects_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")

#research aspects
for(i in 1:length(predictors)){
  dep_aspects_mods_prog[dep_aspects_mods_prog$predictor == predictors[i] & str_detect(dep_aspects_mods_prog$aspect, "res"),]$qval<-p.adjust(p = dep_aspects_mods_prog[dep_aspects_mods_prog$predictor == predictors[i] & str_detect(dep_aspects_mods_prog$aspect, "res"),]$pval, method = "fdr")
}
#teaching aspects
for(i in 1:length(predictors)){
  dep_aspects_mods_prog[dep_aspects_mods_prog$predictor == predictors[i] & str_detect(dep_aspects_mods_prog$aspect, "teach"),]$qval<-p.adjust(p = dep_aspects_mods_prog[dep_aspects_mods_prog$predictor == predictors[i] & str_detect(dep_aspects_mods_prog$aspect, "teach"),]$pval, method = "fdr")
}

dep_aspects_mods_prog |> filter(qval < .05 & predictor != "(Intercept)")


### aspects anx affect research and teaching regressions PROGRAM ----

anx_aspects_mods_prog <- do.call(rbind, lapply(c("anx.res_avoid.tasks",
                                                 "anx.res_no.risks", 
                                                 "anx.res_detail.neg",
                                                 "anx.res_detail.pos",
                                                 "anx.teach_understand",
                                                 "anx.teach_unproductive",
                                                 "anx.teach_detail.neg",
                                                 "anx.teach_detail.pos"),
                                               function(x){
                                                 mod_out <- as.data.frame(summary(
                                                   glm(as.formula(paste0(x,"~ degree.type + field.of.study2")),
                                                       data = my_data,
                                                       family = "binomial"))$coefficients)
                                                 mod_out$aspect <- x
                                                 mod_out$predictor <- rownames(mod_out)
                                                 return(mod_out)
                                               }))
anx_aspects_mods_prog <- anx_aspects_mods_prog |>
  rename(est = Estimate, se = `Std. Error`, zval = `z value`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(anx_aspects_mods_prog) <- NULL
anx_aspects_mods_prog$or <- exp(anx_aspects_mods_prog$est)
anx_aspects_mods_prog <- anx_aspects_mods_prog |> dplyr::select(aspect, predictor, everything())

# add  adjusted pvalues to the regression output df; create new column for qvals, add them in for predictor
anx_aspects_mods_prog$qval <- NA
predictors <- c("degree.typethesis.masters","field.of.study2astro","field.of.study2chem","field.of.study2geosciences",
                "field.of.study2other_stem","field.of.study2physics")

#research aspects
for(i in 1:length(predictors)){
  anx_aspects_mods_prog[anx_aspects_mods_prog$predictor == predictors[i] & str_detect(anx_aspects_mods_prog$aspect, "res"),]$qval<-p.adjust(p = anx_aspects_mods_prog[anx_aspects_mods_prog$predictor == predictors[i] & str_detect(anx_aspects_mods_prog$aspect, "res"),]$pval, method = "fdr")
}
#teaching aspects
for(i in 1:length(predictors)){
  anx_aspects_mods_prog[anx_aspects_mods_prog$predictor == predictors[i] & str_detect(anx_aspects_mods_prog$aspect, "teach"),]$qval<-p.adjust(p = anx_aspects_mods_prog[anx_aspects_mods_prog$predictor == predictors[i] & str_detect(anx_aspects_mods_prog$aspect, "teach"),]$pval, method = "fdr")
}
anx_aspects_mods_prog |> filter(qval < .05 & predictor != "(Intercept)")





### grad experiences (y/n) ~ severity depression/anxiety ----
table(my_data$changed.PIs)
my_data$changed.PIs2 <- NA
my_data[my_data$changed.PIs == "no" & !is.na(my_data$changed.PIs),]$changed.PIs2 <- 0
my_data[my_data$changed.PIs == "yes" & !is.na(my_data$changed.PIs),]$changed.PIs2 <- 1
table(my_data$changed.PIs2)

table(my_data$pi.change.inc.time)

summary(glm(changed.PIs2 ~ anx.sev2, data = my_data, family = "binomial"))
summary(glm(considered.leaving2 ~ anx.sev2, data = my_data, family = "binomial"))

summary(glm(changed.PIs2 ~ dep.sev2, data = my_data, family = "binomial"))
summary(glm(considered.leaving2 ~ dep.sev2, data = my_data, family = "binomial"))

####FIGURES####
### FIGURE 1 #####
{
  ### circle plot research severity negative impact vs frequency ----
  
  neg_res_fig1 <- exacerbate_aspects |>
    mutate(dep_pct = dep_total/sum(my_data$grad_depress)) |> #total participants with depression
    mutate(anx_pct = anx_total/sum(my_data$grad_anx)) |> #total participants with anxiety
    ggplot(aes(x = avg_freq, label = aspect)) +
    geom_point(aes(y = dep_avg, size = dep_pct), color = "dodgerblue4") +
    geom_point(aes(y = anx_avg, size = anx_pct), color = "forestgreen", alpha = 0.8) +
    #  geom_label_repel(aes(y = dep_avg), force = 2) +
    #  geom_label_repel(aes(y = anx_avg), force = 2) +
    scale_y_continuous(limits = c(0, 4.1), breaks = seq(0,4), labels=c("Not at all\n(0)"," Slightly\n(1)",
                                                                       "Somewhat\n(2)", "Very\n(3)", "Extremely\n(4)"),
                       expand = c(0,0)) +
    scale_size_continuous(limits = c(.5,.98)) +
    labs (y = "Severity of impact", x = "Average annual frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 10),
          axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
          legend.position = "none")
  
  neg_res_fig1 
  

  ### circle plot research severity positive impact vs frequency ----
  
  bet_res_fig1 <- better_aspects |>
    mutate(dep_pct = dep_total/sum(my_data$grad_depress)) |> #total participants with depression
    mutate(anx_pct = anx_total/sum(my_data$grad_anx)) |> #total participants with anxiety
    ggplot(aes(x = avg_freq, label = aspect)) +
    geom_point(aes(y = dep_avg, size = dep_pct), color = "dodgerblue4") +
    geom_point(aes(y = anx_avg, size = anx_pct), color = "forestgreen", alpha = 0.8) +
    #  geom_label_repel(aes(y = dep_avg), force = 2) +
    #  geom_label_repel(aes(y = anx_avg), force = 2) +
    scale_y_continuous(limits = c(0, 4.1), breaks = seq(0,4), labels=c("Not at all\n(0)"," Slightly\n(1)",
                                                                       "Somewhat\n(2)", "Very\n(3)", "Extremely\n(4)")) +
    scale_size_continuous(limits = c(.5,.98)) +
    labs (y = "Severity of impact", x = "Average annual frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 10),
          axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
          legend.position = "none")
  
  bet_res_fig1
  

  ### circle plot teaching severity negative impact vs frequency ----
  
  neg_tea_fig1 <- exacerbate_teach_aspects |>
    mutate(dep_pct = dep_total/sum(my_data$grad_depress)) |> #total participants with depression
    mutate(anx_pct = anx_total/sum(my_data$grad_anx)) |> #total participants with anxiety
    ggplot(aes(x = avg_freq, label = aspect)) +
    geom_point(aes(y = dep_avg, size = dep_pct), color = "dodgerblue4") +
    geom_point(aes(y = anx_avg, size = anx_pct), color = "forestgreen", alpha = 0.8) +
    #  geom_label_repel(aes(y = dep_avg), force = 2) +
    #  geom_label_repel(aes(y = anx_avg), force = 2) +
    scale_y_continuous(limits = c(0, 4.1), breaks = seq(0,4), labels=c("Not at all\n(0)"," Slightly\n(1)",
                                                                       "Somewhat\n(2)", "Very\n(3)", "Extremely\n(4)"),
                       expand = c(0,0)) +
    scale_size_continuous(limits = c(.5,.98)) +
    labs (y = "Severity of impact", x = "Average annual frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 10),
          axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
          legend.position = "none")
  
  neg_tea_fig1 
  

  ### circle plot teaching severity positive impact vs frequency ----
  
  bet_tea_fig1 <- better_teach_aspects |>
    mutate(dep_pct = dep_total/sum(my_data$grad_depress)) |> #total participants with depression
    mutate(anx_pct = anx_total/sum(my_data$grad_anx)) |> #total participants with anxiety
    ggplot(aes(x = avg_freq, label = aspect)) +
    geom_point(aes(y = dep_avg, size = dep_pct), color = "dodgerblue4") +
    geom_point(aes(y = anx_avg, size = anx_pct), color = "forestgreen", alpha = 0.8) +
    #  geom_label_repel(aes(y = dep_avg), force = 2) +
    #  geom_label_repel(aes(y = anx_avg), force = 2) +
    scale_y_continuous(limits = c(0, 4.1), breaks = seq(0,4), labels=c("Not at all\n(0)"," Slightly\n(1)",
                                                                       "Somewhat\n(2)", "Very\n(3)", "Extremely\n(4)")) +
    scale_size_continuous(limits = c(.5,.98)) +
    labs (y = "Severity of impact", x = "Average annual frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
          axis.text.y = element_text(family="Helvetica", color = "black", size = 10),
          axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
          legend.position = "none")
  
  bet_tea_fig1
  
}



### FIGURE 3 bar graph impact of dep and anx on research/teaching ----

fig3b <- depanx_impact |>
  filter(str_detect(name, "dep.res")) |>
  ggplot() +
  geom_col(aes(x = perc/100, y = reorder(name, perc)), fill = "dodgerblue4") +
  scale_x_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = .1),
                     limits = c(0, .85)) +
  scale_y_discrete(labels = c("Low self-esteem", "Lack focus", 
                              "Lack motivation")) +
  labs(x = "Percent", y = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))

fig3b


fig3a <- depanx_impact |>
  filter(str_detect(name, "anx.res")) |>
  ggplot() +
  geom_col(aes(x = perc/100, y = reorder(name, perc)), fill = "forestgreen") +
  scale_x_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = .1),
                     limits = c(0, .85)) +
  scale_y_discrete(labels = c("Avoid risk-taking", "Attention to detail:\ntoo time-consuming\n(negative)", 
                              "Attention to detail:\nhigher quality products\n(positive)", "Avoid tasks")) +
  labs(x = "Percent", y = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))

fig3a


fig3d <- depanx_impact |>
  filter(str_detect(name, "dep.teach")) |>
  ggplot() +
  geom_col(aes(x = perc/100, y = reorder(name, perc)), fill = "dodgerblue4") +
  scale_x_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = .1),
                     limits = c(0, .55)) +
  scale_y_discrete(labels = c("Disconnected from students", "Better understanding\nof students", 
                              "Lack of energy")) +
  labs(x = "Percent", y = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))

fig3d


fig3c <- depanx_impact |>
  filter(str_detect(name, "anx.teach")) |>
  ggplot() +
  geom_col(aes(x = perc/100, y = reorder(name, perc)), fill = "forestgreen") +
  scale_x_continuous(expand = c(0,0), labels = scales::label_percent(accuracy = .1),
                     limits = c(0, .65)) +
  scale_y_discrete(labels = c("Attention to detail:\ntoo time-consuming\n(negative)", "Unproductive", 
                              "Attention to detail:\nhigher quality products\n(positive)", 
                              "Better understanding\nof students")) +
  labs(x = "Percent", y = "") +
  theme_classic() +
  theme(axis.text = element_text(family="Helvetica", color = "black", size = 8),
        axis.title = element_text(family="Helvetica", color = "black", size = 8, face = "bold"))

fig3c


