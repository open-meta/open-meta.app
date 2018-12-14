# This file builds a list holding FORM definitions and calculation functions for the Effect Size
#    calculators in the Open-Meta.app. This is all pulled into this file and created as a list so
#    that new or redefined calculators can be installed in the app without restaring it.
# v0.1 - TomW - Nov 2018

library(esc)
library(dplyr)

source("app/imGetBlankFORMrow.R", local=TRUE)  # load the shared imGetBlankFORMrow function

#
calcRow <- function(type) {
   z <- imGetBlankFORMrow(type)
   z$formname <- "makeAcalc"
   z$table <- "extract"
   z$column <- "1,1,0,0"
   return(z)
}
# Form Constants
### top
   r1 <- calcRow("select")
   r1$id <- "OutcomePICO"
   r1$label <- "Outcome:"
   r1$helptext <- ""
   r1$width <- "12"
   r1$value <- ""
   r1$options <- ""
   r1$sameline <- FALSE
   r1$disabled <- FALSE

   r2 <- calcRow("select")
   r2$id <- "Ois"
   r2$label <- "Outcome is:"
   r2$helptext <- ""
   r2$width <- "12"
   r2$value <- ""
   r2$options <- ""
   r2$sameline <- FALSE
   r2$disabled <- FALSE

   r3 <- calcRow("select")
   r3$id <- "Cis"
   r3$label <- "Data Format:"
   r3$helptext <- ""
   r3$width <- "12"
   r3$value <- ""
   r3$options <- ""
   r3$sameline <- TRUE
   r3$disabled <- FALSE

   top <- bind_rows(r1,r2,r3)

### time span
   ts1 <- calcRow("spacer")
   ts1$width <- "12"
   r2 <- calcRow("text")
   r2$width <- "21"
   r2$sameline <- TRUE
   r2$disabled <- TRUE
   r3 <- calcRow("spacer")
   r3$width = "1"
   r3$sameline = TRUE
   ts <- bind_rows(r2,r3)

### group (Control Group text-disabled)
   group <- calcRow("text")
   group$width <- "12"
   group$label <- "Group:"
   group$value <- "Control Group"
   group$disabled <- TRUE

### B.SE  (B means both C and I)
   r1 <- calcRow("number")
   r1$width = "7"
   r1$name = "n"
   r1$label = "n"
   r1$helptext = "Group size."
   r1$sameline = TRUE
   r2 <- calcRow("number")
   r2$width = "7"
   r2$name = "m"
   r2$label = "mean"
   r2$helptext = "Group mean."
   r2$sameline = TRUE
   r3 <- calcRow("number")
   r3$width = "7"
   r3$name = "se"
   r3$label = "SE"
   r3$helptext = "Group SE."
   r3$sameline = TRUE
   r4 <- calcRow("spacer")
   r4$width = "1"
   r4$sameline = TRUE
   B.SE <- bind_rows(r1, r2, r3, r4)

### B.SD
   B.SD <- B.SE
   B.SD$name[3] = "sd"
   B.SD$label[3] = "SD"
   B.SD$helptext[3] = "Group SD."

### C&I.SD1
   C.SD1 <- B.SD
   C.SD1$type[3] = "spacer"
   C.SD1$name[3] = ""
   C.SD1$label[3] = ""
   C.SD1$helptext[3] = ""
   I.SD1 <- B.SD
   I.SD1$helptext[3] = "Overall SD."

### C row with only n
   r1 <- calcRow("number")
   r1$width = "7"
   r1$name = "n"
   r1$label = "n"
   r1$helptext = "Group size."
   r1$sameline = TRUE
   r2 <- calcRow("spacer")
   r2$width = "14"
   r2$sameline = TRUE
   r3 <- calcRow("spacer")
   r3$width = "1"
   r3$sameline = TRUE
   C.n.only <- bind_rows(r1,r2,r3)

### I row for t-Test-t
   r1 <- calcRow("number")
   r1$width = "7"
   r1$name = "n"
   r1$label = "n"
   r1$helptext = "Group size."
   r1$sameline = TRUE
   r2 <- calcRow("number")
   r2$width = "14"
   r2$name = "tt"
   r2$label = "t-Test t-Value"
   r2$helptext = "Enter the t-Value from the t-Test comparing this intervention group to its control group."
   r2$sameline = TRUE
   r3 <- calcRow("spacer")
   r3$width = "1"
   r3$sameline = TRUE
   I.TT <- bind_rows(r1, r2, r3)

### I row for t-Test-P
   I.TP <- I.TT
   I.TP$name[2] = "tp"
   I.TP$label[2] = "t-Test p-Value"
   I.TP$helptext[2]  = "Enter the p-Value from the t-Test comparing this intervention group to its control group."

### I row for One-way Anova
   I.A <- I.TT
   I.A$name[2] = "F"
   I.A$label[2] = "F"
   I.A$helptext[2]  = "Enter the F-Value from a one-way, two-group ANOVA comparing this intervention group to its control group."

### I row for Point-Biserial correlation
   PBR <- I.TT
   PBR$name[2] = "pbr"
   PBR$label[2] = "r"
   PBR$helptext[2]  = "Point-biserial correlation between a dichotomous variable indicating the group and a continuous outcome."

### C row for counts
   r1 <- calcRow("number")
   r1$width = "10.5"
   r1$name = "nw"
   r1$label = "Number worse"
   r1$helptext = "Count of control group members with worse outcomes."
   r1$sameline = TRUE
   r2 <- calcRow("number")
   r2$width = "10.5"
   r2$name = "nb"
   r2$label = "Number better"
   r2$helptext = "Count of control group members with better outcomes."
   r2$sameline = TRUE
   r3 <- calcRow("spacer")
   r3$width = "1"
   r3$sameline = TRUE
   C.C <- bind_rows(r1,r2,r3)

### I row for counts
   I.C <- C.C
   I.C$helptext[1] = "Count of members of this intervention group with worse outcomes."
   I.C$helptext[2] = "Count of members of this intervention group with better outcomes."

### C row for proportions
   r1 <- calcRow("number")
   r1$width = "7"
   r1$name = "n"
   r1$label = "n"
   r1$helptext = "Group size."
   r1$sameline = TRUE
   r2 <- calcRow("number")
   r2$width = "14"
   r2$name = "s"
   r2$label = "% Success"
   r2$helptext = "Enter the percentage of the control group for which the outcome was better."
   r2$sameline = TRUE
   r3 <- calcRow("spacer")
   r3$width = "1"
   r3$sameline = TRUE
   C.P <- bind_rows(r1, r2, r3)

### I row for proportions
   I.P <- C.P
   I.P$name[2] = "s"
   I.P$helptext[2] = "Enter the percentage of this intervention group for which the outcome was better."

### I row for Cohen's d
   I.d <- I.TT
   I.d$name[2] = "d"
   I.d$label[2] = "Cohen's d"
   I.d$helptext[2]  = "d for size of effect between control group and this intervention group."

### I row for Hedge's g
   I.g <- I.TT
   I.g$name[2] = "g"
   I.g$label[2] = "Hedge's g"
   I.g$helptext[2]  = "g for size of effect between control group and this intervention group."

### I row for Odds Ratio
   I.or <- I.TT
   I.or$name[2] = "or"
   I.or$label[2] = "OR"
   I.or$helptext[2]  = "Odds ratio for size of effect between control group and this intervention group."

### I row for Log Odds Ratio
   I.lor <- I.TT
   I.lor$name[2] = "lor"
   I.lor$label[2] = "log OR"
   I.lor$helptext[2]  = "Log odds ratio for size of effect between control group and this intervention group."

### I row for Relative Risk
   I.rr <- I.TT
   I.rr$name[2] = "rr"
   I.rr$label[2] = "RR"
   I.rr$helptext[2]  = "Relative risk for size of effect between control group and this intervention group."

### I row for Log Relative Risk
   I.lrr <- I.TT
   I.lrr$name[2] = "lrr"
   I.lrr$label[2] = "log RR"
   I.lrr$helptext[2]  = "Log relative risk for size of effect between control group and this intervention group."

### I row for Pearson's r
   I.r <- I.TT
   I.r$name[2] = "r"
   I.r$label[2] = "r"
   I.r$helptext[2]  = "Pearson's r for size of effect between control group and this intervention group."

### I row for Cohen's f
   I.f <- I.TT
   I.f$name[2] = "f"
   I.f$label[2] = "f"
   I.f$helptext[2]  = "Cohen's f for size of effect between control group and this intervention group."

### I row for eta-squared
   I.eta2 <- I.TT
   I.eta2$name[2] = "eta2"
   I.eta2$label[2] = "eta^2"
   I.eta2$helptext[2]  = "eta-squared for size of effect between control group and this intervention group."


CC <- list()
CC$C = list(
      "Continuous" = list(
         "Means & SEs" = list(
            "cRow" = B.SE,
            "iRow" = B.SE,
            "params" = tibble(TS=rep("",6), T=c("C","C","C","I","I","I"), P=c("n","m","se","n","m","se"), V=0),
            "calc" = function(t) { return(esc_mean_se(grp1m=t$V[5], grp1se=t$V[6], grp1n=t$V[4],
                                                      grp2m=t$V[2], grp2se=t$V[3], grp2n=t$V[1], es.type="d")) }
         ),
         "Means & SDs" = list(
            "cRow" = B.SD,
            "iRow" = B.SD,
            "params" = tibble(TS=rep("",6), T=c("C","C","C","I","I","I"), P=c("n","m","sd","n","m","sd"), V=0),
            "calc" = function(t) { return(esc_mean_sd(grp1m=t$V[5], grp1sd=t$V[6], grp1n=t$V[4],
                                                      grp2m=t$V[2], grp2sd=t$V[3], grp2n=t$V[1], es.type="d")) }
         ),
         "Means & Overall SD" = list(
            "cRow" = C.SD1,
            "iRow" = I.SD1,
            "params" = tibble(TS=rep("",5), T=c("C","C","I","I","I"), P=c("n","m","n","m","sd"), V=0),
            "calc" = function(t) { return(esc_mean_sd(grp1m=t$V[4], totalsd=t$V[5], grp1n=t$V[3],
                                                      grp2m=t$V[2], grp2n=t$V[1], es.type="d")) }
         ),
         "t-Test t-Value" = list(
            "cRow" = C.n.only,
            "iRow" = I.TT,
            "params" = tibble(TS=rep("",3), T=c("C","I","I"), P=c("n","n","t"), V=0),
            "calc" = function(t) { return(esc_t(t=t$V[3], grp1n=t$V[2], grp2n=t$V[1], es.type="d")) }
         ),
         "t-Test p-Value" = list(
            "cRow" = C.n.only,
            "iRow" = I.TP,
            "params" = tibble(TS=rep("",3), T=c("C","I","I"), P=c("n","n","p"), V=0),
            "calc" = function(t) { return(esc_t(p=t$V[3], grp1n=t$V[2], grp2n=t$V[1], es.type="d")) }
         ),
         "One-way ANOVA" = list(
            "cRow" = C.n.only,
            "iRow" = I.A,
            "params" = tibble(TS=rep("",3), T=c("C","I","I"), P=c("n","n","F"), V=0),
            "calc" = function(t) { return(esc_f(f=t$V[3], grp1n=t$V[2], grp2n=t$V[1], es.type="d")) }
         ),
         "Point-Biserial r" = list(
            "cRow" = C.n.only,
            "iRow" = PBR,
            "params" = tibble(TS=rep("",3), T=c("C","I","I"), P=c("n","n","r"), V=0),
            "calc" = function(t) { return(esc_rpb(r=t$V[3], grp1n=t$V[2], grp2n=t$V[1], es.type="d")) }
         )
      ),
      "Dichotomous" = list(
         "Counts" = list(
            "cRow" = C.C,
            "iRow" = I.C,
            "params" = tibble(TS=rep("",4), T=c("C","C","I","I"), P=c("nb","nw","nb","nw"), V=0),
            "calc" = function(t) { return(esc_2x2(grp1yes=t$V[3], grp1no=t$V[4], grp2yes=t$V[1], grp2no=t$V[2], es.type="d")) }
         ),
         "Proportions" = list(
            "cRow" = C.P,
            "iRow" = I.P,
            "params" = tibble(TS=rep("",4), T=c("C","C","I","I"), P=c("n","s","n", "s"), V=0),
            "calc" = function(t) { return(esc_bin_prop(prop1event=t$V[4], grp1n=t$V[3], prop2event=t$V[2], grp2n=t$V[1], es.type="d")) }
         )
      ),
      "An Effect Size" = list(
         "Cohen's d" = list(
            "cRow" = C.n.only,
            "iRow" = I.d,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","d"), V=0),
            "calc" = "d"
         ),
         "Hedge's g" = list(
            "cRow" = C.n.only,
            "iRow" = I.g,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","g"), V=0),
            "calc" = "g"
         ),
         "Odds Ratio" = list(
            "cRow" = C.n.only,
            "iRow" = I.or,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","or"), V=0),
            "calc" = "or"
         ),
         "Log Odds Ratio" = list(
            "cRow" = C.n.only,
            "iRow" = I.lor,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","lor"), V=0),
            "calc" = "lor"
         ),
         "Relative Risk" = list(
            "cRow" = C.n.only,
            "iRow" = I.rr,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","rr"), V=0),
            "calc" = "rr"
         ),
         "Log Relative Risk" = list(
            "cRow" = C.n.only,
            "iRow" = I.lrr,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","lrr"), V=0),
            "calc" = "lrr"
         ),
         "Pearson's r" = list(
            "cRow" = C.n.only,
            "iRow" = I.r,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","r"), V=0),
            "calc" = "r"
         ),
         "Cohen's f" = list(
            "cRow" = C.n.only,
            "iRow" = I.f,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","f"), V=0),
            "calc" = "f"
         ),
         "eta-squared" = list(
            "cRow" = C.n.only,
            "iRow" = I.eta2,
            "params" = tibble(TS=rep("",3), T=c("C", "I", "I"), P=c("n","n","eta2"), V=0),
            "calc" = "eta2"
         )
      )
   )
   CC$top = top
   CC$ts1 = ts1
   CC$ts2 = ts
   CC$group = group

saveRDS(CC, file="app/Calculators.RDS")
##################################################################################


rm(B.SD, B.SE, C.n.only, C.C, I.C, C.P, I.P, C.SD1, group, I.d, I.eta2,
   I.f, I.g, I.lor, I.lrr, I.or, I.rr, I.r, I.SD1, I.A, PBR, r1, r2, r3,
   r4, I.TP, I.TT, top, ts, ts1, calcRow, imGetBlankFORMrow)

rm(CC)




