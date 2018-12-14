### open-meta.app esR.R
### Tom Weishaar - v0.1 - Dec 2018


# Calculate effect using both Baseline and Control group data.
# From 2008-Morris-Estimating Effect Sizes From Pretest-Posttest-Control Group Designs
#    where this is called "dppc2". Function is vectorized.
dppc2 <- function(n.0.1, m.0.1, sd.0.1,  # C.Pre
                  n.1.1, m.1.1, sd.1.1,  # I.Pre
                  n.0.2, m.0.2, sd.0.2,  # C.Post
                  n.1.2, m.1.2, sd.1.2)  # I.Post
{
   N   <- n.0.1 + n.1.1                        # N is total at baseline
   m   <- (m.1.2 - m.1.1) - (m.0.2 - m.0.1)    # (Intervention @ TS2 - baseline) - (Control @ TS2 - baseline)
   sd  <- sqrt((((n.0.1 - 1)*sd.0.1^2) + ((n.1.1 - 1)*sd.1.1^2)) / (N - 2)) # Pooling total squared error @ baseline
   ssa <- 1 - (3 / (4*(N) - 9))                # Small sample adjustment
   es  <- m/sd*ssa
   v   <- ((N)/(n.0.1*n.1.1)) + (es^2/(2*(N-2)))
   info <- paste0("dppc2")
   se  <- sqrt(v)
   return(list(es = es,
               var = v,
               se = se,
               ci.lo = es - stats::qnorm(.975) * se,
               ci.hi = es + stats::qnorm(.975) * se,
               w = 1/v,
               measure = "d",
               info = info))
}

# TEST - Answer is .7684649; function is vectorized
# dppc2(20, 23.1, 13.8,  # C.Pre
#       20, 30.6, 15.0,  # I.Pre
#       c(20,20), c(19.7,19.7), c(14.8,14.8),  # C.Post
#       c(20,20), c(38.5,38.5), c(11.6,11.6))  # I.Post


# This is our effect size converter. It's in a separate file to make it easier to update
#    without disruputing the app. It was originally part of the CC$ list, but that created
#    problems with R's lexical scoping.

# The idea here is to be able to submit any effect size to the function and get back any other
#   effect size. In general, the function first converts the incoming effect size to Cohen's
#   d. The variance of that Cohen's d is calculated using the number of subjects passed to the
#   function. Then that d and v are used to calculate the requested effect size and its variance.

# In general, the Open-Meta app wants to allow users to enter data in any effect size metric,
#   however, it wants to always store the user's data as Cohen's d. Yet at analysis, the user
#   should be able to request the analysis in any effect size metric, which will be obtained
#   by converting from the stored Cohen's d.

# These functions are called by functions in the CC$C[[es_category]][[es_specific]][["calc"]] list,
#   which also has the information needed to set up each web form for collecting data.

# NOTE: Search for 999 to find unfinished calculators.

esR <- function(n0, n1, es, es.in, es.out) {
   if(length(es)>1 || length(n0)>1 || length(n1)>1) { stop("The esR function is not vectorized.")}
   v=as.numeric(NA)
   switch(es.in,
         # No change:
      "d"    = { d <- es },
         #
      "g"    = { d <- 999 },
         #
      "or"   = { d <- log(es) / (pi / sqrt(3)) },
         # Bornstein-Intro to Meta-Analysis, formula:
      "lor"  = { d <- es * sqrt(3) / pi },           # 7.1
         #
      "rr"   = { d <- 999 },
         #
      "lrr"  = { d <- 999 },
         # Bornstein-Intro to Meta-Analysis, formula:
      "r"    = { d <- (2 * es) / sqrt(1 - (es^2)) }, # 7.5
         #
      "f"    = { d <- 2 * es },
         #
      "eta2" = { d <- 2 * (sqrt(es / (1 - es))) },
      stop(paste0("In esR, ", es.in, " isn't yet a supported effect size metric."))
   )
   if(is.na(v)) {
      v <- ((n1+n0)/(n1*n0)) + (d*d/(2*(n1+n0-2)))
   }
   switch(es.out,
      "d" = { #

         es <- d
         v <- v
      },
      "g" = { #
         es <- 999
         v <- 999
      },
      "or" = { #
         es <- exp(d * pi / sqrt(3))
         v <- v * (pi  ^ 2) / 3
      },
      "lor" = { # Bornstein-Intro to Meta-Analysis, formula:
         es <- d * pi / sqrt(3)                              # 7.3
         v <- v * (pi  ^ 2) / 3                              # 7.4
      },
      "cox-lor" = { #
         es <- d * 1.65
         v <- v / .367
      },
      "rr" = { #
         es <- 999
         v <- 999
      },
      "lrr" = { #
         es <- 999
         v <- 999
      },
      "r" = { # Bornstein-Intro to Meta-Analysis, formula:
         a <- ((n0+n1)^2) / (n0*n1)                          # 7.8
         es <- d / sqrt(d^2 + a)                             # 7.7
         v <- a^2*v / (d^2+a)^3                              # 7.9
      },
      "f" = { #
         es <- d / 2
         v <- 999
      },
      "eta2" = { #
         es <- (d^2 / (1 + d^2)) / 2
         v <- 999
      },
      stop(paste0("In esR, ", es.out, " isn't yet a supported effect size metric."))
   )
   info <- paste0(es.in, " converted to ", es.out)
   se <- sqrt(v)
   return(list(es = es,
               var = v,
               se = se,
               ci.lo = es - stats::qnorm(.975) * se,
               ci.hi = es + stats::qnorm(.975) * se,
               w = 1/v,
               measure = es.out,
               info = info))
}



#dVar from https://trendingsideways.com/the-cohens-d-formula; doesn't exactly match Wilson calculator, but close.
# also https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic has matching formula below
# dVar <- function(n1, n2, d) {
# #   return((((n1+n2)/(n1*n2)) + (d*d/(2*(n1+n2-2)))) * ((n1+n2)/(n1+n2-2)))
#    return((((n1+n2)/(n1*n2)) + (d*d/(2*(n1+n2-2))))) # dropping the multiplication matchs the Wilson calculator
# }

# from: https://stats.stackexchange.com/questions/130237/convert-hazards-ratio-to-odds-ratio
# Exploiting the assumption that hazard ratios are asymptotically similar to relative risks, you can use exploit the formula recommendeed by Grant et al, BMJ 2014:
#    RR = OR / (1 - p + (p * OR)
# where RR is the relative risk, OR is the odds ratio, and p is the control event rate, which leads to the following:
#    OR = ((1 - p) * RR) / (1 - RR * p)
# Thus, for instance, a RR of 2.0 with a p of 0.1 would lead to an OR of 2.25, whereas if p increases to 0.2 it would lead to an OR of 2.67.




