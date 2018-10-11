### open-meta.app sql-init-project.R
### Tom Weishaar

# This file has the code needed to initialize a project

# Need to set dbLink and projectID before running this.
#    Called by sql-initialization to init the sample project
#    Called by prjNew for all other projects

initProject = function(projectID, projectName, pool) {
   dbLink <- poolCheckout(pool)                                    # get a dbLink from the pool
   on.exit(poolReturn(dbLink), add = TRUE)                         # return it when done, even if there's an error
   # distinguish between run at db initialization and run for new project
   newInstall = FALSE
   if(!exists("S")) {                # If the S$ list doesn't exist, this is a new installation, not a new project...
      newInstall = TRUE
      S = list()                     #   ...so we need to make an S$ for inside this function only
      S$U <- userGet(c("userID","userName", "email", "sPowers", "emailOK"), tibble(c("userID", "=", 1)), pool=pool) # Use Admin user
      S$db = paste0("om$prj_", projectID)
   }

   r = dbExecute(dbLink, paste0("CREATE DATABASE `", S$db, "`;"))  # Create the project db
   r = dbExecute(dbLink, createTable(S$db, "settings")) # Create its tables
   r = dbExecute(dbLink, createTable(S$db, "protocol"))
   r = dbExecute(dbLink, createTable(S$db, "comment"))
   r = dbExecute(dbLink, createTable(S$db, "search"))
   r = dbExecute(dbLink, createTable(S$db, "catalog"))
   r = dbExecute(dbLink, createTable(S$db, "review"))
   r = dbExecute(dbLink, createTable(S$db, "pico"))
   r = dbExecute(dbLink, createTable(S$db, "extract"))
   r = dbExecute(dbLink, createTable(S$db, "ids"))
   r = dbExecute(dbLink, createTable(S$db, "trial"))

### Membership table is done by caller on both new installs and new projects

### Complete settings table
   r = newRec("settings")
   r$name[2] = "failBoxNames"
   r$value[2] = toJSON(c("No Fulltext", "Study DNMPC", "Participants DNMPC", "Intervention DNMPC",
                         "Comparison DNMPC", "Outcomes DNMPC"))
   r = recSaveR(r, db=S$db, pool=pool)

   r = newRec("settings")
   r$name[2] = "forceFail"
   r$value[2] = "T"
   r = recSaveR(r, db=S$db, pool=pool)

   r = newRec("settings")
   r$name[2] = "forcePass"
   r$value[2] = "T"
   r = recSaveR(r, db=S$db, pool=pool)

   # Move forms from om$prime.settings to S$db.settings. Also update project's `ids` table
   formIDs = recGet("om$prime", "settings", c("settingsID","name"), WHERE=tibble(c("name", "LIKE", "PrjForm-%")), pool=pool)
      for(i in 1:nrow(formIDs)) {
         # first copy a form to project's settings table
      r = recGet("om$prime", "settings", "**", WHERE=tibble(c("settingsID", "=", formIDs$settingsID[i])), pool=pool)
      r$settingsID <- r$verNum <- 0                  # change both vectors (it's an insert not an update)
      r$comment[1] <- r$name[1] <- r$value[1] <-"-"  # If [1] and [2] are the same, sql.core drops the field!
      r = recSaveR(r, db=S$db, pool=pool)
         # now fix the project's ids table
      f = as.tibble(fromJSON(r$r$value[2]))          # Convert value to a FORM, FORM$name to an ID,
      if(nrow(f)>0) {
         for(name in f$name) {
            id <- paste0("id", paste0(utf8ToInt(name), collapse=""))   # create valid id syntax for both R and JavaScript
            r = dbExecute(dbLink, paste0("INSERT INTO `", S$db, "`.`ids` VALUES ('", id, "', '", formIDs$name[i], "', '", name, "');"))
         }
      }
      }

   # Move base pico records to the project
   r <- recGet("om$prime", "pico", "picoID", WHERE=tibble(c("picoID", ">", "0")), pool=pool)
   for(id in r$picoID) {
      r <- recGet("om$prime", "pico", "**", WHERE=tibble(c("picoID", "=", id)), pool=pool)
      r$picoID <- r$verNum <- 0                      # change both vectors (it's an insert not an update)
      r$comment[1] <- r$name[1] <- r$value[1] <-"-"  # If [1] and [2] are the same, sql.core drops the field!
      r <- recSaveR(r, db=S$db, pool=pool)
   }

### Complete protocol table
   # Get protohelp
   pH = recGet("om$prime", "protohelp", "*", WHERE=tibble(c("protohelpID", ">", "0")), pool=pool)
      # Get new protocol tibble
   proto = newRec("protocol")
      # Update protocol table one row at a time with next order; text is blank
   for(i in pH$order) {
      if(i=="Ba") {
         proto$text[2] = projectName
      } else {
         if(projectID==1) {
            proto$text[2] = VitD[[i]]
         } else {
            if(i %in% c("A", "Aa", "Ab", "B", "C", "D", "Da", "Dd", "Dh", "E")) {   # Leave headers blank
               proto$text[2] = ""
            } else {
               proto$text[2] = "<p><br></p>"
            }
         }
      }
      proto$order[2] = i
      recSaveR(proto, db=S$db, pool=pool)
   }
   return(TRUE)
}

VitD = list()
VitD$A = ''
VitD$Aa = ''
VitD$Ab = ''
VitD$B = ''
VitD$Ba = 'Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance'
VitD$Bb = '<p>Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance: protocol for a systematic review and meta-analysis</p>'
VitD$Bc = '<p>Tom Weishaar. Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance: protocol for a systematic review and meta-analysis. PROSPERO 2017 CRD42017060103 Available from:&nbsp;<a href="http://www.crd.york.ac.uk/PROSPERO/display_record.php?ID=CRD42017060103" target="_blank">http://www.crd.york.ac.uk/PROSPERO/display_record.php?ID=CRD42017060103</a></p>'
VitD$Bd = '<p>Tom Weishaar; Teachers College, Columbia University</p>'
VitD$Be = '<p>None</p>'
VitD$Bf = '<p>None</p>'
VitD$Bg = '<p>The study will be submitted in partial fulfillment of the requirements for a doctorate degree in Health Education. Sponsorship and guidance was provided to the author by Dr. Sonali Rajan, Department of Health and Behavior Studies and by Dr. Beth Tipton, Department of Human Development, Teachers College, Columbia University. The study is not funded.</p>'
VitD$C = ''
VitD$Ca = '<p>The nucleus of each human cell has 48 nuclear receptors. Each of these receptors, after interacting with its related ligands, up- or down-regulates specific genes. One of these nuclear receptors responds specifically to at least three vitamin D metabolites, each of which controls, for the most part, different genes. Vitamin D status has been recognized as a determinant of human health for almost a century and has been associated with a wide range of human diseases and conditions. In the U.S. population, skin color accounts for much of the variation in vitamin D status. Darker skin provides protection from the intense sunlight found near the equator, but at the latitude of the U.S. requires more sun exposure to create as much vitamin D as lighter skin. Research suggests that racial health disparities may be related to these disparities in vitamin D status. These relationships, which are slowly gaining some acceptance among medical and nutritional professionals, have yet to be recognized or taken seriously either by those who study health disparities or by those who set public health policy in the United States. The biggest barrier to complete acceptance of these theories in health policy appears to be discordance among the conclusions of systematic reviews and meta-analyses of randomized controlled trials of vitamin D supplementation.&nbsp;</p><p>The discordant results of these reviews may be due to three unappreciated sources of heterogeneity - the form of the vitamin D given as a supplement, daily versus bolus dosing, and the baseline 25(OH)D status of a study\'s participants. Existing systematic reviews typically assume that vitamin D<sub>2</sub> and vitamin D<sub>3</sub> are equally effective, but this view has been&nbsp;challenged. Existing reviews also tend to assume that vitamin D supplements will be equally effective in either small daily doses or in larger, less-frequent bolus doses. This, too, has been challenged. Existing reviews pay close attention to the dose used in the intervention but not to total vitamin D exposure as measured by the 25(OH)D status of the participants in the study.</p><p>In addition to problems with heterogeneity, most existing vitamin D systematic reviews show a trend toward effectiveness, but that trend is not statistically significant because of the limited number of trials for any particular outcome. However, the health effects of vitamin D may be homogenous enough to combine trials with different health and performance outcomes in a single meta-analysis. This would address the statistical problem posed by the limited number of trials for any single outcome. Generalizing the outcome has a long history in meta-analysis; the first meta-analysis ever done mixed trials with various outcomes of psychotherapy. This systematic review will include any trial arm with an outcome related to human physical or mental health or performance.</p>'
VitD$Cb = '<p>In this context, a <em>beneficial effect</em> is an effect size favorable to daily vitamin D<sub>3</sub> supplementation with a 95% confidence interval that does not include the no-effect value.</p><ul><li>Does daily vitamin D<sub>3</sub> supplementation have a beneficial effect on human health and performance outcomes overall?</li><li>Does daily vitamin D<sub>3</sub> supplementation have a beneficial effect on human health and performance outcomes for which there are known racial health disparities in the U.S. population?</li><li>Does lower control-group 25(OH)D status at outcome measurement have a larger beneficial effect than high status?</li></ul>'
VitD$D = ''
VitD$Da = ''
VitD$Daa = '<p>Language. Adequate information about eligible study arms must be available in English.</p><p><br></p>'
VitD$Dab = '<p>We will accept only study arms from randomized controlled trials (RCTs) with individual randomization (not cluster randomization).</p>'
VitD$Dac = '<p>Humans. No other eligibility limitations. Where the information is available, we will record group mean age and its standard deviation, group mean weight and its standard deviation, percent female for each group, and health status of the participants.</p><p>Setting. No restrictions. The setting describes living conditions of the participants, such as community-dwelling, assisted living, long-term care, or hospice. When available the setting will be recorded.</p><p>Geographic location. No restrictions. When available the latitude of the research location will be recorded.</p>'
VitD$Dad = '<p>The intervention must be daily vitamin D<sub>3</sub> supplementation by any method of administration (by mouth, by injection, etc.). The method of administration will be recorded.</p><p>Timing. This study concerns active supplementation. Arms with an outcome measured less than four weeks after supplementation begins or after supplementation ends are not eligible. Paired control and intervention groups may be measured at multiple times for a single outcome. These will be recorded as separate arms with different durations of supplementation. Duration of supplementation at outcome measurement will be recorded.</p>'
VitD$Dae = '<p>The comparison group can be no-intervention, placebo, or standard-of-care. The type of control group will be recorded. Comparison groups must be identical except for the daily dose of vitamin D<sub>3</sub>, so, for example, both groups may also take an equal amount of calcium. No-intervention control groups will be considered identical to placebo control groups receiving no supplementation. Study arms in which the control group receives a smaller dose of vitamin D<sub>3</sub> than the intervention group (typically a standard-of-care control group) will be accepted. When a trial includes multiple arms with intervention groups taking different dose sizes, the control group for all arms will be the group taking the smallest dose (including none). If a control group receives vitamin D<sub>3</sub>, the difference in dose size between the control and intervention group will be considered the intervention dose for that arm.</p>'
VitD$Daf = '<p>The study will accept any outcome measure related to human physical or mental health or performance. This study does not consider 25(OH)D status itself to be an eligible outcome.</p>'
VitD$Db = '<p>To maximize the author\'s productivity, databases searched for this study will be restricted to the Cochrane Central Register of Controlled Trials (CENTRAL). Since 1998, Cochrane review groups have completed 11 systematic reviews of vitamin D supplementation on pregnancy, infection prevention in children, management of asthma, treatment of chronic pain in adults, mortality (2), cancer prevention, fracture prevention, cystic fibrosis, bone mineral density in children, and corticosteroid-induced osteoporosis. The most recent study on mortality includes more trials than any other vitamin D systematic review ever published.</p><p>CENTRAL contains a record for every study examined in its own reviews. In addition, CENTRAL is updated monthly with records of new randomized controlled trials retrieved from Medline and EMBASE, from specialized registers created by Cochrane\'s review groups, and from Cochrane\'s hand search results register. Because of the comprehensive nature of CENTRAL\'s database of randomized controlled trials, CENTRAL by itself can provide a systematic view of all relevant randomized controlled trials in both the primary and the grey literature. This avoids the additional work of stage 1 reviews finding duplicative results in additional databases that are a mix of randomized controlled trials and other types of publications, making this project feasible for a single author. Complete information on the contents of CENTRAL and how it is updated is available at <a href="http://www.cochranelibrary.com/help/central-creation-details.html" target="_blank">http://www.cochranelibrary.com/help/central-creation-details.html</a>.</p><p>Any other vitamin D<sub>3</sub> trials not discovered by this process may be included in the study. The discovery method of each trial will be recorded. CENTRAL will be notified of any trials missing from its database.</p>'
VitD$Dc = '<p>Search strategy. The CENTRAL search strategy will use the MeSH descriptor [Vitamin D] explode all trees. Adding additional vitamin D-related MeSH terms does not increase the number of records returned by CENTRAL. Adding additional vitamin-D related text terms vastly increases the number of records returned, but the author does not have the resources to review that many papers, particularly since almost all will fail stage 1 review anyhow (these are trials in which vitamin D is mentioned somewhere in the full text; the MeSH descriptor identifies trials that are actually about vitamin D). All trials returned by the MeSH search will be reviewed for eligibility without further limits. CENTRAL is limited to trials in humans by design. Based on preliminary testing of this strategy, CENTRAL will provide records for more than 2,750 journal articles.</p>'
VitD$Dd = ''
VitD$Dda = '<p>The author will use the structured review and meta-analysis app at Open-Meta.org. The application will be used throughout the study.</p><p>The complete record set from the CENTRAL search will be downloaded and then loaded into the Open-Meta app. At a minimum, each CENTRAL record includes the article\'s title, authors, date of publication, journal name, and journal volume, number, and page. Most CENTRAL records also include the article\'s PubMed ID (PMID) and abstract. Open-Meta uses the PMID (as well as other available data for records lacking a PMID) to identify duplicates. The Open-Meta app tracks the original source of a record (CENTRAL or other).</p>'
VitD$Ddb = '<p>The author will complete the Stage 1 review using the Open-Meta app, which will display each trial\'s record, including title and abstract, and allow the reviewer to record the result of the review, a comment about the review, or a comment about the trial. If a PMID is available for the record, the title and abstract will be obtained from PubMed as the page is displayed to the reviewer, otherwise the Open-Meta app will display the information obtained from CENTRAL. The Open-Meta app allows for multiple reviews (and multiple reviewers, although this demonstration study will have only one). If any review receives a stage 1 pass, the trial will be included in the stage 2 review. Reasons for failing a stage 1 review will be: not available in English; not a randomized controlled trial; no valid participants; no valid intervention - not daily; no valid intervention - not D<sub>3</sub>; no valid intervention - other; no valid comparison group; and no valid outcome.</p><p>At least six weeks after the initial review is completed, the author will repeat the Stage 1 review process, blinded to the initial review, for 10% of the initial study database. These ratings will be compared to the original ratings, a reliability score will be calculated and reported, and any additional studies given a Stage 1 Pass will receive a Stage 2 Review.</p>'
VitD$Ddc = '<p>Full-text articles will be obtained for all trials that pass Stage 1 review. As the data is collected from the full-text article, the author will search for any mention that the trial is part of a larger study. If so, the name of the larger study will be recorded and the author will search PubMed for other articles containing that name to determine if there are others from the same larger study. If so, these sets of articles will receive special handling to make sure all information from the larger study is included while duplicative or illogical information is not.</p><p>For each arm reported in each study, the author will enter both mandatory and supplemental information into the Open-Meta app. If any of the mandatory information is missing for all trial arms, or if it becomes apparent during data collection that the trial should not have passed stage 1 review, then the trial will fail stage 2 review and the reason for failure will be recorded.</p>'
VitD$De = '<p>Mandatory information for an arm includes the outcome, how the outcome is reported (e.g., group means, risk ratio), and the duration of supplementation at the time of outcome measurement. Mandatory information for both the control group and the intervention group includes the number of participants at baseline, the number of participants at outcome measurement, the daily vitamin D<sub>3</sub> dose, mean 25(OH)D status at outcome measurement, and the outcome measure in terms of both central tendency and variance. In addition to the mandatory information, supplemental information about each arm will be recorded if available. Supplemental information for the arm includes the type of control group, how the supplement was administered, research setting, latitude, participant diagnosis, pregnancy status, specific risks, and co-treatments. Supplemental information for each group includes mean and standard deviation of age and body weight, as well as percent female - preferably at baseline but acceptable at outcome measurement. Supplemental information for each group will also include the mean and standard deviation of 25(OH)D status at baseline and at outcome measurement.</p>'
VitD$Df = '<p>In this project what most limits the possible universe of studies is the intervention, which is very specific. The project will accept any outcome related to physical or mental health or performance. This strategy results from the conceptual model of the study, which suggests that there is more heterogeneity in vitamin D supplementation interventions than in outcomes. The project is particularly interested in physical or mental health or performance outcomes for which there are known racial disparities in the U.S. population.</p>'
VitD$Dg = '<p>In addition to the mandatory and supplemental information about trial arms, the author will enter data into the Open-Meta app on the risk of bias in each trial. This data will be categorical (high risk of bias, low risk of bias, unclear risk of bias) for the following five trial characteristics: industry funding, randomization and allocation concealment, blinding of participants, blinding of research personnel and outcome assessors, and level of attrition and exclusions. These categories are adapted from Table 8.5.a in the <em>Cochrane Handbook for Systematic Reviews of Interventions</em>. These data will be analyzed and reported in the evaluation of the overall strength of evidence of this systematic review.</p>'
VitD$Dh = ''
VitD$Dha = '<p>The author will synthesize the data using meta-analysis. Because one trial may provide multiple outcomes and even multiple arms for a single outcome (trial arms could have different doses or durations or both), the author will use the robust variance estimation approach proposed by Hedges, Tipton, and Johnson (2010. Robust variance estimation in meta-regression with dependent effect size estimates. Res Synth Methods, 1(1), 39-65.) to address correlated effects within studies. Unlike random effects meta-analysis methods, robust variance meta-analysis methods allow inclusion of all outcomes from a study, but require more studies for stable results than random effect methods. Random effect methods, on the other hand, require selecting just one outcome or averaging the outcomes over each trial, reducing the number of data points for the analysis from the number of outcomes to the number of trials. With robust variance methods, the weight of a trial is proportional to the trial\'s variance, but is distributed across the trial\'s outcomes. In secondary analyses where the number of studies is too limited to use robust variance methods, the author will use random effects methods.</p>'
VitD$Dhb = '<p>Trial results will be recorded in the format used in the report of the trial but will be converted to an effect size reported as a risk ratio and its 95% confidence interval by the Open-Meta app. The author will calculate and report an overall effect size for all outcomes and an effect size for all outcomes associated with racial health disparities. Each effect size will be accompanied by an estimate of trial homogeneity/heterogeneity as measured by the I-squared statistic.</p><p>If there is heterogeneity in either of the first two main analyses (<em>I-squared </em>&gt; 50% or <em>p</em> &lt; .05), the author will investigate and attempt to identify the source of the heterogeneity using subgroup and sensitivity analysis incorporating type of outcome, dose, duration of supplementation, outcome 25(OH)D status in the control and in the intervention groups, and the data recorded about trials (e.g., industry funding), about arms (e.g., setting, latitude, specific risks, co-treatments), and about groups (e.g., age, body weight, percent female, attrition).</p><p>The result for the third specific aim of this study, which relates to 25(OH)D status in the control and intervention groups at outcome, will be determined using meta-regression. In meta-regression the experimental unit is the study. In this regression, study effect sizes will be the dependent variable and the mean 25(OH)D status of the control group at outcome measurement will be the explanatory variable. The results of interest are the relative size, direction, and significance of the regression coefficient for the explanatory variable. If the coefficient for control group 25(OH)D status is negative, that would indicate that lower group baseline status leads to larger effects.</p>'
VitD$Dhc = '<p>In addition to the three main analyses, the author will perform separate subgroup analyses for outcomes related to physical health, mental health, physical performance, and mental performance.</p>'
VitD$Dhd = '<p>We do not expect to do any qualitative analysis beyond the typical narrative included with publications reporting a meta-analysis.</p>'
VitD$Di = '<p>Because the data for this project comes from CENTRAL, it has already taken advantage of all of the Cochrane vitamin D research group\'s work to identify missing trials. The methods required to thoroughly investigate publication and reporting bias, however, require resources beyond those available to the author. Assuming that this study\'s summary analysis finds a beneficial effect, the author will compute Orwin\'s Fail-safe N, with a specified overall RR of 0.95 and a mean effect in the missing studies of zero, to determine how many missing, no-effect studies would nullify the results of the summary analysis.</p><p>Moreover, there is an additional source of meta-bias that the author will be able to examine. Cochrane and many other systematic reviewers don\'t include trials in which the control group received a smaller dose of supplementation than the intervention group. These reviews require a zero dose in the control group. This type of bias could be called Institutional Review Board or IRB bias. High-quality studies from major institutions are dropped from systematic reviews because an IRB insisted that the control group receive the standard-of-care dose of vitamin D supplementation. This is a particular problem with studies of pregnancy and infants. The author will do a subgroup analysis comparing trials by type of control group to investigate whether IRB bias can be detected.</p>'
VitD$Dj = '<p>The strengths (specific intervention, large group of trials and outcomes) and limitations (single author, single database) of this study will be noted in published results, including author rating reliability over time and an analysis of the data collected on the risk of bias in individual studies</p>'
VitD$E = ''
