# TITLE: Custom Functions for insight demo

# packages ----------------------------------------------------------------
library(tidyverse)
library(forcats)
library(stringr)
library(glue)
library(readr)
library(broom)
library(knitr)
library(kableExtra)
library(grid)
library(psych)
library(broman)
library(corrplot)
library(plotrix) # for std.error() 
library(lme4)
library(effects)
library(magrittr)

# ggplot themes -----------------------------------------------------------

## Theme parameters
font_small <- 10
font_medium <- 12
font_large <- 14
font_family <- "Helvetica"
font_color <- "black"
axis_color <- "black"
panel_grid_color <- "#e2e2e2" # a light gray
strip_color <- "gray"

## Themes

report_theme <- 
  theme_minimal() + 
  theme(
    line = element_blank(), # suppresses vertical lines
    axis.ticks.x = element_line(color = axis_color),
    panel.grid.major.y = element_line(color = panel_grid_color),
    panel.grid.minor.y = element_line(color = panel_grid_color),
    axis.line.x = element_line(size = .8),
    axis.text = element_text(size = font_small, family = font_family, 
      colour = axis_color), 
    axis.title.y = element_text(angle = 90, vjust = 1.5, hjust = .47,    
      size = font_medium, family = font_family),  
    axis.title.x = element_text(vjust = -.5, hjust = .52,                    
      size = font_medium, family = font_family),                   
    plot.title = element_text(family = font_family, color = font_color, 
      size = font_large),
    plot.margin = unit(c(.125, .125, .125, .125), "in"), # c(T,R,B,L) 
    strip.text = element_text(size = font_medium),
    strip.background = element_rect(color = strip_color, fill = strip_color),
    legend.text = element_text(size = font_small, family = font_family),  
    legend.title = element_text(size = font_small, family = font_family, 
      face = 'bold'),
    legend.position = ("none")
  )

report_line_theme <- 
  report_theme + 
  theme(    
    legend.title = element_text(face = "bold"),
    legend.position = "right", 
    legend.key.height = unit(3, "line"),
    legend.key.width = unit(2, "line")
  )

# custom functions ####

mean_sd <- function(data, var){
  var <- enquo(var)
  str_c(data %>% pull(!!var) %>% mean(., na.rm = TRUE) %>% myround(2), 
        "(", data %>% pull(!!var) %>% sd(., na.rm = TRUE) %>% myround(2),")")
}

format_pval <- function(x){
  if (is.na(x)) return(x)
  if (x < .001) return(paste('<', '.001'))
  if (x > .250) return(paste('>', '.250'))
  paste('=', broman::myround(x, 3))  
}

ttval <- function(x){
  require(broman)
  x <- data.frame(x)
  str_c("*t*(", x$parameter %>% myround(2), ") = ", x$statistic %>% myround(2), ", *p*", x$p.value %>% format_pval())
}

table_count_format <- function(x){
  x %>%
  kable(align = "r") %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, 
    position = "left") 
}

mcsd <- function(data, x) {
  # PRECAUTION: stop unless all names vars in x are in data
  stopifnot( x %in% names(data) )   
  # custom function to be used within mcsd function
          mcsd_int <- function(x) {
              MC <- x - mean(x, na.rm=T)
              SD <- sd(MC, na.rm=T)
              data.frame(MC = MC, MCmsd = MC - SD, MCpsd = MC + SD)
          }
  # 2 indicates [i] refers to cols, not rows (1).
  cbind(data, apply(data[x], 2, mcsd_int)) 
  
  # EXAMPLE:
  # mtcars <- mcsd(mtcars, c("wt", "mpg"))
  # names(mtcars)
}

# table and figure numbering functions:   -----------------------------------
# <https://rmflight.github.io/posts/2012/10/papersinRmd.html>
incCount <- function(inObj, useName) {
    nObj <- length(inObj)
    useNum <- max(inObj) + 1
    inObj <- c(inObj, useNum)
    names(inObj)[nObj + 1] <- useName
    inObj
}
    # set fidure and table counts to zero
    figCount <- c(`_` = 0)
    tableCount <- c(`_` = 0)

pasteLabel <- function(preText, inObj, objName, insLink = TRUE) {
    objNum <- inObj[objName]

    useText <- paste(preText, objNum, sep = " ")
    if (insLink) {
        useText <- paste("[", useText, "](#", objName, ")", sep = "")
    }
    useText
}

# EXAMPLE
# tableCount <- incCount(tableCount, "t.blogPostDocs"); tableCount
# in body of text you'd write `r I(pasteLabel("Table", tableCount, "t.blogPostDocs"))`

# bar plot relabeling ----

barplot_relabel <- . %>% 
  str_replace("Large", "Stereotypical\nReform") %>%
  str_replace("Psych", "Atypical\nReform") %>%
  str_replace("Weak", "Weak\nEvidence") %>%
  str_replace("Strong", "Strong\nEvidence") %>%
  str_replace("Affirm", "Self-\nAffirm") %>%
  str_replace("Bas_Rt", "Basic\nRight\nFraming") %>%
  str_replace("Precom", "Precommit\nto Criteria") %>%
  str_replace("Tchr", "Retrain\nTeachers") %>%
  str_replace_all("\n\\+\n\\-", "") 

# inline regression pulls  ----------------------------------------------------------
oo <- function(x){
  x %>% broman::myround(2)
}

ooo <- function(x){
  x %>% broman::myround(3)
}

oo2x <- function(x){
        (2*as.numeric(x) ) %>% oo
}

bval <- function(m, v, stat="Estimate"){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef() 
      # pull the stat you need from the table
      x <- cm[v, stat]
      # round that stat to two decimal places
      broman::myround(x, 2)
}

tval <- function(m, v, stat="t value"){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef() 
      # pull the stat you need from the table
      x <- cm[v, stat]
      # round that stat to two decimal places
      broman::myround(x, 2)
}

pval <- function(m, v, stat="Pr(>|t|)"){
      # create table of stats from summary of model
      cm <- m %>% summary() %>% coef() 
      # pull the stat you need from the table
      x <- cm[v, stat]
      # format the output
        if (x < .001) return(paste('*p* <', '.001'))
        if (x > .250) return(paste('*p* >', '.250'))
        broman::myround(x, 3) %>% 
          str_replace("0.", ".") %>% paste('*p* =', . )
}

# Examples
    m <- lm(mpg ~ qsec, mtcars)
    bval(m, "qsec")
    tval(m, "qsec")
    pval(m, "qsec")
    m$df
    
reg_btp <- function(m, v){
  paste0('b = ', bval(m, v),
         ', *t*(', m$df, ') = ', tval(m, v), 
         ", ", pval(m, v) 
         )
  # Example
  # m <- lm(mpg ~ wt, mtcars)
  # reg_btp(m, 'wt')
  # pval(m, "wt")
}
  
# inline glm pulls --------------------------------------------
    
zval <- function(m, v, stat="z value"){
  # create table of stats from summary of model
  cm <- m %>% summary() %>% coef() 
  # pull the stat you need from the table
  x <- cm[v, stat]
  # round that stat to two decimal places
  paste("*z* = ", broman::myround(x, 2) ) 
}

pzval <- function(m, v, stat="Pr(>|z|)"){
  # create table of stats from summary of model
  cm <- m %>% summary() %>% coef() 
  # pull the stat you need from the table
  x <- cm[v, stat]
  # format the output
  if (x < .001) return(paste('*p* <', '.001'))
  if (x > .250) return(paste('*p* >', '.250'))
  broman::myround(x, 3) %>% 
    str_replace("0.", ".") %>% paste('*p* =', . )
}

## practice model
# m <- glm(am ~ wt, family = 'binomial', mtcars)
# bval(m, 'wt') # same as regular regression function
# zval(m, 'wt')
# pzval(m, 'wt')

# inline anova pulls ####
anova_stats <- function(model, varname){
      paste0('*F*(', model[varname, 'Df'], ',', model['Residuals', 'Df'], ') = ', 
             model[varname, 'F value'] %>% oo,
             ', *p* ', format_pval(model[varname, 'Pr(>F)']) )
  # # EXAMPLE
  # m_a <- anova(lm(mpg ~ am, mtcars))
  # anova_stats(m_a, 'am')
}

# t test results
ttest_results <- function(ttest){
    # create shorthand rounding function
    oo <- function(x){broman::myround(x, 2)}
    # paste the prose and stats together for t-test results:
    paste0(
           # t(df)=#.##
           '*t*(', ttest$parameter %>% oo, ') = ', 
           ttest$statistic %>% oo, ', ', 
           # p = .###
           '*p* ', ttest$p.value %>% format_pval
           )
    # # EXAMPLE: 
    # t_vs <- with(mtcars, t.test( mpg[vs == 'V'], mpg[vs == 'S'] ))
    # ttest_results(t_vs) # use this inline `r ttest_results(t_vs)`
}


# table formatting functions   ------------------------------------------------
    
table_format_pval <- function(x){
      if (is.na(x)) return('NA')
      x <- broman::myround(x, 5) %>% as.numeric # added on 12.15.16 to eliminate 
#error message Warning message: In remove_leading_zero(.) : Non-zero leading 
#digit", which was caused by the rounding of very small p-values 
#(e.g., 8.354e-13 ***)
      if (x < .001) return(paste('<', '.001') )
      if (x > .250) return(paste('>', '.250') )
      myround(x, 3) %>% str_replace("0.", ".") # remove leading zero
      }
    
fix_names <- function(x){ # Set intutive table names to replace variable names from data
  x %>%
  str_replace(".Intercept.", "Intercept") %>%
  ## rename conditions
  str_replace_all("type_cc", "Stereotypicality (Contrast)") %>%
  str_replace_all("evid_cc", "Evidence Quality (Contrast)") %>%
  str_replace("rep_f", "Program Stereotypicality Cond") %>%
  str_replace("strong_f", "Evidence Quality Cond") %>%
  str_replace("evalrating_cv", "Eval Quality Rating") %>% 
  str_replace("rep_cc", "Program Stereotypicality Contrast") %>%
  str_replace("strong_cc", "Evidence Quality Contrast") %>%
  str_replace('clarity_causal_mech', 'Causal Clarity') %>%
  str_replace("UNwise_dmy", "Large Intv dmy") %>%
  str_replace("wise_dmy", "Psych Intv dmy") %>%
  str_replace("source_fAERA_2013", "Survey Year 2") %>%
  str_replace("source_fAERA_2014", "Survey Year 3") %>%
  str_replace("source_fAERA_2016", "Survey Year 4") %>%
  str_replace("survey_f", "Survey Year") %>%
  # Clean up special characters
  str_replace_all(".mc", " (MC)") %>%
  str_replace_all(".mcmsd", " (MC - 1SD)") %>%
  str_replace_all(".mcpsd", " (MC + 1SD)") %>%
  str_replace_all(":", " x ")
}

print_reg_table <- function(x){
  x %>% 
  tidy %>%
  mutate(p.value = lapply(p.value, table_format_pval)) %>% 
  mutate_at(., vars(-c(term, p.value)), ~ broman::myround(., digits = 2)) %>% 
  mutate(term = fix_names(term)) %>%
  set_colnames( c("Variable", "Estimate", "SE", "_t_", "_p_") ) %>%
  kable(align = "r")
  # ## Example of regression table
  # m <- lm(mpg ~ wt * cyl, mtcars) 
  # print_reg_table(m)
}
  
print_anova_table <- function(x){
  require(tidyverse)
  x %>%
    broom::tidy() %>%
    mutate(p.value = map_chr(p.value, table_format_pval)) %>% 
    mutate_at(
      ., vars(-c(df, term, p.value)), ~ broman::myround(., digits = 2)
    ) %>% 
    mutate(
      term = fix_names(term),
      # Remove NAs that would appear in the final table
      statistic = ifelse(term == "Residuals", "", statistic),
      p.value = ifelse(term == "Residuals", "", p.value)
    ) %>% 
    rename(
      Variable = term, 
      SS = sumsq, 
      MSE = meansq, 
      `F` = statistic, 
      `_p_` = p.value
    ) %>% 
    kable(align = "r") 
    # # Example 
    # anova(lm(mpg ~ vs * am, mtcars)) %>% print_anova_table()
}
    
print_glmr_table <- function(x){
      x %>%
      tidy %>%
      mutate_at(
        ., vars(-c(term, p.value)), ~ broman::myround(., digits = 2)
      ) %>% 
      mutate(term = fix_names(term)) %>%
      set_colnames(c("Variable", "Estimate", "SE", "_z_", "_p_", "type"))
      kable(align = "r") 
} # not yet working

glmr_table_names <- c("Variable", "Estimate", "SE", "_z_", "_p_", "type")

format_glmr_table <- . %>%
  mutate_at(., vars(-c(term, p.value)), ~ broman::myround(., digits = 2)) %>% 
  mutate(term = fix_names(term), p.value = table_format_pval(p.value)) %>%
  set_colnames(glmr_table_names)

print_glmr_table <- . %>%
  tidy %>%
  format_glmr_table %>%
  kable(align = "r")

cln_tbl <- function(x) {
  tbl <- x
  cn <- colnames(tbl) # save list of column names as cn
  cn <- cn %>% # clean up column names
    str_replace("rep_f" , "Program Type Cond") %>%
    str_replace("wise_t" , "Program Type Cond") %>%
    str_replace("strong_f" , "Evidence Cond") %>%
    str_replace("Prime_t" , "Prime") %>%   
    str_replace("se", "SE") %>%   
    str_replace("sd", "SD") %>%  
    str_replace("incred", "DV: Incredulity") 
  colnames(tbl) <- cn # overwrite summary stats table with clean names
  tbl
}

summstat <- function(data, measurevar, groupvars){
  # create subset of complete cases
  df_cc <- data %>%
    subset(select = c(measurevar, groupvars)) %>%
    .[complete.cases(.),] 
  # apply summarySE to subset of complete cases
  dfc <- df_cc %>% 
    summarySE(measurevar, groupvars) 
  # great paste function from 
  # http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
  dfc$predictor <- apply( dfc[ , groupvars ] , 1 , paste , collapse = "\n+\n" )
  dfc$barorder <- seq(1:nrow(dfc))
  dfc$ttl_n <- nrow(df_cc) # for use with (N = ) at top of plot
  dfc
}

    # ## Example of function in use: 
    # dfc <- summstat(mtcars, "mpg", c("vs", "cyl"))    

print_ss_table <- function(x) {
  x %>% 
    cln_tbl() %>% 
    kable(align = "r", digits = 2)
  # # example of print_ss_table in use
  # dfc <- summarySE(mtcars, "mpg", c("cyl", "am"))  # create your table 
  # print_ss_table(dfc)
}

threewaycovar <- function(data, dv, ivs, covar) {
  stopifnot(all(c(dv, ivs, covar) %in% names(data)))
  form <- as.formula(paste(dv,"~", paste(ivs,collapse="*"), "+", paste(covar, collapse="+")))
  m <- lm(form, data)
  e <- effect(paste(ivs, collapse=":"), m)
  e_df <- data.frame(e)
  e_df$predictor <- apply(e_df[,(seq_along(ivs))], 1, paste, collapse="\n+\n") # merge     first three columns
  ifelse(names(e_df)=="fit", dv, names(e_df))->names(e_df) 
  keep <- c(ivs, "predictor", dv, "se") # create list of names
  d2 <- e_df[,colnames(e_df) %in% keep] # select columns from e_df that match the list keep
  # add N and SD to your table
    dss <- summstat(data, dv, ivs)
    dn <- subset(dss, select = c(predictor, N))
    # merge with dq
    d2 <- merge(d2, dn, by = 'predictor')
    # add SD
    d2$sd <- d2$se * sqrt(d2$N)
    # apply final order
    target <- c(ivs, dv, 'se', 'sd', 'N', 'predictor')
    d2 <- d2[order(match(colnames(d2), target))]
  # return output
  return(d2)
    
    # # example
    # threewaycovar(data=mtcars, dv='mpg', c('trans', 'origin', 'vs'), covar = 'carb')
}   
    
twowaycovar <- function(data, dv, ivs, covar) {
      stopifnot(all(c(dv, ivs, covar) %in% names(data)))
      form <- as.formula(paste(dv,"~", 
                               paste(ivs,collapse="*"), 
                               "+", 
                               paste(covar, collapse="+")))
      m <- lm(form, data)
      e <- effect(paste(ivs, collapse=":"), m) # save cross table
      e_df <- data.frame(e)
      e_df$predictor <- apply(e_df[,(seq_along(ivs))], 1, paste, collapse="\n+\n") # merge first 2 columns
      names(e_df) <- ifelse( names(e_df)=="fit", dv, names(e_df) ) # rename fit as your dv's name
      keep <- c(ivs, "predictor", dv, "se") # create list of names
      d2 <- e_df[,colnames(e_df) %in% keep] # select cols that match the name-list keep
      # add N and SD to your table
        dss <- summstat(data, dv, ivs)
        dn <- subset(dss, select = c(predictor, N))
        # merge with dq
        d2 <- merge(d2, dn, by = 'predictor')
        # add SD
        d2$sd <- d2$se * sqrt(d2$N)
        # apply final order
        target <- c(ivs, dv, 'se', 'sd', 'N', 'predictor')
        d2 <- d2[order(match(colnames(d2), target))]
      # return output
      return(d2)
    } 
    
    # example
    # dp <- twowaycovar(data=mtcars, dv='mpg', c('trans', 'origin'), covar = 'vs'); dp

cln_corr_names <- function(x) {
  fix_corr_names <- . %>%
    str_replace("depend",   "EnvDep_cv") 
  
  colnames(x) <- colnames(x)  %>%  fix_corr_names 
  rownames(x) <- rownames(x)  %>%  fix_corr_names 
  x
}

clean_corr_tbl <- function(x){ 
  x %>% 
  data.frame %>% 
  cln_corr_names() %>% 
  kable(align = "r")
}

inline_format_lmer_coefs <- function(m){
  m_c <- summary(m)$coefficients %>% data.frame %>% mutate(variable = rownames(.))
  # generate confidence interval rather than degrees of freedom for MLM per <http://stats.stackexchange.com/questions/146988/getting-degrees-of-freedom-from-lmer>
  m_ci <- confint(m, oldNames=TRUE)  %>%  data.frame
  # rename ci bounds
  m_ci <- dplyr::rename(m_ci, ci2.5=X2.5.., ci97.5=X97.5..)
  # drop excess rows
  drop_list <- c('.sig01', '.sigma')
  m_ci <- m_ci %>% mutate(variable = rownames(.)) %>% filter(!(variable %in% drop_list))
  # merge for final dataframe
  m <- left_join(m_c, m_ci, by = "variable")
  m %>% select(Variable = variable, Estimate, SE = Std..Error, t.value, ci2.5, ci97.5)
}

    # Example
    # m <- lmer(mpg ~ wt + (1 | am), mtcars) %>% inline_format_lmer_coefs

print_hlm_table_with_cis <- function(m){
  m %>% 
  mutate_at(
    ., vars(-Variable), ~ broman::myround(., digits = 2)
  ) %>% 
  mutate(
    Variable = fix_names(Variable), 
    `95% CI` = str_c("[", ci2.5, ", ", ci97.5, "]")
  ) %>% 
  select(
    Variable, Estimate, SE, `_t_` = t.value, `95% CI`
  ) %>% 
  kable(align = "r") %>% 
  kable_styling()
  ## Example of regression table
  # m <- lmer(mpg ~ wt + (1 | am), mtcars) %>% inline_format_lmer_coefs
  # print_hlm_table_with_cis(m)
}

hlm_fx <- function(m=model, v='var_name'){
  paste0('b = ', m[v, 'Estimate'] %>% oo, ', ', 
         '95% CI = [', m[v, 'ci2.5'] %>% oo, ', ', m[v, 'ci97.5'] %>% oo, '], ',
         '*t* = ', m[v, 't.value'] %>% oo
  )
}
    # Example
    # hlm_fx(m, 'wt')

# inline functions for doubling stats in contrast-coded variables ####
b_2x <- function(m=model, v='var_name'){
  paste0('b = ', (2 * m[v, 'Estimate']) %>% oo)
}
    # Example
    # b_2x(m, 'wt')

bci_2x <- function(m=model, v='var_name'){
  paste0('b = ', (2 * m[v, 'Estimate']) %>% oo, ', ',
         '95% CI = [', (2 * m[v, 'ci2.5']) %>% oo, ', ', (2 * m[v, 'ci97.5']) %>% oo, ']')
}
    # Example
    # bci_2x(m, 'wt')

t_hlm <- function(m=model, v='var_name'){
  paste0('*t* = ', m[v, 't.value'] %>% oo)
}
    # Example
    # t_hlm(m, 'wt')
    
ci2x_t_hlm <- function(m=model, v='var_name'){
  paste0('95% CI = [', (2 * m[v, 'ci2.5']) %>% oo, ', ', (2 * m[v, 'ci97.5']) %>% oo, '], ',
         '*t* = ', m[v, 't.value'] %>% oo
  )
}
       
hlm_fx_2x <- function(m=model, v='var_name'){
  paste0('b = ', (2 * m[v, 'Estimate']) %>% oo, ', ', 
         '95% CI = [', (2 * m[v, 'ci2.5']) %>% oo, ', ', (2 * m[v, 'ci97.5']) %>% oo, '], ',
         '*t* = ', m[v, 't.value'] %>% oo
  )
}
    # Example
    # hlm_fx_2x(m, 'wt')
    
plot_lmer_means <- function(lmer_model, interaction_term){
  e_df <- effects::effect(interaction_term, lmer_model) %>% data.frame
  e_df$predictor <- paste( e_df[,1], e_df[,2], sep = "\n+\n") # merge first 2 columns
  e_df
}
