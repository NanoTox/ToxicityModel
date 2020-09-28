vif <-function(x,df) {
  y = names(df)
  y_new = y[!(y %in% x)]
  eqn = paste(y_new, collapse = '+')
  form = paste(x,"~", eqn)
  vif = 1/(1-summary(lm(as.formula(form), data = df))$r.squared)
  vif
}

