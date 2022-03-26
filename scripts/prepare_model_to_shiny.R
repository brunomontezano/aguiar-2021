# Preparing model to production (Shiny app)

e <- new.env()
load("sessions/25032022_analysis_75_loocv.RData", envir = e)

modl <- e$final_model

dt <- e$dt_prod2


model.list <- list("rf.model" = modl, 
                   "predictors" = colnames(dt)[-1],
                   "newx.to.test" = dt[300, -1])

m <- match(model.list$predictors, names(model.list$newx.to.test))
m

saveRDS(model.list, file = "cache/final_model_random_forest_mtry2d.rds")
