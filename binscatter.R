# From timlrx/binscatter on GitHub, credit to Timothy Lin
# A version of this is on GitHub as ijyliu/binscatter
binscatter <- function(formula, key_var, data, bins=20, partial=FALSE, xlabel = NULL, ylabel = NULL, savedata = NULL, only_savedata = FALSE){
  
  # Packages
  require(lfe)
  require(ggplot2)
  require(pryr)
  require(magrittr)

  print('summarizing starter data...')
  print(summary(data))

  # Flag for savedata option errors
  if (is.null(savedata) & only_savedata) {
    stop("Please specify a file name for the savedata argument.")
  }

  if(partial==TRUE){

    # Partialed regression
    print('starting partial regression')
    # print(mem_used())

    # Get variable names
    y <- unlist(strsplit(formula, "~"))[1]
    x <- unlist(strsplit(formula, "~"))[2]
    controls <- gsub(paste("[[:punct:]]*",key_var,"[[:space:]]*[[:punct:]]*",sep=""),
                    "",x)
    # Regressions
    print('starting regressions')
    print(y)
    print(key_var)
    print(controls)
    print(formula(paste(y, "~", controls, sep="")))
    print(formula(paste(key_var, "~", controls, sep="")))
    # print(mem_used())
    reg_y <- felm(formula(paste(y, "~", controls, sep="")), data=data)
    print('summarizing y on controls')
    print(summary(reg_y))
    resid_y <- resid(reg_y)
    # print(mem_used())
    reg_x <- felm(formula(paste(key_var, "~", controls, sep="")), data=data)
    print('summarizing x on controls')
    print(summary(reg_x))
    resid_x <- resid(reg_x)
    # print(mem_used())
    rm(reg_y, reg_x)
    print('making new resid df')
    # print(mem_used())
    df <- data.frame(resid_y, resid_x)
    # Get clustering variables
    cluster_grp <- trimws(unlist(strsplit(formula, "\\|"))[4])
    # Regression of residuals, clustered or not
    if(is.na(cluster_grp)){
      reg <- felm(resid_y ~ resid_x)  
    } else{
      data$resid_y <- resid_y
      data$resid_x <- resid_x
      reg <- felm(formula(paste("resid_y ~ resid_x | 0 | 0 |",
                                cluster_grp, sep="")), data=data)
    }
    
    print(summary(reg))

    print('ran residual y and x regression')
    # print(mem_used())
    rm(resid_y, resid_x)

  } else if(partial==FALSE){

    print('starting standard regression')

    # Simple reg with no partialling
    reg <- felm(formula(formula),data=data)
    print(summary(reg))
    y <- trimws(unlist(strsplit(formula, "~"))[1])
    print(y)
    print(key_var)
    print(summary(data))
    print(c(y, key_var))
    print(head(data))
    df <- subset(data, select = c(y, key_var))
    #df <- data[, c(y,key_var)]
    print(summary(df))
    print('ran regression')
    # print(mem_used())

  }

  # Get intercept and slope for later calculation
  intercept <- coef(reg)[1]
  slope <- coef(reg)[2]

  # Standard error preparation
  # Use cluster vcov from the correct model, if not available, use robust
  # if(is.null(reg$clustervcv)){
  #   vcov <- reg$robustvcv
  #   se_type <- "robust"
  # } else {
  #   vcov <- reg$clustervcv
  #   se_type <- "cluster"
  # }

  # Get terms from formula and prepare matrix for regression
  # Terms <- terms(reg)
  # m.mat <- model.matrix(Terms, data=df)
  # rm(Terms)
  # Name residual columns in df dataframe
  if (partial == TRUE) {
    print('editing column names')
    colnames(df) <- c(paste("residual",names(df)[1]), paste("residual",names(df)[2]))
  }
  # Predicted values, se caculation for CIs
  # fit <- as.vector(m.mat %*% coef(reg))
  # se.fit <- sqrt(rowSums((m.mat %*% vcov) * m.mat)) 
  # rm(m.mat)
  ## Much faster alternative to sqrt(diag(m.mat%*%vcov%*%t(m.mat))) and works fine since
  ## we only want the diagonal
  # df$upper_ci <- fit + 1.96*se.fit
  # df$lower_ci <- fit - 1.96*se.fit
  # rm(fit, se.fit)

  # Min and max values for plot line drawing
  if (only_savedata == FALSE) {

    min_x <- min(df[,2])
    max_x <- max(df[,2])
    min_y <- intercept + min_x*slope
    max_y <- intercept + max_x*slope
  
  }

  # Binning residuals, most important step
  print('binning')
  # print(mem_used())
  # bins is the number of bins, cut df column 2
  print(summary(df))
  print(head(df))
  print(head(df[,2]))
  #print(as.matrix(df[,2]))
  #print(cut(as.matrix(df[,2]), bins))
  #print('printed cut')
  #print(list(cut(as.matrix(df[,2]), bins)))
  #print('printed list of cut')
  df_bin <- aggregate(df, by = list(cut(as.matrix(df[,2]), bins)), mean)

  print('summarizing binned data')
  print(summary(df_bin))

  # Set up default x and y labels for graph
  if (only_savedata == FALSE) {

    if (is.null(xlabel)) {
      xlabel <- names(df)[2]
    }
    if (is.null(ylabel)) {
      ylabel <- names(df)[1]
    }

  }

  # Save data option
  if (!is.null(savedata)) {
    write.csv(df_bin[,2:3], savedata)
  }

  # Only plot if necessary
  if (only_savedata == FALSE) {

    ggplot(data=df, aes(x=df[,2], y=df[,1])) +
      #geom_point(alpha=0.2) + 
      geom_segment(aes(x = min_x, y = min_y, xend = max_x, yend = max_y),
                  color=cea_blue, size=1) +
      #geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci),alpha=0.18) +
      geom_point(data=df_bin, aes(x=df_bin[,3], y=df_bin[,2]), color=cea_blue, size=2) +
      labs(x = xlabel, y = ylabel) #+
      #theme_classic()

  }

}
