Investigate <-
function(res, file = "Investigate.Rmd", document = c("html_document"), Iselec = "contrib", Vselec = "cos2", Rselec = "contrib", Cselec = "cos2", Mselec = "cos2", Icoef = 1, Vcoef = 1, Rcoef = 1, Ccoef = 1, Mcoef = 1, 
           ncp = NULL, time = "10s", nclust = -1, mmax = 10, nmax = 10, hab = NULL, ellipse = TRUE, display.HCPC = TRUE, out.selec = TRUE, remove.temp = TRUE, parallel = TRUE, cex = 0.7) {
    if(!is.character(file)) {return(warning("o parâmetro 'file' tem de ser uma cadeia de carácter dando o nome do arquivo .Rmd file a ser escrito"))}
    
    # VERIFICATIONS
    if(!is.numeric(Iselec) & !is.character(Iselec)) {return(warning("the argument 'Iselec' deveria ser vetor numérico ou caráter"))}
    if(!is.numeric(Vselec) & !is.character(Vselec)) {return(warning("the argument 'Vselec' deveria ser vetor numérico ou caráter"))}
    if(!is.numeric(Rselec) & !is.character(Rselec)) {return(warning("the argument 'Rselec' deveria ser vetor numérico ou caráter"))}
    if(!is.numeric(Cselec) & !is.character(Cselec)) {return(warning("the argument 'Cselec' deveria ser vetor numérico ou caráter"))}
    if(!is.numeric(Mselec) & !is.character(Mselec)) {return(warning("the argument 'Mselec' deveria ser vetor numérico ou caráter"))}
    
    if(!is.numeric(Icoef)) {return(warning("o argumento 'Icoef' tem de ser numérico"))}
    if(!is.numeric(Vcoef)) {return(warning("o argumento 'Vcoef' tem de ser numérico"))}
    if(!is.numeric(Rcoef)) {return(warning("o argumento 'Rcoef' tem de ser numérico"))}
    if(!is.numeric(Ccoef)) {return(warning("o argumento 'Ccoef' tem de ser numérico"))}
    if(!is.numeric(Mcoef)) {return(warning("o argumento 'Mcoef' tem de ser numérico"))}
    
    if(Icoef < 0) {return(warning("o argumento 'Icoef' tem de se positivo"))}
    if(Vcoef < 0) {return(warning("o argumento 'Vcoef' tem de se positivo"))}
    if(Rcoef < 0) {return(warning("o argumento 'Rcoef' tem de se positivo"))}
    if(Ccoef < 0) {return(warning("o argumento 'Ccoef' tem de se positivo"))}
    if(Mcoef < 0) {return(warning("o argumento 'Mcoef' tem de se positivo"))}
    
    if(!is.character(time)) {return(warning("the argument 'time' has to be a character chain"))}
    if(length(grep("[sL]", time)) == 0) {return(warning("the argument 'time' must specifie the desired unity : add 's' for second or 'L' for the number of repetitions"))}
    
    if(!is.numeric(ncp) & !is.null(ncp)) {return(warning("the argument 'ncp' must be numeric"))}
    if(!is.null(ncp)) {if(ncp < 0) {return(warning("the argument 'ncp' must be positive"))}}
    if(!is.numeric(cex)) {return(warning("the argument 'cex' must be numeric"))} 
    if(!is.null(ncp)) {if(cex < 0) {return(warning("the argument 'cex' must be positive"))}}
    if(!is.numeric(nclust)) {return(warning("the argument 'nclust' must be numeric"))} 
    
    if(!is.numeric(hab) & !is.character(hab) & !is.null(hab)) {return(warning("the argument 'hab' should be the name or the index of the variable used to color the individuals"))}
    
    if(!is.logical(remove.temp)) {return(warning("the argument 'remove.temp' must be logical"))}
    if(!is.logical(ellipse)) {return(warning("the argument 'ellipse' must be logical"))}
    if(!is.logical(display.HCPC)) {return(warning("the argument 'display.HCPC' must be logical"))}
    if(!is.logical(out.selec)) {return(warning("the argument 'out.selec' must be logical"))}
    if(!is.logical(parallel)) {return(warning("the argument 'parallel' must be logical"))}
    
    # verification of the file extension (Rmarkdown only works with .Rmd !!)
    if(length(grep(".Rmd", file, ignore.case=TRUE)) == 0) {file = paste(file, ".Rmd", sep = "")}
    
    # INITIALISATION
    t = Sys.time()
    compteur = 0
    analyse = whichFacto(res)
    if(!analyse %in% c("PCA", "CA", "CaGalt", "MCA", "MFA", "DMFA", "FAMD", "GPA", "HCPC"))
    {return(warning("the parameter 'res' has to be an object of class 'PCA', 'CA', 'CaGalt', 'MCA', 'MFA', 'DMFA', 'FAMD', 'GPA' or 'HCPC'"))}
    param = getParam(res)
    
    cat("-- ", gettext("creation of the .Rmd file"), " (", gettext("time spent"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    createRmd(res, file, document)
    writeRmd("load('Workspace.RData')", file = file, start = TRUE, stop = TRUE, options = "r, echo = FALSE")
    
    memory = res
    res.out = NULL
    if(out.selec) {
      cat("-- ", gettext("detection of outliers"), " (", gettext("time spent"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
      compteur = compteur + 1
      writeRmd("### ", compteur, ". ", gettext("Study of the outliers"), file = file, sep = "")
      out.object = outliers(res, file = file, Vselec = Vselec, nmax = nmax, Vcoef = Vcoef, figure.title = paste("Figure", compteur), graph = FALSE)
      res = out.object$new.res
      res.out = out.object$res.out
      param = getParam(res)
      cat(out.object$N %dim0% 0, gettext("outlier(s) terminated"), "\n\n")
      rm(out.object)
    }
    
    cat("-- ", gettext("análise da inercia"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
    compteur = compteur + 1
    writeRmd("### ", compteur, ". ", gettext("Distribuição da inércia"), file = file, sep = "")
    
    ncp = inertiaDistrib(res, file = file, ncp = ncp, time = time, figure.title = paste("Figura", compteur), graph = FALSE)
    cat(ncp, gettext("componente(s) carregando informação"), ":", gettext("inércia total de"), paste(round(res$eig[ncp, 3], 1), "%", sep = ""), "\n\n")
    
    dim2plot = ncp
    # in case if ncp is odd
    if(ncp %% 2 != 0) {
      if(nrow(res$eig) > ncp) {
        dim2plot = ncp + 1
      }
    }
    
    if(param$ncp.mod < dim2plot) {
      switch(analyse,
             PCA = {
               data = param$data
               quanti.sup = param$quanti.sup
               quali.sup = param$quali.sup
               ind.sup = param$ind.sup
               row.w = param$row.w
               col.w = param$col.w
               scale = param$scale
               
               res = PCA(data, quanti.sup = quanti.sup, quali.sup = quali.sup, ind.sup = ind.sup, graph = FALSE, 
                         scale.unit = scale, row.w = row.w, col.w = col.w, ncp = dim2plot)
               rm(data, quanti.sup, quali.sup, ind.sup, row.w, col.w, scale)
             },
             
             CA = {
               data = param$data
               quanti.sup = param$quanti.sup
               quali.sup = param$quali.sup
               row.sup = param$row.sup
               col.sup = param$col.sup
               row.w = param$row.w
               
               res = CA(data, quanti.sup = quanti.sup, quali.sup = quali.sup, row.sup = row.sup, col.sup = col.sup, 
                        graph = FALSE, row.w = row.w, ncp = dim2plot)
               rm(data, quanti.sup, quali.sup, row.sup, col.sup, row.w)
             },
             
             CaGalt = {},
             
             MCA = {
               data = param$data
               quanti.sup = param$quanti.sup
               quali.sup = param$quali.sup
               ind.sup = param$ind.sup
               row.w = param$row.w
               
               res = MCA(data, quanti.sup = quanti.sup, quali.sup = quali.sup, ind.sup = ind.sup, graph = FALSE, 
                         row.w = row.w, ncp = dim2plot)
               rm(data, quanti.sup, quali.sup, ind.sup, row.w)
             },
             
             MFA = {},
             
             HMFA = {},
             
             DMFA = {},
             
             FAMD = {},
             
             GPA = {},
             
             HCPC = {})
      
      param = getParam(res)
    }
    
    cat("-- ", gettext("descrição do componente"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
    for(q in 1:ceiling(ncp / 2)) {
      dim = c(2 * q - 1, 2 * q)
      writeRmd("\n- - -", file = file, end = "\n\n")
      
      compteur = compteur + 1
      if(ncp >= dim[2]) {
        cat(gettext("plane"), paste(dim[1], ":", dim[2], sep = ""), "\n")
        writeRmd("### ", compteur, ". ", gettext("Descrição do plano"), " ", dim[1], ":", dim[2], file = file, sep = "")
      } else {
        cat(gettext("dim."), dim[1], "\n")
        writeRmd("### ", compteur, ". ", gettext("Descrição da dimensão"), " ", dim[1], file = file, sep = "")
      }
      
      if(dim[1] == nrow(res$eig)) {dim = dim - 1}
      
      factoGraph(res, file = file, dim = dim, hab = hab, ellipse = ellipse, Iselec = Iselec, Vselec = Vselec, Rselec = Rselec, Cselec = Cselec, Mselec = Mselec, 
                 Icoef = Icoef, Vcoef = Vcoef, Rcoef = Rcoef, Ccoef = Ccoef, Mcoef = Mcoef, figure.title = paste("Figure", compteur), graph = FALSE, cex = 0.7)
      
      desc = dim
      if(dim[2] == nrow(res$eig)) {desc = dim[2]}
      if(dim[1] == ncp) {desc = dim[1]}
      description(res, file = file, dim = dim, desc = desc, Iselec = Iselec, Vselec = Vselec, Rselec = Rselec, Cselec = Cselec, Icoef = Icoef, Vcoef = Vcoef, Rcoef = Rcoef, Ccoef = Ccoef, nmax = nmax, mmax = mmax)
    }
    cat("\n")
    writeRmd("\n- - -", file = file, end = "\n\n")
    
    
    if(display.HCPC) {
      cat("-- ", gettext("classificação"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
      if(sum(log(dimActive(res)) ^ 2) < 83.38) {
        compteur = compteur + 1
        writeRmd("### ", compteur,". Classificação", end = "\n\n", file = file, sep = "")
        
        if(analyse %in% c("CA", "CaGalt")) {
          res.hcpc = classif(res, file = file, nclust = nclust, selec = Rselec, coef = Rcoef, nmax = nmax, mmax = mmax, figure.title = paste("Figure", compteur), graph = FALSE)
        } else {
          res.hcpc = classif(res, file = file, nclust = nclust, selec = Iselec, coef = Icoef, nmax = nmax, mmax = mmax, figure.title = paste("Figure", compteur), graph = FALSE)
        }
        cat(length(levels(res.hcpc$data.clust$clust)), gettext("clusters"), "\n\n")
      } else {
        compteur = compteur + 1
        writeRmd("### ", compteur,". Classificação", end = "\n\n", file = file, sep = "")
        
        cat(gettext("dataset muito pesado"), "\n\n")
        writeRmd(gettext("O dataset é muito pesado para conduzir a classificação"), end = ".\n", file = file)
        res.hcpc = NULL
      }
    } else {
      res.hcpc = NULL
    }
    
    cat("-- ", gettext("escrevendo anexo"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    writeRmd("\n- - -", file = file, end = "\n\n")
    writeRmd("## Annexes", file = file)
    
    if(sum(log(dimActive(res)) ^ 2) < 83.38) {
      if(sum(unlist(sapply(dimdesc(res, axes = 1:ncp), lapply, nrow))) <= 50) {
        writeRmd("dimdesc(res, axes = 1:", ncp, ")", sep = "", file = file, start = TRUE, stop = TRUE, options = "r, comment = ''")
        compteur = compteur + 1
        writeRmd("**", paste("Figure", compteur), " - ", gettext("Lista de variáveis caracterizando a dimensão da análise"), end = ".**\n\n", file = file, sep = "")
      }
      writeRmd("\n", file = file)
      
      if(display.HCPC & !is.null(res.hcpc)) {
        if(sum(unlist(sapply(res.hcpc$desc.var, lapply, nrow))) <= 50) {
          writeRmd("res.hcpc$desc.var", sep = "", file = file, start = TRUE, stop = TRUE, options = "r, comment = ''")
          compteur = compteur + 1
          writeRmd("**", paste("Figure", compteur), " - ", gettext("Lista de variáveis caracterizando o cluster de classificação"), end = ".**\n\n", file = file, sep = "")
        }
      }
    }
    writeRmd(file = file)
    
    script = scriptRmd(file)
    
    cat("-- ", gettext("salvando os dados"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    save(res, param, ncp, cex, res.hcpc, memory, res.out, file = "Workspace.RData")
    rm(res, param, res.hcpc, memory, res.out, script)
    
    cat("-- ", gettext("compilação das respostas"), " (", gettext("tempo levado"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n\n", sep = "")
    readRmd(file, document)
    if(remove.temp) {
      file.remove("Workspace.RData")
      file.remove(file)
    }
    cat("-- ", gettext("tarefa completada"), " (", gettext("time spent"), " : ", round(as.numeric(difftime(Sys.time(), t, units = "secs")), 2), "s) --\n", sep = "")
    cat(gettext("Esta interpretação dos resultados foi conduzida automanticamente, ela não se compara à qualidade de uma interpretação pessoal."))
  }
