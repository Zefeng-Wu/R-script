apres<-apcluster(corSimMat(r=1),log_norm_matrix,detail=TRUE) 
  apres_out <-vector(mode="list",length=length(apres))
  for (n in (1:length(apres))){
    cls_num_ap_id <- names(apres@clusters[[n]])
    apres_out[[n]]<-cls_num_ap_id
  }
