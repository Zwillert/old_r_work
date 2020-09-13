library(tidyr)
library(Matrix)
library(pracma)

sort_pers = function(pers_diag){
  pers_diag = data.frame(pers_diag[[1]][[1]])
  pd = c()
  peeps = c(min(pers_diag[,1]):max(pers_diag[,1]))
  pep = c()
  isolated_people = c()
  ncomp = c()
  old_dex = which(pers_diag$pid_pct == 1)
  for(x in min(pers_diag[,1]):max(pers_diag[,1])){
    new_dex = which(pers_diag[,1] == x)
    new_dex_2 = which(pers_diag[,2] == x)
    pd = c(pd,new_dex) 
    pep = c(pep, rep(min(pers_diag[new_dex,2]), length(new_dex)))
  }
  persist_diag = data.frame(cbind(pers_diag[pd,], pep))
  return(data.frame(persist_diag))
}

persistence = function( yr = 113, c = "sen",  remove_slackers = T, slack_ratio = 2, n = 2, scaled = F, ydta){
  com_vec = c()
  ncom_vec = c()
  com_len_vec = c()
  eps_vec = c()
  eps_vec2 = c()
  pid = c()
  pid_n = c()
  
  if(is.null(ydta)){
    ydata = parse_year(y = yr, chamber = c)
    jvotes = just_votes(ydata)
    nvotes = ncol(ydata$votes)
    ydata = cbind(ydata$legis.data[-1,], c(1:nrow(ydata$legis.data[-1,])))
    comp_rats = c()
    weighted_comp_rats = c()
    rat_eps = c()
    slackers = which(rowSums(jvotes == -1) > (ncol(jvotes)/slack_ratio))
  } else {
    ydata = ydta
    jvotes = just_votes(ydata)
    nvotes = ncol(ydata$votes)
    ydata = cbind(ydata$legis.data[-1,], c(1:nrow(ydata$legis.data[-1,])))
    comp_rats = c()
    weighted_comp_rats = c()
    rat_eps = c()
    slackers = which(rowSums(jvotes == -1) > (ncol(jvotes)/slack_ratio))
    
  }
    if(remove_slackers == T){
      jvotes = jvotes[-slackers,]
      ydata = ydata[-slackers,]
    } 
    ex = as.matrix(expand_votes_v(jvotes))
    distm = distmat(ex, ex)
    epmax = max(distm)
    epmin = min(distm)
    epsilon = sum(epmax, epmin)/2
  
  adj = find_adjacencies(distm, epsilon)
  d2 = distm
  ncom = nrow(distm)
  pid_sum = 1
  while(ncom != 1){
    comp_rats = c(comp_rats, ncom)
    weighted_comp_rats = c(weighted_comp_rats, pid_sum)
    epsilon = min(d2)
    rat_eps = c(rat_eps, epsilon)
    adj = find_adjacencies(distm, epsilon)
    comps = n_comps_int(adj)
    ncom = comps$Number_O_components
    pid_sum = 0
    for(x in 1:ncom){
      which_comp = comps[[2]][[x]]
      com_vec = c(com_vec, which_comp[1])
      ncom_vec = c(ncom_vec, ncom)
      com_len_vec = c(com_len_vec, length(which_comp))
      eps_vec = c(eps_vec, epsilon)
      pid = c(pid, ydata$partyCode[which_comp[1]])
      pid_n = c(pid_n, length(which(ydata$partyCode[which_comp] == ydata$partyCode[which_comp[1]])) )#/ length(which_comp))
      pid_sum = (pid_sum + (length(which_comp)/ncol(distm)) * (length(which(ydata$partyCode[which_comp] == ydata$partyCode[which_comp[1]])) == length(which_comp)))
    }
    d2[which(d2 <= epsilon)] = max(d2)
  }
  if(scaled == T){
    eps_vec = eps_vec/nvotes
  }
  eps_vec2 = eps_vec / mean(eps_vec)
  pid_pct = pid_n/com_len_vec
  legis_info = list()
  return(list(list(data.frame(cbind(com_vec, ncom_vec, com_len_vec, eps_vec,eps_vec2, pid, pid_n, pid_pct))), list(data.frame(cbind(comp_rats, weighted_comp_rats, rat_eps))), comps))
  
}


n_comps_int = function(adj){ 
  
  comp_all = c()
  comp_acc = 0
  l = list()
  
  for(x in 1:nrow(adj)){
    if (x %in% comp_all){next}
    comp = which(adj[x,] == 1)
    while (length(setdiff(dex_zer(comp, adj), comp)) !=0){
      comp = sort(union(comp, dex_zer(comp, adj)))  }
    comp_acc = comp_acc + 1
    l = append(l, list(comp))
    names(l)[comp_acc] = paste("comp", comp_acc, sep = "_")
    comp_all = c(comp_all, comp, 0)
    
  }
  num_comps = length(l)
  l = list(num_comps,  l )
  names(l) = c("Number_O_components",  "components")
  return(l)
} 

dex_zer = function(dex_vec, adj, n = 1){
  v = c()
  for(d in dex_vec){v =  c(v,which(adj[d,] == 1))}
  return(v)
}

year_merge = function(y_list, conh = "sen"){
  pers = persistence(c = conh)
  isolation_mat = pers[[2]][[1]]
  pf = sort_pers(pers)
  pf = data.frame(cbind(pf, rep(113, nrow(pf))))
  iso = data.frame(cbind(isolation_mat, rep(113, nrow(isolation_mat))))
  
  for(y in y_list){
    pers = persistence(yr = y, c = conh)
    isolation_mat = pers[[2]][[1]]
    pers = sort_pers(pers)
    pers = data.frame(cbind(pers, rep(y, nrow(pers))))
    isolation_mat = data.frame(cbind(isolation_mat, rep(y, nrow(isolation_mat)))) 
    print(dim(pers))
    print(dim(pf))
    pf = rbind(as.matrix(pf), as.matrix(pers))
    iso = rbind(as.matrix(iso), as.matrix(isolation_mat))
    print(y)
  }
  colnames(pf)[ncol(pf)] = "year"
  colnames(iso)[ncol(iso)] = "year"
  year_merge_list = list(data.frame(pf), data.frame(iso))
  names(year_merge_list) = c()
  return(year_merge_list)
}


year_m = function(pers_list){
  pers = pers_list
  isolation_mat = pers[[2]][[1]]
  pf = sort_pers(pers)
  pf = data.frame(cbind(pf, rep(113, nrow(pf))))
  iso = data.frame(cbind(isolation_mat, rep(113, nrow(isolation_mat))))
  colnames(pf)[ncol(pf)] = "year"
  colnames(iso)[ncol(iso)] = "year"
  year_merge_list = list(data.frame(pf), data.frame(iso))
  names(year_merge_list) = c()
  return(year_merge_list)
}

find_diff = function(new_mat){ 
  
  final_mat = data.frame(matrix(nrow = nrow(new_mat), ncol = nrow(new_mat), 0))
  nc = ncol(new_mat)
  for (x in 1:nrow(new_mat)){
    for (y in (x+1):nrow(new_mat)){
      print(c(x,y))
      final_mat[x,y] = final_mat[y,x] = sqrt(nc - (sum(new_mat[x,] == new_mat[y,])))
    } 
  }
  return(data.frame(final_mat))
}


expand_votes = function(pol_mat){
  new_mat = data.frame(matrix(nrow = nrow(pol_mat), ncol = ncol(pol_mat )*3, data = 0))
  print(dim(pol_mat))
  print(dim(new_mat))
  n = dimnames(pol_mat)[[2]]
  n2 = c(n, paste(n,"_yea", sep = "")) 
  dimnames(new_mat)[[2]] = c(paste(n, "_yea", sep = ""), paste(n, "_nay", sep = ""), paste(n, "_absent", sep = ""))
  dimnames(new_mat)[[1]] = dimnames(pol_mat)[[1]]
  
  for(x in 1:nrow(new_mat)){
    new_mat[x, which(pol_mat[x,] == 2)] = 1
    new_mat[x, which(pol_mat[x,] == 1) + 657] = 1
    new_mat[x, which(pol_mat[x,] == -1) + 657 + 657] = 1
  }
  
  final_mat = data.frame(matrix(nrow = nrow(new_mat), ncol = nrow(new_mat)))
  return(new_mat)
}


find_adjacencies = function(dist_matrix, epsilon){
  adj = dist_matrix * 0
  for(x in 1:nrow(dist_matrix)){
    neighbors = which(dist_matrix[x,] <= epsilon)
    adj[x,neighbors] = 1
  } 
  return(adj)
}


expand_votes_v = function(pol_mat){
  pv = as.vector(unlist(pol_mat))
  new_v = c(as.numeric(pv == 1),as.numeric(pv == 2),as.numeric(pv == -1))
  new_mat = data.frame(matrix(nrow = nrow(pol_mat), ncol = length(new_v)/nrow(pol_mat), data = new_v))
  return(new_mat)
}

