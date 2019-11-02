time_2_increase <- function(list){
    if(length(list)==1){
    list(
        MsM = list %>% extract_features(2,1) %>% diff_find(),
        Bandwidth = list %>% extract_features(1, 1) %>% diff_find()
    )}else if(length(list)>1){
        lapply(seq(length(list)), function(x) 
            list(
                MsM = list %>% extract_features(2, x) %>% diff_find(),
                Bandwidth =  list%>% extract_features(1, x) %>% diff_find()
            )
        )
    }
}

diff_find <- function(list){
    diff(unlist(x = list, use.names = F, recursive = T))
}

extract_features <- function (list, i, d){
    lapply(seq(list[[1]]), function(x) list[[d]][[x]][i])
}

gradients <- function(list){
    lapply(seq(length(list)), function(x) grads(list, x))
}
grads <- function(list, i){Map("/", list[[i]][2], list[[i]][1])}
