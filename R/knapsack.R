


 knapsack_class <- setRefClass("knapsack_class", fields = list(ks_dataset = "data.frame"),
#                                                               n_items = "numeric",
#                                                               ks_size = "numeric",
#                                                               temp_df = "data.frame"),
                              methods = list(
                                
                                initialize = function(){

                                  set.seed(42)
                                  n_items <- 2000
                                  ks_dataset <<-data.frame(
                                    w <-sample(1:4000, size = n_items, replace = TRUE),
                                    v <- runif(n = n_items, 0, 10000))
                                  
                                  # ks_dataset <<-data.frame(
                                  #   w <-sample(1:10, size = n_items, replace = TRUE),
                                  #   v <- runif(n = n_items, 0, 10))
                                  #ks_dataset <<- data.frame(W = c(2,3,4,5),V=c(1,2,5,6))
                                  
                                },
                                
                                knapsack_brute_force = function(X,W){
                                 
                                  i <- 1
                                  total_value <- 0     
                                  item_included <- NULL
                                  weights<- NULL
                                  values<- NULL
                                  
                                  for (i in 1:nrow(X))
                                  {
                                    w_combination <- as.data.frame(combn(X[,1], i))
                                    v_combination <- as.data.frame(combn(X[,2], i))
                                    w_vector <- colSums(w_combination)
                                    v_vector <- colSums(v_combination)
                                    weights_index <- which(w_vector<=W)
                                    
                                    if(length(weights_index) != 0)
                                      { 
                                        values <- v_vector[weights_index]
                                        total_value <- max(values)
                                        temp <- which((values)==total_value)
                                        val_weight_index <- weights_index[temp]
                                        val_weight <- w_combination[, val_weight_index]
                                        
                                        for (j in 1:i) 
                                          {
                                          item_included[j]<-which(X[,1]==val_weight[j])
                                          }
                                      }
                                  }
                                  
                                  return(list(value=total_value,elements=item_included))
                                },
                                
                                dynamic_programming = function(ks_df,ks_size){
                                  #print(ks_df)
                                  names(ks_df) <-c("W","V")   
                                  all_w_positive <- sum(ks_df$W>0)
                                  all_v_positive <- sum(ks_df$V>0)
                                  total_length <- nrow(ks_df)
                                  ks_df <- rbind(c(0,0),ks_df)
                                  
                                  stopifnot(is.data.frame(ks_df) & all_v_positive == total_length & all_w_positive == total_length)
                                  
                                  n_items <- total_length+1
                                  ks_size <- ks_size+1
                                  
                                  # Creating Matrix of size W x elements
                                  temp_df1=c()
                                  for (i in 1:(ks_size*n_items)) 
                                    temp_df1[i] = 1
                                  dim(temp_df1) = c(n_items,ks_size)
                                  temp_df <- as.data.frame(temp_df1)
                                  
                                  # calculating knapsack matrix
                                  for (i in 1:(n_items)) {
                                    for (j in 1:ks_size) {
                                      # print(paste("Row Number ",i,"\n column number ",j))
                                      
                                      if(i==1 | j ==1)
                                        temp_df[i,j] <- 0
                                      else if(ks_df[i,1] < j)
                                      {
                                        temp_i <- i-1
                                        temp_j <- (j-ks_df[i,1])
                                        temp_df[i,j] <- max((ks_df[i,2]+temp_df[temp_i,temp_j]),temp_df[temp_i,j])
                                      }
                                      else
                                      {
                                        temp_df[i,j] <- temp_df[(i-1),j]
                                      }
                                    }
                                  }
                                  
                                  # collecting the selected items from knapsack matrix
                                  i <- n_items
                                  j <- ks_size
                                  k<-1
                                  item_included <- c()
                                  total_weight <- 0
                                  total_value <- 0
                                 
                                   while (i>1 & j>1) 
                                  {
                                    if(temp_df[i,j] == temp_df[i-1,j])
                                    {
                                      #   print(paste(temp_df[i,j]," : ",temp_df[i-1,j]))
                                      i <- i-1
                                    }
                                    else
                                    {
                                      #print(paste("included : ",i-1,", with value ",ks_df[i,2]))
                                      item_included[k] <- i-1
                                      k<-k+1
                                      total_value <- total_value + ks_df[i,2]
                                      #total_weight <-total_weight+ks_df[i,1]
                                      j <- j-ks_df[i,1]
                                      i <- i-1
                                    }
                                  }
                                  #print(paste("Total weight : ",total_weight))
                                  #print(paste("Total Value : ",total_value))
                                  #print(item_included)
                                  result <- list(value = total_value,element = sort(item_included))#, weight = total_weight)
                                  return(result)
                                },
                                
                                greedy_heuristich = function(){
                                  
                                }
                              
                                ))
