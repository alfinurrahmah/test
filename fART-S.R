fakmct <- function(input, rho, alpha, beta, k_init, 
                   max_epochs = 200, max_cluster = 20, eps = 10^-6)
{
    
    ## save parameters
    params = list()
    params[["rho"]]         = rho
    params[["alpha"]]       = alpha
    params[["beta"]]        = beta
    params[["max_epochs"]]  = max_epochs
    params[["k_init"]]      = k_init
    # params[["shuffle"]]     = shuffle
    # params[["random_seed"]] = random_seed
    # params[["w_init"]]      = w_init
    # params[["distance_fn"]] = fuzzy_and
    # params[["match_fn"]]    = choice_function
    # params[["update_fn"]]   = update_weight
    # params[["vigilance_fn"]]= vigilance_check
    
    ## helpers function
    normalize_data <- function(input)
    {
      normalized <- apply(X = input,MARGIN = 2,FUN = function(col){(col-min(col))/(max(col)-min(col))})
      return(normalized)
    }
    
    complement_coded_conv <- function(normalized_data)
    {
      dataset <- cbind(normalized_data,1-normalizedData)
      return(dataset)
    }  
    
    fuzzy_and <- function(inputA, inputB)
    {
      comb <- cbind(inputA,inputB)
      return(apply(X = comb,FUN = min,MARGIN = 1))
    }
    
    fuzzy_norm <- function(input)
    {
      return(sum(abs(input)))
    }
    
    choice_function <- function(input, category_w, alpha)
    {
      Tj <- fuzzy_norm(fuzzy_and(input, category_w))/(alpha+fuzzy_norm(category_w))
      return(Tj)
    }
    
    match_function <- function(input, category_w)
    {
      Sj <- fuzzy_norm(fuzzy_and(input,category_w))/fuzzy_norm(input)
      return(Sj)
    }
    
    vigilance_check <- function(input, category_w, rho)
    {
      vcheck <- match_function(input,category_w) >= rho
      return(vcheck)
    }
    
    update_weight <- function(input, category_w, beta)
    {
      w_new <- beta*fuzzy_and(input,category_w) + (1-beta)*category_w
      return(w_new)
    }
    
    category_winner <- function(input, w, alpha, rho)
    {
      #w
      m = dim(w)[1] # number of clusters
      # matches = rep(0,m)
      # matches = apply(X = w,MARGIN = 1, FUN = function(weight){return(choice_function(input, weight, alpha))})
      for (j in 1:m) {
        matches[j] = choice_function(input, w[j,], alpha)
      }
      
      match_iterations = 0
      while (match_iterations<length(matches)) 
      {
        # select the winning category uses which.max
        winner = which.max(matches) #location of the first max value
        if(vigilance_check(input,w[winner,],rho))
        {
          # induce resonance
          return(winner)
        } else 
        {
          # failed to match, try another Tj
          matches[winner] <- 0
          match_iterations <- match_iterations+1
        }
      } 
      return(length(matches)+1)
    }
    
    learn_pattern <- function(input, w, rho, alpha, beta, num_clusters, max_cluster, 
                              output.learn = list(w = NA, num_clusters = NA, winner = NA))
    {
      # found the location of winning category 
      winner <- category_winner(input = input, w = w, alpha = alpha, rho = rho)
      
      if(winner > num_clusters)
      {
        # create a new category
        num_clusters <- num_clusters + 1
        
        # was it reached the maximum cluster count?
        if(num_clusters > max_cluster_count) stop("Error. The maximum cluster count has been reached!")
        
        w = rbind(w,input)
      }
      
      # update the old weight 
      w[winner,] <- update_weight(input, w[winner,], beta)
      
      output.learn$winner = winner
      output.learn$w = w
      output.learn$num_clusters = num_clusters
      return(output.learn)
    }
    
    
    
    
    
    ## Initialize the fakmct
    
    # normalize data input
    normalized_data <- normalize_data(input)
    
    # complement code conversion
    dataset <- complement_coded_conv(normalized_data)
    
    # determine the size of features/variables data
    num_features <- ncol(input)
    
    # init weight
    #w_init = NA
    w <- matrix(rep(1,num_features*2),nrow = 1)
    
    num_clusters <- dim(w)[1]
    
    fuzzyart_train <- function(input, w, rho, alpha, beta, num_clusters, max_cluster)
    {
      
      #init variables
      max_cluster = max_cluster
      num_clusters_old = num_clusters
      cluster_labels = rep(NA, dim(dataset)[1])
      iterations = 0
      w_old = w+1
      train_output_fa = list(w = NA, num_clusters = NA, winner = NA)
      
      ## Training Fuzzy ART neural network
      index = 1:dim(dataset)[1]
      while (condition) 
      {
        
      
        w_old = w
        for(i in index)
        {
        train_output_fa <- learn_pattern(dataset[i,], w, rho, alpha, beta, num_clusters, max_cluster, 
                                    output.learn =  train_output_fa)
        
        w <- train_output_fa$w
        num_clusters <- train_output_fa$num_clusters
        }
        iterations = iterations+1
        
        if(num_clusters_old != num_clusters)
        {
          w_old = rbind(w_old,replicate(n = num_features*2, rep(1,num_clusters-num_clusters_old)))
          num_clusters_old = num_clusters
        }
      }
      FuzzyArt_Out = list()
      FuzzyArt_Out[["labels"]]=cluster_labels
      FuzzyArt_Out[["weights"]]= w
      FuzzyArt_Out[["params"]] = params
      FuzzyArt_Out[["iterations"]] = iterations
      return(FuzzyArt_Out)
    }
  
  ## Clustering data with FAKMCT begin
  
  fuzzyart_test <- function(input, dataset, FuzzyArt_Out)
  {
    #if()
    labels = apply(dataset, MARGIN = 1,FUN = function(pattern){return(category_winner(pattern = pattern,w = FuzzyArt_Module$w,
                                                                                   alpha = FuzzyArt_Module$params$alpha, rho = FuzzyArt_Module$params$rho,
                                                                                   match_fn = FuzzyArt_Module$params$match_fn,
                                                                                   vigilance_fn = FuzzyArt_Module$params$vigilance_fn))})
    return(labels)
  }
  
  kmeans_train <- kmeans(input, k, max_cluster)
  {
    
  }
  
  
  

  
  
  
  
  
  
  
}