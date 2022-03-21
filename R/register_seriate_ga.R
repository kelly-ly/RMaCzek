.register_seriate_ga<-function(){
## KB edited for changes in seriation  
  if(!.my_check_is_ga_registered()){
 ## if(is.null(try(seriation::show_seriation_methods("dist")$dist_seriate_ga))){
##============================================    
    seriation::set_seriation_method(kind="dist",
                                    name=".seriate_ga",
                                    definition=.seriate_ga,
                                    description="Genetic Algorithm for permutation")

  }
}

## KB added for changes in seriation
.my_check_is_ga_registered<-function(){
    try(is.element(".seriate_ga",seriation::list_seriation_methods()$dist))
}
## ==============================================
