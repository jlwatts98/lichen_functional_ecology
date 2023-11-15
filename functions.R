##### Orginial functions for Lichen Functional Ecology #####

vpd = function(
        temp,
        RH
){
    
    vpd = 0.611 * 2.718^(17.502 * temp/(temp + 240.97)) * (1 - RH/100)
    return(vpd)
}