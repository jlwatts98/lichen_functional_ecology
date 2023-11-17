##### Orginial functions for Lichen Functional Ecology #####

vpd = function(
        temp,
        RH
){
    
    vpd = 0.611 * 2.718^(17.502 * temp/(temp + 240.97)) * (1 - RH/100)
    return(vpd)
}

northing = function(
        aspect
){
    if (aspect > 180) {
        northing = aspect - 270
    } else
    {
        northing = 90 - aspect
    }
    return(northing)
}

easting = function(
        aspect
){
    if (aspect < 180) {
        easting = 90 - abs(90 - aspect)
    } else
    {
        easting = (90 - abs(270 - aspect)) * -1
    }
    return(easting)
}
