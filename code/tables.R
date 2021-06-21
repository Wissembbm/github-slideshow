#This script contain the functions which permit to obtain the different tables for the anlysis

#Variables
#l :List of gas station for a year. It comes from the open data site of government
#no: Position of the gas station in the list
#type_gas: Type of gasoline


#Function 1: Price_update
#This function allow to consider a gas station and extract the different changes in price for this station for a year
#It returns a data frame with the different prices and the date and time of updating

Price_update = function(l,no,type_gas="Gazole"){
  
  base = NULL
  prix=list()
  date=list()
  nom=list()
  j=0
  for(i in 1:length(l[[no]])){
    v=names(l[[no]])
    if(!is.null(v[i])){
      if(v[i]=="prix"){
        j=j+1
        date[[j]]=as.character(l[[no]][[i]]["maj"])
        prix[[j]]=as.character(l[[no]][[i]]["valeur"])
        nom[[j]]=as.character(l[[no]][[i]]["nom"])
      }}
  }
  id=which(unlist(nom)==type_gas) 
  n=length(id) #Pour une station, le nombre de fois que le prix de type_gas a été modifié
  if (n>0){
    jour=function(j) as.Date(substr(date[[id[j]]],1,10),"%Y-%m-%d")
    jour_heure=function(j) as.POSIXct(substr(date[[id[j]]],1,19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ext_h=function(j) substr(date[[id[j]]],12,13)
    ext_mn=function(j) substr(date[[id[j]]],15,16)
    ext_heure =function(j) substr(date[[id[j]]],12,16)
    ext_date = function(j) substr(date[[id[j]]],1,10)
    prix_essence=function(i) as.numeric(prix[[id[i]]])/1000
    base1=data.frame(indice=no,
                     idpdv=l[[no]]$.attrs["id"], 
                     Date = unlist(lapply(1:n,ext_date)),
                     heure = unlist(lapply(1:n,ext_heure)),
                     H=unlist(lapply(1:n,ext_h)),
                     MN=unlist(lapply(1:n,ext_mn)),
                     prix=unlist(lapply(1:n,prix_essence)))
    base1=base1[!is.na(base1$prix),]
  }
  base =rbind(base,base1)
  return(base)
}

#Function 2 : spatial_year
#This function returns a data frame with the annual average price per station
#It also contains information about the stations for a given year

spatial_year<-function(l,type_gas="Gazole"){
  base=NULL
  services = c("Automate CB","Laverie","Toilettes publiques","Boutique alimentaire"
               ,"Boutique non alimentaire","Restauration à emporter","Restauration sur place",
               "Relais colis","Vente de gaz domestique","Vente de fioul domestique","Vente de pétrole lampant",
               "Carburant qualité supérieure", "GPL","Station de gonflage", "Station de lavage", "Lavage multi-programmes",
               "Lavage haute-pression","Baie de service auto","Piste poids lourds","Location de véhicule","")
  for(no in 1:length(l)){  
    h<-0 #Autoroute
    price<-list()
    nom<-list()
    prix<-0
    j=0
    for(i in 1:length(l[[no]])){
      v=names(l[[no]])
      if(!is.null(v[i])){
        if(v[i]=="prix"){
          j=j+1
          price[[j]]=as.character(l[[no]][[i]]["valeur"])
          nom[[j]]=as.character(l[[no]][[i]]["nom"])
        }}
    }
    id=which(unlist(nom)==type_gas) 
    n=length(id) #Pour une station, le nombre de fois que le prix de type_gas a été modifié
    if (n>0){
      for (i in 1:n) {
        prix<- prix + as.numeric(price[[id[i]]])/1000
      }
      prix<-prix /n
    }
    else{prix=NA}
    if(l[[no]]$.attrs["pop"]=="A") h=1
    
    base1=data.frame(idpdv= l[[no]]$.attrs["id"],
                     lat=as.numeric(l[[no]]$.attrs["latitude"])/100000,
                     lon=as.numeric(l[[no]]$.attrs["longitude"])/100000,
                     cp=l[[no]]$.attrs["cp"],
                     adresse=l[[no]]$adresse, 
                     ville=l[[no]]$ville,
                     highway=h,
                     automate_CB=0,
                     laverie=0,
                     toilettes_publiques=0,
                     boutique_alim=0,
                     boutique_non_alim=0,
                     rest_emporter=0,
                     rest_surpl=0,
                     relais_colis=0,
                     vente_gaz=0,
                     vente_fioul=0,
                     vente_petlampant=0,
                     carb_qual=0,
                     gpl=0,
                     gonflage=0,
                     lavage=0,
                     lavage_multi=0,
                     lavage_hp =0,
                     bais_servs =0,
                     piste_poidsl=0,
                     location_vehic =0,
                     Prix=prix)
    if(!is.null(l[[no]]$services)){
      for (i in 1:length(l[[no]]$services)) {
        for (j in 1:length(services)) {
          if(l[[no]]$services[i]==services[j])
          {
            base1[1,7+j]=1 }
        }
      }
    }
    base=rbind(base,base1)
  }
return(base)}


#Function 3: fermeture
#This function returns for a given year the information on the stations having known a closure 

fermeture <- function(l){
  base=NULL
  for(no in 1:length(l)){ 
    if(!is.null(l[[no]]$fermeture )){
      year <- substr(l[[no]]$fermeture["debut"],1,4)
      base1=data.frame(idpdv= l[[no]]$.attrs["id"],
                       type = l[[no]]$fermeture["type"],
                       debutF= l[[no]]$fermeture["debut"],
                       finF = l[[no]]$fermeture["fin"],
                       Annee =year )
      base=rbind(base,base1)}
  }
  return(base)
}


#Function 4: stock
#This function returns for a given year the information on the stations having known a stock shortage 

stock <- function(l){
  base=NULL
  for(no in 1:length(l)){  
    if(!is.null(l[[no]]$rupture)){
      base1=data.frame(idpdv= l[[no]]$.attrs["id"],
                       nom = as.character(l[[no]]$rupture["nom"]),
                       debutR= l[[no]]$rupture["debut"],
                       finR = l[[no]]$rupture["fin"],
                       heure_debut = substr(l[[no]]$rupture["debut"],12,19))
      base=rbind(base,base1)}
  }
  return(base)
}
