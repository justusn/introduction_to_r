#PROBLEM SUMMARY

#Output 1 : total_cost list
#Import tax rate : 10%
#Discount rate : $10 orders above $500


#Output 2 : Shipping charge
#Shipping first item : $4
#Shipping Subsequent item : $2

items_list <- list(49,345,70,100,34,56,11,200,657,340,240)

get_purchase_cost <- function(items_list) {
  total_purchase_cost <- list()
  j = 1
  for ( i in items_list){ 
    total_purchase_cost[j] = i * 1.1
    j<-j+1
  }
  return(total_purchase_cost)
}

#Calculation of shipping cost through iterating the list
get_shipping_cost <- function(items_list,sort_decreasing){
  items_list <- lapply(items_list,sort,decreasing = sort_decreasing)
  total_shipping_cost <- 0
  if (length(items_list)>0){
    total_shipping_cost <- 4
  }
  
  if (length(items_list)>1){
    for ( i in items_list){ 
      total_shipping_cost <- total_shipping_cost + 2
    }
    #Subtract cost applied on the first item
    total_shipping_cost <- total_shipping_cost-2
  }
  
  return(total_shipping_cost)

}

print(get_shipping_cost(items_list,FALSE))



#Alternative calculation of shipping cost using number of items
alt_get_shipping_cost <- function(items_list){
  return ((length(items_list)/length(items_list))*4 + (length(items_list)-1)*2 )
}

print(alt_get_shipping_cost(items_list))


print_total_costs <- function(items_list){
  print("Purchase Costs")
  total_items_costs <- 0
  purchase_costs = get_purchase_cost(items_list)
  j <- 1
  for (i in purchase_costs){
    print(paste("Item",j,":",i))
    total_items_costs = total_items_costs + i
    j <- j+1
  }
  print("")
  print(paste("Total Items Costs :","$",total_items_costs))
  
  shipping_costs <- get_shipping_cost(items_list,FALSE)
  
  total_costs <- shipping_costs + total_items_costs
  
  discounted_costs <- total_costs
  
  discount <- 0
  
  if(total_items_costs>500){
    discount <- 10
    discounted_costs <- total_costs - discount
  }
  
  print("")
  print(paste("Total Shipping COsts :","$",shipping_costs))
  print("")
  print(paste("Total Cost:","$",total_costs))
  print("")
  print(paste("Discount:","$",discount))
  print("")
  print(paste("Net Total:","$",discounted_costs))
}
  
print_total_costs(items_list)
