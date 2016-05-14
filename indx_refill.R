#a sample script to check the missing indices and replacing them
#this was to solve a problem caused when calculating prevalences for all patches
#situation arises when there's no one on a patch, the prevalence is not calculated
#these script regrow the vector so that the patch indices are restored!

#another walkaround to this problem is to transform the prevalence into a table by cbinding/rbinding with the respective patch index


total.patch <- 16
test <-c(3,5,10)

absent <- which(1:total.patch %in% test)

putback <- which(!(1:total.patch %in% test))

for(k in 0:(length(putback)-1)){
absent <- append(absent,0,after=putback[k+1]-1)
}
absent
