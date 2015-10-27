data <- read.table("~/Projekte/Bachelor/paper/paper/sequncing_costs_data.txt", sep = " ", header = T, stringsAsFactors = F)

dates <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1',x=data$Date)

costs <- data$Cost_per_Genome
plot(costs, xaxt = "n", type="o", col="blue",log="y",
     main="Sequenzierungskosten pro Genom",
     xlab="Jahre",
     ylab="Kosten in $")
axis(1,at = 1:length(data$Date),labels=data$Date)

moor <- rep(1,length(data$Cost_per_Mb))
moor[1] <- data$Cost_per_Genome[1]
moor[2] <- moor[1]*exp( -((log(0.5))/(-12))* ( 6 ))
moor[3] <- moor[1]*exp( -((log(0.5))/(-12))* ( 12 ))
moor[4] <- moor[1]*exp( -((log(0.5))/(-12))* ( 18 ))
moor[5] <- moor[1]*exp( -((log(0.5))/(-12))* ( 25 ))

for(i in 6:52){ 
  moor[i] <- moor[1]*exp( -((log(0.5))/(-12))* (25 + (i-5)*3 ))
}

points(moor, type = "o",xaxt = "n")


plot(log(costs), xaxt = "n", type="o", col="blue")
points(log(moor), type = "o",xaxt = "n")
