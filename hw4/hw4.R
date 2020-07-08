rm(list = ls())
ckm_nodes <- read.csv("data/ckm_nodes.csv")
ckm_network <- read.table("data/ckm_network.dat")
# 1
valid_nodes <- !is.na(ckm_nodes$adoption_date)
ckm_network <- ckm_network[valid_nodes, valid_nodes]
ckm_nodes <- ckm_nodes[valid_nodes, ]

# 2
doc.info <- data.frame("doctor" = rep(seq(1, 125), each = 17),
                       "month" = rep(seq(1, 17), time = 125))
doc.info$begin <- ckm_nodes$adoption_date[doc.info$doctor] == doc.info$month
doc.info$before <- ckm_nodes$adoption_date[doc.info$doctor] <= doc.info$month
# Try not to use any loops
for (person in 1:125) {
  contacts <- which(ckm_network[person, ] == TRUE)
  for (month in 1:17) {
    doc.info$num_strict_before[17*(person-1)+month] <-
      sum(ckm_nodes$adoption_date[contacts] < month)
    doc.info$num_begin_before[17*(person-1)+month] <-
      sum(ckm_nodes$adoption_date[contacts] <= month)
  }
}
