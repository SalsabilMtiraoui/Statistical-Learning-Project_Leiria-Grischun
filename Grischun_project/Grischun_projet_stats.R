#Figure 1.1 and 1.4

tableau = read.csv('/Users/paulmicheli/Desktop/Projet Stats/projet_stats.csv')

tableau_filtre = head(tableau[order(tableau$GDP, decreasing = TRUE), ], 30)

ylim_max = max(tableau_filtre$Total) + 10

barplot(tableau_filtre$Total, 
        names.arg = tableau_filtre$Country, 
        main = "Top 30 countries with the biggest GDP and their medals", 
        xlab = " ", 
        ylab = "Total of medals", 
        col = "blue", 
        las = 2,
        cex.names = 0.8,  
        ylim = c(0, ylim_max)) 

text(x = barplot(tableau_filtre$Total, plot = FALSE), 
     y = tableau_filtre$Total, 
     labels = tableau_filtre$Total, 
     pos = 3, 
     cex = 0.8, 
     col = "red")

################################################################################
#Figure 1.2
tableau = read.csv('/Users/paulmicheli/Desktop/Projet Stats/projet_stats.csv')

options(scipen = 999)

tableau_filtre = head(tableau[order(tableau$Total, decreasing = TRUE), ], 30)

ylim_max = 30

barplot(tableau_filtre$GDP, 
        names.arg = tableau_filtre$Country, 
        main = "Top 30 of the countries with the most medals and their GDP", 
        xlab = " ", 
        ylab = "GDP (in 1000 billions)", 
        col = "blue", 
        las = 2,
        cex.names = 0.8,  
        ylim = c(0, ylim_max))

legend("topright",  
       legend = c("number of medals"),
       pch = 20,  
       col = c("red"))  

text(x = barplot(tableau_filtre$GDP, plot = FALSE), 
     y = tableau_filtre$GDP, 
     labels = format(tableau_filtre$Total, scientific = FALSE), 
     pos = 3, 
     cex = 1, 
     col = "red")
################################################################################
#Figure 1.3

tableau = read.csv('/Users/paulmicheli/Desktop/Projet Stats/projet_stats.csv.csv')

tableau = tableau[order(-tableau$GDP), ]

top10 = tableau[1:10, ]

plot(tableau$GDP, tableau$Total, 
     main="Number of medals as a function of GDP (Logarithmic scale on X axis)", 
     xlab="GDP (in billions)[Log]", 
     ylab="Number of medals", 
     pch=21, 
     col="blue", 
     log="x")

text(top10$GDP, top10$Total, 
     labels = paste(top10$Country, " (Rank ", seq_along(top10$Country), ")", sep = ""), 
     pos = 2, 
     col = "black", 
     cex = 0.7)
################################################################################
#Figure 1.5

tableau = read.csv('/Users/paulmicheli/Desktop/Projet Stats/projet_stats.csv')

options(scipen = 999)

tableau_medailles = tableau[tableau$Total > 0, ]

tableau_filtre = head(tableau_medailles[order(tableau_medailles$GDP), ], 30)

ylim_max = max(tableau_filtre$Total) + 5

bp = barplot(tableau_filtre$Total, 
              names.arg = tableau_filtre$Country, 
              main = "Top 30 Countries with the Lowest GDP Winning Medals", 
              xlab = " ", 
              ylab = "Medals", 
              col = "blue", 
              las = 2,
              cex.names = 0.8,
              ylim = c(0, ylim_max))

text(x = bp, 
     y = tableau_filtre$Total + 1,  
     labels = tableau_filtre$Total, 
     pos = 3, 
     cex = 0.8, 
     col = "red")
################################################################################
#Figure 1.6
tableau = read.csv('/Users/paulmicheli/Desktop/Projet Stats/projet_stats.csv')

tableau = tableau[order(-tableau$Population), ]

top10 = tableau[1:10,]

plot(tableau$Population / 1e6, tableau$Total, 
     main="Number of medals as a function of Population", 
     xlab="Population (Millions)", 
     ylab="Number of medals", 
     pch=21, 
     col="blue", 
     log="y",
     axes=FALSE)

text(top10$Population / 1e6, top10$Total, 
     labels = paste(top10$Country, " (Rank ", seq_along(top10$Country), ")", sep = ""), 
     pos = 1, 
     col = "black", 
     cex = 0.8)

axis(1, at = seq(0, max(tableau$Population / 1e6), by = 10), 
     labels = seq(0, max(tableau$Population / 1e6), by = 10))

axis(2, las = 1, at = c(0, 10, 50, 100))