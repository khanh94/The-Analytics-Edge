
# After following the steps in the video, load the data into R
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")

str(movies)

#Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)

length(movies$Comedy)
length(movies$Western)

table(movies$Comedy)
table(movies$Western)

# Compute distances
distances = dist(movies[2:20], method = "euclidean")


# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

cluster2 = subset(movies, clusterGroups==2)

cluster2$Title[1:10]

