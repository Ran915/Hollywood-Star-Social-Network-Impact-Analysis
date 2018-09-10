library(igraph) 
library(network)
#load nodes data into workplace and explore the data
MoviePerson <- read.table("newmovies.txt",header = T,sep="\t",quote = "\"")
str(MoviePerson)
colnames(MoviePerson) <- c('Person_id',	'Name',	'WikiMentions',	'Role',	'Description')
#see whether there are missing values
MoviePerson[MoviePerson==""]  <- NA    
sapply(MoviePerson, function(x) sum(is.na(x)) )
#delete rows with wrong names and rows that are not in Actor-Director-Writer network
Wrong_name_List <- c('Actor','William Shakespeare','Ronald Reagan','George W. Bush','Writers Guild of America','50 Cent')
MoviePerson <- subset(MoviePerson,!(Name %in% Wrong_name_List)&Role!="movie")
MoviePerson$Role <- as.character(MoviePerson$Role)
MoviePerson$Role <- as.factor(MoviePerson$Role)
MoviePerson[1:20,]
#load edges data into workplace
relations <- read.table("edges.txt",header = F,sep="\t")
#see whether there are missing values
relations[relations==""]  <- NA    
sapply(relations, function(x) sum(is.na(x)) )
#delete edges that are not in Actor-Director-Writer network
id <- c(MoviePerson$Person_id)
relations <- subset(relations,V1 %in% id & V2 %in% id)

library(sna)
MoviePerson_name <- subset(MoviePerson,select = c(1,2))

relations_new <- merge(x=relations,y=MoviePerson_name,by.x='V1',by.y = 'Person_id')
relations_new <- merge(x=relations_new,y=MoviePerson_name,by.x='V2',by.y = 'Person_id')
rrelations <- subset(relations_new,select = c(5,4))
#relations_actors <- subset(relations_actors, select = c(1,2))
net=network(rrelations,directed = FALSE)

# party affiliation
x = data.frame(Name = network.vertex.names(net))
x = merge(x, MoviePerson, by = "Name", sort = FALSE)$Role
net %v% "role" = as.character(x)

# color palette
y = RColorBrewer::brewer.pal(3, "Set1")[ c(3, 1, 2) ]
names(y) = levels(x)

# network plot
library(GGally)
library(scales)
ggnet2(net, color = "role", palette = y, alpha = 0.75, size = 4, edge.alpha = 0.5)

#This is a list of oscar winning actors/actress who are considered to be very successful
#The list is selected from the List of Oscar Academy Award for Best Actor&Actress
oscar_winner_list <- c('Emma Stone','Julia Roberts','Julianne Moore',
                       'Cate Blanchett','Rachel Weisz','Meryl Streep',
                       'Natalie Portman','Nicole Kidman','Kate Winslet',
                       'Charlize Theron','Casey Affleck','Matthew McConaughey',
                       'Colin Firth','Leonardo DiCaprio','Tom Hanks',
                       'Brad Pitt','Christian Bale','Adrien Brody',
                       'Jeff Bridges','Robin Williams')

#This is a list of other stars who are not oscar winners but are also famous and have large fan base.
#The list is selected from IMDB hollywood A-listers popularity data
famous_actor_list <- c('Scarlett Johansson','Marilyn Monroe','Jessica Alba',
                       'Jennifer Aniston','Keira Knightley','Ian McKellen',
                       'Mark Ruffalo','Orlando Bloom','James Franco','Ryan Gosling')


### 1.Analyze Actor-Actor relationship
detach(package:sna)
actors <- subset(MoviePerson,Role=="starring")
id_actors <- c(actors$Person_id)
relations_actors <- subset(relations,V1 %in% id_actors & V2 %in% id_actors & V1!=V2)
actors_full <- graph.data.frame(relations_actors) 
actors_full <- as.undirected(actors_full, mode='collapse')
summary(actors_full)
#get actors inner network vertex id
degree_actors <- degree(actors_full)
actors_vertex_id <- as.numeric(rownames(as.data.frame(degree_actors)))
#calculate centrality
#degree centrality
deg_centr_actors <- degree(actors_full)
#closeness centrality
clo_centr_actors <- closeness(actors_full)
#betweenness centrality
bet_centr_actors <- betweenness(actors_full)
#combine into a data frame
actors_name <- subset(actors,select = c(Person_id,Name))
actors_centr <- as.data.frame(cbind(actors_vertex_id,deg_centr_actors,clo_centr_actors,bet_centr_actors))
actors_centr <- merge(x=actors_centr,y=actors_name,by.x="actors_vertex_id",by.y = "Person_id")
actors_centr <- actors_centr[,c('actors_vertex_id','Name','deg_centr_actors','clo_centr_actors','bet_centr_actors')]
#rank by centrality
actors_deg_rank <- actors_centr[order(actors_centr$deg_centr_actors,decreasing = T),]
actors_deg_rank['deg_score'] <- c(nrow(actors_centr):1)
actors_clo_rank <- actors_deg_rank[order(actors_deg_rank$clo_centr_actors,decreasing = T),]
actors_clo_rank['clo_score'] <- c(nrow(actors_centr):1)
actors_bet_rank <- actors_clo_rank[order(actors_clo_rank$bet_centr_actors,decreasing = T),]
actors_bet_rank['bet_score'] <- c(nrow(actors_centr):1)
actors_bet_rank$OverallScore <- actors_bet_rank$deg_score+actors_bet_rank$clo_score+actors_bet_rank$bet_score
actors_rank <- actors_bet_rank[order(actors_bet_rank$OverallScore,decreasing = T),]
actors_rank['rank'] <- c(1:nrow(actors_centr))
#rank of most sociable oscar winners
oscar_actor_rank <- subset(actors_rank,Name %in% oscar_winner_list)
oscar_actor_rank
summary(oscar_actor_rank)
#rank of most sociable famous stars
famous_actor_rank <- subset(actors_rank,Name %in% famous_actor_list)
famous_actor_rank
summary(famous_actor_rank)
#rank of all the actors/actress
actors_rank[1:20,]
summary(actors_rank)


### 2.Analyze Actor-Director relationship
ActDir <- subset(MoviePerson,Role=="starring"|Role=="director")
directors <- subset(MoviePerson,Role=="director")
id_directors <- c(directors$Person_id)
#only relations between actors and directors, we omit relations between actors and actors 
# and relations between directors and directors
relations_ActDir_only <- subset(relations,(V1 %in% id_actors & V2 %in% id_directors) | (V1 %in% id_directors & V2 %in% id_actors))
ActDir_only <- graph.data.frame(relations_ActDir_only) 
ActDir_only <- as.undirected(ActDir_only, mode='collapse')
summary(ActDir_only)
#degree centrality and vertex id
degree_ActDir_only <- degree(ActDir_only)
ActDir_only_vertex_id <- as.numeric(rownames(as.data.frame(degree_ActDir_only)))
#combine into a data frame
ActDir_name <- subset(ActDir,select = c(Person_id,Name,Role))
ActDir_degree <- as.data.frame(cbind(ActDir_only_vertex_id,degree_ActDir_only))
ActDir_degree <- merge(x=ActDir_degree,y=ActDir_name,by.x="ActDir_only_vertex_id",by.y = "Person_id")
ActDir_degree <- ActDir_degree[,c('ActDir_only_vertex_id','Name','Role','degree_ActDir_only')]
ActDir_degree.actors <- subset(ActDir_degree,Role=='starring')
#A rank of actors that know most directors
ActDir.actors_deg_rank <- ActDir_degree.actors[order(ActDir_degree.actors$degree_ActDir_only ,decreasing = T),]
ActDir.actors_deg_rank['rank'] <- c(1:nrow(ActDir.actors_deg_rank))
#rank of oscar winners who know most directors
oscar_actor_dir_rank <- subset(ActDir.actors_deg_rank,Name %in% oscar_winner_list)
summary(oscar_actor_dir_rank)
oscar_actor_dir_rank
#rank of famous stars who know most directors
famous_actor_dir_rank <- subset(ActDir.actors_deg_rank,Name %in% famous_actor_list)
summary(famous_actor_dir_rank)
famous_actor_dir_rank
#rank of all the actors who know most directors
summary(ActDir.actors_deg_rank)
ActDir.actors_deg_rank[1:20,]

### 3.Analyze Actor-Writer relationship
ActWri <- subset(MoviePerson,Role=="starring"|Role=="writer")
writers <- subset(MoviePerson,Role=="writer")
id_writers <- c(writers$Person_id)
#only relations between actors and writers
relations_ActWri_only <- subset(relations,(V1 %in% id_actors & V2 %in% id_writers) | (V1 %in% id_writers & V2 %in% id_actors))
ActWri_only <- graph.data.frame(relations_ActWri_only) 
ActWri_only <- as.undirected(ActWri_only, mode='collapse')
summary(ActWri_only)
#degree centrality and vertex id
degree_ActWri_only <- degree(ActWri_only)
ActWri_only_vertex_id <- as.numeric(rownames(as.data.frame(degree_ActWri_only)))
#combine into a data frame
ActWri_name <- subset(ActWri,select = c(Person_id,Name,Role))
ActWri_degree <- as.data.frame(cbind(ActWri_only_vertex_id,degree_ActWri_only))
ActWri_degree <- merge(x=ActWri_degree,y=ActWri_name,by.x="ActWri_only_vertex_id",by.y = "Person_id")
ActWri_degree <- ActWri_degree[,c('ActWri_only_vertex_id','Name','Role','degree_ActWri_only')]
ActWri_degree.actors <- subset(ActWri_degree,Role=='starring')
#A rank of actors that know most writers
ActWri.actors_deg_rank <- ActWri_degree.actors[order(ActWri_degree.actors$degree_ActWri_only ,decreasing = T),]
ActWri.actors_deg_rank['rank'] <- c(1:nrow(ActWri.actors_deg_rank))
#rank of oscar winners who know most writers
oscar_actor_wri_rank <- subset(ActWri.actors_deg_rank,Name %in% oscar_winner_list)
summary(oscar_actor_wri_rank)
oscar_actor_wri_rank
#rank of famous actors/actress who know most writers
famous_actor_wri_rank <- subset(ActWri.actors_deg_rank,Name %in% famous_actor_list)
summary(famous_actor_wri_rank)
famous_actor_wri_rank
#rank of all actors/actress who know most writers
summary(ActWri.actors_deg_rank)
ActWri.actors_deg_rank[1:20,]

###4.Analyze directors network
relations_directors <- subset(relations,V1 %in% id_directors | V2 %in% id_directors & V1!=V2)
directors_full <- graph.data.frame(relations_directors)
directors_full <- as.undirected(directors_full, mode='collapse')
summary(directors_full)
#get directors network vertex id
degree_directors <- degree(directors_full)
directors_vertex_id <- as.numeric(rownames(as.data.frame(degree_directors)))
#calculate centrality
#degree centrality
deg_centr_directors <- degree(directors_full)
#closeness centrality
clo_centr_directors <- closeness(directors_full)
#betweenness centrality
bet_centr_directors <- betweenness(directors_full)
#combine into a data frame
name <- subset(MoviePerson,select = c(Person_id,Name,Role))
directors_centr <- as.data.frame(cbind(directors_vertex_id,deg_centr_directors,clo_centr_directors,bet_centr_directors))
directors_centr <- merge(x=directors_centr,y=name,by.x="directors_vertex_id",by.y = "Person_id")
directors_centr <- directors_centr[,c('directors_vertex_id','Name','Role','deg_centr_directors','clo_centr_directors','bet_centr_directors')]
directors_centr <- subset(directors_centr,Role=='director')
#rank by centrality
directors_deg_rank <- directors_centr[order(directors_centr$deg_centr_directors,decreasing = T),]
directors_deg_rank['deg_score'] <- c(nrow(directors_centr):1)
directors_clo_rank <- directors_deg_rank[order(directors_deg_rank$clo_centr_directors,decreasing = T),]
directors_clo_rank['clo_score'] <- c(nrow(directors_centr):1)
directors_bet_rank <- directors_clo_rank[order(directors_clo_rank$bet_centr_directors,decreasing = T),]
directors_bet_rank['bet_score'] <- c(nrow(directors_centr):1)
directors_bet_rank$OverallScore <- directors_bet_rank$deg_score+directors_bet_rank$clo_score+directors_bet_rank$bet_score
directors_rank <- directors_bet_rank[order(directors_bet_rank$OverallScore,decreasing = T),]
directors_rank['rank'] <- c(1:nrow(directors_centr))
sociable_directors <- directors_rank[0:100,]
sociable_directors_list <- as.character(sociable_directors$Name)
sociable_directors_id <- as.numeric(sociable_directors$directors_vertex_id)
sociable_directors[1:20,]

###5.Analyze writers network
relations_writers <- subset(relations,V1 %in% id_writers | V2 %in% id_writers & V1!=V2)
writers_full <- graph.data.frame(relations_writers)
writers_full <- as.undirected(writers_full, mode='collapse')
summary(writers_full)
#get writers network vertex id
degree_writers <- degree(writers_full)
writers_vertex_id <- as.numeric(rownames(as.data.frame(degree_writers)))
#calculate centrality
#degree centrality
deg_centr_writers <- degree(writers_full)
#closeness centrality
clo_centr_writers <- closeness(writers_full)
#betweenness centrality
bet_centr_writers <- betweenness(writers_full)
#combine into a data frame
name <- subset(MoviePerson,select = c(Person_id,Name,Role))
writers_centr <- as.data.frame(cbind(writers_vertex_id,deg_centr_writers,clo_centr_writers,bet_centr_writers))
writers_centr <- merge(x=writers_centr,y=name,by.x="writers_vertex_id",by.y = "Person_id")
writers_centr <- writers_centr[,c('writers_vertex_id','Name','Role','deg_centr_writers','clo_centr_writers','bet_centr_writers')]
writers_centr <- subset(writers_centr,Role=='writer')
#rank by centrality
writers_deg_rank <- writers_centr[order(writers_centr$deg_centr_writers,decreasing = T),]
writers_deg_rank['deg_score'] <- c(nrow(writers_centr):1)
writers_clo_rank <- writers_deg_rank[order(writers_deg_rank$clo_centr_writers,decreasing = T),]
writers_clo_rank['clo_score'] <- c(nrow(writers_centr):1)
writers_bet_rank <- writers_clo_rank[order(writers_clo_rank$bet_centr_writers,decreasing = T),]
writers_bet_rank['bet_score'] <- c(nrow(writers_centr):1)
writers_bet_rank$OverallScore <- writers_bet_rank$deg_score+writers_bet_rank$clo_score+writers_bet_rank$bet_score
writers_rank <- writers_bet_rank[order(writers_bet_rank$OverallScore,decreasing = T),]
writers_rank['rank'] <- c(1:nrow(writers_centr))
sociable_writers <- writers_rank[0:100,]
sociable_writers_list <- as.character(sociable_writers$Name)
sociable_writers_id <- as.numeric(sociable_writers$writers_vertex_id)
sociable_writers[1:20,]

###6.Number of important directors and writers oscar winners and famous stars know
oscar_actor_id <- as.numeric(oscar_actor_rank$actors_vertex_id)
oscar_actor_name <- subset(oscar_actor_rank,select=c(actors_vertex_id,Name))
##number of sociable directors oscar winners know
oscar_actor_Auth_Dir <- subset(relations,(V1 %in% oscar_actor_id & V2 %in% sociable_directors_id) |
                                 (V2 %in% oscar_actor_id & V1 %in% sociable_directors_id))
oscar_actor_Auth_Dir_full <- graph.data.frame(oscar_actor_Auth_Dir)
oscar_actor_Auth_Dir_full <- as.undirected(oscar_actor_Auth_Dir_full, mode='collapse')
summary(oscar_actor_Auth_Dir_full)
degree_oscar_actor_Auth_Dir <- degree(oscar_actor_Auth_Dir_full)

oscar_actor_Auth_Dir_vertex_id <- as.numeric(rownames(as.data.frame(degree_oscar_actor_Auth_Dir)))
oscar_actor_Auth_Dir_degree <- as.data.frame(cbind(oscar_actor_Auth_Dir_vertex_id,degree_oscar_actor_Auth_Dir))
oscar_actor_Auth_Dir_degree <- subset(oscar_actor_Auth_Dir_degree,oscar_actor_Auth_Dir_vertex_id %in% oscar_actor_id)
oscar_actor_Auth_Dir_degree <- merge(x=oscar_actor_Auth_Dir_degree,y=oscar_actor_name,
                                     by.x='oscar_actor_Auth_Dir_vertex_id',by.y = 'actors_vertex_id')
oscar_actor_Auth_Dir_degree <- oscar_actor_Auth_Dir_degree[order(oscar_actor_Auth_Dir_degree$degree_oscar_actor_Auth_Dir,decreasing = T),]
##mean authoritative directors oscar winners know
sum(oscar_actor_Auth_Dir_degree$degree_oscar_actor_Auth_Dir)/20
##number of sociable writers oscar winners know
oscar_actor_Auth_Wri <- subset(relations,(V1 %in% oscar_actor_id & V2 %in% sociable_writers_id)                                |(V2 %in% oscar_actor_id & V1 %in% sociable_writers_id))
oscar_actor_Auth_Wri_full <- graph.data.frame(oscar_actor_Auth_Wri)
oscar_actor_Auth_Wri_full <- as.undirected(oscar_actor_Auth_Wri_full, mode='collapse')
summary(oscar_actor_Auth_Wri_full)
degree_oscar_actor_Auth_Wri <- degree(oscar_actor_Auth_Wri_full)

oscar_actor_Auth_Wri_vertex_id <- as.numeric(rownames(as.data.frame(degree_oscar_actor_Auth_Wri)))
oscar_actor_Auth_Wri_degree <- as.data.frame(cbind(oscar_actor_Auth_Wri_vertex_id,degree_oscar_actor_Auth_Wri))
oscar_actor_Auth_Wri_degree <- subset(oscar_actor_Auth_Wri_degree,oscar_actor_Auth_Wri_vertex_id %in% oscar_actor_id)
oscar_actor_Auth_Wri_degree <- merge(x=oscar_actor_Auth_Wri_degree,y=oscar_actor_name,
                                     by.x='oscar_actor_Auth_Wri_vertex_id',by.y = 'actors_vertex_id')
oscar_actor_Auth_Wri_degree <- oscar_actor_Auth_Wri_degree[order(oscar_actor_Auth_Wri_degree$degree_oscar_actor_Auth_Wri,decreasing = T),]
##mean authoritative writers oscar winners know
sum(oscar_actor_Auth_Wri_degree$degree_oscar_actor_Auth_Wri)/20
famous_actor_id <- as.numeric(famous_actor_rank$actors_vertex_id)
famous_actor_name <- subset(famous_actor_rank,select=c(actors_vertex_id,Name))
##number of sociable directors famous actors know
famous_actor_Auth_Dir <- subset(relations,(V1 %in% famous_actor_id & V2 %in% sociable_directors_id) |
                                  (V2 %in% famous_actor_id & V1 %in% sociable_directors_id))
famous_actor_Auth_Dir_full <- graph.data.frame(famous_actor_Auth_Dir)
famous_actor_Auth_Dir_full <- as.undirected(famous_actor_Auth_Dir_full, mode='collapse')
summary(famous_actor_Auth_Dir_full)
degree_famous_actor_Auth_Dir <- degree(famous_actor_Auth_Dir_full)

famous_actor_Auth_Dir_vertex_id <- as.numeric(rownames(as.data.frame(degree_famous_actor_Auth_Dir)))
famous_actor_Auth_Dir_degree <- as.data.frame(cbind(famous_actor_Auth_Dir_vertex_id,degree_famous_actor_Auth_Dir))
famous_actor_Auth_Dir_degree <- subset(famous_actor_Auth_Dir_degree,famous_actor_Auth_Dir_vertex_id %in% famous_actor_id)
famous_actor_Auth_Dir_degree <- merge(x=famous_actor_Auth_Dir_degree,y=famous_actor_name,
                                      by.x='famous_actor_Auth_Dir_vertex_id',by.y = 'actors_vertex_id')
famous_actor_Auth_Dir_degree <- famous_actor_Auth_Dir_degree[order(famous_actor_Auth_Dir_degree$degree_famous_actor_Auth_Dir,decreasing = T),]
##mean authoritative directors famous actors know
sum(famous_actor_Auth_Dir_degree$degree_famous_actor_Auth_Dir)/10
##number of sociable writers oscar winners know
famous_actor_Auth_Wri <- subset(relations,(V1 %in% famous_actor_id & V2 %in% sociable_writers_id) |
                                  (V2 %in% famous_actor_id & V1 %in% sociable_writers_id))
famous_actor_Auth_Wri_full <- graph.data.frame(famous_actor_Auth_Wri)
famous_actor_Auth_Wri_full <- as.undirected(famous_actor_Auth_Wri_full, mode='collapse')
summary(famous_actor_Auth_Wri_full)
degree_famous_actor_Auth_Wri <- degree(famous_actor_Auth_Wri_full)

famous_actor_Auth_Wri_vertex_id <- as.numeric(rownames(as.data.frame(degree_famous_actor_Auth_Wri)))
famous_actor_Auth_Wri_degree <- as.data.frame(cbind(famous_actor_Auth_Wri_vertex_id,degree_famous_actor_Auth_Wri))
famous_actor_Auth_Wri_degree <- subset(famous_actor_Auth_Wri_degree,famous_actor_Auth_Wri_vertex_id %in% famous_actor_id)
famous_actor_Auth_Wri_degree <- merge(x=famous_actor_Auth_Wri_degree,y=famous_actor_name,
                                      by.x='famous_actor_Auth_Wri_vertex_id',by.y = 'actors_vertex_id')
famous_actor_Auth_Wri_degree <- famous_actor_Auth_Wri_degree[order(famous_actor_Auth_Wri_degree$degree_famous_actor_Auth_Wri,decreasing = T),]
##mean authoritative writers famous actors know
sum(famous_actor_Auth_Wri_degree$degree_famous_actor_Auth_Wri)/10
##mean authoritative directors all the actors know
all_dir <- subset(relations,(V1 %in% sociable_directors_id)|(V2 %in% sociable_directors_id))
all_dir_full <- graph.data.frame(all_dir)
all_dir_full <- as.undirected(all_dir_full, mode='collapse')
all_dir.degree <- degree(all_dir_full)
degree.all_dir <- as.numeric(all_dir.degree)
sum(degree.all_dir)/nrow(actors)
##mean authoritative writers all the actors know
all_wri <- subset(relations,(V1 %in% sociable_writers_id)|(V2 %in% sociable_writers_id))
all_wri_full <- graph.data.frame(all_wri)
all_wri_full <- as.undirected(all_wri_full, mode='collapse')
all_wri.degree <- degree(all_wri_full)
degree.all_wri <- as.numeric(all_wri.degree)
sum(degree.all_wri)/nrow(actors)