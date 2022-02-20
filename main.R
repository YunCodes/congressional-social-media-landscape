# GR5018 Adv Analytic Techniques
# Prof. Greg Eirich
# Assignment #1 
# Yun Choi


data_path <- "https://raw.githubusercontent.com/pablobarbera/data-science-workshop/master/sna/data/"
congress_twitter_network_edges <- read_csv(data_path, "congress-twitter-network-edges.csv")
congress_twitter_network_nodes <- read_csv(data_path, "congress-twitter-network-nodes.csv")




# 
congress_twitter_network_edges <- congress_twitter_network_edges %>%
  rename(id_1 = source, id_2 = target) %>%
  mutate(id_1 = as.character(id_1), id_2 = as.character(id_2))

congress_twitter_network_nodes <- congress_twitter_network_nodes %>%
  rename(id = id_str) %>%
  mutate(id = as.character(id),
         followers_amount = 
           case_when(followers_count <= 5617 ~ "top_quarter",
                     followers_count > 5617 & followers_count <= 14773 ~ "medium",
                     TRUE ~ "bottom_quarter"))

all_edges_with_attributes <- merge(congress_twitter_network_edges, 
                                   setNames(congress_twitter_network_nodes, 
                                            paste0(names(congress_twitter_network_nodes),'_1')),
                                   by = "id_1")

all_edges_with_attributes <- merge(all_edges_with_attributes, 
                                   setNames(congress_twitter_network_nodes, 
                                            paste0(names(congress_twitter_network_nodes),'_2')),
                                   by = "id_2")

all_edges_with_attributes <- all_edges_with_attributes %>%
  mutate(homo_gender = if_else(gender_1 == gender_2, 1, 0),
         homo_chamber = if_else(chamber_1 == chamber_2, 1, 0),
         homo_party = if_else(party_1 == party_2, 1, 0),
         homo_follower_amount = if_else(followers_amount_1 == followers_amount_2, 1, 0))

homophily_columns <- paste0("homo_", c("gender", "chamber", "party", "follower_amount"))

ego_degree_stats <- congress_twitter_network_edges %>% 
  distinct() %>%
  group_by(id_1) %>%
  summarise(degree = n())

ego_homophily_stats <- aggregate(all_edges_with_attributes[, homophily_columns], 
                                 by=list(id_1=all_edges_with_attributes$id_1), FUN=mean, na.rm=TRUE)

edge_density(graph.edgelist(as.matrix(congress_twitter_network_edges), directed = FALSE))


graph <- graph_from_edgelist(as.matrix(congress_twitter_network_edges), directed = FALSE)
densities <- unlist(lapply(graph, graph.density))
densities <- unlist(densities)

# Merge these data back with the original attributes
congress_twitter_network_nodes_attributes <- 
  merge(congress_twitter_network_nodes, ego_homophily_stats, by.x="id", by.y="id_1") %>%
  merge(ego_degree_stats, by.x="id", by.y="id_1")

model_1 <- lm(followers_count ~ as.factor(party) + as.factor(chamber) + as.factor(gender) + degree, 
              data = congress_twitter_network_nodes_attributes)
model_2 <- lm(homo_gender ~ as.factor(party) + as.factor(chamber) + as.factor(gender) + followers_count + degree,
              data = congress_twitter_network_nodes_attributes)
model_3 <- lm(homo_chamber ~ as.factor(party) + as.factor(chamber) + as.factor(gender) + degree,
              data = congress_twitter_network_nodes_attributes)

summary(model_1)
summary(model_2)
summary(model_3)
