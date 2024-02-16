# Sarah Gravel
# 00 Data pre-processing

# Creating supertree 
# Pruning supertree to create phylogenies for models

# Note: requires packages in script 01a and output from script 00d-f 

# Supertree: ----

# Supertree constructed from 1) a subset of our chondrichthyan species, 2) a whole chondrichthyan chronogram, and 3) a whole Actinopterygii chronogram  

## Teleost "all-taxon" chronograms from https://fishtreeoflife.org (Rabosky et al 2018):
# bigFishTree_chrono <- ape::read.tree("data/phylogenies/actinopt_list_full.trees") # = list of 100 trees
# random_t_tree <- sample(bigFishTree_chrono,size=1)[[1]] # select random tree
# is.ultrametric(random_t_tree) # not ultrametric
# is.ultrametric(random_t_tree, tol = 0.00001) # actually is, but due to numerical precision issues, need to use phytools::force.ultrametric:
# randoT <- force.ultrametric(random_t_tree, method="extend")
# is.ultrametric(randoT) # yes
# write.tree(randoT, file = "data/phylogenies/actinopt_full_ultrametric.tre")

# Load full teleost tree:
teleostFullChrono <- read.tree("data/phylogenies/actinopt_full_ultrametric.tre")

## Chondrichthyan chronograms and subsets from: https://vertlife.org/sharktree/ (Stein et al 2018):
# chondFull <- read.nexus("data/phylogenies/chondro_list_full.nex")
# randomChondroTree <- sample(chondFull, size = 1)[[1]]
# is.ultrametric(randomChondroTree)
# is.binary(randomChondroTree)
# which(chondroFullChrono$tip.label == "Raja_eglanteria")
# which(chondroFullChrono$tip.label == "Triakis_semifasciata")
# which(chondroFullChrono$tip.label == "Cephaloscyllium_ventriosum")
# write.tree(randomChondroTree, file = "data/phylogenies/chondro_full_ultrametric.tre")

# Load full chondro tree:
chondroFullChrono <- read.tree("data/phylogenies/chondro_full_ultrametric.tre")
# and
# pre-subset tree from Vertlife
chondroSubsets <- ape::read.nexus("data/phylogenies/my-chondros-subset.nex")
is.list(chondroSubsets) # = list of 100 trees
randomChondroSubset <- sample(chondroSubsets,size=1)[[1]] # Randomly selected
randomChondroSubset$tip.label
is.ultrametric(randomChondroSubset) # true
is.binary(randomChondroSubset) # true

# Make sure all are phylo objects:
class(teleostFullChrono) 
class(chondroFullChrono) 
class(randomChondroSubset) 

# Connecting trees to make supertree:

# find the root ages of these trees:
teleostFullRoot <- max(nodeHeights(teleostFullChrono))
teleostFullRoot
chondFullRoot <- max(nodeHeights(chondroFullChrono))
chondFullRoot
chondSubsetRoot <- max(nodeHeights(randomChondroSubset))
chondSubsetRoot 
# bind.tree:
x <- teleostFullChrono
y <- randomChondroSubset
x$root.edge <- 100 # space for the teleost tree, root.edge adds a stem to the tree
y$root.edge <- chondFullRoot - chondSubsetRoot # diff in root ages between full and subset trees
ageDiff <-  chondFullRoot - teleostFullRoot # diff in root ages between full chondros and teleosts
treeAll <- bind.tree(x, y, where = "root", ageDiff)

# Check supertree
is.ultrametric(treeAll)
is.binary(treeAll)
is.rooted(treeAll)

# Save supertree
# write.tree(treeAll, file = "data/phylogenies/fullTeleostChrono_wChondroSubset.tre")



# *****************************************************************************

# Pruned trees: ----

# Load supertree (full teleost chronogram and the chondro subset)
treeAll <- read.tree("data/phylogenies/fullTeleostChrono_wChondroSubset.tre")

# Pruning phylogenetic trees for RMR, MMR, and AS models:

# Update some sp names in supertree:
treeAll$tip.label[treeAll$tip.label== "Dasyatis_lata"] <- "Bathytoshia_lata"
treeAll$tip.label[treeAll$tip.label== "Dasyatis_sabina"] <- "Hypanus_sabinus"
treeAll$tip.label[treeAll$tip.label== "Dasyatis_americana"] <- "Hypanus_americanus"
treeAll$tip.label[treeAll$tip.label== "Myliobatis_californicus"] <- "Myliobatis_californica"
treeAll$tip.label[treeAll$tip.label== "Raja_eglanteria"] <- "Rostroraja_eglanteria"
treeAll$tip.label[treeAll$tip.label=="Theragra_chalcogramma"] <- "Gadus_chalcogrammus"
treeAll$tip.label[treeAll$tip.label=="Chrysophrys_major"] <- "Pagrus_major"
treeAll$tip.label[treeAll$tip.label=="Chrysophrys_auratus"] <- "Pagrus_auratus"
treeAll$tip.label[treeAll$tip.label=="Clupea_pallasii_pallasii"] <- "Clupea_pallasii"

# RMR phylogeny ----
rownames(DF_rmaxRMRbigN) <- str_replace_all(DF_rmaxRMRbigN$ScientificName, " ", "_") # Rename rows by species names with _ (to check against tree tip labels)
nc <- geiger::name.check(treeAll, DF_rmaxRMRbigN) # Make sure all species in list are in tree
nc # should say "data_not_tree: none"
spp_all <- rownames(DF_rmaxRMRbigN) # Save spp names
AllTreeSpp <- treeAll$tip.label # All labels in tree
SppToPrune <- setdiff(AllTreeSpp, spp_all) # Those to drop
treePruned <- drop.tip(treeAll, SppToPrune) # Prune the supertree
# Check tree is binary, ultrametric and rooted:
is.ultrametric(treePruned)
is.binary(treePruned)
is.rooted(treePruned)
# Check if missing species:
prunedTreeSpp <- treePruned$tip.label
SppMissingFromPruned <- setdiff(spp_all, prunedTreeSpp)
SppMissingFromPruned 
# Save tree:
#write.tree(treePruned, "data/phylogenies/pruned_fish_tree_rmaxRMR_ms.tre")

# MMR phylogeny ----
rownames(DF_rmaxMMRbigN) <- str_replace_all(DF_rmaxMMRbigN$ScientificName, " ", "_") # Rename rows by species names with _ (to check against tree tip labels)
nc <- geiger::name.check(treeAll, DF_rmaxMMRbigN) # Make sure all species in list are in tree
nc # should say "data_not_tree: none"
spp_all <- rownames(DF_rmaxMMRbigN) # Save spp names
AllTreeSpp <- treeAll$tip.label # All labels in tree
SppToPrune <- setdiff(AllTreeSpp, spp_all) # Those to drop
treePruned <- drop.tip(treeAll, SppToPrune) # Prune the supertree
# Check tree is binary, ultrametric and rooted:
is.ultrametric(treePruned)
is.binary(treePruned)
is.rooted(treePruned)
# Check if missing species:
prunedTreeSpp <- treePruned$tip.label
SppMissingFromPruned <- setdiff(spp_all, prunedTreeSpp)
SppMissingFromPruned 
# Save tree:
#write.tree(treePruned, "data/phylogenies/pruned_fish_tree_rmaxMMR_ms.tre")

# AS phylogeny ----
rownames(DF_rmaxASbigN) <- str_replace_all(DF_rmaxASbigN$ScientificName, " ", "_") # Rename rows by species names with _ (to check against tree tip labels)
nc <- geiger::name.check(treeAll, DF_rmaxASbigN) # Make sure all species in list are in tree
nc # should say "data_not_tree: none"
spp_all <- rownames(DF_rmaxASbigN) # Save spp names
AllTreeSpp <- treeAll$tip.label # All labels in tree
SppToPrune <- setdiff(AllTreeSpp, spp_all) # Those to drop
treePruned <- drop.tip(treeAll, SppToPrune) # Prune the supertree
# Check tree is binary, ultrametric and rooted:
is.ultrametric(treePruned)
is.binary(treePruned)
is.rooted(treePruned)
# Check if missing species:
prunedTreeSpp <- treePruned$tip.label
SppMissingFromPruned <- setdiff(spp_all, prunedTreeSpp)
SppMissingFromPruned 
# Save tree:
#write.tree(treePruned, "data/phylogenies/pruned_fish_tree_rmaxAS_ms.tre")