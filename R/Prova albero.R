source(here('R', 'librerie.R'))

seqs <- readDNAStringSet(here("dati","Allineamento per poster.fas"))

aln <- msa(seqs, method = "ClustalW")

cv <- msaConvert(aln, type = c("bios2mds::align"))

library(bios2mds)
export.fasta(cv, outfile = "outfile.fas", ncol(aln), open = "w")



bin <- as.DNAbin(aln)

an <- as.alignment(bin)
nm <- as.matrix(an)
nbinmat <- as.matrix(labels(bin))
class(bin)
dnbin <- dist.dna(bin, model = "TN93")
tree <- nj(dnbin)
tree2 <- (dnbin)
ggt <- ggtree(tree, cex = 0.8)+
  geom_tiplab(align=F, size=3.5)+
  geom_treescale(y = - 1, x= -20, aes(color=branch),fontsize = 7,options(ignore.negative.edge=TRUE))
ggt
groupClade(ggt, 180)

tree2 <- tidytree::as_tibble(tree) %>% groupClade(180) %>% tidytree::as.phylo()

ggt2 <- ggtree(tree2, cex = 0.8)+
  geom_tiplab(align=F, size=3.5)+
  geom_treescale(y = - 1, x= -20, aes(color=branch),fontsize = 7,options(ignore.negative.edge=TRUE))

nodeid(ggt, )


njmsaplot<-msaplot(ggt, bin, offset = 0.009, width=1, height = 0.5, color = c(rep("rosybrown", 1), rep("sienna1", 1), rep("lightgoldenrod1", 1), rep("lightskyblue1", 1), rep("green",1), rep("pink",1), rep("darkred",1)))
njmsaplot



# Prove 08/01/2025 --------------------------------------------------------

library(ggtree)

tree <- read.tree(file = "./dati/EUSV.nwk")
tree

samtree <- read.tree(file = "./dati/tree_newick.nwk") #file di esempio scaricato da tutorial


ggtree(tree)
#add a scale
ggtree(tree) + geom_treescale()
ggtree(tree) + theme_tree2()

#plot a cladogram
ggtree(tree, branch.length = "none")
ggtree(tree, branch.length = "none", color = "blue", size= 2, linetype = 3)

#exercises
ggtree(tree, layout = "slanted")
ggtree(tree, layout = "circular")
ggtree(tree, layout = "circular", branch.length = "none", size= 2, color= "red")

p <- ggtree(tree)

identify(p, nodes = T)

p + geom_nodepoint()
p + geom_nodelab()
p + geom_tippoint()
p + geom_tiplab()
p + geom_tiplab2()

p + geom_tiplab(geom = "text", colour="purple") +
  geom_tippoint(pch = 5, color = "purple") + #pch = point character, 5 lo setta in "diamond"
  geom_nodepoint(color = "yellow3", size= 3, alpha= 0.3)

#prove di annotazione

p + geom_tiplab() +
  #geom_label(aes(x=branch, label="CIAO"), fill="lightgreen")
  geom_cladelab(node = 161, label="EriCoV", barcolour="red", barsize= 1.2, textcolour= "blue", textsize= 5, hjust= 0, align=T, offset= -0.1)


ggtree(tree, layout = "rectangular")+
  geom_cladelab(node = 161, label="EriCoV", barcolour="red", textcolour= "blue")


ggtree(tree, branch.length = "none") + geom_text(aes(label=node)) #per vedere il numero dei nodi

ggtree(samtree)+
  geom_cladelabel(node=17, label = "Some random clade", color="red2", offset = .8)+
  geom_cladelabel(node = 21, label="A different clade", color = "blue", offset = .8) 

ggtree(samtree)+
  geom_cladelabel(node=17, label = "Some random clade", color="red2", offset = .8, align = T)+
  geom_cladelabel(node = 21, label="A different clade", color = "blue", offset = .8, align = T) #per allineare le etichette, che però escono dal grafico

ggtree(samtree)+
  geom_cladelabel(node=17, label = "Some random clade", color="red2", offset = .3, align = T)+
  geom_cladelabel(node = 21, label="A different clade", color = "blue", offset = .3, align = T)+
  theme_tree2()+ #fa comparire la scala completa all'asse delle x
  xlim(0, 70)+ #limita la visualizzazione dell'asse x tra 0 e 70, in questo caso allargandola
  theme_tree() #sovrascrive il tema, nascondendo la scala

ggtree(samtree) +
  geom_tiplab() +
  geom_hilight(node = 17, fill="gold") +
  geom_highlight(node = 21, fill="purple")

ggtree(samtree, layout = "circular") + 
  geom_tiplab() + 
  geom_taxalink("E", "H", color="blue3", curvature= .5) +
  geom_taxalink("C", "G", color="orange2", curvature=-.5) #permette di collegare taxa

# Exercise 3
# 
# Produce the figure below.
# 
# First, find what the MRCA is for taxa B+C, and taxa L+J. You can do this in one of two ways:
#   Easiest: use MRCA(tree, tip=c("taxon1", "taxon2")) for B/C and L/J separately.
# Alternatively: use ggtree(tree) + geom_text(aes(label=node), hjust=-.3) to see what the node labels are on the plot. You might also add tip labels here too.
# Draw the tree with ggtree(tree).
# Add tip labels.
# Highlight these clades with separate colors.
# Add a clade label to the larger superclade (node=17) that we saw before that includes A, B, C, D, and E. You’ll probably need an offset to get this looking right.
# Link taxa C to E, and G to J with a dashed gray line (hint: get the geom working first, then try changing the aesthetics. You’ll need linetype=2 somewhere in the geom_taxalink()).
# Add a scale bar to the bottom by changing the theme.
# Add a title.
# Optionally, go back to the original ggtree(tree, ...) call and change the layout to "circular".

MRCA(samtree, tip=c("B","C")) #non funziona, non so perché...
getMRCA(samtree, c("B","C")) #uso invece questa funzione del package ape, che è equivalente
getMRCA(samtree, c("L","J"))

ggtree(samtree)+
  geom_tiplab()+
  geom_highlight(node=19, fill="blue3")+
  geom_cladelabel(node = 19, label = "Superclade 17", color = "red2", offset = .3)+
  geom_highlight(node=23, fill="gold2")+
  geom_taxalink("C", "E", linetype=2, color="darkgrey")+
  geom_taxalink("G","J", linetype=2, color="darkgrey")+
  theme_tree2()


library(ggimage)
phylopic_uid("Erinaceus_europaeus")

ggtree(tree, layout = "rectangular")+
  theme_tree()+
  # geom_text(aes(label=node))+
  xlim(0,0.8)+
  geom_cladelab(node= 161, label = "Hedgehog", image="f83c6893-f0ed-4ec4-b558-aa774c5c9b5b",
                  geom="phylopic", imagecolor = "orange2", offset=-0.3, offset.text= 0.01, hjust=0.5, align=T)+ #attenzione, solo cladelab funziona e non cladelabel. offset.text muove solo il testo o l'immagine, non la barra.
  geom_tiplab(size=1.4)+
  geom_highlight(node=161,fill="orange", alpha=0.2, extend=0.2, type = "rect")
