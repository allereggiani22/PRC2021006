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

tree <- read.tree(file = "./dati/EUSV.nwk")
tree

samtree <- read.tree(file = "./dati/tree_newick.nwk")

identify(p, nodes = T)

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
  theme_tree2()+
  xlim(0, 70)




