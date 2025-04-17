module Data.Blobs.MAGs exposing (magsBlob)

magsBlob : String
magsBlob = String.trim """
id,samples_id,taxonomy,completeness,contamination,#16s_rrna,#5s_rrna,#23s_rrna,#trna,nr_contigs,nr_genes,is_representative,binning_tool,assembly_method,comment,file_size,file_sha256
SHD1_2081,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Phocaeicola;s__Phocaeicola sp900546645,77.43,0.19,6,6,6,21,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2473,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_A;s__Fusobacterium_A sp900543175,61.7,1.09,1,3,1,13,49,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0222,D000,d__Bacteria;p__Campylobacterota;c__Campylobacteria;o__Campylobacterales;f__Campylobacteraceae;g__Campylobacter_D;s__Campylobacter_D upsaliensis,99.99,0.13,3,0,3,24,1,0,True,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0133,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Phocaeicola;s__Phocaeicola coprocola,99.84,0.05,6,6,6,23,3,0,True,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0279,D000,d__Bacteria;p__Actinomycetota;c__Coriobacteriia;o__Coriobacteriales;f__Coriobacteriaceae;g__Collinsella;s__Collinsella intestinalis,99.9,0.15,4,4,4,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0389,D000,d__Bacteria;p__Campylobacterota;c__Campylobacteria;o__Campylobacterales;f__Helicobacteraceae;g__Helicobacter_B;s__,99.82,0.21,2,2,2,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1200,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_B;s__Fusobacterium_B sp900554885,99.68,1.16,7,1,7,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0647,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__CAG-274;g__Gallispira;s__Gallispira sp900543365,98.69,0.22,4,0,4,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1427,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_A;s__Fusobacterium_A sp900555845,99.99,1.77,7,8,7,24,4,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1624,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Enterocloster;s__Enterocloster sp001517625,97.4,1.94,4,0,4,23,19,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1688,D000,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Erysipelotrichaceae;g__Amedibacterium;s__Amedibacterium intestinale,95.77,1.88,5,0,5,22,6,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0393,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Alloprevotella;s__Alloprevotella sp900541335,99.15,0.08,7,7,7,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0784,D000,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Coprobacillaceae;g__MGBC140090;s__,98.78,0.37,4,0,4,22,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1421,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia;s__Blautia hansenii,99.85,1.73,5,0,5,22,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2497,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Ruminococcus_B;s__Ruminococcus_B gnavus,97.93,8.81,2,0,2,23,36,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1250,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia;s__Blautia sp900556555,94.65,0.27,3,0,3,22,24,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1840,D000,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Coprobacillaceae;g__Beduini;s__Beduini sp018365815,98.22,3.06,8,0,8,22,16,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2153,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Roseburia;s__Roseburia sp019421345,82.1,1.63,2,0,2,19,89,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1851,D000,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Erysipelotrichaceae;g__Dielma;s__,91.12,1.68,5,5,5,21,25,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0168,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_B;s__Fusobacterium_B sp900541465,99.89,0.08,6,7,6,23,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0387,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_A;s__Fusobacterium_A mortiferum,99.99,0.24,8,0,8,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1058,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Peptostreptococcales;f__Peptostreptococcaceae;g__Peptacetobacter;s__Peptacetobacter hiranonis,98.89,0.74,11,11,11,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1057,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Clostridium_Q;s__Clostridium_Q sp000435655,98.06,0.57,5,0,5,23,3,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1414,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Ventrimonas;s__Ventrimonas sp900538475,91.45,0.04,6,6,6,23,3,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1727,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia_A;s__Blautia_A sp900541345,95.77,2.04,5,5,4,23,19,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2054,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Phocaeicola;s__Phocaeicola plebeius,77.43,0.01,5,5,4,22,13,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2397,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Oscillospirales;f__Oscillospiraceae;g__Pseudoflavonifractor_A;s__Pseudoflavonifractor_A sp018374635,71.18,1.93,0,2,0,19,71,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0196,D000,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_A;s__Fusobacterium_A sp900555485,99.81,0.08,9,0,9,23,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1333,D000,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Erysipelotrichaceae;g__Amedibacillus;s__Amedibacillus dolichus,97.95,1.11,2,0,2,22,13,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0841,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia;s__Blautia sp000432195,100.0,0.67,5,0,4,22,15,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1059,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Faecalimonas;s__Faecalimonas umbilicata,99.59,0.88,6,6,6,23,13,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1003,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Phocaeicola;s__Phocaeicola vulgatus,96.03,0.07,5,5,5,23,24,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1422,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia_A;s__Blautia_A caecimuris,93.17,0.4,5,5,5,23,26,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2104,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Eisenbergiella;s__Eisenbergiella sp900539715,94.58,3.8,2,0,2,19,22,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2271,D000,d__Bacteria;p__Bacillota_C;c__Negativicutes;o__Selenomonadales;f__Selenomonadaceae;g__Megamonas;s__Megamonas funiformis,86.57,3.62,5,5,5,19,21,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2284,D000,d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides;s__Bacteroides stercoris,69.91,0.44,1,1,1,19,63,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2493,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Clostridiales;f__Clostridiaceae;g__Clostridium;s__Clostridium sp900547475,60.22,1.24,0,4,1,20,143,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2584,D000,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Oscillospirales;f__Ruminococcaceae;g__Faecalibacterium;s__Faecalibacterium sp900540455,53.2,1.36,7,6,7,23,49,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1223,D000,d__Bacteria;p__Pseudomonadota;c__Gammaproteobacteria;o__Enterobacterales;f__Enterobacteriaceae;g__Escherichia;s__Escherichia coli,100.0,1.26,7,8,7,24,4,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2077,D001,d__Bacteria;p__Bacillota_C;c__Negativicutes;o__Selenomonadales;f__Selenomonadaceae;g__Megamonas;s__,78.06,0.28,2,3,2,19,35,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0317,D001,d__Bacteria;p__Campylobacterota;c__Campylobacteria;o__Campylobacterales;f__Campylobacteraceae;g__Campylobacter_D;s__Campylobacter_D upsaliensis,99.99,0.19,3,0,3,24,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0769,D001,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Coprobacillaceae;g__Catenibacterium;s__Catenibacterium sp000437715,100.0,0.6,9,0,9,21,1,0,True,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0507,D001,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Blautia;s__Blautia hansenii,99.96,0.33,5,0,5,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1139,D001,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Ventrimonas;s__Ventrimonas sp900538475,95.13,0.13,6,5,6,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1143,D001,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Clostridium_Q;s__Clostridium_Q sp000435655,97.38,0.59,5,0,5,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_2114,D001,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Faecalimonas;s__,99.93,4.94,6,0,6,23,2,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1331,D001,d__Bacteria;p__Bacillota;c__Bacilli;o__Erysipelotrichales;f__Erysipelotrichaceae;g__Amedibacterium;s__Amedibacterium intestinale,97.89,1.09,0,0,6,23,7,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0742,D001,d__Bacteria;p__Actinomycetota;c__Coriobacteriia;o__Coriobacteriales;f__Coriobacteriaceae;g__Collinsella;s__Collinsella intestinalis,99.99,0.58,4,4,4,22,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_1491,D001,d__Bacteria;p__Fusobacteriota;c__Fusobacteriia;o__Fusobacteriales;f__Fusobacteriaceae;g__Fusobacterium_A;s__Fusobacterium_A sp900555845,99.48,1.84,7,7,7,23,1,0,False,SemiBin2,Flye with Polypolish,,0,TODO
SHD1_0177,D001,d__Bacteria;p__Bacillota_A;c__Clostridia;o__Lachnospirales;f__Lachnospiraceae;g__Faecalimonas;s__Faecalimonas umbilicata,99.9,0.09,6,4,6,23,10,0,False,SemiBin2,Flye with Polypolish,,0,TODO
"""
