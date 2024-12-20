################# ScanTools_ProtEvol README ########################

Author: Patrick Monnahan (https://github.com/pmonnahan/ScanTools)
Contact: pmonnahan@gmail.com
Modified by: Magdalena Bohutínska (holcovam@natur.cuni.cz)
Last modified by: Sonia Celestini (sonia.celestini@natur.cuni.cz)

Program Description:  ScanTools_ProtEvol is a collection of scripts for window-based genomic analyses and site-based analysis of selection acting on amino acid substitutions and a wrapper that facilitates job submission on a PbS Pro-based computing cluster. The program begins with a set of VCF's to be analyzed as well as a population key (example template can be found on github repository) that assigns the individual names in the VCF's to populations. 
For site-based analyses requiring additional annotation of variant effect and amino acid substitution type, the VCF has to be annotated in SnpEff (http://snpeff.sourceforge.net/index.html) prior to the analysis. An example annotation script can be found on repository. 
VCF's are split according by population and converted to a simplified format, which are used for all downstream analyses. Currently, the major downstream analyses implemented are calculation of within population diversity metrics and neutrality tests, between population differentiation metrics, demographic analyses using fastsimcoal2, analyses of dN/dS and scans for highly differentiated amino acid substitutions.

###########################################################

REQUIREMENTS:

- python3.+ (program has been debugged for python-3.5.1)
- python modules: os, subprocess, pandas, math, datetime, time
- bedtools 2.17 or higher
- GATK 3.6 
- fastsimcoal2 version 2.6
- A directory containing a file named 'PopKey.csv'.
(-SnpEff 4.3)

INTRODUCTION

To download the program, login to your cluster system and use git clone <link_to_repository>.  Then, change directory to the newly cloned "ScanTools_ProtEvol" directory and invoke the python3 console. Import the program into python3 by simply typing 'import ScanTools' from within the ScanTools_ProtEvol directory.  Using the wrapper (ScanTools.py) is greatly aided by a rudimentary understanding of python, particularly regarding the use of 'methods' associated with 'objects'.  The first step is to create a 'ScanTools' object and all subsequent operations will be performed by calling the methods associated with this object.  For example,

test = ScanTools.scantools("<path_to_directory_containing_PopKey.csv>", popkey="<name_of_PopKey_csvFile>")

creates a ScanTools object named 'test' and assigns several relevant peices of information to the methods of 'test'.  This directory MUST contain a 'csv' (which you will call with popkey) which will be used to associate individuals to populations.  If the csv is encoded, you will have to specify the type of encoding as additional argument.  However, this should typically not be the case. 

To split VCF's and convert them to the necessary format, you call the '.splitVCFs' method by calling:

test.splitVCFsNorepol(scan_dir="<full_path _to_directory_containing_Scantools_python_scripts>", vcf_dir="<full_path_to_vcf>", vcf_pattern="<pattern_in_name_vcf_you_want_to_use>", min_dp=<minimum_depth_for_genotype_call>, mffg=<maximum_fraction_filtered_genotypes>)

This method makes several calls to GATK to filter the vcfs and convert them to table format and subsequently calls either one or two custom python scripts to convert the genotype calls to numeric format. The output is a txt file (recode.txt or repol.txt if you use a repolarization key) for each column of your Popkey file, unless differently specified with the flag "pops" - to select only few columns of the Popkey file. All subsequent analyses can be carried out similarly by calling the relevant methods. The full list of methods and a brief description is provided below, but look within the python scripts themselves for a more detailed description.  

LIST OF METHODS:

Within python3, you can view all methods associated with an object by calling 'dir(<object_name>)'. Ignore methods surrounded by underscores...these are 'intrinsic' methods that are not directly relevant to our purposes.  Another important note on python methods is that they are quite versatile.  For example, they can be functions that take arguments and carry out operations associated with the parent object.  Or, methods can be objects that store information associated with the parent object.  

Several of the ScanTools methods are OBJECTS containing information about the parent object ('test' in the above example):

- .pops: a list of the populations identified in the PopKey.csv file.  These populations will be used as default for subsequent analyses.
- .samps: a dictionary of samples and their associated populations identified in the PopKey.csv file
- .samp_nums:  a dictionary of populations and their associated sample sizes
- .min_ind: the minimum observed number of individuals in a population
- .dir: the directory containing the PopKey and all output
- .split_dirs: list of directories containing reformatted input files, if any exist.


ScanTools methods that are FUNCTIONS to carry out operations (only required arguments are shown; see code for full list of arguments):

**Important** 
Most of these methods include a print1 argument that, if set to True, will print the shell scripts instead of submitting them to the cluster.  This is a useful check that you should always do before your first real execution of a method.  The argument names provided below are not the actual argument names required by each function.  Rather, the arguments below are descriptions of the actual arguments in the program.  Also, most of the methods have an option to change the partition, time, and memory requested for each job.  For methods that necessarily generate large intermediate files (.splitVCFS(), .calcbpm(), .generateFSC2input()), there is an option to use scratch (use_scratch=True and scratch_path=/path/to/scratch/directory).  However, this requires the user to set up their directory on the scratch drive.

 - .removePop(pops_to_be_removed): takes a list of populations and removes them from '.pops'. E.g. test.removePops(['Pop1','Pop2']).  Even single populations should be specified in a list format.  '.min_ind' will be recalculated.

 - .removeInds(individuals_to_be_removed): takes a list of individuals and removes them from '.samps'.  Recalculates '.min_ind'

 - .combinePops(pop_list, new_pop_name): combines two or more populations into new population with the name specified.  Original populations remain unchanged.

 - .splitVCFs(vcf_directory, minimum_individual_depth, max_fraction_filtered_genotypes):  Split VCF's by population, filter, and convert to input format for downstream analyses. NOTE: it requires a repolarization key. Run the command below if you don't have one.

 - .splitVCFsNorepol(scan_dir="<full_path _to_directory_containing_Scantools_python_scripts>",vcf_dir="<full_path _to_directory_containing_vcf>",ref_path, ref_name,suffix="<pattern_for_output_directory_name>", vcf_pattern="<pattern_in_name_vcf_you_want_to_use>", minimum_individual_depth, max_fraction_filtered_genotypes)
 
 - .splitVCFsAnn(vcf_directory, minimum_individual_depth, max_fraction_filtered_genotypes):  Same as .splitVCFs but works with SnpEff-annotated vcf. 
 
 - .splitVCFsTreeMix(sca_dir, vcf_directory, minimum_individual_depth, max_fraction_filtered_genotypes): Makes input for scripts in TreeMix repository. NOTE: the vcf input file should contain "TreeMix" in its name.
 
 - .recode(table_directory): Should not be necessary for the most part.  This will typically be called during '.splitVCFs', but was left in code as standalone method in case there is need to convert a table (from GATK's VariantsToTable) to the reformatted input used in ScanTools.

 - .repolarize(recoded_file_directory):  Also should not be necessary for most part as it is typically called in '.splitVCFs' if a repolarization key is provided.  A repolarization key is simply a tab-delimited list of sites where each line contains the scaffold and position for a site.  This code will go through the recoded, numeric genotype files and flip the reference and alt alleles, modifying genotypes accordingly.

 - .getPloidies(recoded_file_directory):  Determines ploidy of each population from the recoded files and assigns them to a new method of the ScanTools parent object.

 - .calcAFS(recoded_file_direcory, output_file_suffix, number_of_individuals_to_downsample_to, list_of_populations, allow_one_missing=True):  Calls calcAFS.py for each population in list_of_populations.  Calculates genome-wide allele frequency spectrum.

 - .calcFreqs(self, recode_dir, outfile_name, sites_file, list_of_populations):  Calls calcFreqs_atSites.py.  Takes a list of sites in (sites_file, should be formatted so that each line simply has scaffold and position, with scaffold simply coded as an integer 0-8) and calculates the allele frequency in each population (list_of_populations) at each site

  - .calcwpm(scan_dir, recoded_file_directory, window_size, minimum_snps):  Calls wpm.py, which calculates within-population diversity metrics and neutrality test statistics in windows along the genome as well as genome-wide for each population.  Windows are specified in terms of base pairs.  Windows with fewer than the specified minimum SNPs will not be reported.  All populations are downsampled by default to the '.min_ind' value unless specified otherwise using 'sampind' argument.

  - .concatWPM(directory_containing_wpm_output, suffix, name):  Concatenates output of wpm.py across populations.  Suffix is a string that identifies the name of the output files to concatenate that follows the population name.  'name' is used to name the concatenated file.

  - .calcBPM(scan_dir, recoded_file_directory, pops, output_name, window_size, minimum_snps):  Calls bpm.py, which calculates between-population differentiation metrics: Fst, Rho (for interploidy comparisons), dxy, fixed differences, and allele frequency difference in windows along the genome.  This can calculate differentiation for two or more populations, but will get seriously bogged down if the number of populations is large.  For this reason, you must explicitly provide a list of populations as an argument.
  
  - .calcBPMann(recoded_file_directory, pops, output_name, output_folder, window_size, minimum_snps):  Calls bpmann.py, same as bpm.py but works with SnpEff-annotated vcf. continued in https://github.com/mbohutinska/ProtEvol/blob/master/protScanTools.r

  - .calcPairwisebpm: same as above, but instead calculates metrics for every pair of populations passed in the 'pops' argument.
  
  - .calcPairwisebpmAnn: Same as .calcPairwisebpm but works with SnpEff-annotated vcf, continued in https://github.com/mbohutinska/ProtEvol/blob/master/protScanTools.r
  
  - .outlierAASs:  to get number of outlier high-Fst amino acid substitutions for each gene genome-wide, continued in https://github.com/mbohutinska/ProtEvol/blob/master/protScanTools.r
  
  -.N_SperGene: calculates dN/dS per each gene in the genome, continued in https://github.com/mbohutinska/ProtEvol/blob/master/protScanTools.r

  - .findOutliers(directory, filename, column_index_list, percentile): Identifies outliers from one or more metrics in a provided output file.  A list of column indices (1-based; i.e. first column is column 1 not column 0) identifying the columns containing metrics to be used for determining outliers.

  - .annotateOutliers(directory,outlier_file): annotates 'OutOnly.bed' file with annotation information from gff file using bedtools.

  - .mergeAnnotation(directory, filename):  filename should be the 'OutOnly.csv' file generated from '.findOutliers'.  This method merges the annotation information from '.annotateOutliers' with the metric info in OutOnly.csv.

  - .Outliers(directory, metric_file, column_index_list, percentile):  wraps '.findOutliers', '.annotateOutliers', and '.mergeAnnotation' into one method

  - .graphs: visualize outliers

  - .generateFSC2input(recoded_file_directory, pops, output_name, bootstrap_block_size, bootstrap_reps): generates the multi-dimensional site frequency spectrum input for fastsimcoal2 from the recoded, numeric genotype files (Output is labeled as output_name_DSFS.obs).  Has been tested for 5 populations, but may require much more memory as the number of populations are increased.  The bootstrap arguments are for performing confidence interval calculation as specified in the fastsimcoal manual.  Set bootstrap reps to 0 if you don't want this to happen.  Otherwise, you'll notice many additional files labelled with 'rep#' in the name, which are the bootstrapped replicate data sets.

  - .FSC2(FSC2_Data_Parent_Directory):  This method parallelises job submission of fastsimcoal2, but requires a very specific set up of input files.  The output of '.generateFSC2input' should be a folder that contains the multi-dimensional SFS.  Place this folder in a new folder that will be the FSC2_Data_Parent_Directory.  This directory should also contain one or more template (.tpl) and estimates (.est) files whose format can be found in the fastsimcoal2 documentation.  For each sub-directory containing input data, this method will re-format and rename the .tpl and .est files to reflect the necessary information in the sub-directory multi-dimensional SFS and then submit these jobs to the cluster.  

  - .gatherFSC2output(FSC2_Data_Parent_Directory):  This method collects all information from the '.bestlhood' output files of FSC2 that is buried in the sub-directories and outputs the information into one of two files:  Likelihoods file and parameters file.

  - .queryFSC2input

############################################
FURTHER NOTES FROM S. CELESTINI

- The First column of the Popkey has always to be called "Samples", and the Popkey file doesn't necessarily need to be called "Popkey", use what you want. However, it is important that the csv file is comma separated, not semicolon.

- In the reference folder you must have the fasta file + the reference index file ".fai"

- If the scaffold/contig names in your vcf do not start with "scaffold", you will encounter an error with the splitVCFs functions. Go to the file recode012.py at line number 22 and change the command based on your needs.

- I noticed that the function calcbpm has a bug (and maybe also all the other functions that do similar calculations - to check). Sometimes, in the output file, the last window/position of a chromosome will be labeled with the chromosome name of the row immediately after. This is a problem, especially if your data is divided into many scaffolds. I wrote an R script to correct the output files (I didn't have time to go to the source problem): Correct_ScanTools.R

- Example command pipeline:

git clone https://github.com/sonia23cld/ScanTools_ProtEvol.git #Then put there the files you need

#Run this from inside the ScanTools folder
module add python36-modules-gcc

python3

import ScanTools

test = ScanTools.scantools("/storage/brno12-cerit/home/sonia_celestini/Alyssum_2024/ScanTools_ProtEvol", popkey="PopKey_alyssum.csv")

test.splitVCFsNorepol(scan_dir="/storage/brno12-cerit/home/sonia_celestini/Alyssum_2024/ScanTools_ProtEvol", vcf_dir="/storage/brno12-cerit/home/sonia_celestini/Alyssum_2024/vcfs", ref_path="/storage/brno12-cerit/home/sonia_celestini/Alyssum_2024/fasta/", ref_name="alyssum.fasta", vcf_pattern= "alyssum.merged.masked.bt.v2", suffix="merged.masked", min_dp="8",mffg="0.2", mem="40", time_scratch='10:00:00', ncpu="8", overwrite=True, scratch_gb="1", keep_intermediates=True, use_scratch=True, scratch_path="$SCRATCHDIR", print1=False)

test.calcbpm(scan_dir="/storage/brno12-cerit/home/sonia_celestini/Alyssum_2024/ScanTools_ProtEvol", recode_dir= "VCF_splitVCFsNorepolmerged.masked_DP8.M0.2", pops=['DSP','TSP'], output_name="Aspruneri", output_folder="Dip_vs_Tet", window_size=1, min_snps=1, mem=35, ncpu=2, use_repol=False, keep_intermediates=False, time_scratch="20:00:00", scratch_gb=1, print1=False)

