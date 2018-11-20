# TopicAlignment

## Description

This repository houses data and scripts from Andreotta, Nugroho, Hurlstone, Boschetti, Farrell, Walker, and Paris study on Australian climate change tweets. This research was preregistered on the Open Science Framework under the project "Examining Australian Climate Change Discourse Through a Social Media Lens 1.1", <https://osf.io/mb8kh/>.  The scripts contain an algorithm to align topics, based on the work of Chuang et al. (2015). Identifies similar topics that occur across separate executions of the same topic modeling algorithm (i.e., topics that reproduce). See Chuang et al. (2015) for more detail.

The scripts are coded in R (version 3.5.0; R Core Team, 2018) using RStudio (version 1.1.453; RStudio Team, 2016) for Windows 10. Several packages were used to complete these scripts: tidyverse (version 1.2.1; Wickham, 2017), shiny (version 1.1.0; Chang, Cheng, Allaire, Xie, & McPherson, 2018), shinyWidgets (version 0.4.3, Perrier, Meyer, & Granjon, 2018), DT (version 0.4; Xie, 2018), and xml2 (version 1.2.0, Wickham, Hester, & Ooms, 2018).

The script deploys a Shiny App to display the aligned topics. The app can be used to explore the impact of various parameters of this process. See below for more detail.

The topic alignment algorithm of Chuang at al. (2015) has been augmented to include more stringent criteria for grouping. Before two groups are merged, the similarity between each pair of topics is calculated. If the mean similarity is below 0.3, the groups cannot be merged. This criteria can be altered or removed from the script in the detect_mostsimilar function found in **/script/topicalignment.R**

### References

Chang, W., Cheng, J., Allaire, J.J., & Xie, Y. & McPherson, J. (2018). shiny: Web Application Framework for R. R package version 1.1.0. <https://CRAN.R-project.org/package=shiny>.

Chuang, J., Roberts, M. E., Stewart, B. M., Weiss, R., Tingley, D., Grimmer, J., & Heer, J. (2015). TopicCheck: Interactive alignment for assessing topic model stability. *In Proceedings of the Conference of the North American Chapter of the Association for Computational Linguistics - Human Language Technologies* (pp. 175–184). Denver, Colorado: Association for Computational Linguistics. <http://dx.doi.org/10.3115/v1/N15-1018>.

Perrier, V. Meyer, F. & Granjon, D. (2018). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.4.3. <https://CRAN.R-project.org/package=shinyWidgets>

R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.

RStudio Team (2016). RStudio: Integrated Development for R. <i>RStudio, Inc.</i>, Boston, MA. <http://www.rstudio.com/>.

Wickham, H. (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1. <https://CRAN.R-project.org/package=tidyverse>.

Wickham, H., Hester, J. & Ooms, J. (2018). xml2: Parse XML. R package version 1.2.0. https://CRAN.R-project.org/package=xml2

Xie, Y. (2018). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.4. <https://CRAN.R-project.org/package=DT>.


## Repository structure

**/data** contains the [data set used by Andreotta et al.](/data/rawdata.csv), who modeled the topics of 41 independent batches of tweets concerning climate change, indicated in the *batch* column of the data set. Three topic solutions were generated, each differing according to the topics generated per batch (i.e., *topicsperbatch* = 5, 10, or 20). For each *topicsperbatch*, a unique ID was allocated to each *topic*. For each *topic*, the ten terms with the strongest relationship to the topic are extracted (though only terms with a value higher than the mean value for all terms are extracted). These *keywords* are delineated with a '|' in the data file. Lastly, the <i>vol</i>ume of tweets in each topic is reported.

**/script** contains two scripts. First, *topicalignment.R* is used to detect the similarities between topics (though, these can be manually entered as .csv in **/out**), group topics according to levels of similarity (these results are stored in **/out**), and deploy an Shiny App to explore/refine results. The second script is *extractIDs.R*, demonstrating how Andreotta et al. extracted tweets from each group of topics. The data file of tweet IDs is not provided due to ethical obligations.

**/out** contains .csv of similarity matrices (for each *topicsperbatch*) and .csv output for topic alignment algorithm (for each *topicsperbatch*). The topic alignment output is structured as */data/rawdata.csv*, with the addition of extra columns equivalent to various thresholds (see script). Lastly, **/out** contains the output files from the Shiny App (e.g., *output_k5*). The Shiny App is used to extract groups of topics satisfying certain criteria (see below), and classify the remaining groups as 'ungrouped'. The file is structured as */data/rawdata.csv* with the addition of an *extract* variable denoting each topic's group membership

## Description of App
