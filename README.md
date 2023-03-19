# grape-seed-morphology

These colaboratory and R notebooks are associated with the manuscript: "Changes in grape seed morphology and internal porosity during red wine fermentation detected using X-ray micro-computed tomography". 

To use these notebooks, access the training and testing data for the fcn model here: https://drive.google.com/drive/folders/1B0pnePlngX87RxbNmVDM6vX4NJdWuvug?usp=sharing
Right click on this folder and add it to your google drive account for use. You will need to make sure the input/output paths work with your google drive account.
With these you can repeat all of segmentations using 4 or 5 components.

A copy of all of our 4 and 5 component outputs and our original stack data is also available from this folder.

Using these outputs, you can attempt to run the pore network model. ***At full resolution (scale=1) this take 117 GB of ram which is more than you can access in google colab***. Using a high RAM runtime environment (51 GB), you should be able to approximate our results with a scale of 0.25. ***Results at this scale will be slightly different than they would be with a scale of 1 because the images are being downscaled by 4x in the X-dimension and 4x in the Y-dimension for a total scale of 16x smaller than was used in the paper. This represents a 93.75% loss of information which means taht while the data is rescaled in the output, discrepancies remain.***

The R notebooks are used for analyzing the data we extracted using the FCN model and from our Porespy results.
