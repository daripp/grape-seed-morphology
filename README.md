# grape-seed-morphology

These colaboratory and R notebooks are associated with the manuscript: "Changes in grape seed morphology and internal porosity during red wine fermentation detected using X-ray micro-computed tomography". 

To use these notebooks, access the training and testing data for the fcn model here: https://drive.google.com/drive/folders/1B0pnePlngX87RxbNmVDM6vX4NJdWuvug?usp=sharing
Right click on this folder and add it to your google drive account for use. You will need to make sure the input/output paths work with your google drive account.
With these you can repeat all of segmentations using 4 or 5 components.

A copy of all of our 4 and 5 component outputs and our original stack data is also available from this folder.

Using these outputs, you can attempt to run the pore network model-at full resolution (scale=1) this take 117 GB of ram which is more than you can access in google drive. Using a high RAM runtime environment, you should be able to replicate our results with a scale of 0.25.

The R notebooks are used for analyzing the data we extracted using the FCN model and from our Porespy results.
