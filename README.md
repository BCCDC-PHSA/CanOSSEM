![](images/clipboard-3759337823.png)

##### The Canadian Optimized Statistical Smoke Exposure Model (CanOSSEM) employs random forest machine learning algorithm to estimate daily (24-hour) mean PM~2.5~ at 5 km x 5 km resolution, based on multiple integrated data sources such as remote sensing imagery, satellite-derived estimates and ground-based measurements.

##### This is the coding repository for CanOSSEM. The R scripts used in the development of CanOSSEM will be added here shortly.

| S. No. | Task | Task Description | Directory |
|--------|:--------------------:|:-------------------:|:---------------:|
| 1\. | Project workflow and documents | GitHub workflow, project overview, and other documents | [*docs*](https://github.com/BCCDC-PHSA/CanOSSEM/tree/main/docs) |
| 2\. | Input data ETL and base raster creation | Input data extraction from different sources, cleaning and preparing for transformation; also includes CanOSSEM base raster creation | [data_extraction](https://github.com/BCCDC-PHSA/CanOSSEM/tree/main/data_extraction) |
| 3\. | Data transformation and raster cell assignment | Processed data transformed to match CanOSSEM raster cells, creation of complete datasets binding daily data from different sources | [*data_transformation*](https://github.com/BCCDC-PHSA/CanOSSEM/tree/main/data_transformation) |
| 4\. | Model development | Development of machine learning models, model testing, cross validation etc. | [*model_development*](https://github.com/BCCDC-PHSA/CanOSSEM/tree/main/model_development) |

------------------------------------------------------------------------

*For more information, please contact:*\
\
Naman Paul ([naman.paul\@bccdc.ca](mailto:naman.paul@bccdc.ca))
