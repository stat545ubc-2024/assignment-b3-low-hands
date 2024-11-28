# MyShinyApp - USA Accidents

The goal of my app is to analyze and visualize the dataset regarding with the accidents information in US from 2016 to 2023. The overview of the dashboard is in a ShinyApp hosted on shinyapps.io.

## About Dataset

The Dataset used in this respository is based on the open source taken from Kaggle. The link to the original dataset can be found here: [US Accidents (2016 - 2023)](https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents/data)

### Data Preprosessing

Since the original dataset is too large (7,728,394 rows and the size is around 5GB), I did the following work to compress the size of the dataset (The preprosessing file isn't included here).

#### Assignment B3

-   First, I deleted all the rows that contain missing values.
-   Second, I filtered variables that won't be helpful to my project (only keeping 11 variables left).
-   Last, I only picked the first row out of every twenty, which helped me reduce the size of the dataset considerably (only 386,420 rows left and the size is around 30 MB).

#### Assignment B4

Because the functionality that needs to be implemented has changed and I need to streamline the number of variables, I've reprocessed the dataset. There are only 10 variables left after I did so (375516 rows and 28.4MB). When I tried to deploy it on the website, it was still too large to show some of my app's contents online. **So finally I used `sample_n()` to select 30,000 rows of the dataset randomly.**

## Description

### Assignment B3

I focus on the relationship of numbers of accidents in different states in US based on the range of years, so I conducted the following six features:

1.  I add some beautiful font styles and some colors by CSS, making the UI look nicer for users.

2.  The slider allows filter accident data based on a specific range of years, helping users focus on data from the desired time period, which is useful for identifying trends over time.

3.  The download button enables users to export the accident data of states to a .csv file that they filtered, making it easier to perform following specific analysis according to the file.

4.  The image I added highlights the topics to be researched in this app for users: accidents, also makes the app look nicer.

5.  The text output dynamically updates to display the total number of accidents during the specified year range, providing users with a quick summary of the number of accidents based on their selected filters.

6.  The interactive table allows users to sort and browse the filtered accident data conveniently within the application, making it more convenient to explore and understand the dataset.

### Assignment B4

In B4, I keep **all the features** in B3. What's more, I completed more features and the following are what are newly added.

7.  I created a USA map with `leaflet`. It shows the distribution of locations and the number of accidents in each state. The map is dynamic and you can click on it to see more details about specific accidents.

8.  I implemented checkbox feature, specifically the ability to choose whether or not to display auxiliary lines when drawing histograms.

9.  I implemented button feature. This is reflected in `time` tab, which allows you to choose between hourly and monthly charts.

10. I created histograms in **weather** tab. Features about filters are kept.

11. I created bar plots in **environment** tab. Features about filters are kept and you can choose your favorite colors to draw the plots.

12. I created line graphs in **time** tab. Features about filters are kept.

13. I used **Rhtml** to store the text for the information tabs called "About Dataset", "How to Use the App" and "Conclusion".

## Link

B3 Link is here: <https://ruig.shinyapps.io/assignment-b3-low-hands/>

B4 Link is here: <https://ruig.shinyapps.io/AccidentApp/>

## Acknowledgements

-   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, and Rajiv Ramnath. “[A Countrywide Traffic Accident Dataset.](https://arxiv.org/abs/1906.05409)”, 2019.

-   Moosavi, Sobhan, Mohammad Hossein Samavatian, Srinivasan Parthasarathy, Radu Teodorescu, and Rajiv Ramnath. "[Accident Risk Prediction based on Heterogeneous Sparse Data: New Dataset and Insights.](https://arxiv.org/abs/1909.09638)" In proceedings of the 27th ACM SIGSPATIAL International Conference on Advances in Geographic Information Systems, ACM, 2019.
