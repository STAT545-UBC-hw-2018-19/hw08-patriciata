# hw08-patriciata
This is the repository for STAT547M Homework 8 by Patricia Angkiriwang. Homework prompt can be found [here](http://stat545.com/Classroom/assignments/hw08/hw08.html).

Final Shiny App can be found [here](https://triciaa.shinyapps.io/bcl_app/).

Data and boilerplate template code for this assignment was taken from Dean Attali's [tutorial](https://deanattali.com/blog/building-shiny-apps-tutorial/) (code [here](https://deanattali.com/blog/building-shiny-apps-tutorial/#12-final-shiny-app-code)). Data contains all products sold by BC Liquor, originally found [here](https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices) and [here](http://pub.data.gov.bc.ca/datasets/176284/BC_Liquor_Store_Product_Price_List.csv).

Elements added were:
- Addition of Shiny theme with shinythemes
- Selection of multiple alcohol types
- Filter by countries
- Fixed error that occurred when filtered dataset had 0 rows
- Addition of descriptive text above plot
- Selection of binwidth
- Hide and show binwidth selector using shinyjs
- Update max of bin width selector according to maximum price limit selected 
- Addition of tabs
- Visualization of BC Liquor data as a network of countries and drinks
- (Minor tweaks to histogram)


|  Assignment Files    |     |
|-----------|-------------|
|[BC Liquor App](https://triciaa.shinyapps.io/bcl_app/)| App hosted on shinyapps.io |
|[app.R]()| Code for this Shiny app, with links and comments |
|[bcl_data.csv]()| Dataset used in the app |

