# Toy Products on Amazon

## About Dataset

### Context
This is a pre-crawled dataset, taken as subset of a bigger dataset (more than 115k products) that was created by extracting data from Amazon.com by https://www.promptcloud.com/. The data were released on the following [Kaggle page](https://www.kaggle.com/datasets/PromptCloudHQ/toy-products-on-amazon) under the licence [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/). The data was downloaded in csv format from the kaggle page and was not edited further.

### Content
This dataset has following fields:

**product_name**

**manufacturer** - The item manufacturer, as reported on Amazon. Some common "manufacturers", like Disney, actually outsource their assembly line.

**price**

**number_available_in_stock**

**number_of_reviews**

**number_of_answered_questions** - Amazon includes a Question and Answer service on all or most of its products. This field is a count of how many questions that were asked actually got answered.

**average_review_rating**

**amazon_category_and_sub_category** - A tree-based, >>-delimited categorization for the item in question.

**customers_who_bought_this_item_also_bought** - References to other items that similar users bought. This is a recommendation engine component that played a big role in making Amazon popular initially.

**description**

**product_information**

**product_description**

**items_customers_buy_after_viewing_this_item**

**customer_questions_and_answers** - A string entry with all of the product's JSON question and answer pairs.

**customer_reviews** - A string entry with all of the product's JSON reviews.

**sellers** - A string entry with all of the product's JSON seller information (many products on Amazon are sold by third parties).


## Acknowledgements
This dataset was created by PromptCloud's in-house web-crawling service.

## Inspiration
This detailed dataset can be used to answer questions like:

- What types of toys are most popular on Amazon?
- How dominant are brands in the Amazon toy market?
- Can you break down reviews to analyze their sentiment and contents?