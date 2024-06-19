import csv
import os
from model_product import Product
import matplotlib.pyplot as plt


class ProductOperation:
    def extract_products_from_files(self):
        input_folder = 'data/product/'
        output_file = 'data/products.txt'

        products = []
        #making a list of all product files
        files = ['accessories.csv','bags.csv','beauty.csv','house.csv','jewelry.csv','kids.csv','men.csv','shoes.csv','women.csv']  # Add more file names if needed
        ids=[]
        #making list to store unique product ids
        for file_name in files:

            file_path = os.path.join(input_folder, file_name)
            with open(file_path, "r") as file:
                csv_reader = csv.reader(file)
                #using for skipping the header
                next(csv_reader)
                #getting values and appending to product list
                for row in csv_reader:
                    if row[-2] in ids:
                        continue
                    product = {
                        'pro_id': row[-2],
                        'pro_model': row[-1],
                        'pro_category': row[0],
                        'pro_name': row[2],
                        'pro_current_price': row[3],
                        'pro_raw_price': row[4],
                        'pro_discount': row[6],
                        'pro_likes_count': row[7]
                    }
                    ids.append(row[-2])
                    products.append(product)

        with open(output_file, 'w') as file:
            for product in products:
                file.write(str(product) + '\n')

    def get_product_list(self,page_number):
        input_file = 'data/products.txt'
        products_per_page = 10
        #only 10 products per page
        with open(input_file, 'r') as file:
            lines = file.readlines()
            total_products = len(lines)
            #calculating total pages for product
            total_pages = (total_products + products_per_page - 1) // products_per_page
            start_index = (page_number - 1) * products_per_page
            end_index = start_index + products_per_page
            product_lines = lines[start_index:end_index]
            #appending the product list
            product_lists=[eval(product.strip()) for product in product_lines]
            return product_lists, page_number, total_pages

    def delete_product(self,product_id):
        #deleting product using product id
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
        with open('data/products.txt', 'w') as file:
            #when product id match it will skip that line
            found = False
            for line in lines:
                product_data = eval(line.strip())
                if product_data['pro_id'] == product_id:
                    found = True
                    continue
                file.write(line)

        return found
    def get_product_list_by_keyword(self,keyword):
        #getting product list using keyword
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
        #when keyword match it will append the product list by that product
        product_list = []
        for line in lines:
            product_data = eval(line.strip())
            if keyword.lower() in product_data['pro_name'].lower():
                product = Product(**product_data)
                product_list.append(product)

        return product_list
    def get_product_by_id(self,product_id):
        #getting product by searching product id
        a=Product()
        with open('data/products.txt', 'r') as file:
            for line in file:
                product_data = eval(line.strip())
                if product_data['pro_id'] == product_id:
                    product = Product(
                        pro_id=product_data['pro_id'],
                        pro_model=product_data['pro_model'],
                        pro_category=product_data['pro_category'],
                        pro_name=product_data['pro_name'],
                        pro_current_price=product_data['pro_current_price'],
                        pro_raw_price=product_data['pro_raw_price'],
                        pro_discount=product_data['pro_discount'],
                        pro_likes_count=product_data['pro_likes_count']
                    )
                    return product
        return None

    def generate_category_figure(self):
        categories = {}

        # opening products file in read mode
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
            #counting products by category
        for line in lines:
            product_data = eval(line.strip())
            category = product_data['pro_category']
            if category in categories:
                categories[category] += 1
            else:
                categories[category] = 1

        # sorting
        sorted_categories = sorted(categories.items(), key=lambda x: x[1], reverse=True)

        # x and y axis to plot
        x = [category for category, count in sorted_categories]
        y = [count for category, count in sorted_categories]
        plt.bar(x, y)
        plt.xlabel('Category type')
        plt.ylabel('Number of Products available for category')
        plt.title('Total Number of Products by Category')
        plt.xticks(rotation='vertical')
        plt.savefig('data/figure/generate_category_figure.png')
        plt.show()
    def generate_discount_figure(self):
        #setting range
        discount_ranges = {
            '<30%': 0,
            '30-60%': 0,
            '>60%': 0
        }
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
        for line in lines:
            product_data = eval(line.strip())
            discount = int(product_data['pro_discount'])
            #checking discount range
            if discount < 30:
                discount_ranges['<30%'] += 1
            elif discount >= 30 and discount <= 60:
                discount_ranges['30-60%'] += 1
            else:
                discount_ranges['>60%'] += 1

            labels = list(discount_ranges.keys())
            sizes = list(discount_ranges.values())
            plt.pie(sizes, labels=labels, autopct='%1.1f%%')
            #autopc =auto percentage
            plt.title('Proportion of Products by Discount Range')
            plt.savefig('data/figure/generate_discount_figure.png')
            plt.show()
    def generate_likes_count_figure(self):
        #a bar chart will be created
        #creating a dictionary
        categories_likes = {}
        #opening file in read mode
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
        for line in lines:
            product_data = eval(line.strip())
            category = product_data['pro_category']
            likes = int(product_data['pro_likes_count'])
            #calculating like counts
            if category in categories_likes:
                categories_likes[category] += likes
            else:
                categories_likes[category] = likes
                #sorting
        sorted_categories_likes = sorted(categories_likes.items(), key=lambda x: x[1])
        x = [category for category, likes in sorted_categories_likes]
        y = [likes for category, likes in sorted_categories_likes]
        plt.bar(x, y)
        plt.xlabel('Category')
        plt.ylabel('Sum of Likes Count')
        plt.title('Sum of Likes Count by Category')
        plt.xticks(rotation='vertical')
        plt.savefig('data/figure/generate_likes_count_figure.png')
        plt.show()

    def generate_discount_likes_count_figure(self):
        #scatter chart
        likes = []
        discounts = []
        #using lists
        # opening products file in read mode
        with open('data/products.txt', 'r') as file:
            lines = file.readlines()
            #talking data from product file to count likes and discount
        for line in lines:
            product_data = eval(line.strip())
            likes.append(int(product_data['pro_likes_count']))
            discounts.append(int(product_data['pro_discount']))
        plt.scatter(discounts, likes)
        plt.xlabel('Discount')
        plt.ylabel('Likes Count')
        plt.title('Likes Count and Discount')
        plt.grid(True)
        plt.savefig('data/figure/generate_discount_likes_count_figure.png')
        plt.show()

    def delete_all_products(self):
            # delete all the products from database
        with open('data/products.txt', 'w') as file:
            file.write('')

