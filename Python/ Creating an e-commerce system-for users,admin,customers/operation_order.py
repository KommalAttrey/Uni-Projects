import random
import time
import pandas as pd
import matplotlib.pyplot as plt
from model_customer import Customer
from model_order import Order
from operation_user import UserOperation
from opreation_product import ProductOperation
import os

class OrderOperation:
    def generate_unique_order_id(self):
        # opening file from database in reading mode and checking for already existing userids
        with open('data/orders.txt', 'r') as file:
            existing_order_ids = [line.strip() for line in file.readlines()]

        # generating a unique order id starting from o_ and using randint for random integer after that
        while True:
            order_id = 'o_' + str(random.randint(10000, 99999))
            if order_id not in existing_order_ids:
                break

        #will return the id of order
        return order_id

    def create_an_order(self, customer_id, product_id, create_time=None):
        order_id = self.generate_unique_order_id()

        # when create time is none it is using this
        if create_time is None:
            create_time = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())

        # orders saved in database
        with open('data/orders.txt', 'a') as file:
            file.write(
                f"Order ID: {order_id}, Customer ID: {customer_id}, Product ID: {product_id}, Create Time: {create_time}\n")
        return True

    def delete_order(self, order_id):
        #delete order by order id
        deleted = False
        with open('data/orders.txt', 'r') as file:
            lines = file.readlines()
        with open('data/orders.txt', 'w') as file:
            for line in lines:
                order_dic=eval(line.strip())
                # eval is used to convert it into a dictionary object.
                if order_dic['order_id']!=order_id:
                    file.write(line)
                else:
                    deleted = True
        return deleted
    def get_order_list(self,customer_id, page_number):
        #number of orders it is displaying per page
        orders_per_page = 10
        order_list=[]
        #reading order from file
        with open("data/orders.txt", "r") as file:
            orders=[eval(line.strip())for line in file]
            #filtering the order from file
        filtered_orders = [order for order in orders if customer_id==order['user_id']]
        total_orders = len(filtered_orders)
        total_pages = (total_orders // orders_per_page) + (1 if total_orders % orders_per_page != 0 else 0)
        page_number = int(page_number)
        start_index = (page_number - 1) * orders_per_page
        end_index = start_index + orders_per_page

        # Getting order from current page
        current_page_orders = filtered_orders[start_index:end_index]
        return current_page_orders, page_number, total_pages

    def generate_test_order_data(self):

        p_obj = ProductOperation()


        # Generating 10 customers by myself by fixing a specific format
        customer_ids = []
        product_ids=[]
        with open("data/users.txt", "w") as file:
            name='abcdefghij'
            u_obj = UserOperation()
            for _ in range(10):
                customer_id = u_obj.generate_unique_user_id()
               #writing file in users.txt file in this format
                file.write(
                    str(Customer(customer_id, 'cust' + name[_],u_obj.encrypt_password('custpass1' + name[_]),
                                 time.strftime("%d-%m-%Y_%H:%M:%S"), 'customer', 'cust' + name[_] + '@gmail.com',
                                 '0333431101' + str(_))) + '\n')
                customer_ids.append(customer_id)

        with open('data/products.txt', 'r') as product_file, open('data/orders.txt', 'w') as order_file:
            product_ids = product_file.readlines()
            # generating random number of orders for each customer
            for customer_id in customer_ids:
                num_orders = random.randint(50, 200)

                for _ in range(num_orders):
                    product_id = eval(random.choice(product_ids).strip())
                    order_date = self.random_date()
                    quantity = random.randint(1, 10)
                    #writing the order in the database file in this format
                    order_file.write(
                       '{'+ f"'order_id':'{self.generate_unique_order_id()}','user_id':'{customer_id}','pro_id':'{product_id['pro_id']}','order_time':'{order_date}'"+'}\n')

    def random_date(self):
        # This method generates random date within this range
        start_date = time.mktime(time.strptime("01-01-2022_00:00:00", "%d-%m-%Y_%H:%M:%S"))
        end_date = time.mktime(time.strptime("31-12-2022_23:59:59", "%d-%m-%Y_%H:%M:%S"))
        random_time = random.uniform(start_date, end_date)
        return time.strftime("%d-%m-%Y_%H:%M:%S", time.localtime(random_time))

    def delete_all_orders(self):
        #it will delete all the orders
        with open('data/orders.txt', 'w') as orders_file:
            orders_file.write('')


    def generate_single_customer_consumption_figure(self, customer_id):
        month_consumption = {'M1': 0, 'M2': 0, 'M3': 0, 'M4': 0, 'M5': 0, 'M6': 0, 'M7': 0,
                             'M8': 0, 'M9': 0, 'M10': 0,
                             'M11': 0, 'M12': 0}
        #creating a dictionary as product data which has two empty lists as values
        product_data = {'product_id': [], 'product_price': []}

        with open('data/products.txt', 'r') as products_file:
            for line in products_file:
                product = eval(line)
                product_data['product_id'].append(product['pro_id'])
                product_data['product_price'].append(float(product['pro_current_price']))
        #pd refers to pandas library
        product_frame = pd.DataFrame(product_data)
        #this will set the productid as dataframe
        product_frame.set_index('product_id', inplace=True)
        month_names = list(month_consumption.keys())

        with open('data/orders.txt', 'r') as orders_file:
            for line in orders_file:
                order = eval(line)
                if order['user_id'] == customer_id:
                    #this line is extracting month from order dictionary
                    #calculating cunsumption  amount for each month
                    order_month = int(order['order_time'].split('-')[1].lstrip('0'))
                    product_price = product_frame.loc[order['pro_id']]['product_price']
                    month_consumption[month_names[order_month - 1]] += float(product_price)

        data_frame = pd.DataFrame(
            {'month': list(month_consumption.keys()), 'consumption': list(month_consumption.values())})

        plt.title('single customer consumption figure')
        plt.xlabel('Month names')
        plt.ylabel('Consumption of order')

        plt.bar('month', 'consumption', data=data_frame)
        plt.savefig('data/figure/generate_single_customer_consumption_figure.png')
        plt.show()



    def generate_all_customers_consumption_figure(self):

        #using a dictionary which stores total of order prices (every month)
        monthly_consumption = {}
        product_data = {}
        #getting data from products.txt file and storing in dictionary
        with open('data/products.txt', 'r') as product_file:
            for line in product_file:
                product = eval(line)
                pro_id = product['pro_id']
                pro_price = float(product['pro_current_price'])
                product_data[pro_id] = pro_price

        #reading data from order file
        with open('data/orders.txt', 'r') as order_file:
            for line in order_file:
                #converting it to use as a dictionary
                order_data = eval(line)
                pro_id = order_data['pro_id']
                order_month = order_data['order_time'].split('-')[1]  # Extract the month from the order time

                #using data of product dictionary
                order_price = product_data.get(pro_id)

                if order_price is not None:
                    if order_month in monthly_consumption:
                        monthly_consumption[order_month] += order_price
                    else:
                        monthly_consumption[order_month] = order_price

        # sorting the dictionary
        sorted_monthly_consumption = sorted(monthly_consumption.items(), key=lambda x: x[0])

        # collecting values for plotting
        months = [month for month, _ in sorted_monthly_consumption]
        consumption = [value for _, value in sorted_monthly_consumption]

        # Create the bar chart
        plt.bar(months, consumption)
        plt.xlabel('Months')
        plt.ylabel('Consumption')
        plt.title('Monthly Consumption for All Customers')
        plt.xticks(rotation=90)
        figure_path = 'data/figure/generate_all_customers_consumption_figure.png'
        os.makedirs(os.path.dirname(figure_path), exist_ok=True)
        plt.show()
        # Save the figure
        plt.savefig(figure_path)
        plt.close()

    def generate_all_top_10_best_sellers_figure(self):
        #extracting values of product from product file
        order_data = []
        with open('data/orders.txt', 'r') as order_file:
            for line in order_file:
                order = eval(line)
                order_data.append(order['pro_id'])

        #counting how many times a product is appearing
        product_counts = {}
        for pro_id in order_data:
            product_counts[pro_id] = product_counts.get(pro_id, 0) + 1

        # checking number of orders for each product and then sorting
        sorted_products = sorted(product_counts.items(), key=lambda x: x[1], reverse=True)
        top_10_products = sorted_products[:10]

        product_data = {}
        with open('data/products.txt', 'r') as product_file:
            for line in product_file:
                product = eval(line)
                product_id = product['pro_id']
                if product_id in [pro_id for pro_id, _ in top_10_products]:
                    product_data[product_id] = {
                        'name': product['pro_name'],
                        'price': float(product['pro_current_price'])
                    }

        # will generate the figure
        product_names = [product_data[pro_id]['name'] for pro_id, _ in top_10_products]
        product_prices = [product_data[pro_id]['price'] for pro_id, _ in top_10_products]

        fig, ax = plt.subplots(figsize=(10, 6))  # Adjust the figure size as needed

        ax.barh(product_names, product_prices)  # Use a horizontal bar chart
        ax.set_xlabel('Price')
        ax.set_ylabel('Product')
        ax.set_title('Top 10 Best-Selling Products')
        ax.invert_yaxis()  # Invert the y-axis to display products from top to bottom

        plt.tight_layout()
        plt.savefig('data/figure/generate_all_top_10_best_sellers_figure.png')
        plt.show()








