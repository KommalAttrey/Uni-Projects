from operation_user import UserOperation
from model_customer import Customer
import time
class CustomerOperation(UserOperation):

    def validate_email(self,user_email):
        #splitting user input into two parts by @
        email = user_email.split("@")
        if len(email) == 2:
            user_name = email[0]
            domain_name = email[1]
            #providing conditions to check if username and domain name are in correct format
            if len(user_name) == 0:
                return False
            elif "." not in domain_name:
                return False
            else:
                dot_index = domain_name.index(".")
                #checking for dot_index
                if dot_index == 0 or dot_index == len(domain_name) - 1:
                    return False
        else:
            return False
        return True


    def validate_mobile(self,user_mobile):
        #length of usermobile should be 10 atleast and there should be 1 digit atleast
        if len(user_mobile) == 10  and user_mobile.isdigit():
            #checking if user mobile is starting with 04 or 03
            if user_mobile.startswith('04') or user_mobile.startswith('03'):
                return True
        else:
            return False
    def register_customer(self,user_name,user_password,user_email,user_mobile):
        u = UserOperation()
        #checking if this username already exist
        if u.check_username_exist(user_name):

            return False
        #checking username,userpassword,email and mobile for validation
        if (
            u.validate_username(user_name)
            and u.validate_password(user_password)
            and self.validate_email(user_email)
            and self.validate_mobile(user_mobile)
        ):

            try:
                register_time = time.strftime("%d-%m-%Y_%H:%M:%S")
                #creating an instance of customer class
                customer=Customer(
                    user_id=self.generate_unique_user_id(),user_name=user_name,
                    user_password=self.encrypt_password(user_password),user_register_time=time.strftime("%d-%m-%Y_%H:%M:%S"),
                    user_role='customer',user_email=user_email,user_mobile=user_mobile)
                #opening file and appending the details of customer
                with open("data/users.txt", "a") as file:
                    file.write(str(customer))
                return True
            except IOError:
                return False
        return False
    def update_profile(self,attribute_name,value,customer_object):
        u=UserOperation()
        #first checking the attribute condition what user wants to use
        if attribute_name == "user_name":
            if u.validate_username(value):
                customer_object.user_name = value
            else:
                return False
        elif attribute_name == "user_password":
            if u.validate_password(value):
                customer_object.user_password = value
            else:
                return False
        elif attribute_name == "user_email":
            if self.validate_email(value):
                customer_object.user_email = value
            else:
                return False
        elif attribute_name == "user_mobile":
            if self.validate_mobile(value):
                customer_object.user_mobile = value
            else:
                return False
        else:
            return False
        #after getting the attribute information and checking for validation
        # Updating the information in database file
        updated_lines = []
        with open("data/users.txt", "r") as file:
            for line in file:
                user_data = line.strip().split(",")
                if len(user_data) < 7:
                    updated_lines.append(line)
                    continue

                user_id = user_data[0].strip().split(":")[1].strip().strip("'")
                user_name = user_data[1].strip().split(":")[1].strip().strip("'")
                user_password = user_data[2].strip().split(":")[1].strip().strip("'")
                user_register_time = user_data[3].strip().split(":")[1].strip().strip("'")
                user_role = user_data[4].strip().split(":")[1].strip().strip("'")
                user_email = user_data[5].strip().split(":")[1].strip().strip("'")
                user_mobile = user_data[6].strip().split(":")[1].strip().strip("'")

                if user_id == customer_object.user_id:
                    if attribute_name == "user_name":
                        user_name = value
                    elif attribute_name == "user_password":
                        user_password = u.encrypt_password(value)
                    elif attribute_name == "user_email":
                        user_email = value
                    elif attribute_name == "user_mobile":
                        user_mobile = value

                    updated_line = "'user_id':'{}', 'user_name':'{}', 'user_password':'{}', 'user_register_time':'{}', 'user_role':'{}','user_email':'{}','user_mobile':'{}'\n".format(
                        user_id, user_name, user_password, user_register_time, user_role, user_email, user_mobile)
                    updated_lines.append(updated_line)
                else:
                    updated_lines.append(line)

        with open("data/users.txt", "w") as file:
            file.writelines(updated_lines)
        return True
    def delete_customer(self, customer_id):
        #deleting customer using customer id
        lines = []
        deleted = False
        with open("data/users.txt", "r") as file:
            for line in file:
            #checking if user input is present in  database file
                if customer_id in line:
                    deleted = True
                else:
                    lines.append(line)
        if not deleted:
            return False

        with open("data/users.txt", "w") as file:
            file.writelines(lines)

        return True

    def get_customer_list(self, page_number):
        #getting list of customers using page number
        #there are only 2 pages for customer
        customers_per_page = 10
        start_index = (page_number - 1) * customers_per_page
        end_index = start_index + customers_per_page

        customers = []
        total_pages = 0
        with open("data/users.txt", "r") as file:
            lines = file.readlines()
            total_pages = (len(lines) + customers_per_page - 1) // customers_per_page
            lines = lines[start_index:end_index]
            for line in lines:
                customer_data = eval(line)
                if customer_data['user_role']=='customer':
                    cus1=Customer(
                        user_name=customer_data['user_name'],
                        user_id=customer_data['user_id'],
                        user_role=customer_data['user_role'],
                        user_email=customer_data['user_email'],
                        user_mobile=customer_data['user_mobile'],
                        user_register_time=customer_data['user_register_time']

                    )

                    customers.append(cus1)

        return customers, page_number, total_pages

    def delete_all_customers(self):
        #deleting all customers
        with open("data/users.txt", "w") as file:
            file.write("")

