
from model_user import User
from model_admin import Admin
from model_customer import Customer
from model_order import Order
from model_product import Product
from operation_user import UserOperation
from io_interface import IOInterface
from operation_customer import CustomerOperation
from operation_order import OrderOperation
from opreation_product import ProductOperation
from operation_admin import AdminOperation


_interface=IOInterface()
_user=UserOperation()
_customer=CustomerOperation()
_order=OrderOperation()
_product=ProductOperation()
_admin=AdminOperation()
def login_control():
    while True:
        _interface.main_menu()
        choice = _interface.choice
        if choice=='1':
            username, password=_interface.get_user_input("Enter Username and Password (ex 'user_name' space 'user_pass' ): ", 2)
            en_pass=_user.encrypt_password(password)
            user_details=_user.login(username,_user.decrypt_password(en_pass))
            if user_details is None:
                _interface.print_error_message('user_details','Check your login details again something went wrong')
            else:
                if isinstance(user_details, Customer) and user_details.user_role == 'customer':
                    customer_control(user_details)
                elif isinstance(user_details, Admin) and user_details.user_role == 'admin':

                    admin_control(user_details)
                else:
                    _interface.print_error_message('Login','Check again for login details ')
        elif choice=='2':
            username=_interface.get_user_input("Enter your username as customer or admin: ",1)[0]
            if username.lower() == "admin":
                _interface.print_message("You can't register as Admin.")

            elif username.lower() == "customer":
                name,password,email,mobile=_interface.get_user_input(' Username, Password, Email, Mobile is required to register a customer provide this information with a space between them : ' ,4)
                _customer.register_customer(name,password,email,mobile)
                _interface.print_message("Registered successfully you can log in now using your username and password ")
            else:
                _interface.print_message("Check username again")
        elif choice=='3':
            break

        else:
            _interface.print_message("Not a correct choice!")


def customer_control(user_details):
    while True:
        _interface.customer_menu()
        user_choice = _interface.get_user_input("Enter your choice here: ", 1)[0]

        if user_choice == '1':
            _interface.print_message('User ID: ' + user_details.user_id)
            _interface.print_message('Username: ' + user_details.user_name)
            _interface.print_message('User Email: ' + user_details.user_email)
            _interface.print_message('User Mobile: ' + user_details.user_mobile+ '\n')
        elif user_choice == '2':
            attr,value=_interface.get_user_input("enter the attr name(ex-'user_name') and new value for update",2)
            _customer.update_profile(attr,value,user_details)
            if not _customer.update_profile(attr,value,user_details):
                _interface.print_message("Something went wrong check again your input")
            else:
                _interface.print_message("----------Updated---------------------------")
        elif user_choice == '3':
            page,key=_interface.get_user_input("Enter the page number and keyword : ",2)
            if not page.isdigit():
                _interface.print_error_message('integer', 'page number must be a digit')
            elif key == '':
                _interface.show_list(user_details.user_role,'Product', _product.get_product_list(eval(page)))
            else:
                _interface.show_list(user_details.user_role, 'Product',
                             [_product.get_product_list_by_keyword(key), 'NA', 'NA'])

        elif user_choice == '4':
            page = _interface.get_user_input("enter the page number",1)[0]
            if page==None:
                _interface.print_error_message("integer","page number must be digit")
            else:

                _interface.show_list(user_details.user_role,'Order',_order.get_order_list(user_details.user_id, page))

        elif user_choice == '5':
            c=_interface.get_user_input("customer id for consumption figure",1)[0]
            _interface.print_message("you press 5 it is generating figures please wait")
            _order.generate_single_customer_consumption_figure(c)


        elif user_choice == '6':
            #Get_product_id_by_using productid
            pro_info=_interface.get_user_input("Enter product id :",1)[0]
            a=_product.get_product_by_id(pro_info)
            _interface.print_object(a)
        elif user_choice=='7':
            break
        else:
            _interface.print_message("Choose correct option.")
def admin_control(user_details):
    while True:
        _interface.admin_menu()
        user_choice = _interface.get_user_input("Enter your choice here: ", 1)[0]
        if user_choice=='1':
            page=_interface.get_user_input("Enter page number",1)[0]
            if not page.isdigit():
                _interface.print_message(" Page Number must be a digit: ")
                continue
            _interface.show_list('Admin', 'Product', _product.get_product_list(eval(page)))

        elif user_choice=='2':
            username, password, email, mobile= _interface.get_user_input(" Username, Password, Email, Mobile is required to register a customer provide this information with a space between them: ",4)
            if not _customer.register_customer(username, password, email, mobile):
                _interface.print_message("check again the input you provided to reqister a customer")
            else:
                _interface.print_message("-------------Registered---------------------------!\n")

        elif user_choice=='3':
            page = _interface.get_user_input(" Page Number: ",1)[0]
            if not page.isdigit():
                _interface.print_message("Page number must be a digit check it again")
                continue
            _interface.show_list('Admin', 'Customer', _customer.get_customer_list(eval(page)))

        elif user_choice=='4':
            cust_id, page = _interface.get_user_input(" custmoer id and page number for list :", 2)
            while 'u_' not in cust_id or len(cust_id) < 12 or not page.isdigit():
                _interface.print_message("check input again")
                order_str = _interface.get_user_input("Enter Customer ID and page number separated by space: ",1)[0]
                cust_id, page = _interface.get_user_input(order_str, 2)

            _interface.show_list('Admin', 'Order', _order.get_order_list(cust_id, eval(page)))


        elif user_choice=='5':
            _order.generate_test_order_data()
            _product.extract_products_from_files()
            _interface.print_message("Data is generated")
        elif user_choice=='6':
            _interface.print_message("please wait generating figures")
            _order.generate_all_customers_consumption_figure()
            _order.generate_all_top_10_best_sellers_figure()
            _product.generate_likes_count_figure()
            _product.generate_discount_figure()  # discount like count
            _product.generate_category_figure()  # category
            _product.generate_discount_likes_count_figure()
        elif user_choice=='7':
            _order.delete_all_orders()
            _product.delete_all_products()
            _customer.delete_all_customers()
            _interface.print_message("Data Deleted!\n")
        elif user_choice=='8':

            cust_id= _interface.get_user_input("Provide ID of Customer : ",1)[0]
            if not _customer.delete_customer(cust_id):
                _interface.print_message("NOT Exist")
            else:
                _interface.print_message("Customer Deleted from database!\n")
        elif user_choice=='9':
            ord_id = _interface.get_user_input("Provide Order  ID: ",1)[0]
            if not _order.delete_order(ord_id):
                _interface.print_message("NOT Exist")
            else:
                _interface.print_message(" ORDER Deleted from database!\n")
        elif user_choice=='10':
            pro_id= _interface.get_user_input("Provide Product ID : ",1)[0]
            if not _product.delete_product(pro_id):
                _interface.print_message("Not Exist")
            else:
                _interface.print_message("Product Deleted from database!\n")
        elif user_choice=='11':
            break
        else:
            _interface.print_message("not a correct choice ")

def main():


    _product.extract_products_from_files()
    _order.generate_test_order_data()
    _admin.register_admin()

    login_control()
    #calling main
if __name__ == "__main__":
    main()