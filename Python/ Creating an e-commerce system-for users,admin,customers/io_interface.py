class IOInterface:

    def get_user_input(self,message, num_of_args):
        #getting user input by this method
        user_input = input(message).split()
        result = user_input[:num_of_args]
        #setting default if correct input is not provided
        if len(result) < num_of_args:
            result.extend([''] * (num_of_args - len(result)))
        return result
    def main_menu(self):
        global choice
        print("***************")
        print("    MAIN MENU    ")
        print("***************")
        print("1. Login")
        print("2. Register")
        print("3. Quit")

        self.choice = input("Enter your choice (1-3): ")

        '''if choice == '2':
            username = input("Enter a username: ")
            if username.lower() == "admin":
                print("Admin account cannot be registered.")
                return'''
    def admin_menu(self):
        print("******************")
        print("    ADMIN MENU     ")
        print("******************")
        print("1. Show products")
        print("2. Add customers")
        print("3. Show customers")
        print("4. Show orders")
        print("5. Generate test data")
        print("6. Generate all statistical figures")
        print("7. Delete all data")
        print("8. Delete customer using customer ids")
        print("9. Delete order using order id")
        print("10. Delete product using product id")
        print("11. Logout")

    def customer_menu(self):

        print("******************")
        print("  CUSTOMER MENU   ")
        print("******************")
        print("1. Show profile")
        print("2. Update profile")
        print("3. Show products (e.g., '3 keyword' or '3')")
        print("4. Show history orders")
        print("5. Generate all consumption figures")
        print("6. Get product using product id")
        print("7. Logout")

    def show_list(self,user_role, list_type, object_list):
        #checking type of list and user role
        if list_type == "Customer" and user_role == "customer":
            print("Access denied. You don't have permission to view customer list.")
            return
        #checking if object list is none
        if not object_list or object_list[0] is None:
            print("No Data Available")
            return
        #getting page number and total page from object list
        page_number = object_list[1]
        total_page = object_list[2]

        print(f"List Type: {list_type}")
        print(f"Page Number: {page_number}/{total_page}")
        print()
        for dic in object_list[0]:
            print(dic)

    def print_error_message(self,error_source, error_message):
        print("Error occurred in:", error_source)
        print("Error message:", error_message)

    def print_message(self,message):
        print(message)

    def print_object(self,target_object):
        print(str(target_object))



