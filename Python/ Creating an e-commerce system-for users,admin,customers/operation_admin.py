from operation_user import UserOperation
from model_admin import Admin

class AdminOperation(UserOperation):
    def register_admin(self):
        #creating admin id manually
        ad_id = 'a_123456'
        #fixing username and password for admin
        admin_username = "admin"
        admin_pass="admin11"
        # making an instance of admin class
        admin = Admin(user_id=ad_id, user_name = "admin", user_password=self.encrypt_password(admin_pass))
        # opening file in appending mode as writing the instance
        with open("data/users.txt", "a") as file:
            file.write(str(admin) + "\n")


