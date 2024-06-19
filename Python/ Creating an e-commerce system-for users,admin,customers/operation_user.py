
import random
import string

from model_admin import Admin
from model_customer import Customer


class UserOperation:
    def generate_unique_user_id(self):
        #generating user id for users starting from u_
        user_id= 'u_' + ''.join(random.choices('0123456789', k=10))
        return user_id
    def encrypt_password(self,user_password):
        user_password_length=len(user_password)
        #doubling the length
        encrypt_password_length=user_password_length*2
        #using  a-zA-Z0-9
        characters_chosen= string.digits + string.ascii_letters
        #generating a new string  from ^^ and ending by $$
        random_string=''.join(random.choices(characters_chosen,k=encrypt_password_length))
        encrypted_password="^^"
        index=0
        #generating a patteren for password
        for char in user_password:
            encrypted_password=encrypted_password+random_string[index]+random_string[index+1]+char
            index+=2
        encrypted_password+="$$"
        return encrypted_password
    def decrypt_password(self,encrypted_password):
        #decrypting the password
        #removing first and last two character from it
        encrypted_password = encrypted_password[2:-2]
        decrypted_password = ""
        index = 0
        while index < len(encrypted_password):
            decrypted_password += encrypted_password[index + 2]
            index += 3
        return decrypted_password

    def check_username_exist(self, user_name):
        #matching the given username by every user in database
        with open('data/users.txt', 'r') as f:
            for x1 in f:
                x = x1.strip().split(',')
                if x[1] == user_name:
                    return True
            return False

    def validate_username(self,user_name):
        if len(user_name) >= 5 and "_" in user_name or user_name.isalpha() :
            return True
        else:
            return False

    def validate_password(self,user_password):
        if len(user_password) < 5:
            return False
        #setting flags
        has_letter = False
        has_number = False
        for char in user_password:
            if char.isalpha():
                has_letter = True
            elif char.isdigit():
                has_number = True
        if has_letter and has_number:
            return True
        else:
            return False

    def login(self, user_name, user_password):
        with open('data/users.txt', 'r') as file:
            for line in file:
                user_data = eval(line.strip())
                #checking for username and password to login
                if user_data['user_name'].upper() == user_name.upper():
                    if self.decrypt_password(user_data['user_password']) == user_password:
                        if user_data['user_role']=='customer':
                            user_role=user_data['user_role']
                            user_id=user_data['user_id']
                            user_name=user_data['user_name']
                            user_pass=user_data['user_password']
                            user_email = user_data['user_email']
                            user_mobile = user_data['user_mobile']
                            user_reg_time=user_data['user_register_time']


                            return Customer(user_id,user_name,user_password,user_reg_time,user_role,user_email,user_mobile)
                        else:
                            return Admin( user_name, user_password)


        return False



