
#user class is the base class for customer and admin classes
class User:
    def __init__(self,user_id='u_0000000000',user_name='',user_password =None ,user_register_time="00-00-0000_00:00:00",user_role='customer' ):
        self.user_id = user_id
        self.user_name = user_name
        self.user_password = user_password
        self.user_register_time = user_register_time
        self.user_role = user_role

    def __str__(self):
        ret_str =  "'user_id':'{}', 'user_name':'{}', 'user_password':'{}', 'user_register_time':'{}', 'user_role':'{}'".format(
            self.user_id, self.user_name, self.user_password, self.user_register_time, self.user_role)
        return '{' + ret_str + '}'

