class Product:
    def __init__(self,pro_id="",pro_model="",pro_category="",pro_name="",pro_current_price=0.0,pro_raw_price=0.0,pro_discount=0.0,pro_likes_count=0):
        self.pro_id = pro_id
        self.pro_model = pro_model
        self.pro_category = pro_category
        self.pro_name = pro_name
        self.pro_current_price = pro_current_price
        self.pro_raw_price = pro_raw_price
        self.pro_discount = pro_discount
        self.pro_likes_count = pro_likes_count
    def __str__(self):
        return f"Product ID: {self.pro_id}\nModel: {self.pro_model}\nCategory: {self.pro_category}\nName: {self.pro_name}\nCurrent Price: {self.pro_current_price}\nRaw Price: {self.pro_raw_price}\nDiscount: {self.pro_discount}\nLikes Count: {self.pro_likes_count}"















