class Order:
    def __init__(self, order_id="o_00000", user_id="", pro_id="", order_time="00-00-0000_00:00:00"):
        self.order_id = order_id
        self.user_id = user_id
        self.pro_id = pro_id
        self.order_time = order_time

    def __str__(self):
        return "{{'order_id':'{}', 'user_id':'{}', 'pro_id':'{}', 'order_time':'{}'}}".format(
            self.order_id, self.user_id, self.pro_id, self.order_time)
