import json
import os.path
from shop import *

command=None

while command!="close":

    if command in ("add","list","sell"):
        
        try:
            json_file=open("vegan_shop_inventory.json","r+",encoding='UTF-8')
            inventory=json.load(json_file)

        except:
            json_file=open("vegan_shop_inventory.json","w",encoding='UTF-8') 
            inventory=[]

        finally:

            if command=="add":
                
                product_to_add=input("Product name: ")
                while is_valid_name(product_to_add)==False:
                    print("Name format is not valid")
                    product_to_add=input("Product name: ")
                
                
                quantity_to_add=input("Quantity: ")
                while is_valid_quantity(quantity_to_add)==False:
                    print("Quantity format is not valid")
                    quantity_to_add=input("Quantity: ")
                quantity_to_add=int(quantity_to_add)

                product_is_present=False

                for dic_inv in inventory:
                    
                    if dic_inv["name"]==product_to_add:
                        dic_inv["quantity"]+=quantity_to_add
                        product_is_present=True
                        break
                
                if not product_is_present:
                    
                    purchase_price=input("Purchase price: ")
                    while is_valid_price(purchase_price)==False:
                        print("Price format is not valid")
                        purchase_price=input("Purchase price: ")
                    purchase_price=float(purchase_price)
                    
                    selling_price=input("Selling price: ")
                    while is_valid_price(selling_price)==False:
                        print("Price format is not valid")
                        selling_price=input("Selling price: ")
                    selling_price=float(selling_price)
                    
                    inventory.append({"name": product_to_add, "quantity": quantity_to_add, "purchase price": purchase_price, "selling price": selling_price})

                print(f"Added: {quantity_to_add} X {product_to_add}")

                json_file.seek(0)
                json.dump(inventory,json_file,ensure_ascii=False,indent=4)
                json_file.truncate()
                
            elif command=="list":

                show_products_details(inventory)


            elif command=="sell":

                if inventory!=[]:

                    selling_request="yes"

                    products_to_sell=[]

                    while selling_request=="yes":

                        product_to_sell=input("Product name: ")
                        while is_valid_name(product_to_sell)==False:
                            print("Name format is not valid")
                            product_to_sell=input("Product name: ")
                        
                        product_is_present=False

                        for dic_inv in inventory:
                            
                            if dic_inv["name"]==product_to_sell:

                                product_is_present=True
                        
                                quantity_to_sell=input("Quantity: ")
                                while is_valid_quantity(quantity_to_sell)==False:
                                    print("Quantity format is not valid")
                                    quantity_to_sell=input("Quantity: ")
                                quantity_to_sell=int(quantity_to_sell)
                                dic_inv["quantity"]-=quantity_to_sell

                                selling_price=dic_inv["selling price"]

                                purchase_price=dic_inv["purchase price"]

                                break

                        if not product_is_present:
                            print ("Warning: out of stock!")
                            continue

                        products_to_sell.append({"name": product_to_sell, "quantity": quantity_to_sell, "purchase price": purchase_price, "selling price": selling_price})

                        selling_request=input("Do you want to add another product? [yes/no]: ")

                        
                    json_file.seek(0)
                    json.dump(inventory,json_file,ensure_ascii=False,indent=4)
                    json_file.truncate()
                    
                    with open("sales.json", "a+", encoding='UTF-8') as json_sales:
                        path=os.getcwd()
                        if os.path.getsize(path+"\sales.json")!=0:
                            json_sales.seek(0)
                            products_sold=json.load(json_sales)
                        else:
                            products_sold=[]
                    products_sold.extend(products_to_sell)
                    with open("sales.json", "w",encoding='UTF-8') as json_sales:
                        json.dump(products_sold,json_sales,ensure_ascii=False,indent=4)
                    
                    print("\nSale recorded")
                    show_products_details(products_to_sell)

                else:
                    print("NO products to sell")

        
    elif command=="revenues":

        path=os.getcwd()
        
        if os.path.exists(path+"\sales.json"):
            
            with open("sales.json", encoding='UTF-8') as json_sales:
                
                sales=json.load(json_sales)
            
                gross_profit=0
                net_profit=0
            
                for dic_sales in sales:
                    gross_profit+=round(dic_sales["selling price"]*dic_sales["quantity"],2)
                    net_profit+=round((dic_sales["selling price"]-dic_sales["purchase price"])*dic_sales["quantity"],2)
                
                print(f"Gross profit: {gross_profit} €"
                "\n"
                f"Net profit: {net_profit} €")
        else:
            print("NO Revenues")

    elif command=="help":

        print("\nAvailable commands:\n"
            "add: add new products to stock\n"
            "list: list the products in the stock\n"
            "sell: register a sell \n"
            "revenues: show total revenues\n"
            "help: show possible commands\n"
            "close: close the program")

    else:

        print("Available commands:\n"
            "add: add new products to stock\n"
            "list: list the products in the stock\n"
            "sell: register a sell \n"
            "revenues: show total revenues\n"
            "help: show possible commands\n"
            "close: close the program")
        
    print("\n")
    command=input("Insert a command: ")