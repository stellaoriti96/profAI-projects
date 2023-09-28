def show_products_details(inventory):

    """
    To print product name, quantity and selling price 

    Parameters: 
    inventory: a list of dictionaries, each one containing the product info

    Return values:
    none

    """

    for dic in inventory:
        print(f'{dic["quantity"]} X {dic["name"]}: € {dic["selling price"]}')


def is_valid_name(name):

    """ 
    To estabilish name validity 

    Parameters:
    name: string

    Return values:
    Booleans

    """
    return name.isalpha()


def is_valid_quantity(quantity):

    """
    To estabilish quantity validity
    
    Parameters:
    quantity: str

    Return values:
    Booleans

    """
    return quantity.isdigit()


def is_valid_price(price):

    """
    To estabilish price validity

    Parameters:
    price: str

    Return values:
    Booleans

    """

    if price.isdigit():
        return True

    try:
        price_num=float(price)
        parts=price.split(".")
        if price_num >0 and len(parts[1])<=2:
            return True
        return False
    except ValueError:
        return False
        
