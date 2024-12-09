# -*- coding: utf-8 -*-
"""
Created on Sun Nov 24 00:08:48 2024

@author: o_kho
"""


def fetch_urbanseeds_products():
    import requests
    from bs4 import BeautifulSoup
    import pandas as pd

    url = "https://nourishevv.org/bags/"
    headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/109.0',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    #'Referer': 'https://nourishevv.org/',
    'Connection': 'keep-alive'
}

    # Send the GET request
    response = requests.get(url, headers=headers)

    # Save the raw HTML for debugging
    with open("urbanseeds_debug.html", "w", encoding="utf-8") as f:
        f.write(response.text)

    # Parse the HTML
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find all product containers
    product_containers = soup.find_all('li', class_='product')
    if not product_containers:
        raise ValueError("No product containers found on the page")

    products = []
    for container in product_containers:
        try:
            # Extract the product name
            name_tag = container.find('h3', class_='card-title')
            name = name_tag.a.string.strip() if name_tag and name_tag.a else "Unknown"
    
            # Extract the product price using .string
            price_tag = container.find('span', class_='price price--withoutTax')
            price = price_tag.string.strip() if price_tag else "N/A"
            
            products.append({'name': name, 'price': price})
        except Exception as e:
            # Log errors and continue processing the next product
            with open("error_log.txt", "a", encoding="utf-8") as log:
                log.write(f"Error processing container: {e}\n")
                log.write(f"Container content: {container}\n")

    # Convert to a DataFrame
    product_df = pd.DataFrame(products)
    return {'name': product_df['name'].tolist(), 'price': product_df['price'].tolist()}

#d = fetch_urbanseeds_products()

