# -*- coding: utf-8 -*-
"""
Created on Tue Nov 19 09:02:42 2024

@author: o_kho
"""
# import requests
# from bs4 import BeautifulSoup
# import json
# import pandas as pd


def fetch_walmart_products(search_query, zip_code):
    
    import requests
    from bs4 import BeautifulSoup
    import json
    import pandas as pd
    
    # Construct the search URL
    #url = f"https://www.walmart.com/search/?query={search_query}"
    url = f"https://www.walmart.com/search/?query={search_query}&zipcode={zip_code}"
    # Set headers to mimic a browser request
    # headers = {
    #     'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    # }
    
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
        'Accept-Language': 'en-US,en;q=0.9',
        'Accept-Encoding': 'gzip, deflate, br',
        'Connection': 'keep-alive'
    }

    # Send a GET request to the URL
    response = requests.get(url, headers=headers)
    
    # Save raw HTML for debugging
    with open("walmart_debug.html", "w", encoding="utf-8") as f:
        f.write(response.text)
    
    # Parse the HTML content
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find the script tag containing the product data
    script_tag = soup.find('script', id='__NEXT_DATA__')
    
    if script_tag is None:
        raise ValueError("Required script tag '__NEXT_DATA__' not found in the HTML")
    
    
    json_data = json.loads(script_tag.string)
    
    items = json_data['props']['pageProps']['initialData']['searchResult']['itemStacks'][0]['items']

    # Extract product details
    products = []
    for item in items:
        name = item.get('name', {})
        price_info = item.get('price', {})
        products.append({
            'name': name,
            'price': price_info
        })
        
    
    product_df = pd.DataFrame(products)
    
    return product_df['name'], product_df['price']

#d = fetch_walmart_products("chicken", 47714)
# dd = fetch_walmart_products("meat")
# ddd = fetch_walmart_products("vegtabels")



