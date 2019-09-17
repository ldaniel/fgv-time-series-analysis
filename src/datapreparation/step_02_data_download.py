# -*- coding: utf-8 -*-
"""
Created on Fri Jun 28 19:23:37 2019

@author: @RodriGonca
"""
import os
import re
import requests
from bs4 import BeautifulSoup

def download_data():
    URL = f'http://sisweb.tesouro.gov.br/apex/f?p=2031:2'
    result = requests.get(URL).content
    soup = BeautifulSoup(result, "html.parser")
    a_tags = soup.find_all("a")

    links = []
    anchor_url = URL.split('f?')[0]
    for a_tag in a_tags:
        link = re.findall(r'href="(.*)" style="padding-right:5px;"', str(a_tag))
        if link:
            links.append(anchor_url + link[0])

    for link in links:
        resp = requests.get(link)
        filename = re.findall(r'filename="(.*)"; filename',
                              str(resp.headers["Content-Disposition"]))
        print(filename[0])
        with open(f'data\\raw\\{filename[0]}', 'wb') as output:
            output.write(resp.content)

    del a_tags, anchor_url, link, URL, filename

    return links

if __name__ == '__main__':
    #os.chdir("..")
    links = download_data()
