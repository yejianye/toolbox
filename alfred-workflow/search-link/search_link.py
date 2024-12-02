#!/usr/bin/env python

"""
Alfred client for Ryan's personal search
"""

import argparse
import re
import json
import requests

__version__ = '0.7.0'

TERM_MINLEN = 2
API_URL = "http://localhost:3000/search-link"
LIMIT = 50

def icon(icon_name):
    return {'type': 'png',
            'path': icon_name+'.png'}

def parse_query(query_string):
    query = {'limit': LIMIT}
    match = re.match("^(.*) @([^ ]+)$", query_string)
    if match:
        term, source = match.groups()
        query['term'] = term
        query['source'] = source
    else:
        query['term'] = query_string
    return query

def search(query, query_group=None):
    query = parse_query(query)
    if query_group:
        query['group'] = query_group
    if len(query['term']) < TERM_MINLEN:
        return {'items': [
            {'title': 'Search Links in bookmark, history and org files',
             'icon': icon('search')}]}
    else:
        resp = requests.get(API_URL, params=query)
        data = resp.json()['data']
        query_id = resp.json()['id']
        items = []
        for item in data:
            url, title, source, rank = item['link'], item['title'], item['source'], item['rank']
            items.append({'title': title,
                           'subtitle': url,
                           'icon': icon(source),
                           'arg': json.dumps({"event": "click",
                                              "url": url,
                                              "rank": rank,
                                              "query_id": query_id})})
        return {'items': items}

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("query", help="search query")
    parser.add_argument("--group", help="query group")
    args = parser.parse_args()
    print(json.dumps(search(args.query, args.group)))
