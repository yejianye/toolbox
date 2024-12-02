#!/usr/bin/env python

import argparse
import re
import json
import requests

API_URL = "http://localhost:3000/lark/auto-translate"

def translate(text):
    resp = requests.post(API_URL, json={'text': text})
    text = resp.json()['text']
    return {'items': [{'title': text, 'arg': text}]}

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("text", help="Text to be translated")
    args = parser.parse_args()
    print(json.dumps(translate(args.text)))
