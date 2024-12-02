#!/usr/bin/env python

"""
Alfred client for creating lark doc
"""

import argparse
from rypy import lark

__version__ = '0.1.0'

def create_doc(client):
    return client.create_docx()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--profile", default="bytedance", help="Specify which Feishu open app to provide API access")
    args = parser.parse_args()
    client = lark.FeishuClient(profile=args.profile)
    print(create_doc(client))
