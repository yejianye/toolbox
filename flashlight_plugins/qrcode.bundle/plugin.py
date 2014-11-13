#!/usr/bin/env python
import os
import time
import datetime
import logging

import requests

logging.basicConfig(filename='/tmp/qrcode.log', format='%(asctime)s %(message)s', level=logging.INFO)

def results(parsed, original_query):
    return {
        "title": "Turn text into QR-code (press enter)",
        "run_args": [parsed['~text']],
    }

def run(text):
    logging.info('Generate QR-code for: %s', text)
    timestr = time.strftime('%Y-%m-%d_%H.%M.%S', datetime.datetime.now().timetuple())
    output_filename = '{}/Downloads/qrcode_{}.png'.format(os.environ['HOME'], timestr)
    logging.info('Output filepath: %s', output_filename)
    shorten = 1 if text.startswith('http') else 0
    resp = requests.post('http://cli.im/api/browser/generate', {'data':text, 'zm':1, 'dwz':shorten})
    if resp.status_code == 200:
        result = resp.json()
        logging.info('JSON Response from cli.im: %s', result)
        os.system('curl -o "{}" {}'.format(output_filename, result['data']['qr_filepath']))
        os.system('qlmanage -p {} &'.format(output_filename))
    else:
        logging.error('Error Response from cli.im: code=%d content=%s', resp.status_code, resp.content)
