#!/usr/bin/env python
import os
import time
import datetime
import logging

import requests

logging.basicConfig(filename='/tmp/qrcode.log', format='%(asctime)s %(message)s', level=logging.INFO)

MIN_TEXT_LEN = 10 

html_tmpl = '''
<html>
<head>
<style>
html, body {
    background-color: #001A22;
    color: #F2F3EE;
    margin: 0;
    font-family: "HelveticaNeue-Light", "Helvetica Neue Light", "Helvetica Neue", Helvetica, Arial;
    font-size: x-large;
}
div {
    position: fixed;
    left: 50%%;
    top: 50%%;
    margin-left: -150px;
    margin-top: -150px;
    width: 300px;
    height: 300px;
    text-align:center;
    vertical-align:middle;
}
</style>
</head>
<body>
    <div>
    %(content)s
    </div>
</body>
</html>
'''

def qrcode_url(text):
    shorten = 1 if text.startswith('http') else 0
    resp = requests.post('http://cli.im/api/browser/generate', {'data':text, 'zm':1, 'dwz':shorten})
    if resp.status_code == 200:
        result = resp.json()
        return result['data']['qr_filepath']
    else:
        logging.error('Error Response from cli.im: code=%d content=%s', resp.status_code, resp.content)
        return ''

def results(parsed, original_query):
    try:
        text = parsed['~text']
        if len(text) > MIN_TEXT_LEN: 
            image_url = qrcode_url(text)
            html = html_tmpl % {'content': "<img src='{}'></img>".format(image_url)}
        else:
            image_url = ''
            html = html_tmpl % {'content': "Text too short (less than 10 characters)"}
        return {
            "title": "QR-code for '{}'".format(text),
            "html": html,
            "run_args": [image_url]
        }
    except Exception as e:
        logging.exception()

def run(image_url):
    if not image_url:
        return
    timestr = time.strftime('%Y-%m-%d_%H.%M.%S', datetime.datetime.now().timetuple())
    output_filename = '{}/Downloads/qrcode_{}.png'.format(os.environ['HOME'], timestr)
    logging.info('Output filepath: %s', output_filename)
    os.system('curl -o "{}" {}'.format(output_filename, image_url))
    os.system('open {}'.format(output_filename))
