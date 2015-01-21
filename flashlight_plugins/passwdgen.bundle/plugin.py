#!/usr/bin/env python
import string
import random
import os 

def password_generator(length):
    return ''.join(random.choice(string.letters + string.digits) for x in xrange(length))

def results(parsed, original_query):
    password = password_generator(16)
    return {
        "title": "Copy password '{}' to clipboard".format(password),
        "run_args": [password],
        "html": "<h1>{}</h1>".format(password)
    }

def run(password):
    os.system("echo '{}' | tr -d '\n' | pbcopy".format(password))
