#!/usr/bin/env python

"""
Alfred client for adding reminders

Supported syntax:
[Y] do something crazy
[Y] today release the hamsters into the wild
[Y] tomorrow bring about financial ruin upon my enemies
[Y] 24/12/13 to forget everything I know about things in movies
[Y] thursday have a banana medium priority
[Y] next thursday ask some difficult questions
[N] 31-12-99 23:22 panic about the millennium bug
[N] at 2pm wait for nothing in particular
[N] !!! in 2 weeks an important meeting
"""

import os
import sys
import subprocess
import re

__version__ = '0.1.0'
weekdays = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday']
week_regex = '|'.join(weekdays)
next_week_regex = '|'.join(['next '+x for x in weekdays])
weekdays_regex = 'monday|tuesday|wednesday|thursday|friday|saturday|sunday'
date_regex = ['today', 'tomorrow', '[0-9]+/[0-9]+', '[0-9]+/[0-9]+/[0-9]+', week_regex, next_week_regex]
reminder_prog = '/usr/local/bin/reminders'
default_list = "inbox"

def extract_date(content):
    for regex in date_regex:
        regex = '^({}) *(.+)'.format(regex)
        match = re.match(regex, content)
        if match:
            return match.groups()
    return None, content

def extract_priority(content):
    priority_map = {'p0': 'high', 'p1': 'medium', 'p2': 'low'}
    match = re.match("^(p[012]) *(.*)", content)
    if match:
        priority, content = match.groups()
        priority = priority_map.get(priority)
        return priority, content
    return None, content

def extract_list(content):
    match = re.match("^(.+) @(.+)$", content)
    if match:
        content, reminder_list = match.groups()
        return reminder_list, content
    else:
        return default_list, content

def remind(content):
    due_date, content = extract_date(content)
    priority, content = extract_priority(content)
    reminder_list, content = extract_list(content)
    cmds = [reminder_prog, "add", reminder_list, content]
    if due_date:
        cmds += ["--due-date", due_date]
    if priority:
        cmds += ["--priority", priority]
    result = subprocess.run(cmds, stdout=subprocess.PIPE)
    return result.stdout.decode('utf-8').strip()

if __name__ == '__main__':
    print(remind(' '.join(sys.argv[1:])))
