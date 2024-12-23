#!/usr/bin/env python
"""
An alfred plugin that can perform bi-directional translation, proof read etc, using OpenAI
"""
import argparse
import sys
import json

import markdown
from openai import OpenAI

client = OpenAI()
DEFAULT_MODEL = 'gpt-4o-mini'

def md_to_html(md):
    html_content = markdown.markdown(md)
    html_with_style = f"""
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>Proof Read</title>
            <style>
                body {{
                    font-family: sans-serif;
                    margin: 15px;
                }}
                li {{
                    margin-bottom: 10px;
                }}
            </style>
        </head>
        <body>
            {html_content}
        </body>
        </html>
        """
    return html_with_style

def translate(text, lang1='Chinese', lang2='English', model=DEFAULT_MODEL):
    prompt = f"""
    If INPUT_TEXT is written in {lang1}, translate INPUT_TEXT into {lang2}.
    If INPUT_TEXT is written in {lang2}, translate INPUT_TEXT into {lang1}.
    Do not output anything other than the translation.

    INPUT_TEXT:
    {text}
    """

    resp = client.chat.completions.create(
        model=model,
        messages=[{"role": "user", "content": prompt}],
    )
    return resp.choices[0].message.content.strip()

def proofread(text, model=DEFAULT_MODEL):
    prompt = f"""
    TEXT:
    {text}

    TASK1:
    Proof read the input text.

    OUTPUT FORMAT:
    ## Changes & Suggestions
    [suggested edits or changes]

    ## Proofread Version
    [proofread version of the text]

    TASK2:
    Rewrite the input text in more authentic English

    OUTPUT FORMAT:
    ## Rewritten Version
    [Rewritten version of the text]

    """

    resp = client.chat.completions.create(
        model=model,
        messages=[{"role": "user", "content": prompt}],
    )
    content = resp.choices[0].message.content.strip()
    rewritten_version = content.split("## Rewritten Version")[1].strip()
    html = md_to_html(content)
    return {"rewritten": rewritten_version, "html": html}

def text_to_speech(text, output_file="/tmp/alfred-openai-tts.mp3"):
    with client.audio.speech.with_streaming_response.create(
        model="tts-1",
        voice="echo",
        input=text
    ) as response:
        response.stream_to_file(output_file)
    return output_file

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("text", help="Text to be translated")
    parser.add_argument("--action", default='translate', help="Action to be performed for the text. Value: translate-filter, translate, proofread")
    parser.add_argument("--model", default='gpt-4o', help="LLM to use")
    parser.add_argument("--lang1", default='Chinese', help="Primary Language")
    parser.add_argument("--lang2", default='English', help="Secondary Language")
    args = parser.parse_args()
    text = sys.stdin.read() if args.text == '-' else args.text
    if args.action == 'translate-filter':
        translation = translate(text, lang1=args.lang1, lang2=args.lang2, model=args.model)
        value = json.dumps({'text': text, 'translation': translation})
        print(json.dumps({'items': [{'title': translation, 'arg': value}]}))
    elif args.action == 'translate':
        print(translate(text, lang1=args.lang1, lang2=args.lang2, model=args.model))
    elif args.action == 'proofread':
        print(json.dumps(proofread(text, model=args.model)))
    elif args.action == 'tts':
        print(text_to_speech(text))
