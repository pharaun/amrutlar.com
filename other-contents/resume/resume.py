#! /usr/bin/env python

import os
import re
import sys
import yaml
import time
import datetime
from optparse import OptionParser
import jinja2

################################################################################
def load_yaml(filename):
    document = []

    try:
        for docs in yaml.load_all(file(filename, 'r')):
            document.append(docs)
    except yaml.YAMLError, exc:
        print "Error - ", exc

    return document


################################################################################
def get_yaml_section(sections, which):
    for s in sections:
        try:
            section = s[which]
            return {which : section}
        except KeyError:
            1+1

    return None


################################################################################
def update_date_formatting(vevent, date_format):
    year_regex = re.compile(r"^\d{4}$")
    year_month_regex = re.compile(r"^\d{4}-\d{2}$")

    for k, v in vevent.items():
        if v != None:
            if isinstance(v, int):
                v = str(v)

            if isinstance(v, str):
                if year_regex.match(v):
                    vevent[k] = datetime.date(*time.strptime(v, '%Y')[:3])
                elif year_month_regex.match(v):
                    vevent[k] = datetime.date(*time.strptime(v, '%Y-%m')[:3])

            # Finally format the actual date
            vevent[k] = vevent[k].strftime(date_format)
        else:
            if k == 'dtend':
                vevent['dtend'] = "Present"

            if k == 'dtstart':
                vevent['dtstart'] = "Unknown"

    return vevent


################################################################################
def date_formatting(section, date_format):
    if isinstance(section, dict):
        for k, v in section.items():
            if k == 'vevent':
                section['vevent'] = update_date_formatting(v, date_format)

            else:
                date_formatting(v, date_format)
    elif isinstance(section, list):
        for item in section:
            date_formatting(item, date_format)

    return section


################################################################################
def yaml_filtering(section, filtering):
    return section


################################################################################
def render_section(section, env, filename):
    tmpl = env.get_template(filename)

    if section != None:
        return tmpl.render(section)
    else:
        return tmpl.render()


################################################################################
if __name__ == '__main__':
    # Option Parser
    usage = "usage: %prog [options] <config yaml>"
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", dest="verbose", help="Output more details")

    options, args = parser.parse_args()
    if len(args) != 1:
        parser.error("Need a config yaml file to process")

    config = load_yaml(args[0])[0]

    # Process the config and break it up
    resume = config['resume']['input']
    output = config['output_file']
    template_path = config['template']['path']

    # Load up the keywords
    keywords = dict()
    tmp_date = config['template']['global_date_format']
    for k in config['template']['keyword']:
        name = k['name']
        filename = k['file']
        date = k.get('date_format', tmp_date)

        keywords[name] = (filename, date)

    # Load up the ordering
    ordering = config['template']['ordering']

    # Load up the Markers
    try:
        block = config['template']['block']
    except KeyError:
        block = None

    try:
        variable = config['template']['variable']
    except KeyError:
        variable = None

    try:
        comment = config['template']['comment']
    except KeyError:
        comment = None

    # Load up the ypaths for filtering
    filtering = None

    # Loading up the resume yaml file
    doc = load_yaml(resume)

    # Setup the templating environment
    if block != None:
        if variable != None:
            if comment != None:
                env = jinja2.Environment(loader=jinja2.FileSystemLoader(template_path),
                        block_start_string=block[0], block_end_string=block[1],
                        variable_start_string=variable[0], variable_end_string=variable[1],
                        comment_start_string=comment[0], comment_end_string=comment[1])
            else:
                env = jinja2.Environment(loader=jinja2.FileSystemLoader(template_path),
                    block_start_string=block[0], block_end_string=block[1],
                    variable_start_string=variable[0], variable_end_string=variable[1])
        else:
            env = jinja2.Environment(loader=jinja2.FileSystemLoader(template_path),
                block_start_string=block[0], block_end_string=block[1])
    else:
        env = jinja2.Environment(loader=jinja2.FileSystemLoader(template_path))

    # Output the template/rendering
    with open(output, 'w') as f:
        for t in ordering:
            filename, date_format = keywords[t]
            data = date_formatting(get_yaml_section(doc, t), date_format)
            data = yaml_filtering(data, filtering)

            out = render_section(data, env, filename)
            f.write(out)

            sys.stdout.write(out)

    print
