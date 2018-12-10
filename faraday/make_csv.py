#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Parse SSL file to CSV
"""

# Replace with `argparse` as required.
from smartparse import ArgumentParser, FileType

def get_args():
    parser = ArgumentParser(description=__doc__)
    parser.add_argument("-f", type=FileType("r"), default="-",
                        help="input file", metavar="FILE")
    return parser.parse_args()

def parse_file(f):
    for line in f:
        if line.startswith("AD="):
            print(",".join(line.strip()[3:].split(",")[:-1]))

if __name__ == "__main__":
    args = get_args()
    parse_file(args.f)
