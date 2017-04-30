#!/usr/bin/env python
import parse
import sys

git_head = parse.parse('ref: {}', sys.argv[1])[0].strip()
tag_description = sys.argv[2].strip()

head_parts = git_head.split('/')
if len(head_parts) > 2:
    if head_parts[-2] in ['release', 'hotfix', 'support']:
        print head_parts[-1]
    else:
        print tag_description
else:
    print tag_description
