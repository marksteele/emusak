#!/bin/bash
/usr/bin/flac -c -d --totally-silent "$1" | /usr/bin/opusenc - - 2>/dev/null
