#!/bin/bash

TESTS_OK=1

for dir in test_[1-9]*; do
    echo "==== $dir ===="
    diff -s -y --suppress-common-lines <(alr run -s --args="$dir/data.json $dir/template.jintp") "$dir/expected.txt"
    success=$?
    if [ "$success" -eq 0 ]; then
        echo -e "--> OK\n"
    else
        echo -e "--> NOK\n"
        TESTS_OK=0
    fi
done
if [ "$TESTS_OK" -eq "1" ]; then
    echo "All tests passed: OK"
    exit 0
else
    echo "Some tests failed: NOK"
    exit 1
fi
