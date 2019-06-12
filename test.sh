#!/bin/bash

result=0
for test_input in $1/*.bills; do
	test_input_base="$(basename ${test_input})";
	test_name="${test_input_base%.*}";
	test_output="$1/${test_name}.output";

	echo "Running test \"${test_name}\"...";
	if ./bills < "${test_input}" | sort | diff - "${test_output}"; then
		echo " -> SUCCEEDED!";
	else
		echo " -> FAILED!";
		result=1;
	fi
done

exit $result;
