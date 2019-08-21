#!/bin/bash

result=0
for test_input in $1/*.bills; do
	test_input_base="$(basename ${test_input})";
	test_name="${test_input_base%.*}";
	test_output="$1/${test_name}.output";
	test_cmd="./bills ${test_input}"

	if [ -x "${test_input}" ]; then
		test_cmd="./${test_input}"
	fi

	if [[ "${test_name}" == *"-FAIL" ]]; then
		test_expected=1
	else
		test_expected=0
	fi

	echo "Running test \"${test_name}\"...";
	${test_cmd} 2>/dev/null | sort | diff -d - "${test_output}";
	test_result=${PIPESTATUS[0]};
	diff_result=$?;
	if [ $test_result -ne $test_expected ]; then
		echo " -> FAILED! (expected exit code ${test_expected}, got ${test_result})";
		result=1;
	elif [ $diff_result -ne 0 ]; then
		echo " -> FAILED! (output did not match what was expected)";
		result=1;
	else
		echo " -> SUCCEEDED!"
	fi
done

exit $result;
