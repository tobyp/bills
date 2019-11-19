#!/bin/bash

shopt -s nullglob

if [ -t 1 ]; then
	RED="$(echo -ne '\033[7m\033[0;31m')"
	GREEN="$(echo -ne '\033[7m\033[0;32m')"
	RESET="$(echo -ne '\033[0m')"
else
	RED=''
	GREEN=''
	RESET=''
fi

result=0
for test_input in "$1/"*".bills"; do
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
	if [ -e "${test_output}" ]; then
		${test_cmd} 2>/dev/null | diff -d - "${test_output}";
		test_result=${PIPESTATUS[0]};
		diff_result=$?;
	else
		${test_cmd} 2>/dev/null
		test_result=${PIPESTATUS[0]};
		diff_result=0;
	fi

	if [ $test_result -ne $test_expected ]; then
		echo " -> ${RED}FAILED!${RESET} (expected exit code ${test_expected}, got ${test_result})";
		result=1;
	elif [ $diff_result -ne 0 ]; then
		echo " -> ${RED}FAILED!${RESET} (output did not match what was expected)";
		result=1;
	else
		echo " -> ${GREEN}PASSED!${RESET}"
	fi
done

echo ""
if [ $result != 0 ]; then
	echo "${RED}Some tests failed!${RESET}"
else
	echo "${GREEN}All tests passed :)${RESET}"
fi

exit $result;
