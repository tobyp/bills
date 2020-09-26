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

test_inputs=("$1/"*".bills");
#test_inputs=("tests/regression-syntax-spaceless-define-FAIL.bills")

result=0
for test_input in "${test_inputs[@]}"; do
	test_input_base="$(basename ${test_input})";
	test_name="${test_input_base%.*}";
	test_exp_stdout="$1/${test_name}.stdout";
	test_exp_stderr="$1/${test_name}.stderr";
	test_cmd="./bills ${test_input}"

	if [ -x "${test_input}" ]; then
		test_cmd="./${test_input}"
	fi

	if [[ "${test_name}" == *"-FAIL" ]]; then
		test_expected=1
	else
		test_expected=0
	fi

	test_run_stdout=$(mktemp ${test_name}-XXXXXX.stdout)
	test_run_stderr=$(mktemp ${test_name}-XXXXXX.stderr)
	echo "Running test \"${test_name}\"...";
	${test_cmd} >"${test_run_stdout}" 2>"${test_run_stderr}";
	test_result=$?;
	diff_result=0

	if [ -e "${test_exp_stdout}" ]; then
		sort "${test_run_stdout}" | diff -d - "${test_exp_stdout}";
		diff_result=$(( $diff_result + ${PIPESTATUS[1]} ));
	fi

	if [ -e "${test_exp_stderr}" ]; then
		diff -d "${test_run_stderr}" "${test_exp_stderr}";
		diff_result=$(( $diff_result + $? ));
	fi

	if [ $test_result -ne $test_expected ]; then
		echo " -> ${RED}FAILED!${RESET} (expected exit code ${test_expected}, got ${test_result})";
		echo "   -- stdout: ${test_run_stdout}"
		echo "   -- stderr: ${test_run_stderr}"
		result=1;
	elif [ $diff_result -ne 0 ]; then
		echo " -> ${RED}FAILED!${RESET} (output did not match what was expected)";
		echo "   -- stdout: ${test_run_stdout}"
		echo "   -- stderr: ${test_run_stderr}"
		result=1;
	else
		echo " -> ${GREEN}PASSED!${RESET}"
		rm "${test_run_stdout}"
		rm "${test_run_stderr}"
	fi
done

echo ""
if [ $result != 0 ]; then
	echo "${RED}Some tests failed!${RESET}"
else
	echo "${GREEN}All tests passed :)${RESET}"
fi

exit $result;
