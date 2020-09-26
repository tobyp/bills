#!/bin/bash

shopt -s nullglob;

if [ -t 1 ]; then
	RED="$(tput setaf 1)";
	GREEN="$(tput setaf 2)";
	YELLOW="$(tput setaf 3)";
	RESET="$(tput sgr0)";
else
	RED='';
	GREEN='';
	YELLOW='';
	RESET='';
fi

test_inputs=();
if [ $# -eq 0 ]; then
	test_inputs+=("tests/"*".bills");
fi
while [ $# -gt 0 ]; do
	if [ -d "$1" ]; then
		test_inputs+=("$1/"*".bills");
	elif [ -f "$1" ]; then
		test_inputs+=("$1");
	else
		echo "Unexpected test input $1, ignoring";
	fi
	shift;
done

tests_count=0
tests_failed=0
for test_input in "${test_inputs[@]}"; do
	test_input_base="$(basename ${test_input})";
	test_name="${test_input_base%.*}";
	test_exp_stdout="${test_input%.*}.stdout";
	test_exp_stderr="${test_input%.*}.stderr";
	test_cmd="./bills ${test_input}"

	if [ -x "${test_input}" ]; then
		test_cmd="./${test_input}";
	fi

	if [[ "${test_name}" == *"-FAIL" ]]; then
		test_expected=1;
	else
		test_expected=0;
	fi

	test_run_stdout="$(mktemp ${test_name}-XXXXXX.stdout)";
	test_run_stderr="$(mktemp ${test_name}-XXXXXX.stderr)";
	echo "Running test \"${test_name}\"...";
	${test_cmd} >"${test_run_stdout}" 2>"${test_run_stderr}";
	test_result=$?;
	diff_result=0;

	if [ -e "${test_exp_stdout}" ]; then
		sort "${test_run_stdout}" | diff -d - "${test_exp_stdout}";
		diff_result=$(( $diff_result + ${PIPESTATUS[1]} ));
	fi

	if [ -e "${test_exp_stderr}" ]; then
		diff -d "${test_run_stderr}" "${test_exp_stderr}";
		diff_result=$(( $diff_result + $? ));
	fi

	tests_count=$(( $tests_count + 1));
	if [ $test_result -ne $test_expected ]; then
		echo " -> ${RED}FAILED!${RESET} (expected exit code ${test_expected}, got ${test_result})";
		echo "   -- stdout: ${test_run_stdout}";
		echo "   -- stderr: ${test_run_stderr}";
		tests_failed=$(( $tests_failed + 1 ));
	elif [ $diff_result -ne 0 ]; then
		echo " -> ${RED}FAILED!${RESET} (output did not match what was expected)";
		echo "   -- stdout: ${test_run_stdout}";
		echo "   -- stderr: ${test_run_stderr}";
		tests_failed=$(( $tests_failed + 1 ));
	else
		echo " -> ${GREEN}PASSED!${RESET}";
		rm "${test_run_stdout}";
		rm "${test_run_stderr}";
	fi
done

echo ""
if [ $tests_failed != 0 ]; then
	echo "${RED}${tests_failed} tests failed! (of ${tests_count})${RESET}";
else
	echo "${GREEN}All ${tests_count} tests passed :)${RESET}";
fi

exit $result;
