.PHONY: clean
clean :
	-rm README.translator.txt tests/rtest_mixima.txt ChangeLog.txt function_list.txt function_list
	-rm compatibility/function_list.lisp compatibility/mixdoc.lisp compatibility/shadow.mac compatibility/shadow_math_functions.mac
	-rm -rf ./build/
