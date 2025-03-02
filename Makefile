compile:
	as -arch arm64 -o output.o output.s 
	ld -o output output.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _start -arch arm64 
