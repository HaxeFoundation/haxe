test-1: test-neko test-js test-python test-php test-hl
test-all: test-neko test-js test-python test-php test-java test-cs test-cpp test-hl
test-static: test-java test-cs test-cpp

test-neko:
	cd ../tests/unit && haxe-i18n compile-neko.hxml
	cd ../tests/unit && neko bin/unit.n

test-js:
	cd ../tests/unit && haxe-i18n compile-js.hxml 
	cd ../tests/unit && node bin/unit.js 

test-python:
	cd ../tests/unit && haxe-i18n compile-python.hxml
	cd ../tests/unit && python3 bin/unit.py 

test-java:
	cd ../tests/unit && haxe-i18n compile-java.hxml
	cd ../tests/unit && java -jar bin/java/TestMain-Debug.jar

test-cs:
	cd ../tests/unit && haxe-i18n compile-cs.hxml
	cd ../tests/unit && bin/cs/bin/TestMain-Debug.exe 

test-cpp:
	cd ../tests/unit && haxe-i18n compile-cpp.hxml
	cd ../tests/unit && bin/cpp/TestMain-debug 

test-lua:
	cd ../tests/unit && haxe-i18n compile-lua.hxml
	cd ../tests/unit && luajit bin/unit.lua 

test-swf:
	cd ../tests/unit && haxe-i18n compile-flash9.hxml
	cd ../tests/unit && flashplayerdebugger bin/unit9.swf 

test-php:
	cd ../tests/unit && haxe-i18n compile-php.hxml
	cd ../tests/unit && php bin/php/index.php 

test-hl:
	cd ../tests/unit && haxe-i18n compile-hl.hxml
	cd ../tests/unit && hl bin/unit.hl 

check-cpp:
	haxe-i18n -main Check -cpp bin/cpp -debug -dce no
	bin/cpp/Check-debug

check-neko:
	haxe-i18n -main Check -neko bin/check.n -debug -dce no
	neko bin/check.n

check-java:
	haxe-i18n -main Check -java bin/java -debug -dce no
	java -jar bin/java/Check-Debug.jar

check-cs:
	haxe-i18n -main Check -cs bin/cs -debug -dce no
	bin/cs/bin/Check-Debug.exe

check-lua:
	haxe-i18n -main Check -lua bin/check.lua
	luajit bin/check.lua

check-python:
	haxe-i18n -main Check -python bin/check.py -debug  -dce no -D analyzer-code-motion
	python3 bin/check.py

check-php:
	haxe-i18n -main Check -php bin/php -debug -dce no
	php bin/php/index.php

check-js:
	haxe-i18n -main Check -js bin/check.js -debug -D nodejs -dce no
	node bin/check.js

check-swf:
	haxe-i18n -main Check -swf bin/check.swf -debug -dce no 
	flashplayerdebugger bin/check.swf

check-hl:
	haxe-i18n -main Check -hl bin/check.hl -debug -dce no
	hl bin/check.hl

# benchmarks

benchmark-cpp:
	haxe-i18n -main Benchmark -cpp bin/cpp -debug -dce no
	bin/cpp/Benchmark-debug

benchmark-neko:
	haxe-i18n -main Benchmark -neko bin/benchmark.n -debug -dce no
	neko bin/benchmark.n

benchmark-java:
	haxe-i18n -main Benchmark -java bin/java -debug -dce no
	java -jar bin/java/Benchmark-Debug.jar

benchmark-cs:
	haxe-i18n -main Benchmark -cs bin/cs -debug -dce no
	bin/cs/bin/Benchmark-Debug.exe

benchmark-lua:
	haxe-i18n -main Benchmark -lua bin/benchmark.lua
	luajit bin/benchmark.lua

benchmark-python:
	haxe-i18n -main Benchmark -python bin/benchmark.py -debug  -dce no -D analyzer-code-motion
	python3 bin/benchmark.py

benchmark-php:
	haxe-i18n -main Benchmark -php bin/php -debug -dce no
	php bin/php/index.php

benchmark-php7:
	haxe-i18n -main Benchmark -D php7 -php bin/php -debug -dce no
	php bin/php/index.php

benchmark-js:
	haxe-i18n -main Benchmark -js bin/benchmark.js -debug -D nodejs -dce no
	node bin/benchmark.js

benchmark-swf:
	haxe-i18n -main Benchmark -swf bin/benchmark.swf -debug -dce no 
	flashplayerdebugger bin/benchmark.swf

benchmark-hl:
	haxe-i18n -main Benchmark -hl bin/benchmark.hl -debug -dce no
	hl bin/benchmark.hl