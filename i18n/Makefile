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
	cd ../tests/unit && lua bin/unit.lua 

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

check-lua:
	haxe-i18n -main Check -lua bin/check.lua -debug -dce no
	lua bin/check.lua

check-python:
	haxe-i18n -main Check -python bin/check.py -debug  -dce no
	python3 bin/check.py

check-php:
	haxe-i18n -main Check -php bin/php -debug -dce no
	php bin/php/index.php

check-js:
	haxe-i18n -main Check -js bin/check.js -debug -D nodejs -dce no
	node bin/check.js