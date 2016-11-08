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