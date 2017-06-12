HAXEPATH=$(PWD)/..
HAXE_LIBRARY_PATH=$(HAXEPATH)/std
HAXE_STD_PATH=$(HAXEPATH)/std
PATH1=$(HAXEPATH):$(PATH)
ENV=HAXEPATH=$(HAXEPATH) HAXE_LIBRARY_PATH=$(HAXE_LIBRARY_PATH) HAXE_STD_PATH=$(HAXE_STD_PATH) PATH=$(PATH1)
HAXE_EXEC=$(ENV) $(HAXEPATH)/haxe
PYPY3=/home/frabbit/Downloads/pypy3-v5.7.1-linux64/bin/pypy3
HASHLINK_PATH=/home/frabbit/github/hashlink
HL_ENV=LD_LIBRARY_PATH=$(HASHLINK_PATH)
HL=$(HL_ENV) $(HASHLINK_PATH)/hl

test-1: test-neko test-js test-python test-php test-hl
test-all: test-neko test-js test-python test-php test-java test-cs test-cpp test-lua test-hl
test-static: test-java test-cs test-cpp



test-neko:
	cd ../tests/unit && $(HAXE_EXEC) compile-neko.hxml
	cd ../tests/unit && neko bin/unit.n

test-js:
	cd ../tests/unit && $(HAXE_EXEC) compile-js.hxml
	cd ../tests/unit && node bin/unit.js

test-python:
	cd ../tests/unit && $(HAXE_EXEC) compile-python.hxml
	cd ../tests/unit && python3 bin/unit.py

test-pypy:
	cd ../tests/unit && $(HAXE_EXEC) compile-python.hxml
	cd ../tests/unit && $(PYPY3) bin/unit.py

test-java:
	cd ../tests/unit && $(HAXE_EXEC) compile-java.hxml
	cd ../tests/unit && java -jar bin/java/TestMain-Debug.jar

test-cs:
	cd ../tests/unit && $(HAXE_EXEC) compile-cs.hxml
	cd ../tests/unit && bin/cs/bin/TestMain-Debug.exe

test-cpp:
	cd ../tests/unit && $(HAXE_EXEC) compile-cpp.hxml
	cd ../tests/unit && bin/cpp/TestMain-debug

test-lua:
	cd ../tests/unit && $(HAXE_EXEC) compile-lua.hxml
	cd ../tests/unit && luajit bin/unit.lua

test-swf:
	cd ../tests/unit && $(HAXE_EXEC) compile-flash9.hxml
	cd ../tests/unit && flashplayerdebugger bin/unit9.swf

test-php:
	cd ../tests/unit && $(HAXE_EXEC) compile-php.hxml
	cd ../tests/unit && php bin/php/index.php

test-php7:
	cd ../tests/unit && $(HAXE_EXEC) compile-php7.hxml
	cd ../tests/unit && php bin/php/index.php

test-hl:
	cd ../tests/unit && $(HAXE_EXEC) compile-hl.hxml
	cd ../tests/unit && $(HL) bin/unit.hl

test-interp:
	cd ../tests/unit && $(HAXE_EXEC) -cp ../../standalone compile-macro.hxml

check-cpp:
	$(HAXE_EXEC) -main Check -cpp bin/cpp -debug -dce no
	bin/cpp/Check-debug

check-neko:
	$(HAXE_EXEC) -main Check -neko bin/check.n -debug -dce no
	neko bin/check.n

check-java:
	$(HAXE_EXEC) -main Check -java bin/java -debug -dce no
	java -jar bin/java/Check-Debug.jar

check-cs:
	$(HAXE_EXEC) -main Check -cs bin/cs -debug -dce no
	bin/cs/bin/Check-Debug.exe

check-lua:
	$(HAXE_EXEC) -main Check -lua bin/check.lua
	luajit bin/check.lua

check-python:
	$(HAXE_EXEC) -main Check -python bin/check.py -debug  -dce no -D analyzer-code-motion
	python3 bin/check.py

check-php:
	$(HAXE_EXEC) -main Check -php bin/php -debug -dce no
	php bin/php/index.php

check-js:
	$(HAXE_EXEC) -main Check -js bin/check.js -debug -D nodejs -dce no
	node bin/check.js

check-swf:
	$(HAXE_EXEC) -main Check -swf bin/check.swf -debug -dce no
	flashplayerdebugger bin/check.swf

check-hl:
	$(HAXE_EXEC) -main Check -hl bin/check.hl -debug -dce no
	hl bin/check.hl

# benchmarks

benchmark-cpp:
	$(HAXE_EXEC) -main Benchmark -cpp bin/cpp1 -dce no
	bin/cpp1/Benchmark

benchmark-cpp-2:
	$(HAXE_EXEC) -D HXCPP-GC-GENERATIONAL -main Benchmark -cpp bin/cpp2 -dce no
	bin/cpp2/Benchmark

benchmark-neko:
	$(HAXE_EXEC) -main Benchmark -neko bin/benchmark.n -dce no
	neko bin/benchmark.n

benchmark-java:
	$(HAXE_EXEC) -main Benchmark -java bin/java -dce no
	java -jar bin/java/Benchmark.jar

benchmark-cs:
	$(HAXE_EXEC) -main Benchmark -cs bin/cs -dce no
	bin/cs/bin/Benchmark.exe

benchmark-lua:
	$(HAXE_EXEC) -main Benchmark -lua bin/benchmark.lua
	luajit bin/benchmark.lua

benchmark-python:
	$(HAXE_EXEC) -main Benchmark -python bin/benchmark.py -dce no
	python3 bin/benchmark.py

benchmark-pypy:
	$(HAXE_EXEC) -main Benchmark -python bin/benchmark.py -dce no
	$(PYPY3) bin/benchmark.py

benchmark-interp:
	$(HAXE_EXEC) --run Benchmark

benchmark-php:
	$(HAXE_EXEC) -main Benchmark -php bin/php -dce no
	php bin/php/index.php

benchmark-php7:
	$(HAXE_EXEC) -main Benchmark -D php7 -php bin/php -dce no
	php bin/php/index.php

benchmark-js:
	$(HAXE_EXEC) -main Benchmark -js bin/benchmark.js -D nodejs -dce no
	node bin/benchmark.js

benchmark-swf:
	$(HAXE_EXEC) -main Benchmark -swf bin/benchmark.swf -dce no
	flashplayerdebugger bin/benchmark.swf

benchmark-hl:
	$(HAXE_EXEC) -main Benchmark -hl bin/benchmark.hl -dce no
	$(HL) bin/benchmark.hl

benchmark-hl-interp:
	$(HAXE_EXEC) -main Benchmark -hl bin/benchmark.hl -dce no -D interp -D hl-check

benchmark-hlc:
	$(HAXE_EXEC) -main Benchmark -hl bin/benchmark-hl.c -dce no
	cd bin && gcc -o bench-hl -I ./ -I $(HASHLINK_PATH)/src benchmark-hl.c -std=c11 -lm $(HASHLINK_PATH)/libhl.so -Wl,-rpath,$(HASHLINK_PATH)
	bin/bench-hl
