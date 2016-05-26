This test has not been written for pure speed - object allocations are deliberately (over) used to measure how memory access/GC mixes with numeric processing.

Each target generates 2 outputs - one that uses classes and one that uses anonymous objects, so the speed can be compared.

Usage:
```
haxe compile-cpp.hxml
./bin/cpp/Mandelbrot
./bin/cpp-anon/Mandelbrot

# Note - need to time externally at the moment
haxe compile-cppia.hxml
time haxelib run hxcpp bin/Mandelbrot.cppia
time haxelib run hxcpp bin/Mandelbrot-anon.cppia

# Time externally to get sub-second accuracy
haxe compile-java.hxml
time java -jar bin/java/Mandelbrot.jar
time java -jar bin/java-anon/Mandelbrot.jar

haxe compile-js.hxml
node bin/Mandelbrot.js
node bin/Mandelbrot-anon.js

haxe compile-neko.hxml
neko bin/Mandelbrot.n
neko bin/Mandelbrot-anon.n

haxe compile-php.hxml
php bin/php/index.php
php bin/php-anon/index.php
```
