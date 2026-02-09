import haxe.PosInfos;
import haxe.ds.StringMap;
import haxe.ds.Vector;

enum Color {
	Red;
	Green;
	Blue;
	Rgb(r:Int, g:Int, b:Int);
}

interface IGreeter {
	function greet():String;
}

class Person implements IGreeter {
	public var name:String;

	public function new(name:String) {
		this.name = name;
	}

	public function greet():String {
		return 'Hello, I am $name';
	}
}

@:analyzer(ignore)
class Main {
	static function main() {
		untyped __cs__("System.Console.WriteLine({0})", "Main.main()");

		trueValue = true;
		falseValue = false;

		testAssignment();
		testNullAssignment();
		testUnops();
		testNullUnops();
		testOps();
		testNullOps();
		testLoops();
		testExceptions();
		testVector();
		testStringMap();
		testLambdas();
		testArrays();
		testEnums();
		testInterfaces();
		testStrings();
		testMath();
		testDynamicLambdas();
		testTypedLambdasWithMiddleOptional();
		testMethodReferences();
		testMethodReferencesWithOptionals();
		testStaticMethodReferences();
		testHigherOrderFunctions();
		testLambdaAssignments();
		testFunctionTypeVariations();
		testReflectAndType();
		testThreads();
		testAtomics();
		testDynamicArrays();
		testGenericMetadata();
		testNullEquality();
		testIifeOptimization();
		testSerialization();
		testBreakInSwitchInLoop();
		testVoidTypeParam();
		#if hxcoro
		CoroutineTests.run();
		#end

		untyped __cs__("System.Console.WriteLine({0})", 'Done $numTests tests with $numFailures failures');
	}

	public static var numTests:Int = 0;
	public static var numFailures:Int = 0;

	@:generic static function eq<T>(expected:T, actual:T, ?p:PosInfos) {
		numTests++;
		if (expected != actual) {
			numFailures++;
			var line = p != null ? p.lineNumber : 0;
			untyped __cs__("System.Console.WriteLine({0})", 'FAIL at line $line: expected $expected, got $actual');
		}
	}

	static function t(v:Bool, ?p:PosInfos) {
		eq(true, v, p);
	}

	static function f(v:Bool, ?p:PosInfos) {
		eq(false, v, p);
	}

	function foo3(nullableInt:Null<Int>) {}
	function foo4(?optInt:Int) {}
	function foo5(nullableIntWithDef:Null<Int> = 4) {}
	function foo6(?optIntWithDef:Int = 4) {}

	static var trueValue:Bool;
	static var falseValue:Bool;
	static var staticVar:Int;
	static var staticNullVar:Null<Int>;

	// IIFE optimization test helpers
	static var iifeCounter:Int = 0;

	static function getNullIntWithSideEffect():Null<Int> {
		iifeCounter++;
		return 42;
	}

	static function acceptNullFloat(x:Null<Float>):Float {
		return x != null ? x : 0.0;
	}

	static function acceptTwoNullFloats(x:Null<Float>, y:Null<Float>):Float {
		var xv:Float = x != null ? x : 0.0;
		var yv:Float = y != null ? y : 0.0;
		return xv + yv;
	}

	static function returnNullFloatFromInt():Null<Float> {
		return getNullIntWithSideEffect();
	}

	var localVar:Int;
	var localNullVar:Null<Int>;

	function new() {}

	static function testAssignment() {
		var a = 1;
		eq(1, a);
		a = 2;
		eq(2, a);

		staticVar = 1;
		eq(1, staticVar);

		var m = new Main();
		m.localVar = 1;
		eq(1, m.localVar);

		m.localVar += m.localVar += 1;
		eq(3, m.localVar);

		m.localVar = m.localVar += 1;
		eq(4, m.localVar);
	}

	static function testNullAssignment() {
		var a:Null<Int> = 1;
		eq(1, a);
		a = 2;
		eq(2, a);

		staticNullVar = 1;
		eq(1, staticNullVar);

		var m = new Main();
		m.localNullVar = 1;
		eq(1, m.localNullVar);

		m.localNullVar += m.localNullVar += 1;
		eq(3, m.localNullVar);

		m.localNullVar = m.localNullVar += 1;
		eq(4, m.localNullVar);
	}

	static function testUnops() {
		var a = 0;
		eq(0, a++);
		eq(1, a);
		eq(2, ++a);
		eq(2, a);

		var a = 0;
		eq(0, a--);
		eq(-1, a);
		eq(-2, --a);
		eq(-2, a);

		staticVar = 0;
		eq(0, staticVar++);
		eq(1, staticVar);
		eq(2, ++staticVar);
		eq(2, staticVar);

		staticVar = 0;
		eq(0, staticVar--);
		eq(-1, staticVar);
		eq(-2, --staticVar);
		eq(-2, staticVar);

		var m = new Main();
		m.localVar = 0;
		eq(0, m.localVar++);
		eq(1, m.localVar);
		eq(2, ++m.localVar);
		eq(2, m.localVar);

		m.localVar = 0;
		eq(0, m.localVar--);
		eq(-1, m.localVar);
		eq(-2, --m.localVar);
		eq(-2, m.localVar);
	}

	static function testNullUnops() {
		var a:Null<Int> = 0;
		eq(0, a++);
		eq(1, a);
		eq(2, ++a);
		eq(2, a);

		var a:Null<Int> = 0;
		eq(0, a--);
		eq(-1, a);
		eq(-2, --a);
		eq(-2, a);

		staticNullVar = 0;
		eq(0, staticNullVar++);
		eq(1, staticNullVar);
		eq(2, ++staticNullVar);
		eq(2, staticNullVar);

		staticNullVar = 0;
		eq(0, staticNullVar--);
		eq(-1, staticNullVar);
		eq(-2, --staticNullVar);
		eq(-2, staticNullVar);

		var m = new Main();
		m.localNullVar = 0;
		eq(0, m.localNullVar++);
		eq(1, m.localNullVar);
		eq(2, ++m.localNullVar);
		eq(2, m.localNullVar);

		m.localNullVar = 0;
		eq(0, m.localNullVar--);
		eq(-1, m.localNullVar);
		eq(-2, --m.localNullVar);
		eq(-2, m.localNullVar);
	}

	static function testOps() {
		var a = 10;
		// arithmetic
		eq(9, a - 1);
		eq(20, a * 2);
		eq(1, a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10, -a);
		eq(-11, ~a);

		// boolean
		var b = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);

		eq(false, 0 > a);
		eq(false, 0 >= a);
		eq(true, 0 < a);
		eq(true, 0 <= a);
		eq(true, 0 != a);
		eq(false, 0 == a);

		var minusA = -10;
		eq(true, 0 > minusA);
		eq(true, 0 >= minusA);
		eq(false, 0 < minusA);
		eq(false, 0 <= minusA);
		eq(true, 0 != minusA);
		eq(false, 0 == minusA);
	}

	static function testNullOps() {
		var a:Null<Int> = 10;
		// arithmetic
		eq(9, a - 1);
		eq(20, a * 2);
		eq(1, a % 3);

		// bit
		eq(20, a << 1);
		eq(5, a >> 1);
		eq(5, a >>> 1);
		eq(10, a & 15);
		eq(15, a | 15);
		eq(2, a ^ 8);

		// unary
		eq(-10, -a);
		eq(-11, ~a);

		// boolean
		var b:Null<Bool> = true;
		eq(false, !b);
		eq(false, b && falseValue);
		eq(true, b && trueValue);
		eq(true, b || falseValue);
		eq(true, b || trueValue);

		b = false;
		eq(true, !b);
		eq(false, b && falseValue);
		eq(false, b && trueValue);
		eq(false, b || falseValue);
		eq(true, b || trueValue);

		eq(true, a > 5);
		eq(true, a >= 5);
		eq(false, a < 5);
		eq(false, a <= 5);
		eq(true, a != 5);
		eq(false, a != 10);

		eq(false, 0 > a);
		eq(false, 0 >= a);
		eq(true, 0 < a);
		eq(true, 0 <= a);
		eq(true, 0 != a);
		eq(false, 0 == a);

		var minusA:Null<Int> = -10;
		eq(true, 0 > minusA);
		eq(true, 0 >= minusA);
		eq(false, 0 < minusA);
		eq(false, 0 <= minusA);
		eq(true, 0 != minusA);
		eq(false, 0 == minusA);
	}

	static function testLoops() {
		// while loop
		var sum = 0;
		var i = 0;
		while (i < 5) {
			sum += i;
			i++;
		}
		eq(10, sum); // 0+1+2+3+4 = 10

		// do-while loop (simulated with while + initial)
		sum = 0;
		i = 0;
		do {
			sum += i;
			i++;
		} while (i < 5);
		eq(10, sum);

		// for loop with range
		sum = 0;
		for (j in 0...5) {
			sum += j;
		}
		eq(10, sum);

		// for loop with array
		var arr = [1, 2, 3, 4, 5];
		sum = 0;
		for (x in arr) {
			sum += x;
		}
		eq(15, sum);

		// break
		sum = 0;
		for (k in 0...10) {
			if (k >= 5) break;
			sum += k;
		}
		eq(10, sum);

		// continue
		sum = 0;
		for (k in 0...10) {
			if (k % 2 == 1) continue;
			sum += k;
		}
		eq(20, sum); // 0+2+4+6+8 = 20
	}

	static function testExceptions() {
		// basic try/catch
		var caught = false;
		try {
			throw "error";
		} catch (e:String) {
			caught = true;
			eq("error", e);
		}
		t(caught);

		// try/catch with different exception types
		caught = false;
		try {
			throw 42;
		} catch (e:Int) {
			caught = true;
			eq(42, e);
		}
		t(caught);

		// rethrow
		var rethrown = false;
		try {
			try {
				throw "inner";
			} catch (e:String) {
				throw e + "_rethrown";
			}
		} catch (e:String) {
			rethrown = true;
			eq("inner_rethrown", e);
		}
		t(rethrown);

		// exception not caught by wrong type
		var wrongType = false;
		var rightType = false;
		try {
			throw "string_error";
		} catch (e:Int) {
			wrongType = true;
		} catch (e:String) {
			rightType = true;
		}
		f(wrongType);
		t(rightType);

	}

	static function testVector() {
		// Test Vector<Int>
		var v = new Vector<Int>(5);
		eq(5, v.length);

		// Set values
		v[0] = 10;
		v[1] = 20;
		v[2] = 30;
		v[3] = 40;
		v[4] = 50;

		// Get values
		eq(10, v[0]);
		eq(20, v[1]);
		eq(30, v[2]);
		eq(40, v[3]);
		eq(50, v[4]);

		// toArray
		var arr = v.toArray();
		eq(5, arr.length);
		eq(10, arr[0]);
		eq(50, arr[4]);

		// Test Vector<String>
		var vs = new Vector<String>(3);
		vs[0] = "hello";
		vs[1] = "world";
		vs[2] = "!";
		eq("hello", vs[0]);
		eq("world", vs[1]);
		eq("!", vs[2]);
	}

	static function testStringMap() {
		var map = new StringMap<Int>();

		// set and get
		map.set("one", 1);
		map.set("two", 2);
		map.set("three", 3);

		eq(1, map.get("one"));
		eq(2, map.get("two"));
		eq(3, map.get("three"));
		eq(null, map.get("four"));

		// exists
		t(map.exists("one"));
		t(map.exists("two"));
		f(map.exists("four"));

		// remove
		t(map.remove("two"));
		f(map.exists("two"));
		f(map.remove("nonexistent"));

		// overwrite
		map.set("one", 100);
		eq(100, map.get("one"));

		// add back two
		map.set("two", 2);

		// Test keys() iteration
		var keysCollected = new Array<String>();
		var keysIter = map.keys();
		while (keysIter.hasNext()) {
			keysCollected.push(keysIter.next());
		}
		eq(3, keysCollected.length);
		t(keysCollected.indexOf("one") >= 0);
		t(keysCollected.indexOf("two") >= 0);
		t(keysCollected.indexOf("three") >= 0);

		// Test iterator() (values iteration)
		var valuesCollected = new Array<Int>();
		var valuesIter = map.iterator();
		while (valuesIter.hasNext()) {
			valuesCollected.push(valuesIter.next());
		}
		eq(3, valuesCollected.length);
		t(valuesCollected.indexOf(100) >= 0); // "one" was set to 100
		t(valuesCollected.indexOf(2) >= 0);
		t(valuesCollected.indexOf(3) >= 0);

		// Test copy()
		var copied = map.copy();
		eq(100, copied.get("one"));
		eq(2, copied.get("two"));
		eq(3, copied.get("three"));
		// Modify original, copy should be unaffected
		map.set("one", 999);
		eq(100, copied.get("one")); // Copy still has old value

		// Test toString()
		var str = copied.toString();
		t(str.indexOf("one") >= 0);
		t(str.indexOf("=>") >= 0);

		// Test size()
		eq(3, copied.size());

		// Test keyValueIterator()
		var kvCount = 0;
		var kvIter = map.keyValueIterator();
		while (kvIter.hasNext()) {
			var kv = kvIter.next();
			kvCount++;
		}
		eq(3, kvCount);

		// clear
		map.clear();
		f(map.exists("one"));
		f(map.exists("three"));
		f(map.exists("two"));
		eq(0, map.size());
	}

	static function testLambdas() {
		// Basic lambda
		var add = function(a:Int, b:Int):Int {
			return a + b;
		};
		eq(7, add(3, 4));

		// Lambda with capture
		var multiplier = 3;
		var mult = function(x:Int):Int {
			return x * multiplier;
		};
		eq(15, mult(5));

		// Higher-order function
		var apply = function(f:Int->Int, x:Int):Int {
			return f(x);
		};
		eq(10, apply(function(n) return n * 2, 5));
	}

	static function testArrays() {
		var arr = [1, 2, 3, 4, 5];

		// Basic access
		eq(5, arr.length);
		eq(1, arr[0]);
		eq(3, arr[2]);
		eq(5, arr[4]);

		// push
		arr.push(6);
		eq(6, arr.length);
		eq(6, arr[5]);

		// pop
		var popped = arr.pop();
		eq(6, popped);
		eq(5, arr.length);

		// sum via for loop
		var sum = 0;
		for (item in arr) {
			sum += item;
		}
		eq(15, sum);

		// indexOf
		eq(2, arr.indexOf(3));
		eq(-1, arr.indexOf(99));

		// concat
		var arr2 = [10, 20];
		var combined = arr.concat(arr2);
		eq(7, combined.length);
		eq(10, combined[5]);
	}

	static function testDynamicArrays() {
		// Test 1: Cast Array<Int> to Array<Dynamic>
		var intArr:Array<Int> = [1, 2, 3, 4, 5];
		eq(5, intArr.length);
		eq(3, intArr[2]);

		// Cast to Dynamic - should work since Array is non-generic at runtime
		var dynArr:Array<Dynamic> = cast intArr;
		eq(5, dynArr.length);
		eq(3, dynArr[2]);

		// Modifying through Dynamic reference should work
		dynArr.push(6);
		eq(6, dynArr.length);
		eq(6, intArr.length); // Same underlying array

		// Test 2: Cast Array<Dynamic> (with only ints) to Array<Int>
		var dynArr2:Array<Dynamic> = [];
		dynArr2.push(10);
		dynArr2.push(20);
		dynArr2.push(30);
		eq(3, dynArr2.length);

		// Cast to Int array - should work since it only contains ints
		var intArr2:Array<Int> = cast dynArr2;
		eq(3, intArr2.length);
		eq(10, intArr2[0]);
		eq(20, intArr2[1]);
		eq(30, intArr2[2]);

		// Sum through typed array
		var sum = 0;
		for (v in intArr2) {
			sum += v;
		}
		eq(60, sum);

		// Test 3: Array operations preserve type after cast
		intArr2.push(40);
		eq(4, intArr2.length);
		eq(4, dynArr2.length); // Same underlying array
		eq(40, dynArr2[3]);

		// Test 4: String array casting
		var strArr:Array<String> = ["a", "b", "c"];
		var dynStrArr:Array<Dynamic> = cast strArr;
		eq(3, dynStrArr.length);
		eq("b", dynStrArr[1]);

		// Cast back
		var strArr2:Array<String> = cast dynStrArr;
		eq("c", strArr2[2]);
	}

	static function testEnums() {
		// Simple enum value - test enum comparison (switch/pattern matching requires block expression support)
		var color = Color.Red;
		t(color == Color.Red);
		f(color == Color.Blue);
		f(color == Color.Green);

		// Parametric enum - basic construction test
		var rgb = Color.Rgb(255, 128, 0);
		t(rgb != Color.Red);
		t(rgb != Color.Blue);
		t(rgb != Color.Green);

		// Test that different instances of parametric enums are equal if values match
		var rgb2 = Color.Rgb(255, 128, 0);
		// Note: In Haxe, enum instances with same values may or may not be == depending on target
		// Just test that creation works
		t(rgb2 != null);
	}

	static function testInterfaces() {
		var person = new Person("Alice");
		eq("Hello, I am Alice", person.greet());

		// Interface reference
		var greeter:IGreeter = person;
		eq("Hello, I am Alice", greeter.greet());
	}

	static function testStrings() {
		var s = "Hello World";

		// length
		eq(11, s.length);

		// charAt
		eq("H", s.charAt(0));
		eq("e", s.charAt(1));
		eq("d", s.charAt(10));

		// charCodeAt
		eq(72, s.charCodeAt(0)); // 'H'
		eq(101, s.charCodeAt(1)); // 'e'

		// indexOf with explicit startIndex to avoid optional param issue
		eq(7, s.indexOf("o", 5));

		// substring with explicit end
		eq("Hello", s.substring(0, 5));
		eq("World", s.substring(6, 11));

		// toLowerCase / toUpperCase
		eq("hello world", s.toLowerCase());
		eq("HELLO WORLD", s.toUpperCase());

		// split
		var parts = s.split(" ");
		eq(2, parts.length);
		eq("Hello", parts[0]);
		eq("World", parts[1]);

		// string interpolation
		var name = "Bob";
		var age = 25;
		eq("Name: Bob, Age: 25", 'Name: $name, Age: $age');
	}

	static function testMath() {
		// abs
		eq(5.0, Math.abs(-5.0));
		eq(5.0, Math.abs(5.0));

		// floor/ceil/round
		eq(3.0, Math.floor(3.7));
		eq(4.0, Math.ceil(3.2));
		eq(4.0, Math.round(3.7));
		eq(3.0, Math.round(3.2));

		// min/max
		eq(5.0, Math.max(5.0, 3.0));
		eq(3.0, Math.min(5.0, 3.0));

		// sqrt
		eq(3.0, Math.sqrt(9.0));

		// pow
		eq(8.0, Math.pow(2.0, 3.0));

		// trigonometry (basic check)
		t(Math.sin(0.0) == 0.0);
		t(Math.cos(0.0) == 1.0);
	}

	static function testDynamicLambdas() {
		// Test 1: Basic lambda assigned to Dynamic
		var add:Dynamic = function(a:Int, b:Int):Int {
			return a + b;
		};
		// Try calling it directly
		var result:Int = add(3, 4);
		eq(7, result);

		// Test 2: Lambda with optional parameter assigned to Dynamic
		var greet:Dynamic = function(name:String, ?greeting:String):String {
			if (greeting == null) greeting = "Hello";
			return greeting + ", " + name + "!";
		};
		var greetResult1:String = greet("World");
		eq("Hello, World!", greetResult1);
		var greetResult2:String = greet("World", "Hi");
		eq("Hi, World!", greetResult2);

		// Test 3: Lambda stored in a typed variable, then passed to Dynamic
		var mult = function(x:Int, y:Int):Int {
			return x * y;
		};
		var dynMult:Dynamic = mult;
		var multResult:Int = dynMult(3, 4);
		eq(12, multResult);

		// Test 4: Reflect.callMethod on a lambda
		var sub = function(a:Int, b:Int):Int {
			return a - b;
		};
		// var result = Reflect.callMethod(null, sub, [10, 3]);
		// eq(7, result);

		// Test 5: Reflect.isFunction on lambdas
		var fn = function():Void {};
		var isFn = Reflect.isFunction(fn);
		t(isFn);

		// Test 6: isFunction on non-functions
		f(Reflect.isFunction(null));
		f(Reflect.isFunction(42));
		f(Reflect.isFunction("hello"));

		// Test 7: isFunction on typed lambdas
		var add2 = function(a:Int, b:Int):Int { return a + b; };
		t(Reflect.isFunction(add2));

		// Ensure Dynamic-typed lambdas are also recognized
		t(Reflect.isFunction(add));
		t(Reflect.isFunction(greet));
		t(Reflect.isFunction(dynMult));
		t(Reflect.isFunction(sub));

		// For now, just test that we can assign and the type system accepts it
		t(add != null);
		t(greet != null);
		t(dynMult != null);
		t(sub != null);
		t(fn != null);

		// Test 8: Lambda with optional class type parameter (Null<Person>)
		var greetPerson:Dynamic = function(defaultGreeting:String, ?person:Person):String {
			if (person == null) return defaultGreeting;
			var p:Person = person;  // Implicit conversion from Null<Person> to Person
			return p.greet();
		};
		var greetPersonResult1:String = greetPerson("No one here");
		eq("No one here", greetPersonResult1);
		var greetPersonResult2:String = greetPerson("No one here", new Person("Alice"));
		eq("Hello, I am Alice", greetPersonResult2);

		// Test 9: Cast Dynamic back to typed function and call normally
		// Note: The function must have matching signature (no optional params transform)
		var typedAdd:(Int, Int) -> Int = cast add;
		var typedResult = typedAdd(10, 20);
		eq(30, typedResult);

		// Test 10: Round-trip: typed -> Dynamic -> typed -> call (simple signature)
		var original = function(x:Int):Int { return x * 2; };
		var asDynamic:Dynamic = original;
		var backToTyped:(Int) -> Int = cast asDynamic;
		eq(10, backToTyped(5));

		// Test 11: Lambda with optional parameter in the MIDDLE (not at the end)
		// NOTE: Dynamic invocation with middle optionals requires all args to be provided
		// because at runtime we can't determine which params are optional vs required.
		// The arguments are matched positionally.
		var middleOpt:Dynamic = function(first:Int, ?middle:Int, last:Int):Int {
			var m = middle == null ? 0 : middle;
			return first + m + last;
		};
		// Call with all arguments provided
		var middleOptResult1:Int = middleOpt(1, 2, 3);
		eq(6, middleOptResult1);  // 1 + 2 + 3 = 6

		// Test 12: Multiple optional parameters - all provided for dynamic call
		var multiOpt:Dynamic = function(a:Int, ?b:Int, c:Int, ?d:Int):String {
			var bVal = b == null ? 0 : b;
			var dVal = d == null ? 0 : d;
			return 'a=$a b=$bVal c=$c d=$dVal';
		};
		// All args provided
		var multiOptResult1:String = multiOpt(1, 2, 3, 4);
		eq("a=1 b=2 c=3 d=4", multiOptResult1);

		// Test 13: Optional class type in middle position - all args provided
		var middleClass:Dynamic = function(prefix:String, ?person:Person, suffix:String):String {
			if (person == null) return prefix + "[no one]" + suffix;
			var p:Person = person;
			return prefix + p.greet() + suffix;
		};
		// All args provided
		var middleClassResult1:String = middleClass(">>", new Person("Bob"), "<<");
		eq(">>Hello, I am Bob<<", middleClassResult1);

		// Pass null explicitly for middle optional Person
		var middleClassResult2:String = middleClass(">>", null, "<<");
		eq(">>[no one]<<", middleClassResult2);
	}

	static function testTypedLambdasWithMiddleOptional() {
		// Test typed lambdas with optional parameters in the middle (not Dynamic)
		// NOTE: In typed Haxe calls on static platforms (C#, JVM), you CANNOT pass null
		// for optional primitive types (Int, Float, Bool). This is a language limitation.
		// Middle optional skipping only works with Dynamic calls at runtime.
		//
		// For typed calls with optional primitives in the middle:
		// - You MUST provide all args (can't skip middle optionals)
		// - You can't pass `null` for Int/Float/Bool on static platforms
		// - For reference types (classes, String), you CAN pass null

		// Test 1: Simple middle optional Int - must provide all args, can't pass null
		var middleOpt = function(first:Int, ?middle:Int, last:Int):Int {
			var m = middle == null ? 0 : middle;
			return first + m + last;
		};
		// Call with all arguments provided
		eq(6, middleOpt(1, 2, 3));  // 1 + 2 + 3 = 6
		eq(4, middleOpt(1, 0, 3));  // Use 0 instead of null for "no value"

		// Test 2: Optional class type in middle position - CAN use null
		var middleClass = function(prefix:String, ?person:Person, suffix:String):String {
			if (person == null) return prefix + "[no one]" + suffix;
			var p:Person = person;
			return prefix + p.greet() + suffix;
		};
		// All args provided
		eq(">>Hello, I am Bob<<", middleClass(">>", new Person("Bob"), "<<"));

		// Pass null explicitly for middle optional Person - this works for reference types!
		eq(">>[no one]<<", middleClass(">>", null, "<<"));

		// Test 3: Optional String in middle - String is reference type, can use null
		var middleString = function(a:Int, ?sep:String, b:Int):String {
			var s = sep == null ? "-" : sep;
			return '$a$s$b';
		};
		eq("1|2", middleString(1, "|", 2));
		eq("1-2", middleString(1, null, 2));  // null works for String

		// Test 4: Trailing optional Int - this is the standard supported case
		var trailingOpt = function(a:Int, b:Int, ?c:Int):Int {
			var cVal = c == null ? 0 : c;
			return a + b + cVal;
		};
		eq(6, trailingOpt(1, 2, 3));  // All provided
		eq(3, trailingOpt(1, 2));     // Trailing optional omitted - this works!

		// Test 5: Multiple trailing optionals - all can be omitted
		var multiTrailing = function(a:Int, ?b:Int, ?c:Int):Int {
			var bVal = b == null ? 0 : b;
			var cVal = c == null ? 0 : c;
			return a + bVal + cVal;
		};
		eq(6, multiTrailing(1, 2, 3));  // All provided
		eq(3, multiTrailing(1, 2));     // c omitted
		eq(1, multiTrailing(1));        // b and c omitted

		// Test 6: Explicitly typed function variable with middle optional Int
		// The type annotation includes the optional: (Int, ?Int, Int) -> Int
		var typedMiddleOpt:(Int, ?Int, Int) -> Int = function(first:Int, ?middle:Int, last:Int):Int {
			var m = middle == null ? 0 : middle;
			return first + m + last;
		};
		eq(6, typedMiddleOpt(1, 2, 3));  // All provided
		eq(4, typedMiddleOpt(1, 0, 3));  // 0 for middle

		// Test 7: Explicitly typed with middle optional String (reference type)
		var typedMiddleStr:(Int, ?String, Int) -> String = function(a:Int, ?sep:String, b:Int):String {
			var s = sep == null ? "-" : sep;
			return '$a$s$b';
		};
		eq("1|2", typedMiddleStr(1, "|", 2));
		eq("1-2", typedMiddleStr(1, null, 2));  // null works for String

		// Test 8: Explicitly typed with middle optional class (Person)
		var typedMiddleClass:(String, ?Person, String) -> String = function(prefix:String, ?person:Person, suffix:String):String {
			if (person == null) return prefix + "[none]" + suffix;
			var p:Person = person;
			return prefix + p.greet() + suffix;
		};
		eq("<<Hello, I am Eve>>", typedMiddleClass("<<", new Person("Eve"), ">>"));
		eq("<<[none]>>", typedMiddleClass("<<", null, ">>"));

		// Test 9: Assign lambda to typed var, then pass to another typed var
		var fn1:(Int, ?Int, Int) -> Int = function(a:Int, ?b:Int, c:Int):Int {
			var bVal = b == null ? 0 : b;
			return a + bVal + c;
		};
		var fn2:(Int, ?Int, Int) -> Int = fn1;  // Copy to another typed var
		eq(6, fn2(1, 2, 3));
		eq(4, fn2(1, 0, 3));

		// Test 10: Multiple middle optionals with explicit type
		var multiMiddle:(Int, ?Int, ?Int, Int) -> Int = function(a:Int, ?b:Int, ?c:Int, d:Int):Int {
			var bVal = b == null ? 0 : b;
			var cVal = c == null ? 0 : c;
			return a + bVal + cVal + d;
		};
		eq(10, multiMiddle(1, 2, 3, 4));  // All provided: 1+2+3+4
		eq(5, multiMiddle(1, 0, 0, 4));   // Middle nulls as 0: 1+0+0+4
	}

	static function testMethodReferences() {
		// Test wrapping instance methods as lambdas (typed and dynamic)

		// Test 1: Typed reference to instance method (no optional params)
		var person = new Person("Alice");
		var greetFn:() -> String = person.greet;
		eq("Hello, I am Alice", greetFn());

		// Test 2: Dynamic reference to instance method
		var greetDyn:Dynamic = person.greet;
		var dynResult:String = greetDyn();
		eq("Hello, I am Alice", dynResult);

		// Test 3: Instance method with parameters - create a helper class
		var calc = new Calculator();
		var addFn:(Int, Int) -> Int = calc.add;
		eq(7, addFn(3, 4));

		// Test 4: Dynamic reference to method with parameters
		var addDyn:Dynamic = calc.add;
		var addDynResult:Int = addDyn(10, 20);
		eq(30, addDynResult);

		// Test 5: Method with optional parameter (dynamic only)
		// NOTE: Typed method references with optional params have signature mismatch issues
		// because Haxe ?Int becomes Null<Int> in C# which doesn't match Int
		var optDyn:Dynamic = calc.addWithOptional;
		var optDynResult1:Int = optDyn(10, 5);
		eq(15, optDynResult1);
		var optDynResult2:Int = optDyn(10);
		eq(10, optDynResult2);

		// Test 6: Method with middle optional (dynamic)
		// NOTE: Dynamic invocation requires all args - can't skip middle optionals
		var midDyn:Dynamic = calc.addWithMiddleOptional;
		var midDynResult1:Int = midDyn(1, 2, 3);
		eq(6, midDynResult1);
		// Must provide all args for dynamic call with middle optional
		var midDynResult2:Int = midDyn(1, 0, 3);  // Pass 0 for middle optional
		eq(4, midDynResult2);

		// Test 7: Store method ref, call later (verifies closure capture)
		var personBob = new Person("Bob");
		var bobGreet = personBob.greet;
		var personCarol = new Person("Carol");
		var carolGreet = personCarol.greet;
		// Both should still refer to their original instances
		eq("Hello, I am Bob", bobGreet());
		eq("Hello, I am Carol", carolGreet());
	}

	static function testMethodReferencesWithOptionals() {
		// Comprehensive tests for method references with optional parameters

		var calc = new Calculator();

		// Test 1: Typed method reference with trailing optional - call with all args
		var optFn:(Int, ?Int) -> Int = calc.addWithOptional;
		eq(15, optFn(10, 5));

		// Test 2: Typed method reference with trailing optional - call without optional
		eq(10, optFn(10));

		// Test 3: Multiple trailing optionals - typed reference
		var twoOptFn:(?Int, ?Int) -> Int = calc.addWithTwoOptionals;
		eq(0, twoOptFn());
		eq(5, twoOptFn(5));
		eq(15, twoOptFn(5, 10));

		// Test 4: Three trailing optionals
		var threeOptFn:(?Int, ?Int, ?Int) -> Int = calc.addWithThreeOptionals;
		eq(0, threeOptFn());
		eq(1, threeOptFn(1));
		eq(3, threeOptFn(1, 2));
		eq(6, threeOptFn(1, 2, 3));

		// Test 5: Dynamic reference to method with two optionals
		var twoOptDyn:Dynamic = calc.addWithTwoOptionals;
		eq(0, (twoOptDyn() : Int));
		eq(5, (twoOptDyn(5) : Int));
		eq(15, (twoOptDyn(5, 10) : Int));

		// Test 6: Optional String parameter (reference type)
		var concatFn:(String, ?String) -> String = calc.concatWithOptional;
		eq("Hello", concatFn("Hello"));
		eq("HelloWorld", concatFn("Hello", "World"));

		// Test 7: Dynamic reference with optional String
		var concatDyn:Dynamic = calc.concatWithOptional;
		eq("Hi", (concatDyn("Hi") : String));
		eq("HiThere", (concatDyn("Hi", "There") : String));

		// Test 8: Optional class type parameter
		var greetFn:(?Person) -> String = calc.greetOptionalPerson;
		eq("Hello, stranger!", greetFn());
		eq("Hello, stranger!", greetFn(null));
		eq("Hello, Alice!", greetFn(new Person("Alice")));

		// Test 9: Dynamic reference with optional class type
		var greetDyn:Dynamic = calc.greetOptionalPerson;
		eq("Hello, stranger!", (greetDyn() : String));
		eq("Hello, Bob!", (greetDyn(new Person("Bob")) : String));

		// Test 9b: Optional class type with direct field access (no explicit unwrap)
		var greetAltFn:(?Person) -> String = calc.greetOptionalPersonAlt;
		eq("Hello, stranger!", greetAltFn());
		eq("Hello, stranger!", greetAltFn(null));
		eq("Hello, Charlie!", greetAltFn(new Person("Charlie")));

		// Test 9c: Optional class type with Dynamic assignment and dynamic field access
		var greetAlt2Fn:(?Person) -> String = calc.greetOptionalPersonAlt2;
		eq("Hello, stranger!", greetAlt2Fn());
		eq("Hello, stranger!", greetAlt2Fn(null));
		eq("Hello, Dave!", greetAlt2Fn(new Person("Dave")));

		// Test 10: Middle optional - typed reference (must provide all args)
		var midOptFn:(Int, ?Int, Int) -> Int = calc.addWithMiddleOptional;
		eq(6, midOptFn(1, 2, 3));
		eq(4, midOptFn(1, 0, 3));  // Use 0 for "no value"

		// Test 11: Reassigning method references
		var fn1:(Int, Int) -> Int = calc.add;
		var fn2:(Int, Int) -> Int = fn1;
		eq(10, fn1(3, 7));
		eq(10, fn2(3, 7));
	}

	static function testStaticMethodReferences() {
		// Test static method references

		// Test 1: Simple static method reference - typed
		var addFn:(Int, Int) -> Int = Calculator.staticAdd;
		eq(15, addFn(7, 8));

		// Test 2: Static method reference - dynamic
		var addDyn:Dynamic = Calculator.staticAdd;
		eq(20, (addDyn(8, 12) : Int));

		// Test 3: Static method with optional - typed
		var optFn:(Int, ?Int) -> Int = Calculator.staticAddOptional;
		eq(10, optFn(10));
		eq(15, optFn(10, 5));

		// Test 4: Static method with optional - dynamic
		var optDyn:Dynamic = Calculator.staticAddOptional;
		eq(20, (optDyn(20) : Int));
		eq(25, (optDyn(20, 5) : Int));

		// Test 5: Store static method ref, call later
		var storedFn = Calculator.staticAdd;
		eq(100, storedFn(60, 40));

		// Test 6: Pass static method as argument
		var calc = new Calculator();
		var result = calc.applyTwice(function(x) return x + 1, 5);
		eq(7, result);  // (5+1)+1 = 7
	}

	static function testHigherOrderFunctions() {
		// Test functions that return or accept functions

		var calc = new Calculator();

		// Test 1: Method returning a function
		var add5 = calc.getAdder(5);
		eq(15, add5(10));
		eq(8, add5(3));

		// Test 2: Store returned function in typed variable
		var adder:(Int) -> Int = calc.getAdder(10);
		eq(25, adder(15));

		// Test 3: Chain function-returning methods
		var add3 = calc.getAdder(3);
		var add7 = calc.getAdder(7);
		eq(13, add3(add7(3)));  // add7(3)=10, add3(10)=13

		// Test 4: Method taking function as parameter
		var double = function(x:Int):Int return x * 2;
		eq(20, calc.applyTwice(double, 5));  // double(double(5)) = double(10) = 20

		// Test 5: Lambda with captured variable
		var multiplier = 3;
		var multiplyFn = function(x:Int):Int return x * multiplier;
		eq(36, calc.applyTwice(multiplyFn, 4));  // 4*3=12, 12*3=36

		// Test 6: Nested lambdas
		var makeMultiplier = function(n:Int):(Int) -> Int {
			return function(x:Int):Int return x * n;
		};
		var times4 = makeMultiplier(4);
		eq(20, times4(5));
		eq(64, calc.applyTwice(times4, 4));  // 4*4=16, 16*4=64

		// Test 7: Dynamic higher-order
		var getAdderDyn:Dynamic = calc.getAdder;
		var dynAdder:Dynamic = getAdderDyn(100);
		eq(150, (dynAdder(50) : Int));
	}

	static function testLambdaAssignments() {
		// Test various lambda assignment patterns

		// Test 1: Assign lambda to var, no type annotation
		var addLambda = function(a:Int, b:Int):Int return a + b;
		eq(10, addLambda(4, 6));

		// Test 2: Assign lambda to typed var
		var subLambda:(Int, Int) -> Int = function(a:Int, b:Int):Int return a - b;
		eq(5, subLambda(12, 7));

		// Test 3: Reassign lambda
		var fn:(Int) -> Int = function(x:Int):Int return x + 1;
		eq(6, fn(5));
		fn = function(x:Int):Int return x * 2;
		eq(10, fn(5));

		// Test 4: Lambda with no parameters
		var getHello:() -> String = function():String return "Hello";
		eq("Hello", getHello());

		// Test 5: Lambda returning void (Action)
		var counter = 0;
		var increment:() -> Void = function():Void counter++;
		increment();
		increment();
		eq(2, counter);

		// Test 6: Lambda with optional param
		var optLambda = function(a:Int, ?b:Int):Int {
			var bVal = b == null ? 0 : b;
			return a + bVal;
		};
		eq(10, optLambda(10));
		eq(15, optLambda(10, 5));

		// Test 7: Explicitly typed lambda with optional
		var typedOpt:(Int, ?Int) -> Int = function(a:Int, ?b:Int):Int {
			var bVal = b == null ? 100 : b;
			return a + bVal;
		};
		eq(110, typedOpt(10));  // Uses default 100
		eq(15, typedOpt(10, 5));

		// Test 8: Lambda stored in Dynamic
		var dynLambda:Dynamic = function(a:Int, b:Int):Int return a * b;
		eq(24, (dynLambda(4, 6) : Int));

		// Test 9: Complex return type
		var makeGreeter = function(prefix:String):(String) -> String {
			return function(name:String):String return prefix + name;
		};
		var hiGreeter = makeGreeter("Hi, ");
		eq("Hi, World", hiGreeter("World"));

		// Test 10: Block expressions as values (smart prefix pattern)
		// This tests the C# target's "smart" block expression handling
		// where `var x = { stmts; value; }` becomes `T x; { stmts; x = value; }`
		var blockResult = {
			var a = 10;
			var b = 20;
			a + b;  // block returns this value
		};
		eq(30, blockResult);

		// Test 11: Nested block expressions
		var nestedBlock = {
			var outer = 5;
			var inner = {
				var x = 10;
				x * 2;
			};
			outer + inner;
		};
		eq(25, nestedBlock);

		// Test 12: Block expression with conditionals
		var condBlock = {
			var value = 7;
			if (value > 5) {
				value * 2;
			} else {
				value;
			}
		};
		eq(14, condBlock);

		// Test 13: Lambda with multiple optional params
		var multiOpt = function(?a:Int, ?b:Int, ?c:Int):Int {
			var aVal = a == null ? 1 : a;
			var bVal = b == null ? 2 : b;
			var cVal = c == null ? 3 : c;
			return aVal + bVal + cVal;
		};
		eq(6, multiOpt());  // 1+2+3
		eq(10, multiOpt(5));  // 5+2+3
		eq(12, multiOpt(5, 4));  // 5+4+3
		eq(15, multiOpt(5, 4, 6));  // 5+4+6
	}

	static function testFunctionTypeVariations() {
		// Test various function type declarations and usages

		// Test 1: Func with primitive types
		var intToInt:(Int) -> Int = function(x:Int):Int return x * 2;
		eq(20, intToInt(10));

		// Test 2: Func with Float
		var floatFn:(Float) -> Float = function(x:Float):Float return x * 1.5;
		eq(15.0, floatFn(10.0));

		// Test 3: Func with Bool
		var boolFn:(Bool) -> Bool = function(x:Bool):Bool return !x;
		eq(false, boolFn(true));
		eq(true, boolFn(false));

		// Test 4: Func with String
		var strFn:(String) -> String = function(s:String):String return s.toUpperCase();
		eq("HELLO", strFn("hello"));

		// Test 5: Func with class type
		var personFn:(Person) -> String = function(p:Person):String return p.greet();
		eq("Hello, I am Test", personFn(new Person("Test")));

		// Test 6: Func returning class
		var createPerson:(String) -> Person = function(name:String):Person return new Person(name);
		var p = createPerson("Created");
		eq("Hello, I am Created", p.greet());

		// Test 7: Multiple parameters of different types
		var mixedFn:(Int, String, Bool) -> String = function(n:Int, s:String, b:Bool):String {
			var bStr = b ? "yes" : "no";  // Avoid Bool->String which differs between platforms
			return '$n-$s-$bStr';
		};
		eq("42-test-yes", mixedFn(42, "test", true));

		// Test 8: Action types (void return)
		var action0:() -> Void = function():Void {};
		action0();  // Just verify it compiles and runs

		var sideEffect = "";
		var action1:(String) -> Void = function(s:String):Void sideEffect = s;
		action1("modified");
		eq("modified", sideEffect);

		// Test 9: Nested function types
		var nested:((Int) -> Int) -> Int = function(fn:(Int) -> Int):Int return fn(10);
		eq(20, nested(function(x:Int):Int return x * 2));
		eq(100, nested(function(x:Int):Int return x * 10));

		// Test 10: Function type with generic-like pattern (using Any)
		var identity:(Dynamic) -> Dynamic = function(x:Dynamic):Dynamic return x;
		eq(42, (identity(42) : Int));
		eq("hello", (identity("hello") : String));

		// Test 11: Curried function pattern
		var curry = function(a:Int):(Int) -> Int {
			return function(b:Int):Int return a + b;
		};
		var add10 = curry(10);
		eq(15, add10(5));
		eq(30, curry(20)(10));

		// Test 12: Function composition helper
		var compose = function(f:(Int) -> Int, g:(Int) -> Int):(Int) -> Int {
			return function(x:Int):Int return f(g(x));
		};
		var double = function(x:Int):Int return x * 2;
		var addOne = function(x:Int):Int return x + 1;
		var doubleAddOne = compose(addOne, double);  // addOne(double(x))
		eq(11, doubleAddOne(5));  // double(5)=10, addOne(10)=11
	}

	static function testReflectAndType() {
		// Test Reflect API and Type API

		// Test 1: Reflect.field - read field from object
		var person = new Person("Alice");
		var nameValue = Reflect.field(person, "name");
		eq("Alice", nameValue);

		// Test 2: Reflect.setField - write field to object
		Reflect.setField(person, "name", "Bob");
		eq("Bob", person.name);

		// Test 3: Reflect.fields - get list of fields
		var fields = Reflect.fields(person);
		t(fields.indexOf("name") >= 0);

		// Test 4: Reflect.hasField
		t(Reflect.hasField(person, "name"));
		f(Reflect.hasField(person, "nonexistent"));

		// Test 5: Reflect.isFunction
		var fn = function():Void {};
		t(Reflect.isFunction(fn));
		f(Reflect.isFunction(person));
		f(Reflect.isFunction(null));
		f(Reflect.isFunction(42));
		f(Reflect.isFunction("hello"));

		// Test 6: Dynamic field access - read and write
		var dynPerson:Dynamic = person;
		var dynName:String = dynPerson.name;
		eq("Bob", dynName);  // Bob from Test 2
		dynPerson.name = "Charlie";
		eq("Charlie", person.name);

		// Test 7: Reflect.compare
		eq(0, Reflect.compare(5, 5));
		eq(-1, Reflect.compare(3, 5));
		eq(1, Reflect.compare(7, 5));
		eq(0, Reflect.compare("abc", "abc"));
		eq(-1, Reflect.compare("abc", "def"));
		eq(1, Reflect.compare("def", "abc"));

		// Test 8: Type.getClass
		var personClass = Type.getClass(person);
		t(personClass != null);

		// Test 9: Type.getClassName
		var className = Type.getClassName(personClass);
		eq("Person", className);

		// Test 10: Type.createInstance
		var newPerson:Person = Type.createInstance(personClass, ["Eve"]);
		eq("Eve", newPerson.name);

		// Test 11: Type.getInstanceFields
		var instanceFields = Type.getInstanceFields(personClass);
		t(instanceFields.indexOf("name") >= 0);
		t(instanceFields.indexOf("greet") >= 0);

		// Test 12: Type.typeof for Int
		var intType = Type.typeof(42);
		switch (intType) {
			case TInt:
				t(true);
			default:
				t(false);
		}

		// Test 13: Type.typeof for String
		var strType = Type.typeof("hello");
		switch (strType) {
			case TClass(c):
				eq("String", Type.getClassName(c));
			default:
				t(false);
		}

		// Test 14: Type.typeof for class instance
		var personType = Type.typeof(person);
		switch (personType) {
			case TClass(c):
				eq("Person", Type.getClassName(c));
			default:
				t(false);
		}

		// Test 15: Type.typeof for enum
		var colorType = Type.typeof(Color.Red);
		switch (colorType) {
			case TEnum(e):
				eq("Color", Type.getEnumName(e));
			default:
				t(false);
		}

		// Test 16: Type.enumConstructor
		eq("Red", Type.enumConstructor(Color.Red));
		eq("Rgb", Type.enumConstructor(Color.Rgb(255, 128, 0)));

		// Test 17: Type.enumParameters
		var rgbParams = Type.enumParameters(Color.Rgb(255, 128, 0));
		eq(3, rgbParams.length);
		eq(255, rgbParams[0]);
		eq(128, rgbParams[1]);
		eq(0, rgbParams[2]);

		// Test 18: Type.enumIndex
		eq(0, Type.enumIndex(Color.Red));
		eq(1, Type.enumIndex(Color.Green));
		eq(2, Type.enumIndex(Color.Blue));
		eq(3, Type.enumIndex(Color.Rgb(1, 2, 3)));

		// Test 19: Type.getEnumConstructs
		var colorConstructs = Type.getEnumConstructs(Color);
		eq(4, colorConstructs.length);
		t(colorConstructs.indexOf("Red") >= 0);
		t(colorConstructs.indexOf("Green") >= 0);
		t(colorConstructs.indexOf("Blue") >= 0);
		t(colorConstructs.indexOf("Rgb") >= 0);

		// Test 20: Type.createEnum
		var createdRed:Color = Type.createEnum(Color, "Red");
		t(createdRed == Color.Red);
		var createdRgb:Color = Type.createEnum(Color, "Rgb", [100, 200, 50]);
		switch (createdRgb) {
			case Rgb(r, g, b):
				eq(100, r);
				eq(200, g);
				eq(50, b);
			default:
				t(false);
		}

		// Test 21: Type.enumEq
		t(Type.enumEq(Color.Red, Color.Red));
		f(Type.enumEq(Color.Red, Color.Blue));
		t(Type.enumEq(Color.Rgb(1, 2, 3), Color.Rgb(1, 2, 3)));
		f(Type.enumEq(Color.Rgb(1, 2, 3), Color.Rgb(1, 2, 4)));
	}

	static function testThreads() {
		// Test basic threading functionality

		// Test 1: Create a thread and wait for it
		var result:Int = 0;
		var thread = sys.thread.Thread.create(function() {
			result = 42;
		});
		// Wait a bit for the thread to complete
		Sys.sleep(0.1);
		eq(42, result);

		// Test 2: Thread with Lock for synchronization
		var lock = new sys.thread.Lock();
		var threadValue:Int = 0;
		sys.thread.Thread.create(function() {
			Sys.sleep(0.05);  // Small delay
			threadValue = 123;
			lock.release();
		});
		lock.wait();  // Wait for the thread to signal
		eq(123, threadValue);

		// Test 3: Mutex for mutual exclusion
		ThreadTestHelper.sharedCounter = 0;
		var iterations = 100;

		// Create two threads that increment a shared counter
		var lock1 = new sys.thread.Lock();
		var lock2 = new sys.thread.Lock();

		sys.thread.Thread.create(function() {
			for (i in 0...iterations) {
				ThreadTestHelper.mutex.acquire();
				ThreadTestHelper.sharedCounter++;
				ThreadTestHelper.mutex.release();
			}
			lock1.release();
		});

		sys.thread.Thread.create(function() {
			for (i in 0...iterations) {
				ThreadTestHelper.mutex.acquire();
				ThreadTestHelper.sharedCounter++;
				ThreadTestHelper.mutex.release();
			}
			lock2.release();
		});

		// Wait for both threads to complete
		lock1.wait();
		lock2.wait();

		// Without mutex, this could be less than 200 due to race conditions
		eq(200, ThreadTestHelper.sharedCounter);

		// Test 4: Thread-local storage (Tls)
		var tls = new sys.thread.Tls<Int>();
		tls.value = 999;
		eq(999, tls.value);

		var tlsLock = new sys.thread.Lock();
		var otherThreadValue:Int = 0;
		sys.thread.Thread.create(function() {
			// TLS should be separate for this thread
			tls.value = 777;
			otherThreadValue = tls.value;
			tlsLock.release();
		});
		tlsLock.wait();

		// Main thread TLS should still be 999
		eq(999, tls.value);
		// Other thread saw its own value
		eq(777, otherThreadValue);

		// Test 5: Semaphore
		var sem = new sys.thread.Semaphore(2);  // Allow 2 concurrent accesses

		// Acquire both permits
		sem.acquire();
		sem.acquire();

		// Try to acquire without blocking (should fail)
		f(sem.tryAcquire());

		// Release one
		sem.release();

		// Now tryAcquire should succeed
		t(sem.tryAcquire());
	}

	static function testGenericMetadata() {
		// Test @:generic metadata - creates specialized C# generic classes

		// Test 1: Box<Int> - specialized for Int
		var intBox = new Box<Int>(42);
		eq(42, intBox.get());
		intBox.set(100);
		eq(100, intBox.get());
		eq(100, intBox.value);

		// Test 2: Box<String> - specialized for String
		var strBox = new Box<String>("hello");
		eq("hello", strBox.get());
		strBox.set("world");
		eq("world", strBox.get());
		eq("world", strBox.value);

		// Test 3: Verify they are truly separate specializations
		// (In C#, Box<int> and Box<string> are different types)
		var intBox2 = new Box<Int>(999);
		eq(999, intBox2.get());
		eq(100, intBox.get()); // intBox should still have its value

		// Test 4: Box<Float>
		var floatBox = new Box<Float>(3.14);
		eq(3.14, floatBox.get());
		floatBox.set(2.71);
		eq(2.71, floatBox.get());

		// Test 5: Box with class type
		var personBox = new Box<Person>(new Person("Alice"));
		eq("Hello, I am Alice", personBox.get().greet());
		personBox.set(new Person("Bob"));
		eq("Hello, I am Bob", personBox.get().greet());
	}

	static function testAtomics() {
		// Test atomic operations

		// Test 1: AtomicInt basic operations
		var atomicInt = new haxe.atomic.AtomicInt(10);
		eq(10, atomicInt.load());

		atomicInt.store(20);
		eq(20, atomicInt.load());

		// Test 2: AtomicInt exchange
		var old = atomicInt.exchange(30);
		eq(20, old);
		eq(30, atomicInt.load());

		// Test 3: AtomicInt compareExchange - success case
		var prev = atomicInt.compareExchange(30, 40);
		eq(30, prev);  // Returns old value
		eq(40, atomicInt.load());  // Should be updated to 40

		// Test 4: AtomicInt compareExchange - failure case
		prev = atomicInt.compareExchange(30, 50);  // Expected is wrong
		eq(40, prev);  // Returns current value (40)
		eq(40, atomicInt.load());  // Should not have changed

		// Test 5: AtomicInt add
		atomicInt.store(100);
		var oldVal = atomicInt.add(5);
		eq(100, oldVal);  // Returns old value before add
		eq(105, atomicInt.load());

		// Test 6: AtomicInt sub
		oldVal = atomicInt.sub(10);
		eq(105, oldVal);  // Returns old value before sub
		eq(95, atomicInt.load());

		// Test 7: AtomicBool basic operations
		var atomicBool = new haxe.atomic.AtomicBool(false);
		eq(false, atomicBool.load());

		atomicBool.store(true);
		eq(true, atomicBool.load());

		// Test 8: AtomicBool exchange
		var oldBool = atomicBool.exchange(false);
		eq(true, oldBool);
		eq(false, atomicBool.load());

		// Test 9: AtomicBool compareExchange
		var prevBool = atomicBool.compareExchange(false, true);
		eq(false, prevBool);
		eq(true, atomicBool.load());

		// Test 10: AtomicObject basic operations
		var person1 = new Person("Alice");
		var person2 = new Person("Bob");
		var atomicPerson = new haxe.atomic.AtomicObject<Person>(person1);
		eq("Alice", atomicPerson.load().name);

		atomicPerson.store(person2);
		eq("Bob", atomicPerson.load().name);

		// Test 11: AtomicObject exchange
		var person3 = new Person("Charlie");
		var oldPerson = atomicPerson.exchange(person3);
		eq("Bob", oldPerson.name);
		eq("Charlie", atomicPerson.load().name);

		// Test 12: AtomicObject compareExchange - success
		var prevPerson = atomicPerson.compareExchange(person3, person1);
		eq("Charlie", prevPerson.name);
		eq("Alice", atomicPerson.load().name);
	}

	static function testNullEquality() {
		// Test Null<T> equality comparisons
		// These should use .hasValue for null checks, not boxing

		// ============================================
		// Test 1: Null<Int> == null  (left side Null<T>)
		// ============================================
		var nullInt:Null<Int> = null;
		t(nullInt == null);   // Should generate: !nullInt.hasValue

		var someInt:Null<Int> = 42;
		f(someInt == null);   // Should generate: !someInt.hasValue

		// ============================================
		// Test 2: null == Null<Int>  (right side Null<T>)
		// ============================================
		t(null == nullInt);   // Should generate: !nullInt.hasValue
		f(null == someInt);   // Should generate: !someInt.hasValue

		// ============================================
		// Test 3: Null<Int> != null  (left side Null<T>)
		// ============================================
		f(nullInt != null);   // Should generate: nullInt.hasValue
		t(someInt != null);   // Should generate: someInt.hasValue

		// ============================================
		// Test 4: null != Null<Int>  (right side Null<T>)
		// ============================================
		f(null != nullInt);   // Should generate: nullInt.hasValue
		t(null != someInt);   // Should generate: someInt.hasValue

		// ============================================
		// Test 5: Null<Int> == Null<Int>  (struct equality)
		// ============================================
		var nullInt2:Null<Int> = null;
		var someInt2:Null<Int> = 42;
		var someInt3:Null<Int> = 99;

		t(nullInt == nullInt2);   // Both null -> equal
		t(someInt == someInt2);   // Both 42 -> equal
		f(someInt == someInt3);   // 42 != 99
		f(nullInt == someInt);    // null != 42
		f(someInt == nullInt);    // 42 != null

		// ============================================
		// Test 6: Null<Int> != Null<Int>  (struct inequality)
		// ============================================
		f(nullInt != nullInt2);   // Both null -> equal, so != is false
		f(someInt != someInt2);   // Both 42 -> equal, so != is false
		t(someInt != someInt3);   // 42 != 99
		t(nullInt != someInt);    // null != 42
		t(someInt != nullInt);    // 42 != null

		// ============================================
		// Test 7: Null<String> (reference type) == null
		// ============================================
		var nullStr:Null<String> = null;
		var someStr:Null<String> = "hello";

		t(nullStr == null);
		f(someStr == null);
		t(null == nullStr);
		f(null == someStr);

		// ============================================
		// Test 8: Null<String> != null
		// ============================================
		f(nullStr != null);
		t(someStr != null);
		f(null != nullStr);
		t(null != someStr);

		// ============================================
		// Test 9: Null<T> from method return
		// ============================================
		t(NullEqualityHelper.getNullInt() == null);
		f(NullEqualityHelper.getSomeInt(5) == null);
		t(NullEqualityHelper.getNullString() == null);
		f(NullEqualityHelper.getSomeString("x") == null);

		// ============================================
		// Test 10: Null<T> passed to eq() method (boxing scenario)
		// This tests the .toDynamic() coercion we implemented
		// ============================================
		eq(null, nullInt);       // Should work: nullInt.toDynamic() returns null
		eq(42, someInt);         // Should work: someInt.toDynamic() returns boxed 42
		eq(null, nullStr);       // Should work: nullStr.toDynamic() returns null
		eq("hello", someStr);    // Should work: someStr.toDynamic() returns "hello"

		// ============================================
		// Test 11: Null<Bool> equality
		// ============================================
		var nullBool:Null<Bool> = null;
		var trueBool:Null<Bool> = true;
		var falseBool:Null<Bool> = false;

		t(nullBool == null);
		f(trueBool == null);
		f(falseBool == null);

		t(trueBool != falseBool);  // true != false
		f(trueBool == falseBool);  // true == false is false

		// ============================================
		// Test 12: Null<Float> equality
		// ============================================
		var nullFloat:Null<Float> = null;
		var someFloat:Null<Float> = 3.14;

		t(nullFloat == null);
		f(someFloat == null);

		eq(null, nullFloat);
		eq(3.14, someFloat);
	}

	static function testIifeOptimization() {
		// Test IIFE (Immediately Invoked Function Expression) optimization
		// When converting Null<Int> -> Null<Float>, side effects should only happen once

		// Test 1: Variable declaration - Null<Int> -> Null<Float>
		iifeCounter = 0;
		var a:Null<Float> = getNullIntWithSideEffect();
		eq(1, iifeCounter);
		eq(42.0, a);

		// Test 2: Assignment - Null<Int> -> Null<Float>
		iifeCounter = 0;
		var b:Null<Float> = 0;
		b = getNullIntWithSideEffect();
		eq(1, iifeCounter);
		eq(42.0, b);

		// Test 3: Function argument - Null<Int> -> Null<Float>
		iifeCounter = 0;
		var r1 = acceptNullFloat(getNullIntWithSideEffect());
		eq(1, iifeCounter);
		eq(42.0, r1);

		// Test 4: Multiple function arguments - each evaluated once
		iifeCounter = 0;
		var r2 = acceptTwoNullFloats(getNullIntWithSideEffect(), getNullIntWithSideEffect());
		eq(2, iifeCounter);
		eq(84.0, r2);

		// Test 5: Nested call - inner result passed to outer
		iifeCounter = 0;
		var r3 = acceptNullFloat(getNullIntWithSideEffect());
		var r4:Null<Float> = r3;
		eq(1, iifeCounter);

		// Test 6: Return statement - Null<Int> -> Null<Float> in return
		iifeCounter = 0;
		var r5 = returnNullFloatFromInt();
		eq(1, iifeCounter);
		eq(42.0, r5);
	}

	static function testSerialization() {
		// Test 1: Simple ASCII string
		var s1 = new haxe.Serializer();
		s1.serialize("test");
		eq("y4:test", s1.toString());

		// Test 2: Unicode string
		var s2 = new haxe.Serializer();
		s2.serialize("éあ");
		eq("y15:%C3%A9%E3%81%82", s2.toString());

		// Test 3: Static Serializer.run
		eq("y4:test", haxe.Serializer.run("test"));

		// Test 4: Static Serializer.run with Unicode
		eq("y15:%C3%A9%E3%81%82", haxe.Serializer.run("éあ"));

		// Test 5: Serialize integer
		var s5 = new haxe.Serializer();
		s5.serialize(42);
		eq("i42", s5.toString());

		// Test 6: Serialize null
		var s6 = new haxe.Serializer();
		s6.serialize(null);
		eq("n", s6.toString());

		// Test 7: Serialize bool
		eq("t", haxe.Serializer.run(true));
		eq("f", haxe.Serializer.run(false));
	}

	static function testBreakInSwitchInLoop() {
		// Test break in switch inside loop (generates goto label in C#)
		// Assert before loop so the label is the last thing in the block
		t(true);
		while (true) {
			switch (Std.random(10)) {
				case 0:
					break;
				case 1:
					t(true);
				case _:
			}
			break;
		}
	}

	static function testVoidTypeParam() {
		// Test that generic functions with Void type param generate valid C#
		runFunc(() -> {});
		t(true);
	}

	static function runFunc<T>(f:() -> T):T {
		return f();
	}
}

// Helper class for method reference tests
class Calculator {
	public function new() {}

	public function add(a:Int, b:Int):Int {
		return a + b;
	}

	public function addWithOptional(a:Int, ?b:Int):Int {
		var bVal = b == null ? 0 : b;
		return a + bVal;
	}

	public function addWithMiddleOptional(a:Int, ?b:Int, c:Int):Int {
		var bVal = b == null ? 0 : b;
		return a + bVal + c;
	}

	// Methods with multiple optional parameters
	public function addWithTwoOptionals(?a:Int, ?b:Int):Int {
		var aVal = a == null ? 0 : a;
		var bVal = b == null ? 0 : b;
		return aVal + bVal;
	}

	public function addWithThreeOptionals(?a:Int, ?b:Int, ?c:Int):Int {
		var aVal = a == null ? 0 : a;
		var bVal = b == null ? 0 : b;
		var cVal = c == null ? 0 : c;
		return aVal + bVal + cVal;
	}

	// Method with optional String (reference type)
	public function concatWithOptional(a:String, ?b:String):String {
		var bVal = b == null ? "" : b;
		return a + bVal;
	}

	// Method with optional class type
	public function greetOptionalPerson(?person:Person):String {
		if (person == null) return "Hello, stranger!";
		var p:Person = person;  // Unwrap from Null<Person>
		return "Hello, " + p.name + "!";
	}

	// Method with optional class type - direct field access (should work without any cast/unwrap)
	public function greetOptionalPersonAlt(?person:Person):String {
		if (person == null) return "Hello, stranger!";
		// Access name directly on person without explicit unwrap - THIS SHOULD WORK
		return "Hello, " + person.name + "!";
	}

	// Method with optional class type - assign to Dynamic and access name dynamically
	public function greetOptionalPersonAlt2(?person:Person):String {
		if (person == null) return "Hello, stranger!";
		// Assign an optional person to Dynamic and use dynamic field access
		// (this makes sure unwrapping is working correctly when assigning to Dynamic)
		var p:Dynamic = person;
		return "Hello, " + p.name + "!";
	}

	// Static methods for static method reference tests
	public static function staticAdd(a:Int, b:Int):Int {
		return a + b;
	}

	public static function staticAddOptional(a:Int, ?b:Int):Int {
		var bVal = b == null ? 0 : b;
		return a + bVal;
	}

	// Method returning a function
	public function getAdder(base:Int):(Int) -> Int {
		return function(x:Int):Int {
			return base + x;
		};
	}

	// Method taking a function as parameter
	public function applyTwice(fn:(Int) -> Int, value:Int):Int {
		return fn(fn(value));
	}
}

// Helper class for testing method references with generics
class GenericCalculator<T> {
	public var defaultValue:T;

	public function new(defaultValue:T) {
		this.defaultValue = defaultValue;
	}

	public function getOrDefault(?value:T):T {
		return value == null ? defaultValue : value;
	}
}

// Thread test helper class
class ThreadTestHelper {
	public static var sharedCounter:Int = 0;
	public static var mutex:sys.thread.Mutex = new sys.thread.Mutex();
}

// Helper class for Null<T> equality tests
class NullEqualityHelper {
	// Returns Null<Int> with no value (hasValue=false)
	public static function getNullInt():Null<Int> {
		return null;
	}

	// Returns Null<Int> with value (hasValue=true)
	public static function getSomeInt(v:Int):Null<Int> {
		return v;
	}

	// Returns Null<String> with no value
	public static function getNullString():Null<String> {
		return null;
	}

	// Returns Null<String> with value
	public static function getSomeString(v:String):Null<String> {
		return v;
	}
}

// Generic class for testing @:generic metadata
// This creates specialized C# generic classes (Box<int>, Box<string>) instead of erased versions
@:generic
class Box<T> {
	public var value:T;

	public function new(value:T) {
		this.value = value;
	}

	public function get():T {
		return value;
	}

	public function set(newValue:T):Void {
		value = newValue;
	}
}

#if hxcoro
class CoroutineTests {
	static function eq<T>(expected:T, actual:T, ?p:haxe.PosInfos) {
		Main.numTests++;
		if (expected != actual) {
			Main.numFailures++;
			var line = p != null ? p.lineNumber : 0;
			untyped __cs__("System.Console.WriteLine({0})", 'FAIL at line $line: expected $expected, got $actual');
		}
	}

	public static function run() {
		// Test async coroutine with timer delay — validates suspension,
		// resumption, and the event loop on C# target.
		var result = hxcoro.CoroRun.run(delayTest);
		eq(true, result);
	}

	@:coroutine static function delayTest():Bool {
		var start = haxe.Timer.stamp();
		hxcoro.Coro.delay(50); // suspend for 50ms
		var elapsed = haxe.Timer.stamp() - start;
		return elapsed >= 0.04; // generous margin to avoid CI flakiness
	}
}
#end
