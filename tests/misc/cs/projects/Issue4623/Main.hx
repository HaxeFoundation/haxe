@:nativeGen
class Main {
	static var voidResult:String;

	static function testVoid(a:Int, b:String = 'hello', ?c:String):Void {
		voidResult = '$a $b $c';
	}

	static function test(a:Int, b:String = 'hello', ?c:String):String {
		return '$a $b $c';
	}

	public static function main() {
		untyped __cs__('global::Main.testVoid(1, "foo")');
		var expected = '1 foo null';
		if(voidResult != expected) {
			throw 'Invalid result of testVoid(1, "foo"). Expected: $expected. Got: $voidResult';
		}

		untyped __cs__('global::Main.testVoid(2)');
		var expected = '2 hello null';
		if(voidResult != expected) {
			throw 'Invalid result of testVoid(2). Expected: $expected. Got: $voidResult';
		}

		var expected = '3 bar null';
		var result = untyped __cs__('global::Main.test(3, "bar")');
		if(expected != result) {
			throw 'Invalid result of test(3, "bar"). Expected: $expected. Got: $result';
		}

		var expected = '4 hello null';
		var result = untyped __cs__('global::Main.test(4)');
		if(expected != result) {
			throw 'Invalid result of test(4). Expected: $expected. Got: $result';
		}

		var n:CtorTest = untyped __cs__('new global::CtorTest(20);') ;
		if(n.a != 20 || n.b != 'hello') {
			throw 'Invalid result of new CtorTest(20)';
		}

		var n:CtorTest = untyped __cs__('new global::CtorTest();') ;
		if(n.a != 10 || n.b != 'hello') {
			throw 'Invalid result of new CtorTest()';
		}
	}
}

@:nativeGen
class CtorTest {
	public var a:Int;
	public var b:String;

	public function new(a:Int = 10, b:String = 'hello') {
		this.a = a;
		this.b = b;
	}
}