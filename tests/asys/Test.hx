import sys.FileSystem;
import utest.Assert;
import utest.Async;
import haxe.io.Bytes;

// copy of Test from Haxe unit test sources
// + beq, noExc, sub
class Test implements utest.ITest {
	static var testCounter = 0;

	public function new() {}

	var asyncDone = 0;
	var asyncExpect = 0;

	var testDir:String;

	function setup() {
		testDir = "resources-rw/" + testCounter++;
		FileSystem.createDirectory(testDir);
		TestBase.uvSetup();
		asyncDone = 0;
		asyncExpect = 0;
	}

	function sub(async:Async, f:(done:() -> Void) -> Void, ?localExpect:Int = 1):Void {
		asyncExpect += localExpect;
		var localDone = 0;
		f(() -> {
			localDone++;
			asyncDone++;
			if (asyncDone > asyncExpect || localDone > localExpect)
				assert("too many done calls");
			if (asyncDone == asyncExpect)
				async.done();
		});
	}

	function teardown() {
		if (asyncDone < asyncExpect)
			assert("not enough done calls");
		TestBase.uvTeardown();
	}

	function eq<T>(v:T, v2:T, ?pos:haxe.PosInfos) {
		Assert.equals(v, v2, pos);
	}

	function neq<T>(v:T, v2:T, ?pos:haxe.PosInfos) {
		Assert.notEquals(v, v2, pos);
	}

	function feq(v:Float, v2:Float, ?pos:haxe.PosInfos) {
		Assert.floatEquals(v, v2, pos);
	}

	function aeq<T>(expected:Array<T>, actual:Array<T>, ?pos:haxe.PosInfos) {
		Assert.same(expected, actual, pos);
	}

	function beq(a:Bytes, b:Bytes, ?pos:haxe.PosInfos) {
		Assert.isTrue(a.compare(b) == 0, pos);
	}

	function t(v, ?pos:haxe.PosInfos) {
		Assert.isTrue(v, pos);
	}

	function f(v, ?pos:haxe.PosInfos) {
		Assert.isFalse(v, pos);
	}

	function assert(?message:String, ?pos:haxe.PosInfos) {
		Assert.fail(message, pos);
	}

	function exc(f:Void->Void, ?pos:haxe.PosInfos) {
		Assert.raises(f, pos);
	}

	function noExc(f:Void->Void, ?pos:haxe.PosInfos) {
		Assert.isTrue(try {
			f();
			true;
		} catch (e:Dynamic) false, pos);
	}

	function unspec(f:Void->Void, ?pos) {
		try {
			f();
		} catch (e:Dynamic) {}
		noAssert();
	}

	function allow<T>(v:T, values:Array<T>, ?pos) {
		Assert.contains(v, values, pos);
	}

	function noAssert(?pos:haxe.PosInfos) {
		t(true, pos);
	}

	function hf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		t(Lambda.has(Type.getInstanceFields(c), n));
	}

	function nhf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		f(Lambda.has(Type.getInstanceFields(c), n));
	}

	function hsf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		t(Lambda.has(Type.getClassFields(c), n));
	}

	function nhsf(c:Class<Dynamic>, n:String, ?pos:haxe.PosInfos) {
		f(Lambda.has(Type.getClassFields(c), n));
	}
}
