import Types;

using Lambda;

@:autoBuild(Macro.buildTestCase())
class DisplayTestCase {
	var ctx:DisplayTestContext;
	var methods:Array<Void->Void>;
	var numTests:Int;
	var numFailures:Int;
	var testName:String;

	// api
	inline function pos(name) return ctx.pos(name);
	inline function fields(pos) return ctx.fields(pos);
	inline function toplevel(pos) return ctx.toplevel(pos);
	inline function type(pos) return ctx.type(pos);
	inline function position(pos) return ctx.position(pos);
	inline function usage(pos) return ctx.usage(pos);
	inline function range(pos1, pos2) return ctx.range(pos1, pos2);
	inline function signature(pos1) return ctx.signature(pos1);
	inline function metadataDoc(pos1) return ctx.metadataDoc(pos1);

	inline function noCompletionPoint(f) return ctx.noCompletionPoint(f);

	function assert(v:Bool) if (!v) throw "assertion failed";

	function eq<T>(expected:T, actual:T, ?pos:haxe.PosInfos) {
		numTests++;
		if (expected != actual) {
			numFailures++;
			report("Assertion failed", pos);
			report("Expected: " + expected, pos);
			report("Actual:   " + actual, pos);
		}
	}

	function arrayEq(expected:Array<String>, actual:Array<String>, ?pos:haxe.PosInfos) {
		numTests++;
		var leftover = expected.copy();
		for (actual in actual) {
			if (!leftover.remove(actual)) {
				numFailures++;
				report("Result not part of expected Array:", pos);
				report(actual, pos);
			}
		}
		for (leftover in leftover) {
			numFailures++;
			report("Expected result was not part of actual Array:", pos);
			report(leftover, pos);
			return;
		}
	}

	function arrayCheck<T>(expected:Array<T>, actual:Array<T>, f : T -> String, ?pos:haxe.PosInfos) {
		var expected = [for (expected in expected) f(expected) => expected];
		for (actual in actual) {
			var key = f(actual);
			if (!expected.exists(key)) {
				numFailures++;
				report("Result not part of expected Array:", pos);
				report(Std.string(actual), pos);
			}
			expected.remove(key);
		}

		for (expected in expected) {
			numFailures++;
			report("Expected result was not part of actual Array:", pos);
			report(Std.string(expected), pos);
			return;
		}
	}

	function hasField(a:Array<FieldElement>, name:String, type:String, ?kind:String):Bool {
		return a.exists(function(t) return t.type == type && t.name == name && (kind == null || t.kind == kind));
	}

	function hasToplevel(a:Array<ToplevelElement>, kind:String, name:String, ?type:String = null):Bool {
		return a.exists(function(t) return t.kind == kind && t.name == name && (type == null || t.type == type));
	}

	function hasPath(a:Array<FieldElement>, name:String):Bool {
		return a.exists(function(t) return t.name == name);
	}

	function sigEq(arg:Int, params:Array<Array<String>>, sig:SignatureHelp, ?pos:haxe.PosInfos) {
		eq(arg, sig.activeParameter, pos);
		eq(params.length, sig.signatures.length, pos);
		for (i in 0...params.length) {
			var sigInf = sig.signatures[i];
			var args = params[i];
			eq(sigInf.parameters.length, args.length, pos);
			for (i in 0...args.length) {
				eq(sigInf.parameters[i].label, args[i], pos);
			}
		}
	}

	function report(message, pos:haxe.PosInfos) {
		haxe.Log.trace(message, pos);
	}

	public function run() {
		for (method in methods) {
			method();
		}
		return {
			testName: testName,
			numTests: numTests,
			numFailures: numFailures
		}
	}
}