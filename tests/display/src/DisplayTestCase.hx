import haxe.display.Position.Range;
import utest.Assert;
import Types;

using Lambda;

@:autoBuild(Macro.buildTestCase())
class DisplayTestCase implements utest.ITest {
	var ctx:DisplayTestContext;

	public function new() {}

	// api
	inline function pos(name)
		return ctx.pos(name);

	inline function fields(pos)
		return ctx.fields(pos);

	inline function toplevel(pos)
		return ctx.toplevel(pos);

	inline function type(pos)
		return ctx.type(pos);

	inline function position(pos)
		return ctx.position(pos);

	inline function usage(pos)
		return ctx.usage(pos);

	inline function range(pos1, pos2)
		return ctx.range(pos1, pos2);

	inline function signature(pos1)
		return ctx.signature(pos1);

	inline function metadataDoc(pos1)
		return ctx.metadataDoc(pos1);

	inline function diagnostics()
		return ctx.diagnostics();

	inline function noCompletionPoint(f)
		return ctx.hasErrorMessage(f, "No completion point");

	inline function typeNotFound(f, typeName)
		return ctx.hasErrorMessage(f, "Type not found : " + typeName);

	function assert(v:Bool)
		Assert.isTrue(v);

	function eq<T>(expected:T, actual:T, ?pos:haxe.PosInfos) {
		Assert.equals(expected, actual, pos);
	}

	function arrayEq<T>(expected:Array<T>, actual:Array<T>, ?pos:haxe.PosInfos) {
		Assert.same(expected, actual, pos);
	}

	function arrayCheck<T>(expected:Array<T>, actual:Array<T>, f:T->String, ?pos:haxe.PosInfos) {
		var expected = [for (expected in expected) f(expected) => expected];
		for (actual in actual) {
			var key = f(actual);
			Assert.isTrue(expected.exists(key), "Result not part of expected Array: " + Std.string(actual), pos);
			expected.remove(key);
		}

		for (expected in expected) {
			Assert.fail("Expected result was not part of actual Array: " + Std.string(expected), pos);
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

	function diagnosticsRange(start:Position, end:Position):Range {
		var range = ctx.source.findRange(start, end);
		// this is probably correct...?
		range.start.character--;
		range.end.character--;
		return range;
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
		Assert.fail(message, pos);
	}
}
