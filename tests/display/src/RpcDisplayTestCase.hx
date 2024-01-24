import haxe.display.Display;
import haxe.display.Position.Range;
import utest.Assert;
import Types;

using Lambda;

@:autoBuild(Macro.buildTestCase())
class RpcDisplayTestCase implements utest.ITest {
	var ctx:RpcDisplayTestContext;

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

	inline function doc(pos1)
		return ctx.doc(pos1);

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

	function hasField<T>(a:Array<DisplayItem<T>>, name:String, type:String, ?kind:String):Bool {
		return a.exists(t -> isField(t, name, type, kind));
	}

	function isField<T>(t:DisplayItem<T>, name:String, ?type:String, ?kind:String):Bool {
		return switch (t.kind) {
			case ClassField:
				var f = t.args.field;
				if (f.name != name) return false;

				// Oh dear...
				switch [kind, f.kind.kind] {
					case [null, _]:
					case ["static", _]: if (f.scope != Static) return false;
					case ["member", _]: if (f.scope != Member) return false;
					case ["method", FMethod]:
					case ["var", FVar]:
					case _: return false;
				}

				if (type == null || f.type.args.path.typeName == type) return true;
				return type == ctx.displayPrinter.printType(f.type);
			case EnumField:
				if (kind != null && kind != "enum") return false;
				t.args.field.name == name;
			case EnumAbstractField:
				if (kind != null && kind != "var") return false;
				t.args.field.name == name;
			case Module:
				if (kind != null && kind != "type") return false;
				t.args.path.moduleName == name;
			case Type:
				if (kind != null && kind != "type") return false;
				t.args.path.typeName == name;
			case TypeParameter:
				if (kind != null && kind != "type") return false;
				t.args.name == name;
			case Package:
				if (kind != null && kind != "package") return false;
				t.args.path.pack[0] == name;
			case Local:
				if (kind != null && kind != "local") return false;
				t.args.name == name;
			case Literal:
				if (kind != null && kind != "literal") return false;
				t.args.name == name;
			case Keyword:
				if (kind != null && kind != "keyword") return false;
				t.args.name == name;
			case Metadata:
				if (kind != null && kind != "metadata") return false;
				t.args.name == name;
			case _:
				false;
		}
	}

	function hasToplevel<T>(a:Array<DisplayItem<T>>, kind:String, name:String, ?type:String = null):Bool {
		return a.exists(t -> isToplevel(t, name, type, kind));
	}

	function isToplevel<T>(t:DisplayItem<T>, name:String, ?type:String = null, ?kind:String = null):Bool {
		return isField(t, name, type, kind);
	}

	function hasPath<T>(a:Array<DisplayItem<T>>, name:String):Bool {
		return a.exists(function(t) {
			return switch (t.kind) {
				case ClassField: t.args.field.name == name;
				case Type: t.args.path.typeName == name;
				case Module: t.args.path.moduleName == name;
				case Metadata: t.args.name == name;
				case _: false;
			}
		});
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
