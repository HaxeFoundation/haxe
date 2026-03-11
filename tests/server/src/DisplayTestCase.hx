import haxe.display.Position;
import haxe.Exception;
import utils.Markers;
import haxe.display.FsPath;
import haxe.display.Display;
import haxe.display.JsonModuleTypes;
import utest.Assert;

typedef DisplaySignatureHelp = {
	var signatures:Array<{label:String, ?documentation:String, parameters:Array<{label:String}>}>;
	var activeSignature:Int;
	var activeParameter:Int;
}

/**
	Display test should have a snippet with `{-N-}` markers
	in the doc block.
	The snippet will be automatically parsed.
**/
class DisplayTestCase extends TestCase {
	/** The snippet with markers removed */
	var source(get,never):String;
	/** A file created for the snippet */
	final file = new FsPath("Main.hx");
	/** Data extracted from the snippet */
	var markers(get, never):Markers;
	@:noCompletion var _markers:Null<Markers>;

	var printer(get, never):DisplayPrinter;
	@:noCompletion var _printer:Null<DisplayPrinter>;

	inline function get_printer():DisplayPrinter {
		if (_printer == null)
			_printer = new DisplayPrinter();
		return _printer;
	}

	inline function get_markers():Markers
		return switch _markers {
			case null: throw new Exception('Markers are not initialized');
			case m: m;
		}

	inline function get_source():String
		return markers.source;

	/**
	 * Returns an offset of the n-th marker.
	 * Amount of characters from the beginning of the parsed document excluding markers.
	 */
	public function offset(n:Int):Int
		return markers.offset(n);

	/**
	 * Returns a position of n-th marker.
	 * Line number and character number from the beginning of the line.
	 */
	public function pos(n:Int):Position
		return markers.pos(n);

	/**
	 * Returns a range between positions of `startMarker` and `endMarker`
	 */
	public function range(startMarker:Int, endMarker:Int):Range
		return markers.range(startMarker, endMarker);

	// Helper coroutine methods for common display requests

	@:coroutine
	function fields(n:Int):Array<DisplayItem<Dynamic>> {
		runHaxeJson([], DisplayMethods.Completion, {file: file, offset: offset(n), wasAutoTriggered: false});
		return parseCompletion().result.items;
	}

	@:coroutine
	function toplevel(n:Int):Array<DisplayItem<Dynamic>> {
		return fields(n);
	}

	@:coroutine
	function type(n:Int):String {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(n)});
		return printer.printType(parseHover().result.item.type);
	}

	@:coroutine
	function position(n:Int):Range {
		runHaxeJson([], DisplayMethods.GotoDefinition, {file: file, offset: offset(n)});
		return parseGotoDefintion().result[0].range;
	}

	@:coroutine
	function usage(n:Int):Array<Range> {
		runHaxeJson([], DisplayMethods.FindReferences, {file: file, offset: offset(n)});
		return parseGotoDefintion().result.map(l -> l.range);
	}

	@:coroutine
	function signature(n:Int):DisplaySignatureHelp {
		runHaxeJson([], DisplayMethods.SignatureHelp, {file: file, offset: offset(n), wasAutoTriggered: false});
		var res = parseSignatureHelp().result;
		return {
			signatures: res.signatures.map(s -> ({
				label: printer.printCallArguments(s, printer.printSignatureFunctionArgument) + ':' + printer.printType(s.ret),
				documentation: s.documentation,
				parameters: s.args.map(a -> ({
					label: printer.printSignatureFunctionArgument(a)
				} : {label:String}))
			} : {label:String, ?documentation:String, parameters:Array<{label:String}>})),
			activeSignature: res.activeSignature,
			activeParameter: res.activeParameter
		};
	}

	@:coroutine
	function doc(n:Int):String {
		runHaxeJson([], DisplayMethods.Hover, {file: file, offset: offset(n)});
		var result = parseHover().result;
		return switch result.item.kind {
			case ClassField | EnumAbstractField: StringTools.trim(result.item.args.field.doc);
			case EnumField: StringTools.trim(result.item.args.field.doc);
			case Type: StringTools.trim(result.item.args.doc);
			case Metadata: result.item.args.doc;
			case Define: result.item.args.doc;
			case _: null;
		};
	}

	@:coroutine
	function metadataDoc(n:Int):String {
		return doc(n);
	}

	// Completion assertion helpers

	function hasField<T>(items:Array<DisplayItem<T>>, name:String, ?type:String, ?kind:String):Bool {
		return items.exists(t -> isField(t, name, type, kind));
	}

	function hasToplevel<T>(items:Array<DisplayItem<T>>, kind:String, name:String, ?type:String):Bool {
		return items.exists(t -> isToplevel(t, kind, name, type));
	}

	function hasPath<T>(items:Array<DisplayItem<T>>, name:String):Bool {
		return items.exists(function(t) {
			return switch (t.kind) {
				case ClassField: t.args.field.name == name;
				case Type: t.args.path.typeName == name;
				case Module: t.args.path.moduleName == name;
				case Metadata: t.args.name == name;
				case _: false;
			}
		});
	}

	function isField<T>(t:DisplayItem<T>, name:String, ?type:String, ?kind:String):Bool {
		return switch (t.kind) {
			case ClassField:
				var f = t.args.field;
				if (f.name != name) return false;
				switch [kind, f.kind.kind] {
					case [null, _]:
					case ["static", _]: if (f.scope != Static) return false;
					case ["member", _]: if (f.scope != Member) return false;
					case ["method", FMethod]:
					case ["var", FVar]:
					case _: return false;
				}
				if (type == null) return true;
				return (try f.type.args.path.typeName == type catch(e:Dynamic) false)
					|| type == printer.printType(f.type);
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

	function isToplevel<T>(t:DisplayItem<T>, kind:String, name:String, ?type:String):Bool {
		return isField(t, name, type, kind);
	}

	function sigEq(arg:Int, params:Array<Array<String>>, sig:DisplaySignatureHelp, ?pos:haxe.PosInfos) {
		Assert.equals(arg, sig.activeParameter, pos);
		Assert.equals(params.length, sig.signatures.length, pos);
		for (i in 0...params.length) {
			var sigInf = sig.signatures[i];
			var args = params[i];
			Assert.equals(args.length, sigInf.parameters.length, pos);
			for (i in 0...args.length) {
				Assert.equals(args[i], sigInf.parameters[i].label, pos);
			}
		}
	}

	function eq<T>(expected:T, actual:T, ?pos:haxe.PosInfos) {
		Assert.equals(expected, actual, pos);
	}

	function arrayEq<T>(expected:Array<T>, actual:Array<T>, ?pos:haxe.PosInfos) {
		Assert.same(expected, actual, pos);
	}
}
