import haxe.io.Bytes;
import haxe.io.BytesBuffer;

using StringTools;

import Types;

enum abstract UsageKind(String) to String {
	var Normal = '';
	var Base = '@base';
	var Descendants = '@descendants';
}

class HaxeInvocationException {
	public var message:String;
	public var fieldName:String;
	public var arguments:Array<String>;
	public var source:String;

	public function new(message:String, fieldName:String, arguments:Array<String>, source:String) {
		this.message = message;
		this.fieldName = fieldName;
		this.arguments = arguments;
		this.source = source;
	}

	public function toString() {
		return 'HaxeInvocationException($message, $fieldName, $arguments, $source])';
	}
}

class DisplayTestContext {
	static var haxeServer = haxeserver.HaxeServerSync.launch("haxe", []);

	var markers:Map<Int, Int>;
	var fieldName:String;

	public final source:File;

	public function new(path:String, fieldName:String, source:String, markers:Map<Int, Int>) {
		this.fieldName = fieldName;
		this.source = new File(path, source);
		this.markers = markers;
	}

	public function pos(id:Int):Position {
		var r = markers[id];
		if (r == null)
			throw "No such marker: " + id;
		return new Position(r);
	}

	public function range(pos1:Int, pos2:Int) {
		return normalizePath(source.formatRange(pos(pos1), pos(pos2)));
	}

	public function fields(pos:Position):Array<FieldElement> {
		return extractFields(callHaxe('$pos'));
	}

	public function signatures(pos:Position):Array<String> {
		return extractSignatures(callHaxe('$pos'));
	}

	public function toplevel(pos:Position):Array<ToplevelElement> {
		return extractToplevel(callHaxe('$pos@toplevel'));
	}

	public function type(pos:Position):String {
		return extractType(callHaxe('$pos@type'));
	}

	public function positions(pos:Position):Array<String> {
		return extractPositions(callHaxe('$pos@position'));
	}

	public function position(pos:Position):String {
		return positions(pos)[0];
	}

	public function usage(pos:Position, kind:UsageKind = Normal):Array<String> {
		return extractPositions(callHaxe('$pos@usage$kind'));
	}

	public function documentSymbols():Array<ModuleSymbolEntry> {
		return haxe.Json.parse(callHaxe("0@module-symbols"))[0].symbols;
	}

	public function signature(pos:Position):SignatureHelp {
		return haxe.Json.parse(callHaxe('$pos@signature'));
	}

	public function metadataDoc(pos:Position):String {
		return extractMetadata(callHaxe('$pos@type'));
	}

	public function diagnostics():Array<Diagnostic<Dynamic>> {
		var result = haxe.Json.parse(callHaxe('0@diagnostics'))[0];
		return if (result == null) [] else result.diagnostics;
	}

	public function hasErrorMessage(f:Void->Void, message:String) {
		return try {
			f();
			false;
		} catch (exc:HaxeInvocationException) {
			return exc.message.indexOf(message) != -1;
		}
	}

	function callHaxe(displayPart:String) {
		var args = ["--display", source.path + "@" + displayPart];
		var result = runHaxe(args, source.content);
		if (result.hasError || result.stderr == "") {
			throw new HaxeInvocationException(result.stderr, fieldName, args, source.content);
		}
		return result.stderr;
	}

	static public function runHaxe(args:Array<String>, ?stdin:String) {
		return haxeServer.rawRequest(args, stdin == null ? null : Bytes.ofString(stdin));
	}

	static function extractType(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "type") {
			return null;
		}
		return StringTools.trim(xml.firstChild().nodeValue);
	}

	static function extractSignatures(result:String) {
		var xml = Xml.parse('<x>$result</x>');
		xml = xml.firstElement();
		var ret = [];
		for (xml in xml.elementsNamed("type")) {
			ret.push(StringTools.trim(xml.firstChild().nodeValue));
		}
		return ret;
	}

	static function extractPositions(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "list") {
			return null;
		}
		var ret = [];
		for (xml in xml.elementsNamed("pos")) {
			ret.push(normalizePath(xml.firstChild().nodeValue.trim()));
		}
		return ret;
	}

	static function extractToplevel(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "il") {
			return null;
		}
		var ret = [];
		for (xml in xml.elementsNamed("i")) {
			ret.push({kind: xml.get("k"), type: xml.get("t"), name: xml.firstChild().nodeValue});
		}
		return ret;
	}

	static function extractFields(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "list") {
			return null;
		}
		var ret = [];
		for (xml in xml.elementsNamed("i")) {
			ret.push({name: xml.get("n"), type: xml.firstElement().firstChild().nodeValue, kind: xml.get("k")});
		}
		return ret;
	}

	static function extractMetadata(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "metadata") {
			return null;
		}
		return xml.firstChild().nodeValue;
	}

	static function normalizePath(p:String):String {
		if (!haxe.io.Path.isAbsolute(p)) {
			p = Sys.getCwd() + p;
		}
		if (Sys.systemName() == "Windows") {
			// on windows, haxe returns paths with backslashes, drive letter uppercased
			p = p.substr(0, 1).toUpperCase() + p.substr(1);
			p = p.replace("/", "\\");
		}
		return p;
	}
}
