import haxe.Json;
import haxe.display.Display;
import haxe.display.Protocol;

import BaseDisplayTestContext.normalizePath;

using StringTools;

import Types;

class XmlDisplayTestContext extends BaseDisplayTestContext {
	public function new(path:String, fieldName:String, source:String, markers:Map<Int, Int>) {
		super(path, fieldName, source, markers);
	}

	public function fields(pos:Position):Array<FieldElement> {
		return extractFields(callHaxe('$pos'));
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

	public function usage(pos:Position):Array<String> {
		return extractPositions(callHaxe('$pos@usage'));
	}

	public function documentSymbols():Array<ModuleSymbolEntry> {
		return Json.parse(callHaxe("0@module-symbols"))[0].symbols;
	}

	public function signature(pos:Position):SignatureHelp {
		return Json.parse(callHaxe('$pos@signature'));
	}

	public function doc(pos:Position):String {
		return extractDoc(callHaxe('$pos@type'));
	}

	public function metadataDoc(pos:Position):String {
		return extractMetadata(callHaxe('$pos@type'));
	}

	public function diagnostics():Array<Diagnostic<Dynamic>> {
		var result = Json.parse(callHaxe('0@diagnostics'))[0];
		return if (result == null) [] else result.diagnostics;
	}

	function callHaxe(displayPart:String) {
		var args = ["--display", source.path + "@" + displayPart];
		var result = BaseDisplayTestContext.runHaxe(args, source.content);
		if (result.hasError || result.stderr == "") {
			throw new HaxeInvocationException(result.stderr, fieldName, args, source.content);
		}
		return result.stderr;
	}

	static function extractType(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "type") {
			return null;
		}
		return StringTools.trim(xml.firstChild().nodeValue);
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

	static function extractDoc(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "type") {
			return null;
		}
		return StringTools.trim(xml.get('d'));
	}

	static function extractMetadata(result:String) {
		var xml = Xml.parse(result);
		xml = xml.firstElement();
		if (xml.nodeName != "metadata") {
			return null;
		}
		return xml.firstChild().nodeValue;
	}
}
