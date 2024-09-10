import haxe.Json;
import haxe.display.Display;
import haxe.display.FsPath;
import haxe.display.Position;
import haxe.display.Protocol;

using StringTools;

import Types;

class RpcDisplayTestContext extends BaseDisplayTestContext {
	public var displayPrinter(default,null):DisplayPrinter;

	public function new(path:String, fieldName:String, source:String, markers:Map<Int, Int>) {
		super(path, fieldName, source, markers);
		displayPrinter = new DisplayPrinter();
	}

	public function fields(pos:Position):Array<DisplayItem<Dynamic>> {
		return extractFields(callDisplay(DisplayMethods.Completion, {
			file: new FsPath(source.path),
			offset: pos,
			wasAutoTriggered: false
		}));
	}

	public function toplevel(pos:Position):Array<DisplayItem<Dynamic>> {
		return fields(pos);
	}

	public function type(pos:Position):String {
		return extractType(callDisplay(DisplayMethods.Hover, {
			file: new FsPath(source.path),
			offset: pos,
		}).result);
	}

	public function positions(pos:Position):Array<String> {
		return extractPositions(callDisplay(DisplayMethods.GotoDefinition, {
			file: new FsPath(source.path),
			offset: pos,
		}).result);
	}

	public function position(pos:Position):String {
		return positions(pos)[0];
	}

	public function usage(pos:Position):Array<String> {
		return extractPositions(callDisplay(DisplayMethods.FindReferences, {
			file: new FsPath(source.path),
			offset: pos,
		}).result);
	}

	// TODO: migrate module-symbols to json rpc and update here
	public function documentSymbols():Array<ModuleSymbolEntry> {
		return Json.parse(callHaxe("0@module-symbols"))[0].symbols;
	}

	public function signature(pos:Position):SignatureHelp {
		var res = callDisplay(DisplayMethods.SignatureHelp, {
			file: new FsPath(source.path),
			offset: pos,
			wasAutoTriggered: false
		}).result;

		return {
			signatures: res.signatures.map(s -> {
				label: displayPrinter.printCallArguments(s, displayPrinter.printSignatureFunctionArgument) + ':' + displayPrinter.printType(s.ret),
				documentation: s.documentation,
				parameters: s.args.map(a -> {
					label: displayPrinter.printSignatureFunctionArgument(a),
					documentation: null
				})
			}),
			activeParameter: res.activeParameter,
			activeSignature: res.activeSignature
		};
	}

	public function doc(pos:Position):String {
		return extractDoc(callDisplay(DisplayMethods.Hover, {
			file: new FsPath(source.path),
			offset: pos,
		}).result);
	}

	public function metadataDoc(pos:Position):String {
		return extractMetadata(callDisplay(DisplayMethods.Hover, {
			file: new FsPath(source.path),
			offset: pos,
		}).result);
	}

	public function diagnostics():Array<Diagnostic<Any>> {
		var result = callDisplay(DisplayMethods.Diagnostics, {
			file: new FsPath(source.path),
		}).result;
		return if (result == null || result.length == 0) [] else cast result[0].diagnostics;
	}

	// Can be removed once module-symbols is migrated to json rpc
	function callHaxe(displayPart:String) {
		var args = ["--display", source.path + "@" + displayPart];
		var result = BaseDisplayTestContext.runHaxe(args, source.content);
		if (result.hasError || result.stderr == "") {
			throw new HaxeInvocationException(result.stderr, fieldName, args, source.content);
		}
		return result.stderr;
	}

	function callDisplay<TParams, TResponse>(method:HaxeRequestMethod<TParams, TResponse>, methodArgs:TParams):TResponse {
		var methodArgs = {method: method, id: 1, params: methodArgs};
		var args = ['--display', Json.stringify(methodArgs), '-D', 'disable-hxb-cache'];

		var result = BaseDisplayTestContext.runHaxe(args, source.content);
		if (result.hasError || result.stderr == "") {
			throw new HaxeInvocationException(result.stderr, fieldName, args, source.content);
		}
		var json = Json.parse(result.stderr);
		if (json.result != null) {
			return json.result;
		} else {
			throw new HaxeInvocationException(json.error.data.join("\n"), fieldName, args, source.content);
		}
	}

	function extractType(result:HoverDisplayItemOccurence<Dynamic>) {
		return displayPrinter.printType(result.item.type);
	}

	static function extractPositions(result:Array<Location>) {
		return result.map(r -> formatRange(r.file, r.range));
	}

	static function extractFields(result:CompletionResult) {
		return result.result.items;
	}

	static function formatRange(path:FsPath, range:Range):String {
		// Offset difference with legacy display
		var offset = 1;

		var start = range.start;
		var end = range.end;
		var pos = if (start.line == end.line) {
			if (start.character == end.character)
				'character ${start.character + offset}';
			else
				'characters ${start.character + offset}-${end.character + offset}';
		} else {
			'lines ${start.line + 1}-${end.line + 1}';
		}
		return '$path:${start.line + 1}: $pos';
	}

	function extractDoc(result:HoverDisplayItemOccurence<Dynamic>) {
		return StringTools.trim(result.item.args.field.doc);
	}

	function extractMetadata(result:HoverDisplayItemOccurence<Dynamic>) {
		return result.item.args.doc;
	}
}
