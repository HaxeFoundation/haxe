import SkipReason;
import haxe.PosInfos;
import haxe.Exception;
import haxe.display.Position;
import haxeserver.HaxeServerRequestResult;
import haxe.display.JsonModuleTypes;
import haxe.display.Display;
import haxe.display.Protocol;
import haxe.display.Diagnostic;
import haxe.Json;
import haxeserver.process.HaxeServerProcessNode;
import haxeserver.HaxeServerAsync;
import utest.Assert;
import utest.ITest;
import utils.Vfs;

using StringTools;
using Lambda;

@:autoBuild(utils.macro.BuildHub.build())
interface ITestCase {}

class TestException extends Exception {
	public final pos:PosInfos;

	public function new(message:String, ?pos:PosInfos) {
		super(message);
		this.pos = pos;
	}
}

typedef JsonRpcError<Data> = {
	code:Int,
	message:String,
	?data:Data
}

typedef JsonRpcResponse<Result, ErrorData> = {
	jsonrpc:String,
	id:Int,
	?result:Result,
	?error:JsonRpcError<ErrorData>
}

class TestCase implements ITest implements ITestCase {
	static public var debugLastResult:{
		hasError:Bool,
		stdout:String,
		stderr:String,
		prints:Array<String>
	};

	static public var server:HaxeServerAsync;
	static public var rootCwd:String;

	var vfs:Vfs;
	var testDir:String;
	var lastResult:HaxeServerRequestResult;
	var messages:Array<String> = [];
	var errorMessages = [];

	static var i:Int = 0;

	public function new() {}

	function debugMessages(?pos:PosInfos) {
		for (m in messages)
			haxe.Log.trace(m, pos);
	}

	function debugErrorMessages(?pos:PosInfos) {
		for (m in errorMessages)
			haxe.Log.trace(m, pos);
	}

	function messagesWith(s:String, ?pos:PosInfos) {
		for (m in messages)
			if (m.contains(s))
				haxe.Log.trace(m, pos);
	}

	function errorMessagesWith(s:String, ?pos:PosInfos) {
		for (m in errorMessages)
			if (m.contains(s))
				haxe.Log.trace(m, pos);
	}

	static public function printSkipReason(ddr:SkipReason) {
		return switch (ddr) {
			case DependencyDirty(path): 'DependencyDirty $path';
			case Tainted(cause): 'Tainted $cause';
			case FileChanged(file): 'FileChanged $file';
			case Shadowed(file): 'Shadowed $file';
			case LibraryChanged: 'LibraryChanged';
		}
	}

	@:timeout(3000)
	public function setup(async:utest.Async) {
		testDir = "test/cases/" + i++;
		vfs = new Vfs(testDir);

		hxcoro.CoroRun.promise(() -> {
			runHaxeJson(["--cwd", rootCwd, "--cwd", testDir], Methods.ResetState, {});

			if (!async.timedOut)
				async.done();
		});
	}

	public function teardown() {}

	function handleResult(result:HaxeServerRequestResult) {
		lastResult = result;
		debugLastResult = {
			hasError: lastResult.hasError,
			prints: lastResult.prints,
			stderr: lastResult.stderr,
			stdout: lastResult.stdout
		};
		sendLogMessage(result.stdout);
		for (print in result.prints) {
			var line = print.trim();
			messages.push('Haxe print: $line');
		}
	}

	@:coroutine
	function runHaxe(args:Array<String>) {
		messages = [];
		errorMessages = [];

		hxcoro.Coro.suspend(cont -> {
			server.rawRequest(args, null, function(result) {
				handleResult(result);
				if (result.hasError) {
					sendErrorMessage(result.stderr);
				}
				cont.resume(null, null);
			}, err -> {
				sendErrorMessage(err);
				cont.resume(null, null);
			});
		});
	}

	@:coroutine
	function runHaxeJson<TParams, TResponse>(args:Array<String>, method:HaxeRequestMethod<TParams, Response<TResponse>>, methodArgs:TParams,
			?pos:PosInfos):TResponse {
		var methodArgs = {method: method, id: 1, params: methodArgs};
		args = args.concat(['--display', Json.stringify(methodArgs)]);
		messages = [];
		errorMessages = [];

		return hxcoro.Coro.suspend(cont -> {
			server.rawRequest(args, null, function(result) {
				handleResult(result);
				if (result.hasError) {
					sendErrorMessage(result.stderr);
				}
				var json:JsonRpcResponse<Response<TResponse>, Array<Any>> = try {
					Json.parse(result.stderr);
				} catch (e) {
					cont.resume(null, new TestException("Response: " + result.stderr, pos));
					return;
				}

				if (json.result != null) {
					cont.resume(json.result?.result, null);
				} else {
					// TODO: This needs some serious cleanup in the compiler so all methods return
					// properly typed data.
					final obj = json.error.data[0];
					final message:String = if (obj is String) {
						(obj : String);
					} else {
						(obj : HaxeResponseErrorData).message;
					}
					cont.resume(null, new TestException(message, pos));
				}
			}, function(msg) {
				cont.resume(null, new TestException(msg, pos));
			});
		});
	}

	function sendErrorMessage(msg:String) {
		var split = msg.split("\n");
		for (message in split) {
			errorMessages.push(message.trim());
		}
	}

	function sendLogMessage(msg:String) {
		var split = msg.split("\n");
		for (message in split) {
			messages.push(message.trim());
		}
	}

	function getTemplate(templateName:String) {
		return sys.io.File.getContent("test/templates/" + templateName);
	}

	function getTemplateWithMarkers(templateName:String) {
		var content = sys.io.File.getContent("test/templates/" + templateName);
		return Marker.extractMarkers(content);
	}

	function storeAndParseTemplate(templatePath:String, asPath:String) {
		var tpl = getTemplateWithMarkers(templatePath);
		vfs.putContent(asPath, tpl.source);
		return tpl;
	}

	function hasMessage<T>(msg:String) {
		for (message in messages) {
			if (message.endsWith(msg)) {
				return true;
			}
		}
		return false;
	}

	function hasErrorMessage<T>(msg:String) {
		for (message in errorMessages) {
			if (message.endsWith(msg)) {
				return true;
			}
		}
		return false;
	}

	function getStoredType<T>(typePackage:String, typeName:String) {
		var storedTypes:Array<JsonModuleType<T>> = try {
			Json.parse(lastResult.stderr).result.result;
		} catch (e:Dynamic) {
			trace(e);
			[];
		}
		for (type in storedTypes) {
			if (type.pack.join(".") == typePackage && type.name == typeName) {
				return type;
			}
		}
		return null;
	}

	function parseCompletion():CompletionResult {
		return Json.parse(lastResult.stderr).result;
	}

	function parseHover():HoverResult {
		return Json.parse(lastResult.stderr).result;
	}

	function parseSignatureHelp():SignatureHelpResult {
		return Json.parse(lastResult.stderr).result;
	}

	function parseGotoTypeDefinition():GotoTypeDefinitionResult {
		return Json.parse(lastResult.stderr).result;
	}

	function parseGotoDefintion():GotoDefinitionResult {
		return haxe.Json.parse(lastResult.stderr).result;
	}

	function parseDiagnostics():Array<Diagnostic<Any>> {
		var json:Dynamic = haxe.Json.parse(lastResult.stderr);
		// JSON-RPC format: {id, result: {result: [{file, diagnostics}]}}
		var rpcResult:Dynamic = json.result;
		if (rpcResult != null) {
			var files:Dynamic = rpcResult.result;
			if (files != null && files.length > 0)
				return files[0].diagnostics;
			return [];
		}
		// Legacy format: [{file, diagnostics}]
		var result = json[0];
		return if (result == null) [] else result.diagnostics;
	}

	function parseGotoDefinitionLocations():Array<Location> {
		switch parseGotoTypeDefinition().result {
			case null:
				throw new Exception('No result for GotoDefinition found');
			case result:
				return result;
		}
	}

	function assertSilence() {
		return Assert.isTrue(lastResult.stderr == "");
	}

	function assertSuccess(?p:haxe.PosInfos) {
		return Assert.isTrue(0 == errorMessages.length, p);
	}

	function assertErrorMessage(message:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(hasErrorMessage(message), p);
	}

	function assertHasPrint(line:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(hasMessage("Haxe print: " + line), null, p);
	}

	function assertReuse(module:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(hasMessage('reusing $module'), null, p);
	}

	function assertSkipping(module:String, reason:SkipReason, ?p:haxe.PosInfos) {
		var msg = 'skipping $module (${printSkipReason(reason)})';
		return Assert.isTrue(hasMessage(msg), null, p);
	}

	function assertNotCacheModified(module:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(hasMessage('$module not cached (modified)'), null, p);
	}

	function assertHasType(typePackage:String, typeName:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(getStoredType(typePackage, typeName) != null, null, p);
	}

	function assertHasField(typePackage:String, typeName:String, fieldName:String, isStatic:Bool, ?p:haxe.PosInfos) {
		var type = getStoredType(typePackage, typeName);
		Assert.isTrue(type != null, p);
		function check<T>(type:JsonModuleType<T>) {
			return switch [type.kind, type.args] {
				case [Class, c]:
					(isStatic ? c.statics : c.fields).exists(cf -> cf.name == fieldName);
				case _: false;
			}
		}
		if (type != null) {
			Assert.isTrue(check(type), null, p);
		}
	}

	function assertClassField(completion:CompletionResult, name:String, ?callback:(field:JsonClassField) -> Void, ?pos:PosInfos) {
		for (item in completion.result.items) {
			switch item.kind {
				case ClassField if (item.args.field.name == name):
					switch callback {
						case null: Assert.pass(pos);
						case fn: fn(item.args.field);
					}
					return;
				case _:
			}
		}
		Assert.fail(pos);
	}

	function assertHasCompletion<T>(completion:CompletionResult, f:DisplayItem<T>->Bool, ?p:haxe.PosInfos) {
		for (type in completion.result.items) {
			if (f(type)) {
				Assert.pass();
				return;
			}
		}
		Assert.fail("No such completion", p);
	}

	function assertHasNoCompletion<T>(completion:CompletionResult, f:DisplayItem<T>->Bool, ?p:haxe.PosInfos) {
		for (type in completion.result.items) {
			if (f(type)) {
				Assert.fail("Unexpected completion", p);
				return;
			}
		}
		Assert.pass();
	}

	function strType(t:JsonType<JsonTypePathWithParams>):String {
		var path = t.args.path;
		var params = t.args.params;
		var parts = path.pack.concat([path.typeName]);
		var s = parts.join('.');
		if (params.length == 0) {
			return s;
		}
		var sParams = params.map(strType).join('.');
		return '$s<$sParams>';
	}
}
