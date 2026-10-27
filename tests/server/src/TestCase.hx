import SkipReason;
import haxe.PosInfos;
import haxe.Exception;
import haxe.display.Position;
import haxeserver.HaxeServerRequestResult;
import haxe.display.JsonModuleTypes;
import haxe.display.Display;
import haxe.display.Server;
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

typedef JsonRpcNotification = {
	jsonrpc:String,
	method:String,
	?params:Dynamic
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
	var notifications:Array<JsonRpcNotification> = [];

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
			runHaxeJson([], Methods.ResetState, {});
			runHaxeJson([], ServerMethods.SetCwd, {dir: testDir});

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
		notifications = [];

		return hxcoro.Coro.suspend(cont -> {
			server.rawRequest(args, null, function(result) {
				// TODO: would be nicer to not have that here either, but it makes 3 tests fail.
				sendLogMessage(result.stdout);
				collectNotifications(result.prints);
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

	function collectNotifications(prints:Array<String>) {
		for (print in prints) {
			var trimmed = print.trim();
			if (trimmed.length == 0) continue;
			// Prints contain mixed content (trace output, compiler messages, etc.).
			// We only extract entries that parse as JSON-RPC 2.0 notifications.
			var parsed:Dynamic = try Json.parse(trimmed) catch (_) null;
			if (parsed != null && Reflect.field(parsed, "jsonrpc") == "2.0" && Reflect.field(parsed, "method") != null) {
				notifications.push({
					jsonrpc: parsed.jsonrpc,
					method: parsed.method,
					params: parsed.params
				});
			}
		}
	}

	function hasNotification(method:String) {
		for (n in notifications) {
			if (n.method == method) return true;
		}
		return false;
	}

	function assertHasNotification(method:String, ?p:haxe.PosInfos) {
		return Assert.isTrue(hasNotification(method), null, p);
	}

	function assertNoNotification(method:String, ?p:haxe.PosInfos) {
		return Assert.isFalse(hasNotification(method), null, p);
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

	function getStoredType<T>(storedTypes:Array<JsonModuleType<T>>, typePackage:String, typeName:String) {
		if (storedTypes == null) return null;
		for (type in storedTypes) {
			if (type.pack.join(".") == typePackage && type.name == typeName) {
				return type;
			}
		}
		return null;
	}

	function assertSilence() {
		return Assert.isTrue(lastResult.stderr == "");
	}

	function assertSuccess(?p:haxe.PosInfos) {
		var res = 0 == errorMessages.length;
		if (!res) debugErrorMessages();
		return Assert.isTrue(res, p);
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

	function assertClassField(completion:CompletionResponse<Dynamic, Dynamic>, name:String, ?callback:(field:JsonClassField) -> Void, ?pos:PosInfos) {
		for (item in completion.items) {
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

	function assertHasCompletion<T>(completion:CompletionResponse<Dynamic, Dynamic>, f:DisplayItem<T>->Bool, ?p:haxe.PosInfos) {
		for (type in completion.items) {
			if (f(type)) {
				Assert.pass();
				return;
			}
		}
		Assert.fail("No such completion", p);
	}

	function assertHasNoCompletion<T>(completion:CompletionResponse<Dynamic, Dynamic>, f:DisplayItem<T>->Bool, ?p:haxe.PosInfos) {
		for (type in completion.items) {
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
