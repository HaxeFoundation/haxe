import haxeserver.HaxeServerRequestResult;
import haxe.display.JsonModuleTypes;
import haxe.display.Display;
import haxe.display.Protocol;
import haxe.Json;
import haxeserver.process.HaxeServerProcessNode;
import haxeserver.HaxeServerAsync;
import utest.Assert;
import utest.ITest;

using StringTools;
using Lambda;

@:autoBuild(AsyncMacro.build())
class HaxeServerTestCase implements ITest {
	var server:HaxeServerAsync;
	var vfs:Vfs;
	var testDir:String;
	var lastResult:HaxeServerRequestResult;
	var messages:Array<String> = [];
	var errorMessages = [];

	static var i:Int = 0;

	public function new() {}

	public function setup() {
		testDir = "test/cases/" + i++;
		vfs = new Vfs(testDir);
		server = new HaxeServerAsync(() -> new HaxeServerProcessNode("haxe", ["-v", "--cwd", testDir]));
	}

	public function teardown() {
		server.stop();
	}

	function runHaxe(args:Array<String>, done:Void->Void) {
		messages = [];
		errorMessages = [];
		server.rawRequest(args, null, function(result) {
			lastResult = result;
			sendLogMessage(result.stdout);
			for (print in result.prints) {
				var line = print.trim();
				messages.push('Haxe print: $line');
			}
			if (result.hasError) {
				sendErrorMessage(result.stderr);
			}
			done();
		}, sendErrorMessage);
	}

	function runHaxeJson<TParams, TResponse>(args:Array<String>, method:HaxeRequestMethod<TParams, TResponse>, methodArgs:TParams, done:Void->Void) {
		var methodArgs = {method: method, id: 1, params: methodArgs};
		args = args.concat(['--display', Json.stringify(methodArgs)]);
		runHaxe(args, done);
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

	function parseGotoDefinition():GotoTypeDefinitionResult {
		return Json.parse(lastResult.stderr).result;
	}

	function assertSuccess(?p:haxe.PosInfos) {
		Assert.isTrue(0 == errorMessages.length, p);
	}

	function assertErrorMessage(message:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasErrorMessage(message), p);
	}

	function assertHasPrint(line:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage("Haxe print: " + line), null, p);
	}

	function assertReuse(module:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage('reusing $module'), null, p);
	}

	function assertSkipping(module:String, ?dependency:String, ?p:haxe.PosInfos) {
		var msg = 'skipping $module';
		if (dependency != null) {
			msg += '($dependency)';
		}
		Assert.isTrue(hasMessage(msg), null, p);
	}

	function assertNotCacheModified(module:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage('$module not cached (modified)'), null, p);
	}

	function assertHasType(typePackage:String, typeName:String, ?p:haxe.PosInfos) {
		Assert.isTrue(getStoredType(typePackage, typeName) != null, null, p);
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
		if(params.length == 0) {
			return s;
		}
		var sParams = params.map(strType).join('.');
		return '$s<$sParams>';
	}
}
