import haxe.display.JsonModuleTypes.JsonModuleType;
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
	var storedTypes:Array<JsonModuleType<Any>>;
	var messages:Array<String> = [];
	var errorMessages = [];
	var i:Int = 0;

	public function new() {}

	public function setup() {
		testDir = "test/cases/" + i++;
		vfs = new Vfs(testDir);
		server = new HaxeServerAsync(() -> new HaxeServerProcessNode("haxe", ["-v", "--cwd", testDir]));
	}

	public function teardown() {
		server.stop();
	}

	function runHaxe(args:Array<String>, storeTypes = false, done:Void->Void) {
		messages = [];
		errorMessages = [];
		storedTypes = [];
		if (storeTypes) {
			args = args.concat(['--display', '{ "method": "typer/compiledTypes", "id": 1 }']);
		}
		server.rawRequest(args, null, function(result) {
			sendLogMessage(result.stdout);
			for (print in result.prints) {
				var line = print.trim();
				messages.push('Haxe print: $line');
			}
			if (result.hasError) {
				sendErrorMessage(result.stderr);
			}
			if (storeTypes) {
				storedTypes = try {
					Json.parse(result.stderr).result.result;
				} catch (e:Dynamic) {
					trace(e);
					[];
				}
			}
			done();
		}, sendErrorMessage);
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

	function getStoredType(typePackage:String, typeName:String) {
		for (type in storedTypes) {
			if (type.pack.join(".") == typePackage && type.name == typeName) {
				return type;
			}
		}
		return null;
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
		Assert.isTrue(type != null);
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
}
