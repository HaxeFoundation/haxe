import haxe.display.JsonModuleTypes.JsonModuleType;
import haxe.Json;
import HaxeServer;
import utest.Assert;
using StringTools;
using Lambda;

class TestContext {
	public var messages:Array<String> = []; // encapsulation is overrated

	public var displayServerConfig:DisplayServerConfigBase;

	public function new(config:DisplayServerConfigBase) {
		this.displayServerConfig = config;
	}

	public function sendErrorMessage(msg:String) { }

	public function sendLogMessage(msg:String) {
		var split = msg.split("\n");
		for (message in split) {
			messages.push(message.trim());
		}
	}
}

class HaxeServerTestCase {
	var context:TestContext;
	var server:HaxeServer;
	var vfs:Vfs;
	var testDir:String;
	var storedTypes:Array<JsonModuleType<Any>>;

	public function new() {
		testDir = "test/cases/" + Type.getClassName(Type.getClass(this));
	}

	public function setup() {
		context = new TestContext({
			haxePath: "haxe",
			arguments: ["-v", "--cwd", testDir],
			env: { }
		});
		vfs = new Vfs(testDir);
		server = new HaxeServer(context);
		server.start();
	}

	public function teardown() {
		server.stop();
	}

	function runHaxe(args:Array<String>, storeTypes = false, done:Void -> Void) {
		context.messages = [];
		storedTypes = [];
		if (storeTypes) {
			args = args.concat(['--display', '{ "method": "typer/compiledTypes", "id": 1 }']);
		}
		server.process(args, null, function(result) {
			if (storeTypes) {
				storedTypes = Json.parse(result).result.result;
			}
			done();
		}, function(message) {
			Assert.fail(message);
			done();
		});
	}

	function getTemplate(templateName:String) {
		return sys.io.File.getContent("test/templates/" + templateName);
	}

	function hasMessage<T>(msg:String) {
		for (message in context.messages) {
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