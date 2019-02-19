import HaxeServer;
import utest.Assert;
using StringTools;

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

	function runHaxe(args:Array<String>, done:Void -> Void) {
		context.messages = [];
		server.process(args, null, function(_) {
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
}