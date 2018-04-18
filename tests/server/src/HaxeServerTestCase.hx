import HaxeServer;
import haxe.Json;
import utest.Assert;
using StringTools;

typedef Message<T> = {
	kind: String,
	data: T
}

class TestContext {
	public var displayServerConfig:DisplayServerConfigBase;

	public function new(config:DisplayServerConfigBase) {
		this.displayServerConfig = config;
	}

	public function sendErrorMessage(msg:String) { }

	public function sendLogMessage(msg:String) { }
}

class HaxeServerTestCase {
	var context:TestContext;
	var server:HaxeServer;
	var vfs:Vfs;
	var testDir:String;
	var messages:Array<Message<Any>>;

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

	function haxe(args:Array<String>, done:Void -> Void) {
		args = args.concat(["-D", "compilation-server-test"]);
		server.process(args, null, function(result) {
			if (result == "") {
				result = "{}";
			}
			messages = Json.parse(result);
			done();
		}, function(message) {
			Assert.fail(message);
			done();
		});
	}

	function getTemplate(templateName:String) {
		return sys.io.File.getContent("test/templates/" + templateName);
	}

	function hasMessage<T>(msg:{kind: String, data:T}) {
		function compareData(data1:Dynamic, data2:Dynamic) {
			return switch (msg.kind) {
				case "reusing" | "notCached": data1 == data2;
				case "skipping": data1.skipped == data2.skipped && data1.dependency == data2.dependency;
				case "print": Std.string(data1).trim() == Std.string(data2).trim();
				case _: false;
			}
		}
		for (message in messages) {
			if (message.kind == msg.kind && compareData(message.data, cast msg.data)) {
				return true;
			}
		}
		return false;
	}

	function assertHasPrint(line:String) {
		Assert.isTrue(hasMessage({kind: "print", data: line}));
	}

	function assertReuse(module:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage({kind: "reusing", data: module}), null, p);
	}

	function assertSkipping(module:String, ?dependency:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage({kind: "skipping", data: {skipped: module, dependency: dependency == null ? module : dependency}}), null, p);
	}

	function assertNotCacheModified(module:String, ?p:haxe.PosInfos) {
		Assert.isTrue(hasMessage({kind: "notCached", data: module}), null, p);
	}
}