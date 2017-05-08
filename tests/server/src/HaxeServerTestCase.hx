import HaxeServer;
import utest.Assert;

class TestContext {
	public var displayServerConfig:DisplayServerConfigBase;
	var messages:Array<String>;
	var messageString:Null<String>;

	public function new(config:DisplayServerConfigBase) {
		this.displayServerConfig = config;
		messages = [];
	}

	public function sendErrorMessage(msg:String) {
		messages.push(msg);
	}

	public function sendLogMessage(msg:String) {
		messages.push(msg);
	}

	public function hasMessage(msg:String) {
		if (messageString == null) {
			messageString = messages.join("\n");
		}
		return new EReg(EReg.escape(msg), "").match(messageString);
	}

	public function resetMessages() {
		messageString = null;
		messages = [];
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

	function haxe(args:Array<String>, done:Void -> Void) {
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

	inline function hasMessage(msg:String) {
		return context.hasMessage(msg);
	}

	inline function resetMessages() {
		context.resetMessages();
	}
}