import haxe.macro.Context;

var lookups = [];

function init() {
	Context.onTypeNotFound(path -> {
		lookups.push(path);
		return null;
	});
	Context.onAfterTyping(_ -> {
		var json = haxe.Json.stringify(lookups);
		@:privateAccess Context.load("send_json", 1)(json);
	});
}
