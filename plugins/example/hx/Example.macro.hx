import haxe.PosInfos;

using haxe.io.Path;

typedef ExamplePluginApi = {
	function hello():Void;
	function stringifyPosition(p:haxe.macro.Expr.Position):String;
	function hijackStaticTest():Void;
}

class Example {
	/** Access plugin API */
	static public var plugin(get,never):ExamplePluginApi;

	static var _plugin:ExamplePluginApi;
	static function get_plugin():ExamplePluginApi {
		if(_plugin == null) {
			try {
				_plugin = eval.vm.Context.loadPlugin(getPluginPath());
			} catch(e:Dynamic) {
				throw 'Failed to load plugin: $e';
			}
		}
		return _plugin;
	}

	static function getPluginPath():String {
		var currentFile = (function(?p:PosInfos) return p.fileName)();
		var srcDir = currentFile.directory().directory();
		return Path.join([srcDir, 'cmxs', Sys.systemName(), 'plugin.cmxs']);
	}
}