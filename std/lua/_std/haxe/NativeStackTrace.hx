package haxe;

import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
	}

	static public function callStack():Array<String> {
		return switch lua.Debug.traceback() {
			case null: [];
			case s: s.split('\n').slice(3);
		}
	}

	static public function exceptionStack():Array<String> {
		return []; //Not implemented. Maybe try xpcal instead of pcal in genlua.
	}

	static public function toHaxe(native:Array<String>, skip:Int = 0):Array<StackItem> {
		var stack = [];
		var cnt = -1;
		for (item in native) {
			var parts = item.substr(1).split(":"); //`substr` to skip a tab at the beginning of a line
			var file = parts[0];
			if(file == '[C]') {
				continue;
			}
			++cnt;
			if(skip > cnt) {
				continue;
			}
			var line = parts[1];
			var method = if(parts.length <= 2) {
				null;
			} else {
				var methodPos = parts[2].indexOf("'");
				if(methodPos < 0) {
					null;
				} else {
					Method(null, parts[2].substring(methodPos + 1, parts[2].length - 1));
				}
			}
			stack.push(FilePos(method, file, Std.parseInt(line)));
		}
		return stack;
	}
}