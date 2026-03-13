package jsonrpc;

class ErrorUtils {
	public static function errorToString(error:Dynamic, intro:String):String {
		var result = intro + Std.string(error);
		var stack = haxe.CallStack.exceptionStack();
		if (stack != null && stack.length > 0)
			result += "\n" + haxe.CallStack.toString(stack);
		return result;
	}
}
