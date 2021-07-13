package haxe.exceptions;

using StringTools;

/**
	An exception that carry position information of a place where it was created.
**/
class PosException extends Exception {
	/**
		Position where this exception was created.
	**/
	public final posInfos:PosInfos;

	var __fieldPath:Null<String>;

	public function new(message:String, ?previous:Exception, ?pos:PosInfos):Void {
		super(message, previous);
		if (pos == null) {
			posInfos = { fileName:'(unknown)', lineNumber:0, className:'(unknown)', methodName:'(unknown)' }
		} else {
			posInfos = pos;
		}
	}

	/**
		Returns exception message.
	**/
	override function toString():String {
		var fieldPath = switch __fieldPath {
			case null: __fieldPath = fieldPath(posInfos);
			case s: s;
		}
		return '${super.toString()} in $__fieldPath at ${posInfos.fileName}:${posInfos.lineNumber}';
	}

	static function fieldPath(pos:PosInfos):String {
		var className = if(pos.className.endsWith('_Impl_')) {
			var parts = pos.className.split('.');
			parts.pop();
			parts[parts.length - 1] = parts[parts.length - 1].substr(1);
			parts.join('.');
		} else {
			pos.className;
		}
		var fieldName = switch pos.methodName.substr(0, 4) {
			case 'get_' | 'set_': pos.methodName.substr(4);
			case _: pos.methodName;
		}
		return '$className.$fieldName';
	}
}