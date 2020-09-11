package haxe.exceptions;

using StringTools;

/**
	An exception that is thrown when requested function or operation is
	not supported or cannot be implemented.
**/
class NotSupportedException extends Exception {
	/**
		Returns an instance of `NotSupportedException` with the message telling
		that the caller of this method is not supported on current platform.
	**/
	static public function field(?pos:PosInfos):NotSupportedException {
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
		return new NotSupportedException('$className.$fieldName is not supported on this platform');
	}

	public function new(message:String = 'Operation not supported', ?previous:Exception):Void {
		super(message, previous);
	}
}