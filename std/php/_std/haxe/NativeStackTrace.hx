package haxe;

import php.*;
import haxe.CallStack.StackItem;

private typedef NativeTrace = NativeIndexedArray<NativeAssocArray<Dynamic>>;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
@:allow(haxe.Exception)
class NativeStackTrace {
	/**
		If defined this function will be used to transform call stack entries.
		@param String - generated php file name.
		@param Int - Line number in generated file.
	**/
	static public var mapPosition:String->Int->Null<{?source:String, ?originalLine:Int}>;

	static var lastExceptionTrace:Null<NativeTrace>;

	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public function saveStack(e:Throwable) {
		var nativeTrace = e.getTrace();

		// Reduce exception stack to the place where exception was caught
		var currentTrace = Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS);
		var count = Global.count(currentTrace);

		for (i in -(count - 1)...1) {
			var exceptionEntry:NativeAssocArray<Dynamic> = Global.end(nativeTrace);

			if (!Global.isset(exceptionEntry['file']) || !Global.isset(currentTrace[-i]['file'])) {
				Global.array_pop(nativeTrace);
			} else if (currentTrace[-i]['file'] == exceptionEntry['file'] && currentTrace[-i]['line'] == exceptionEntry['line']) {
				Global.array_pop(nativeTrace);
			} else {
				break;
			}
		}

		// Remove arguments from trace to avoid blocking some objects from GC
		var count = Global.count(nativeTrace);
		for (i in 0...count) {
			nativeTrace[i]['args'] = new NativeArray();
		}

		lastExceptionTrace = complementTrace(nativeTrace, e);
	}

	static public inline function callStack():NativeTrace {
		return Global.debug_backtrace(Const.DEBUG_BACKTRACE_IGNORE_ARGS);
	}

	static public function exceptionStack():NativeTrace {
		return lastExceptionTrace == null ? new NativeIndexedArray() : lastExceptionTrace;
	}

	static public function toHaxe(native:NativeTrace, skip:Int = 0):Array<StackItem> {
		var result = [];
		var count = Global.count(native);

		for (i in 0...count) {
			if(skip > i) {
				continue;
			}

			var entry = native[i];
			var item = null;

			if (i + 1 < count) {
				var next = native[i + 1];

				if (!Global.isset(next['function']))
					next['function'] = '';
				if (!Global.isset(next['class']))
					next['class'] = '';

				if ((next['function'] : String).indexOf('{closure}') >= 0) {
					item = LocalFunction();
				} else if (Global.strlen(next['class']) > 0 && Global.strlen(next['function']) > 0) {
					var cls = Boot.getClassName(next['class']);
					item = Method(cls, next['function']);
				}
			}
			if (Global.isset(entry['file'])) {
				if (mapPosition != null) {
					var pos = mapPosition(entry['file'], entry['line']);
					if (pos != null && pos.source != null && pos.originalLine != null) {
						entry['file'] = pos.source;
						entry['line'] = pos.originalLine;
					}
				}
				result.push(FilePos(item, entry['file'], entry['line']));
			} else if (item != null) {
				result.push(item);
			}
		}

		return result;
	}

	static function complementTrace(nativeTrace:NativeTrace, e:Throwable):NativeTrace {
		var thrownAt = new NativeAssocArray<Dynamic>();
		thrownAt['function'] = '';
		thrownAt['line'] = e.getLine();
		thrownAt['file'] = e.getFile();
		thrownAt['class'] = '';
		thrownAt['args'] = new NativeArray();
		Global.array_unshift(nativeTrace, thrownAt);
		return nativeTrace;
	}
}