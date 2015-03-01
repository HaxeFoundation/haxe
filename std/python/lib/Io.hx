
package python.lib;

import python.lib.io.IOBase;

@:pythonImport("io")
extern class Io {

	public static var DEFAULT_BUFFER_SIZE:Int;

	public static function open(file:String, mode:String, ?buffering:Int = -1, ?encoding:String = null, ?errors : String, ?newline:String, ?closefd:Bool, ?opener:String->Int->FileDescriptor):IOBase;
}