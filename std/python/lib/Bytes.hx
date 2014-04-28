
package python.lib;

import python.lib.ByteArray;

extern class Bytes extends ByteArray {

	//public function decode(encoding:String="utf-8", errors:String="strict"):String;

	static function __init__ ():Void
	{
		Syntax.importFromAs("builtins", "bytes", "python.lib.Bytes");
	}


}