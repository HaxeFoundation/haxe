
package python.lib;

import python.lib.Types;
import python.NativeIterator;

extern class Glob {

	public static function glob (pathname:String):Array<String>;
	public static function iglob (pathname:String):NativeIterator<String>;



	static function __init__ ():Void
	{
		Syntax.importAs("glob", "python.lib.Glob");
	}

}