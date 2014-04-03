
package python.lib;

import python.lib.Types;

extern class Glob {

	public static function glob (pathname:String):Array<String>;
	public static function iglob (pathname:String):PyIterator<String>;



	static function __init__ ():Void
	{
		Syntax.importAs("glob", "python.lib.Glob");
	}

}