
package python.lib;


import python.NativeIterator;

@:pythonImport("glob")
extern class Glob {

	public static function glob (pathname:String):Array<String>;
	public static function iglob (pathname:String):NativeIterator<String>;

}