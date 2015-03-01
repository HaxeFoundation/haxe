
package python.lib.io;

import python.lib.io.BufferedIOBase;
import python.lib.io.IOBase;

@:pythonImport("io", "TextIOBase")
extern class TextIOBase extends IOBase implements ITextIOBase {

	public var encoding:String;
	public var error:String;
	public var newlines:Null<haxe.EitherType<String, Tuple<String>>>;

	public function detach ():BufferedIOBase;

	public function write (s:String):Int;

	public function read (n:Int):String;

	public var buffer:BufferedIOBase;

}

@:remove extern interface ITextIOBase extends IIOBase {

	public var encoding:String;
	public var error:String;
	public var newlines:Null<haxe.EitherType<String, Tuple<String>>>;

	public var buffer:BufferedIOBase;

	public function detach ():BufferedIOBase;

	public function write (s:String):Int;

	public function read (n:Int):String;




}
