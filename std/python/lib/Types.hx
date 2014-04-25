
package python.lib;


import python.lib.Builtin;
import python.lib.io.IOBase;

abstract Choice <A,B>(Dynamic) {
	@:from public static inline function fromA <A,B>(x:A):Choice<A,B> return cast x;
	@:from public static inline function fromB <A,B>(x:B):Choice<A,B> return cast x;
}

abstract KwArgs (Dict<String, Dynamic>)
{
	inline function new (d:Dict<String, Dynamic>) this = d;

	@:to inline function toDict ():Dict<String, Dynamic>
	{
		return this;
	}
	@:from static inline function fromDict (d:Dict<String, Dynamic>):KwArgs
	{
		return new KwArgs(d);
	}

	public function get <V>(key:String, def:V):V
	{
		return this.get(key, def);
	}
}

abstract VarArgs (Array<Dynamic>)
{
	inline function new (d:Array<Dynamic>) this = d;

	@:to inline function toArray ():Array<Dynamic>
	{
		return this;
	}
	@:from static inline function fromArray (d:Array<Dynamic>):VarArgs
	{
		return new VarArgs(d);
	}
}


extern class ByteArray implements ArrayAccess<Int> {
	public var length(get, null):Int;
	public inline function get_length ():Int {
		return Builtin.len(this);
	}

	public inline function get(i:Int):Int {
		return python.Syntax.arrayAccess(this, i);
	}

	public inline function set(i:Int,v:Int):Void {
        this.__setitem__(i,v);
    }

    public function __setitem__(i:Int,v:Int):Void;

	public function decode(encoding:String="utf-8", errors:String="strict"):String;
}

extern class Bytes extends ByteArray {

	//public function decode(encoding:String="utf-8", errors:String="strict"):String;

	static function __init__ ():Void
	{
		Syntax.importFromAs("builtins", "bytes", "python.lib.Bytes");
	}


}

typedef Variant<A,B> = Dynamic;
typedef Variant3<A,B,C> = Dynamic;
typedef Variant4<A,B,C,D> = Dynamic;

abstract PyIterator <T>(NativeIterator<T>) to NativeIterator<T> to PyIterable<T> {
	public inline function new (p:NativeIterator<T>) this = p;
	@:to public static inline function toHaxeIterator <T>(p:NativeIterator<T>):HaxeIterator<T> return new HaxeIterator(p);
	@:to public static inline function toPyIterable <T>(p:NativeIterator<T>):PyIterable<T> return p;
	public function getNativeIterator <T>():NativeIterator<T> return this;
}

abstract PyIterable <T>(NativeIterable<T>) to NativeIterable<T> from NativeIterable<T> {
	@:to public static inline function toHaxeIterable <T>(p:NativeIterable<T>):HaxeIterable<T> return new HaxeIterable(p);

	//@:from public static inline function fromArray <T>(p:Array<T>):PyIterable<T> return cast p;

	public inline function iterator <T>() return IterHelper.iterableToIterator(this);
	public function getNativeIterable <T>():NativeIterable<T> return this;
	public function getNativeIterator <T>():NativeIterator<T> return this.__iter__();

}

class IterHelper {
	public static inline function iterableToIterator <T>(it:PyIterable<T>):Iterator<T>
	{
		return it.toHaxeIterable().iterator();
	}
}

typedef NativeIterator<T> = {
	function __next__ ():T;
	function __iter__ ():PyIterator<T>;
}

typedef NativeIterable<T> = {
	function __iter__():PyIterator<T>;

}

typedef List<T> = Array<T>;



typedef Hashable = {
	public function __hash__():Int;
}

typedef Equal = {
	public function __eq__(other:Dynamic):Int;
}

typedef Comparable = {

	public function __cmp__(other:Dynamic):Int;
}

typedef FileObject = IOBase;

extern class FileDescriptor {

}


//typedef DictKey<T> = {
//	function __hash__():Int;
//	function __eq__(other:Dynamic):Int;
//	function __cmp__(other:Dynamic):Int;
//}

//@:native("set")







@:native("BaseException")
extern class BaseException
{
	public function new (msg:String):Void;
}



@:native("BufferError")
extern class BufferError extends BaseException
{

}

@:native("GeneratorExit")
extern class GeneratorExit extends BaseException
{

}

@:native("KeyboardInterrupt")
extern class KeyboardInterrupt extends BaseException
{

}

@:native("Exception")
extern class Exception extends BaseException
{

}

@:native("SyntaxError")
extern class SyntaxError extends Exception
{

}

@:native("StopIteration")
extern class StopIteration extends Exception
{
	public function new (?message:String);
}

@:native("RuntimeError")
extern class RuntimeError extends Exception
{

}

@:native("NotImplementedError")
extern class NotImplementedError extends RuntimeError
{

}

@:native("IndentationError")
extern class IndentationError extends SyntaxError
{

}

@:native("EnvironmentError")
extern class EnvironmentError extends Exception
{

}

@:native("OSError")
extern class OSError extends EnvironmentError
{

}

@:native("BlockingIOError")
extern class BlockingIOError extends OSError
{

}

@:native("ChildProcessError")
extern class ChildProcessError extends OSError
{

}

@:native("ConnectionError")
extern class ConnectionError extends OSError
{

}

@:native("BrokenPipeError")
extern class BrokenPipeError extends ConnectionError
{

}

@:native("ConnectionAbortedError")
extern class ConnectionAbortedError extends ConnectionError
{

}
@:native("ConnectionRefusedError")
extern class ConnectionRefusedError extends ConnectionError
{

}
@:native("ConnectionResetError")
extern class ConnectionResetError extends ConnectionError
{

}

@:native("FileExistsError")
extern class FileExistsError extends OSError
{

}
@:native("FileNotFoundError")
extern class FileNotFoundError extends OSError
{

}
@:native("InterruptedError")
extern class InterruptedError extends OSError
{

}
@:native("IsADirectoryError")
extern class IsADirectoryError extends OSError
{

}
@:native("NotADirectoryError")
extern class NotADirectoryError extends OSError
{

}
@:native("PermissionError")
extern class PermissionError extends OSError
{

}
@:native("ProcessLookupError")
extern class ProcessLookupError extends OSError
{

}
@:native("TimeoutError")
extern class TimeoutError extends OSError
{

}


@:native("NameError")
extern class NameError extends Exception
{

}

@:native("UnboundLocalError")
extern class UnboundLocalError extends NameError
{

}

@:native("MemoryError")
extern class MemoryError extends Exception
{

}

@:native("AssertionError")
extern class AssertionError extends Exception
{

}

@:native("AttributeError")
extern class AttributeError extends Exception
{

}

@:native("EOFError")
extern class EOFError extends Exception
{

}

@:native("ArithmeticError")
extern class ArithmeticError extends Exception
{

}



@:native("FloatingPointError")
extern class FloatingPointError extends ArithmeticError
{

}

@:native("OverflowError")
extern class OverflowError extends ArithmeticError
{

}


@:native("ZeroDivisionError")
extern class ZeroDivisionError extends ArithmeticError
{

}



@:native("ImportError")
extern class ImportError extends Exception
{

}

@:native("LookupError")
extern class LookupError extends Exception
{

}

@:native("IndexError")
extern class IndexError extends LookupError
{

}

@:native("KeyError")
extern class KeyError extends LookupError
{

}







@:native("IOError")
extern class IOError extends EnvironmentError
{

}

@:native("VMSError")
extern class VMSError extends OSError
{

}

@:native("WindowsError")
extern class WindowsError extends OSError
{

}






@:native("ValueError")
extern class ValueError extends Exception
{

}

@:native("UnicodeError")
extern class UnicodeError extends ValueError
{

}
@:native("UnicodeDecodeError")
extern class UnicodeDecodeError extends UnicodeError
{

}
@:native("UnicodeEncodeError")
extern class UnicodeEncodeError extends UnicodeError
{

}
@:native("UnicodeTranslateError")
extern class UnicodeTranslateError extends UnicodeError
{

}

@:native("Warning")
extern class Warning extends Exception
{

}

@:native("DeprecationWarning")
extern class DeprecationWarning extends Warning
{

}
@:native("PendingDeprecationWarning")
extern class PendingDeprecationWarning extends Warning
{

}
@:native("RuntimeWarning")
extern class RuntimeWarning extends Warning
{

}
@:native("SyntaxWarning")
extern class SyntaxWarning extends Warning
{

}
@:native("UserWarning")
extern class UserWarning extends Warning
{

}
@:native("FutureWarning")
extern class FutureWarning extends Warning
{

}
@:native("ImportWarning")
extern class ImportWarning extends Warning
{

}
@:native("UnicodeWarning")
extern class UnicodeWarning extends Warning
{

}
@:native("BytesWarning")
extern class BytesWarning extends Warning
{

}
@:native("ResourceWarning")
extern class ResourceWarning extends Warning
{

}

extern class TB {}
extern class Frame {}
