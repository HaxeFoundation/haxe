package java.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Output;
import java.io.Exceptions;

@:native('haxe.java.io.NativeOutput') class NativeOutput extends Output
{
	var stream:java.io.OutputStream;
	public function new(stream)
	{
		this.stream = stream;
	}
	
	override public function writeByte(c:Int):Void 
	{
		try
		{
			stream.write(c);
		}
		
		catch (e:EOFException) {
			throw new Eof();
		}
		
		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
	
	override public function close():Void
	{
		try
		{
			stream.close();
		}
		
		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
	
	override public function flush():Void
	{
		try
		{
			stream.flush();
		}
		
		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
}