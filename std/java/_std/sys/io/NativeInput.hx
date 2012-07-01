package sys.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Input;
import java.io.Exceptions;

class NativeInput extends Input
{
	var stream:java.io.InputStream;
	public function new(stream)
	{
		this.stream = stream;
	}
	
	override public function readByte():Int 
	{
		try
		{
			return stream.read();
		} 
		catch (e:EOFException) {
			throw new Eof();
		}
		
		catch (e:IOException) {
			throw haxe.io.Error.Custom(e);
		}
	}
	
	override public function readBytes(s:Bytes, pos:Int, len:Int):Int 
	{
		try
		{
			return stream.Read(s.getDate(), pos, len);
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
}