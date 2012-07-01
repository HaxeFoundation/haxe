package sys.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Input;

class NativeInput extends Input
{
	public var canSeek(get_canSeek, null):Bool;
	
	var stream:system.io.Stream;
	public function new(stream)
	{
		this.stream = stream;
		if (!stream.CanRead) throw "Write-only stream";
	}
	
	override public function readByte():Int 
	{
		var ret = stream.ReadByte();
		if (ret == -1) throw new Eof();
		return ret;
	}
	
	override public function readBytes(s:Bytes, pos:Int, len:Int):Int 
	{
		return stream.Read(s.getData(), pos, len);
	}
	
	override public function close():Void
	{
		stream.Close();
	}
	
	private function get_canSeek():Bool
	{
		return stream.CanSeek;
	}
	
	public function seek( p : Int, pos : FileSeek ) : Void
	{
		var p = switch(pos)
		{
			case SeekBegin: system.io.SeekOrigin.Begin;
			case SeekCur: system.io.SeekOrigin.Current;
			case SeekEnd: system.io.SeekOrigin.End;
		};
		
		stream.Seek(cast(p, Int64), p);
	}
	
	public function tell() : Int
	{
		return cast(stream.Position, Int);
	}
	
	public function eof() : Bool
	{
		return stream.Position == stream.Length;
	}
}