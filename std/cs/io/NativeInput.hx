package cs.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Eof;
import haxe.io.Error;
import haxe.io.Input;

class NativeInput extends Input
{
	public var canSeek(get_canSeek, null):Bool;
	
	var stream:cs.system.io.Stream;
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
		if( pos < 0 || len < 0 || pos + len > s.length )
			throw Error.OutsideBounds;
		var ret = stream.Read(s.getData(), pos, len);
		if (ret == 0)
			throw new Eof();
		return ret;
	}
	
	override public function close():Void
	{
		stream.Close();
	}
	
	private function get_canSeek():Bool
	{
		return stream.CanSeek;
	}
	
	public function seek( p : Int, pos : sys.io.FileSeek ) : Void
	{
		var p = switch(pos)
		{
			case SeekBegin: cs.system.io.SeekOrigin.Begin;
			case SeekCur: cs.system.io.SeekOrigin.Current;
			case SeekEnd: cs.system.io.SeekOrigin.End;
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