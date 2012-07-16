package cs.io;
import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.Output;

class NativeOutput extends Output
{
	public var canSeek(get_canSeek, null):Bool;
	
	var stream:cs.system.io.Stream;
	public function new(stream)
	{
		this.stream = stream;
		if (!stream.CanWrite) throw "Read-only stream";
	}
	
	override public function writeByte(c:Int):Void 
	{
		stream.WriteByte(c);
	}
	
	override public function close():Void
	{
		stream.Close();
	}
	
	override public function flush():Void
	{
		stream.Flush();
	}
	
	override public function prepare(nbytes:Int):Void
	{
		//TODO see if implementation is correct
		stream.SetLength(haxe.Int64.add(stream.Length, cast(nbytes, Int64)));
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
}