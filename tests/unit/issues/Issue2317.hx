package unit.issues;
import haxe.Int64;

class Issue2317 extends unit.Test
{
	public function test()
	{
		var i = Int64.make(0xA, 0x828D97A8);
		t( Int64.compare(i, Int64.ofInt(0) ) > 0 );
	}
}
