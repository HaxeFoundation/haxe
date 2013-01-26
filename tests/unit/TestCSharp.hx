package unit;
import haxe.Int32;
using haxe.Int32;

//C#-specific tests, like unsafe code
class TestCSharp extends Test
{
	#if cs

	@:skipReflection private function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}

	@:skipReflection private function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 2;
	}

	public function testRef()
	{
		var i = 10;
		refTest(i);
		eq(i, 20);

		var cl:NativeClass = new HxClass();
		cl.refTest(i);
		eq(i, 80);
	}

	public function testOut()
	{
		var i = 0;
		outTest(i, 10);
		eq(i, 20);

		var cl:NativeClass = new HxClass();
		cl.outTest(i, 10);
		eq(i, 40);
	}

	public function testChecked()
	{
		exc(function()
		{
			cs.Lib.checked({
				var x = 1000;
				while(true)
				{
					x *= x;
				}
			});
		});
	}

	#if unsafe

	@:unsafe public function testUnsafe()
	{
		var x:cs.NativeArray<Int> = new cs.NativeArray(10);
		cs.Lib.fixed({
			var p = cs.Lib.pointerOfArray(x);
			for (i in 0...10)
			{
				p[0] = i;
				p++;
			}
		});

		cs.Lib.fixed( {
			var p = cs.Lib.pointerOfArray(x);
			for (i in 0...10)
			{
				eq(p[i], i);
			}
		});

		var x:Int = 0;
		var addr = cs.Lib.addressOf(x);
		eq(cs.Lib.valueOf(addr), 0);
		eq(addr[0], 0);
		addr[0] = 42;
		eq(cs.Lib.valueOf(addr), 42);
		eq(addr[0], 42);
		eq(x, 42);


	}

	#end

	#end
}

@:nativeGen private class NativeClass
{
	public function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 2;
	}

	public function refTest(i:cs.Ref<Int>):Void
	{
		i *= 2;
	}
}

private class HxClass extends NativeClass
{
	public function new()
	{

	}

	//here it would normally fail due to the added fast reflection field
	override public function outTest(out:cs.Out<Int>, x:Int):Void
	{
		out = x * 4;
	}

	override public function refTest(i:cs.Ref<Int>):Void
	{
		super.refTest(i);
		i *= 2;
	}
}