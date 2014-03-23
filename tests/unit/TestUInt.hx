package unit;
import haxe.unit.TestCase;

class TestUInt extends TestCase
{

  function testFromFloat()
  {
    inline function floatToInt(float:Float):Int
    {
      return cast float;
    }
    inline function intToFloat(int:Int):Float
    {
      return int;
    }
    inline function floatToUInt(float:Float):UInt
    {
      return cast float;
    }
    inline function uintToInt(uint:UInt):Int
    {
      return uint;
    }
    inline function uintToFloat(uint:UInt):Float
    {
      return uint;
    }

    assertEquals(-536870912, floatToInt(-536870912.0));
    assertEquals(-536870912, uintToInt(floatToUInt(-536870912.0)));
    assertNotEquals(-536870912.0, uintToFloat(floatToUInt( -536870912.0)));
    assertNotEquals(-536870911.0, floatToUInt(-536870912.0) + 1.0);
  }

	function assertNotEquals<T>( expected: T , actual: T,  ?c : haxe.PosInfos ) : Void 	{
		currentTest.done = true;
		if (actual == expected){
			currentTest.success = false;
			currentTest.error   = "expected not '" + expected + "' but was '" + actual + "'";
			currentTest.posInfos = c;
			throw currentTest;
		}
	}

  function testMul()
  {
    var m:UInt = 0xE0000001;
    assertEquals(3221225473, m * m);
    var i:Int = m;
    assertEquals(-536870911, i);
    assertEquals(-1073741823, i * m);
    assertEquals( -1073741823, i * i);
  }

  function testCompare()
  {
    var u0:UInt = 0xE0000001;
    var u1:UInt = 123;
    assertTrue(u0 > u1);
    assertTrue(u0 >= u1);
    assertFalse(u0 < u1);
    assertFalse(u0 <= u1);
    var i0:Int = u0;
    var i1:Int = u1;
    assertTrue(i0 < i1);
    assertTrue(i0 <= i1);
    assertFalse(i0 > i1);
    assertFalse(i0 >= i1);
  }

  function testPostfixIncrement()
  {
    var m:UInt = 0xE0000001;
    var m0:UInt = 0xE0000001;
    var m1:UInt = 0xE0000002;
    assertEquals(m0, m++);
    assertEquals(m1, m);
  }

  function testPrefixIncrement()
  {
    var m:UInt = 0xE0000000;
    var m1:UInt = 0xE0000001;
    assertEquals(m1, ++m);
    assertEquals(m1, m);
  }

  function testToInt()
  {
    var m:UInt = 0xE0000001;
    var i:Int = m;
    var f:Float = m;
    assertTrue(i != f);
  }

}
