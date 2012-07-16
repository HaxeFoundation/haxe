package unit;

class TestJava extends Test
{
	#if java
	
	function textException()
	{
		var native = new NativeClass();
		var hx:NativeClass = new HxClass();
		
		exc(function() try native.excTest() catch (e:Dynamic) throw e);
		var dyn:Dynamic = native;
		exc(dyn.excTest);
		
		try 
			hx.excTest()
		catch(e:Dynamic) throw e; //shouldn't throw any exception
	}
	
	#end
}

@:nativegen private class NativeClass
{
	public function new()
	{
		
	}
	
	@:throws("java.lang.Throwable")
	public function excTest():Void
	{
		throw new java.lang.Throwable("test", null);
	}
}

private class HxClass extends NativeClass
{
	
	@:throws("java.lang.Throwable")
	override public function excTest():Void 
	{
		
	}
}