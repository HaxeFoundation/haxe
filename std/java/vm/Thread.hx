package java.vm;
import java.Lib;

@:native('haxe.java.vm.Thread') class Thread
{

	@:private static var javaThreadToHaxe = new haxe.ds.WeakMap<java.lang.Thread, java.vm.Thread>();
	@:private static var mainJavaThread = java.lang.Thread.currentThread();
	@:private static var mainHaxeThread = {
		var ret = new Thread();
		javaThreadToHaxe.set(mainJavaThread, ret);
		ret;
	};


	private static function getThread(jt:java.lang.Thread):Thread
	{
		if (Std.is(jt, HaxeThread))
		{
			var t:HaxeThread = cast jt;
			return t.threadObject;
		}
		else if (jt == mainJavaThread)
		{
			return mainHaxeThread;
		}
		else
		{
			var ret = null;
			untyped __lock__(javaThreadToHaxe, {
				ret = javaThreadToHaxe.get(jt);
				if (ret == null)
				{
					ret = new Thread();
					javaThreadToHaxe.set(jt, ret);
				}

			});
			return ret;
		}
	}

	private var messages:Deque<Dynamic>;

  function new()
	{
		this.messages = new Deque();
	}

	public function sendMessage(obj:Dynamic)
	{
		messages.add(obj);
	}

	public static function current():Thread
	{
		return getThread( java.lang.Thread.currentThread() );
	}

	public static function readMessage(block : Bool) : Dynamic
	{
		return current().messages.pop(block);
	}

	public static function create(fn:Void->Void):Thread
	{
		var ret = new Thread();
		var t = new HaxeThread(ret, fn);
		t.start();
		return ret;
	}
}

@:native('haxe.java.vm.HaxeThread')
private class HaxeThread extends java.lang.Thread
{
	public var threadObject(default, null):Thread;
	private var runFunction:Void->Void;
	@:overload override public function run():Void
	{
		runFunction();
	}
	public function new(hxThread:Thread, run:Void->Void)
	{
		super();
		threadObject = hxThread;
		runFunction = run;
		setDaemon(true);
	}
}
