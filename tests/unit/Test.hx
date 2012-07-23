package unit;

@:keepSub class Test #if swf_mark implements mt.Protect #end #if as3 implements haxe.Public #end {

	public function new() {
	}

	function eq<T>( v : T, v2 : T, ?pos ) {
		count++;
		if( v != v2 ) report(Std.string(v)+" should be "+Std.string(v2),pos);
	}

	function feq( v : Float, v2 : Float, ?pos ) {
		count++;
		if( Math.abs(v - v2) > 1e-18 ) report(v+" should be "+v2,pos);
	}

	function t( v, ?pos ) {
		eq(v,true,pos);
	}

	function f( v, ?pos ) {
		eq(v,false,pos);
	}

	function assert( ?pos ) {
		report("Assert",pos);
	}

	function exc( f : Void -> Void, ?pos ) {
		count++;
		try {
			f();
			report("No exception occured",pos);
		} catch( e : Dynamic ) {
		}
	}

	function unspec( f : Void -> Void, ?pos ) {
		count++;
		try {
			f();
		} catch( e : Dynamic ) {
		}
	}

	function allow<T>( v : T, values : Array<T>, ?pos ) {
		count++;
		for( v2 in values )
			if( v == v2 )
				return;
		report(v+" not in "+Std.string(values),pos);
	}

	function infos( m : String ) {
		reportInfos = m;
	}

	function async<Args,T>( f : Args -> (T -> Void) -> Void, args : Args, v : T, ?pos : haxe.PosInfos ) {
		if( asyncWaits.length >= AMAX ) {
			asyncCache.push(callback(async,f,args,v,pos));
			return;
		}
		asyncWaits.push(pos);
		f(args,function(v2) {
			count++;
			if( !asyncWaits.remove(pos) ) {
				report("Double async result",pos);
				return;
			}
			if( v != v2 )
				report(v2+" should be "+v,pos);
			checkDone();
		});
	}

	function asyncExc<Args>( seterror : (Dynamic -> Void) -> Void, f : Args -> (Dynamic -> Void) -> Void, args : Args, ?pos : haxe.PosInfos ) {
		if( asyncWaits.length >= AMAX ) {
			asyncCache.push(callback(asyncExc,seterror,f,args,pos));
			return;
		}
		asyncWaits.push(pos);
		seterror(function(e) {
			count++;
			if( asyncWaits.remove(pos) )
				checkDone();
			else
				report("Multiple async events",pos);
		});
		f(args,function(v) {
			count++;
			if( asyncWaits.remove(pos) ) {
				report("No exception occured",pos);
				checkDone();
			} else
				report("Multiple async events",pos);
		});
	}

	function log( msg, ?pos : haxe.PosInfos ) {
		haxe.Log.trace(msg,pos);
	}

	static var count = 0;
	static var reportInfos = null;
	static var reportCount = 0;
	static var checkCount = 0;
	static var asyncWaits = new Array<haxe.PosInfos>();
	static var asyncCache = new Array<Void -> Void>();
	static var AMAX = 3;
	static var timer : haxe.Timer;

	dynamic static function report( msg : String, ?pos : haxe.PosInfos ) {
		if( reportInfos != null ) {
			msg += " ("+reportInfos+")";
			reportInfos = null;
		}
		haxe.Log.trace(msg,pos);
		reportCount++;
		if( reportCount == 50 ) {
			trace("Too many errors");
			report = function(msg,?pos) {};
		}
	}

	static function checkDone() {
		if( asyncWaits.length != 0 ) return;
		if( asyncCache.length == 0 ) {
			report("DONE ["+count+" tests]");
			return;
		}
		resetTimer();
		while( asyncCache.length > 0 && asyncWaits.length < AMAX )
			asyncCache.shift()();
	}

	static function asyncTimeout() {
		if( asyncWaits.length == 0 )
			return;
		for( pos in asyncWaits )
			report("TIMEOUT",pos);
		asyncWaits = new Array();
		checkDone();
	}

	static function resetTimer() {
		#if (neko || php || cpp)
		#else
		if( timer != null ) timer.stop();
		timer = new haxe.Timer(10000);
		timer.run = asyncTimeout;
		#end
	}

	static function onError( e : Dynamic, msg : String, context : String ) {
		var msg = "???";
		var stack :String = #if js
			e.stack;
		#else
			haxe.Stack.toString(haxe.Stack.exceptionStack());
		#end
		try msg = Std.string(e) catch( e : Dynamic ) {};
		reportCount = 0;
		report("ABORTED : "+msg+" in "+context);
		reportInfos = null;
		trace("STACK :\n"+stack);
	}

	static function main() {
		#if neko
		if( neko.Web.isModNeko )
			neko.Web.setHeader("Content-Type","text/plain");
		#elseif php
		if( php.Web.isModNeko )
			php.Web.setHeader("Content-Type","text/plain");
		#end
		resetTimer();
		trace("START");
		#if flash9
		var tf : flash.text.TextField = untyped flash.Boot.getTrace();
		tf.selectable = true;
		tf.mouseEnabled = true;
		#elseif flash
		var tf : flash.TextField = untyped flash.Boot.getTrace();
		tf.selectable = true;
		#end
		var classes = [
			new TestOps(),
			new TestBasetypes(),
			new TestBytes(),
			new TestInt32(),
			new TestIO(),
			new TestLocals(),
			new TestEReg(),
			new TestXML(),
			new TestMisc(),
			new TestResource(),
			new TestInt64(),			
			new TestReflect(),
			new TestSerialize(),
			new TestMeta(),
			#if cs
			new TestCSharp(),
			#end
			#if java
			new TestJava(),
			#end
			new TestDCE(),
			//new TestUnspecified(),
			//new TestRemoting(),
		];
		var current = null;
		#if (!fail_eager)
		try 
		#end
		{
			asyncWaits.push(null);
			for( inst in classes ) {
				current = Type.getClass(inst);
				for( f in Type.getInstanceFields(current) )
					if( f.substr(0,4) == "test" ) {
						#if fail_eager
						Reflect.callMethod(inst,Reflect.field(inst,f),[]);
						#else
						try {
							Reflect.callMethod(inst,Reflect.field(inst,f),[]);
						}
						#if !as3
						catch( e : Dynamic ) {
							onError(e,"EXCEPTION",Type.getClassName(current)+"."+f);
						}
						#end
						#end
						reportInfos = null;
					}
			}
			asyncWaits.remove(null);
			checkDone();
		}
		#if (!as3 && !(fail_eager))
		catch( e : Dynamic ) {
			asyncWaits.remove(null);
			onError(e,"ABORTED",Type.getClassName(current));
		}
		#end
	}

}
