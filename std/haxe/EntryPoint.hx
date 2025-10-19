package haxe;

/**
	If `haxe.MainLoop` is kept from DCE, then we will insert an `haxe.EntryPoint.run()` call just at then end of `main()`.
	This class can be redefined by custom frameworks so they can handle their own main loop logic.
**/
class EntryPoint {

	@:keep public static function run() @:privateAccess {
		#if js
			var nextTick = haxe.EventLoop.main.getNextTick();
			inline function setTimeoutNextTick() {
				if (nextTick >= 0) {
					(untyped setTimeout)(run, nextTick * 1000);
				}
			}
			#if nodejs
			setTimeoutNextTick();
			#else
			if(js.Lib.typeof(js.Browser.window) != 'undefined') {
				var window:Dynamic = js.Browser.window;
				var rqf:Dynamic = window.requestAnimationFrame || window.webkitRequestAnimationFrame || window.mozRequestAnimationFrame;
				if(rqf != null) {
					rqf(run);
				} else {
					setTimeoutNextTick();
				}
			} else {
				setTimeoutNextTick();
			}
			#end
			haxe.EventLoop.main.loopOnce();
		#elseif flash
			flash.Lib.current.stage.addEventListener(flash.events.Event.ENTER_FRAME, function(_) haxe.EventLoop.main.loopOnce());
		#elseif lua
			inline function luvRun(mode:String):Bool
				return untyped __lua__('_hx_luv.run({0})', mode);
			var events = haxe.EventLoop.main;
			while (true) {
				events.loopOnce();
				if( !events.hasEvents(true) )
					break;
				var nextTick = events.getNextTick();
				if(untyped __lua__('_hx_luv.loop_alive()')) {
					if(nextTick < 0)
						luvRun("once")
					else
						luvRun("nowait");
				}
			}
		#else
			haxe.EventLoop.main.loop();
		#end
	}
}
