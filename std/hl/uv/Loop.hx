package hl.uv;

@:enum abstract LoopRunMode(Int) {
	var Default = 0;
	var Once = 1;
	var NoWait = 2;
}

abstract Loop(hl.Abstract<"uv_loop">) {

	@:hlNative("uv","loop_close") public function close() : Int {
		return 0;
	}

	@:hlNative("uv","run") public function run( mode : LoopRunMode ) : Int {
		return 0;
	}

	@:hlNative("uv","loop_alive") public function alive() : Int {
		return 0;
	}

	@:hlNative("uv","stop") public function stop() : Void {
	}

	public static function getDefault() : Loop {
		var def = default_loop();
		if( loopEvent == null )
			loopEvent = haxe.MainLoop.add(function() {
				// if no more things to process, stop
				if( def.run(NoWait) == 0 ) {
					loopEvent.stop();
					loopEvent = null;
				}
			});
		return def;
	}

	@:hlNative("uv", "default_loop") static function default_loop() : Loop {
		return null;
	}

	static var loopEvent : haxe.MainLoop.MainEvent;

}
