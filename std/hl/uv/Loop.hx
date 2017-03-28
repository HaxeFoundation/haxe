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

	@:hlNative("uv","default_loop")
	public static function getDefault() : Loop {
		return null;
	}

}
