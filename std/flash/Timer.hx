package flash;

class Timer {

	private var id : Int;

	public function new( time : Int ) {
		id = untyped _global.setInterval(this,"run",time);
	}

	public function run() {
	}

	public function stop() {
		untyped _global.clearInterval(id);
		id = null;
	}

}