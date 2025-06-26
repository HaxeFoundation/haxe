import haxe.MainLoop;

class Main {
	static function main() {
		trace('main');
		var event = null;
		var done 	= false;
		event = MainLoop.add(
			() -> {
				if(!done){
					done = true;
					trace('ok');
				}
				if(event !=null){
					trace("stop");
					event.stop();
				}
			}
		);
	}
}
