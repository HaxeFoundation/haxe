class Main{
	static public function main(){
		var event = null;
		event = haxe.MainLoop.add(
			() -> {
				event.stop();
			}
		);
	}
}