class Main2 {
	static function main() {
		var event : haxe.MainLoop.MainEvent = null;
		var count = 0;
		event = haxe.MainLoop.add(function() {
			trace(count++);
			if( count == 10 ) {
				event.stop();
				trace(haxe.MainLoop.hasEvents());
			}
		});
	}

}
