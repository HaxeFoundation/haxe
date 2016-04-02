class Main {

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


		var t = new haxe.Timer(200);
		var count = 0;
		t.run = function() {
			trace("T" + count++);
			if( count == 5 )
				t.stop();
		};

		haxe.MainLoop.addThread(function() {
			var event : haxe.MainLoop.MainEvent = null;
			var count = 0;
			event = haxe.MainLoop.add(function() {
				trace(String.fromCharCode("A".code + count++));
				if( count == 5 ) event.stop();
			});
			#if neko
			Sys.sleep(3);
			#end
		});
	}

}