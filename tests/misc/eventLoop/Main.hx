class Main {

	static function main() {
		var event : haxe.MainLoop.MainEvent = null;
		var count = 0;
		event = haxe.MainLoop.add(function() {
			trace(count++);
			if( count == 10 ) {
				event.stop();
				trace(haxe.EventLoop.main.hasEvents());
			}
		});

		#if sys
		// if we don't use native timer, this should execute before
		var t = new haxe.Timer(100);
		t.run = function() {
			trace("BEFORE1");
			t.stop();
		};
		#end

		var t = new haxe.Timer(200);
		var count = 0;
		t.run = function() {
			trace("T" + count++);
			if( count == 5 )
				t.stop();
		};

		#if sys
		// if we don't use native timer, this should execute before
		var t = new haxe.Timer(100);
		t.run = function() {
			trace("BEFORE2");
			t.stop();
		};
		#end

		#if sys
		Sys.sleep(0.3);
		#end

		haxe.EventLoop.addTask(function() {
			var event = haxe.EventLoop.main.addAsync();
			var count = 0;
			event.start(function() {
				trace(String.fromCharCode("A".code + count++));
				if( count == 5 ) event.stop();
			});
		});
	}

}
