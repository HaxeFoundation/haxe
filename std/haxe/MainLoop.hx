package haxe;

typedef MainEvent = haxe.EventLoop.Event;

/**
	This class exists for backward compatibility. You should use haxe.EventLoop instead.
**/
class MainLoop {

	public static function add(f:Void->Void, priority = 0) : MainEvent {
		return EventLoop.main.add(f, priority);
	}

}
