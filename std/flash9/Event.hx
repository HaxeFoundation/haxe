package flash;

import flash.events.MouseEvent;

typedef MEvent = Event<MouseEvent,flash.display.DisplayObject>;

class Event<T,Target : flash.events.EventDispatcher> {

	var name : String;

	function new(name) {
		this.name = name;
	}

	public function bind( t : Target, f : Void -> Void ) {
		t.addEventListener(name,function(_) { f(); });
	}

	public function ebind( t : Target, f : T -> Void ) {
		t.addEventListener(name,f);
	}

	public function lazyBind( t : Target, fval : Void -> Void ) {
		for( f in Type.getInstanceFields(Type.getClass(t)) )
			if( Reflect.field(t,f) == fval ) {
				t.addEventListener(name,function(_) {
					Reflect.field(t,f)();
				});
				return;
			}
		throw "Function value for event "+name+" not found in "+t.toString();
	}

	public function toString() {
		return "Event:"+name;
	}

	public static var click = new MEvent(MouseEvent.CLICK);
	public static var doubleClick = new MEvent(MouseEvent.DOUBLE_CLICK);
	public static var over = new MEvent(MouseEvent.MOUSE_OVER);
	public static var out = new MEvent(MouseEvent.MOUSE_OUT);

}