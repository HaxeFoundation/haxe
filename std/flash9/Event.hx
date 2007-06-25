package flash;

import flash.events.MouseEvent;

typedef MEvent = Event<MouseEvent,flash.display.InteractiveObject>;

class Event<T,Target : flash.events.EventDispatcher> {

	var name : String;
	var onAdd : Target -> Void;

	function new(name,onAdd) {
		this.name = name;
		this.onAdd = onAdd;
	}

	public function bind( t : Target, f : Void -> Void ) {
		t.addEventListener(name,function(_) { f(); });
		onAdd(t);
	}

	public function ebind( t : Target, f : T -> Void ) {
		t.addEventListener(name,f);
		onAdd(t);
	}

	public function lazyBind( t : Target, fval : Void -> Void ) {
		for( f in Type.getInstanceFields(Type.getClass(t)) )
			if( Reflect.field(t,f) == fval ) {
				t.addEventListener(name,function(_) {
					Reflect.field(t,f)();
				});
				onAdd(t);
				return;
			}
		throw "Function value for event "+name+" not found in "+t.toString();
	}

	public function toString() {
		return "Event:"+name;
	}

	static function mouseEnable( t : flash.display.InteractiveObject ) {
		t.mouseEnabled = true;
	}

	static function dcEnable( t : flash.display.InteractiveObject ) {
		t.mouseEnabled = true;
		t.doubleClickEnabled = true;
	}

	public static var click = new MEvent(MouseEvent.CLICK,mouseEnable);
	public static var doubleClick = new MEvent(MouseEvent.DOUBLE_CLICK,dcEnable);
	public static var over = new MEvent(MouseEvent.MOUSE_OVER,mouseEnable);
	public static var out = new MEvent(MouseEvent.MOUSE_OUT,mouseEnable);
	public static var down = new MEvent(MouseEvent.MOUSE_DOWN,mouseEnable);
	public static var up = new MEvent(MouseEvent.MOUSE_UP,mouseEnable);
	public static var move = new MEvent(MouseEvent.MOUSE_MOVE,mouseEnable);

	public static function drag( t : flash.display.InteractiveObject, f : Void -> Void ) {
		var stage = flash.Lib.current.stage;
		var mouseX = stage.mouseX;
		var mouseY = stage.mouseY;
		var fevent = function(e) {
			if( mouseX != stage.mouseX || mouseY != stage.mouseY ) {
				mouseX = stage.mouseX;
				mouseY = stage.mouseY;
				f();
			}
		};
		var stop;
		stop = function(e) {
			t.removeEventListener(flash.events.Event.ENTER_FRAME,fevent);
			stage.removeEventListener(flash.events.MouseEvent.MOUSE_UP,stop);
		};
		t.addEventListener(flash.events.Event.ENTER_FRAME,fevent);
		stage.addEventListener(flash.events.MouseEvent.MOUSE_UP,stop);
	}

}