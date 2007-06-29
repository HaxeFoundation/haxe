/*
 * Copyright (c) 2005-2007, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package flash;

import flash.events.MouseEvent;
import flash.events.EventDispatcher;

typedef MEvent = Event<MouseEvent,flash.display.InteractiveObject>;

class Event<T,Target : EventDispatcher> {

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
		#if flash9
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
			stage.removeEventListener(MouseEvent.MOUSE_UP,stop);
		};
		t.addEventListener(flash.events.Event.ENTER_FRAME,fevent);
		stage.addEventListener(MouseEvent.MOUSE_UP,stop);
		#end
	}

}
