/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:final @:native("$") extern class JQuery implements ArrayAccess<js.html.Element> {
	/**
		Add the previous set of elements on the stack to the current set.
	**/
	public function andSelf():js.jquery.JQuery;
	/**
		A string containing the jQuery version number.
	**/
	public var jquery : String;
	/**
		Get the HTML contents of the first element in the set of matched elements.
	**/
	@:overload(function(htmlString:String):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function html():String;
	/**
		Attach a function to be executed before an Ajax request is sent. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxSend(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Remove from the queue all items that have not yet been run.
	**/
	public function clearQueue(?queueName:String):js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements to the end of the target.
	**/
	public function appendTo(target:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, js.jquery.JQuery>>>>):js.jquery.JQuery;
	/**
		Bind an event handler to the "mousedown" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mousedown():js.jquery.JQuery;
	/**
		Retrieve the elements matched by the jQuery object.
	**/
	@:overload(function(index:Int):js.html.Element { })
	public function get():Array<js.html.Element>;
	/**
		Bind an event handler to the "keydown" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keydown():js.jquery.JQuery;
	/**
		Hide the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideUp(options:Dynamic):js.jquery.JQuery;
	/**
		Attach a function to be executed whenever an Ajax request completes successfully. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxSuccess(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Get the value of an attribute for the first element in the set of matched elements.
	**/
	@:overload(function(attributes:Dynamic):js.jquery.JQuery { })
	@:overload(function(attributeName:String, value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(attributeName:String, _function:Int -> String -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function attr(attributeName:String):String;
	/**
		Get all preceding siblings of each element up to but not including the element matched by the selector, DOM node, or jQuery object.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function prevUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Encode a set of form elements as an array of names and values.
	**/
	public function serializeArray():Array<Dynamic>;
	/**
		Get the siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function siblings(?selector:String):js.jquery.JQuery;
	/**
		Display the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideDown(options:Dynamic):js.jquery.JQuery;
	/**
		Get the combined text contents of each element in the set of matched elements, including their descendants.
	**/
	@:overload(function(text:haxe.extern.EitherType<String, haxe.extern.EitherType<Float, Bool>>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function text():String;
	/**
		Search for a given element from among the matched elements.
	**/
	@:overload(function(selector:String):Float { })
	@:overload(function(element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>):Float { })
	public function index():Float;
	/**
		Execute all handlers attached to an element for an event.
	**/
	@:overload(function(event:js.jquery.Event, ?extraParameters:haxe.extern.EitherType<Array<Dynamic>, Dynamic>):Dynamic { })
	public function triggerHandler(eventType:String, ?extraParameters:haxe.extern.EitherType<Array<Dynamic>, Dynamic>):Dynamic;
	/**
		Get the current computed height for the first element in the set of matched elements, including padding but not border.
	**/
	@:overload(function(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Float -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function innerHeight():Float;
	/**
		End the most recent filtering operation in the current chain and return the set of matched elements to its previous state.
	**/
	public function end():js.jquery.JQuery;
	/**
		Bind an event handler to the "dblclick" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function dblclick():js.jquery.JQuery;
	/**
		Get the immediately following sibling of each element in the set of matched elements. If a selector is provided, it retrieves the next sibling only if it matches that selector.
	**/
	public function next(?selector:String):js.jquery.JQuery;
	/**
		Get the ancestors of each element in the current set of matched elements, optionally filtered by a selector.
	**/
	public function parents(?selector:String):js.jquery.JQuery;
	/**
		Get the current vertical position of the scroll bar for the first element in the set of matched elements or set the vertical position of the scroll bar for every matched element.
	**/
	@:overload(function(value:Float):js.jquery.JQuery { })
	public function scrollTop():Int;
	/**
		Register a handler to be called when Ajax requests complete. This is an <a href="/Ajax_Events/">AjaxEvent</a>.
	**/
	public function ajaxComplete(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Bind an event handler to the "blur" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function blur():js.jquery.JQuery;
	/**
		Get the current computed height for the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Int -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function height():Float;
	/**
		Bind an event handler to the "focus" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focus():js.jquery.JQuery;
	/**
		Bind an event handler to the "keypress" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keypress():js.jquery.JQuery;
	/**
		Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	public function replaceWith(newContent:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Perform a custom animation of a set of CSS properties.
	**/
	@:overload(function(properties:Dynamic, ?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function animate(properties:Dynamic, options:Dynamic):js.jquery.JQuery;
	/**
		Adjust the opacity of the matched elements.
	**/
	@:overload(function(duration:haxe.extern.EitherType<String, Float>, opacity:Float, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeTo(duration:haxe.extern.EitherType<String, Float>, opacity:Float, ?complete:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Set a timer to delay execution of subsequent items in the queue.
	**/
	public function delay(duration:Int, ?queueName:String):js.jquery.JQuery;
	/**
		Bind an event handler to the "mouseout" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseout():js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, to the end of each element in the set of matched elements.
	**/
	@:overload(function(content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function append(_function:Int -> String -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Bind an event handler to the "focusin" event.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focusin(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Remove elements from the set of matched elements.
	**/
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):js.jquery.JQuery { })
	public function not(selector:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, Array<js.html.Element>>>):js.jquery.JQuery;
	/**
		Bind an event handler to be fired when the mouse leaves an element, or trigger that handler on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseleave():js.jquery.JQuery;
	/**
		Accepts a string containing a CSS selector which is then used to match a set of elements.
	**/
	@:selfCall
	@:overload(function(element:js.html.Element):Void { })
	@:overload(function(elementArray:haxe.extern.EitherType<js.html.NodeList, Array<js.html.Element>>):Void { })
	@:overload(function(selection:js.jquery.JQuery):Void { })
	@:overload(function(callback:haxe.Constraints.Function):Void { })
	@:overload(function(object:Dynamic):Void { })
	@:overload(function(html:String, attributes:Dynamic):Void { })
	@:overload(function(selector:String, ?context:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>):Void { })
	@:overload(function(html:String, ?ownerDocument:js.html.Document):Void { })
	public function new():Void;
	/**
		Attach a handler to one or more events for all elements that match the selector, now or in the future, based on a specific set of root elements.
	**/
	@:overload(function(selector:String, eventType:String, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String, eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function delegate(selector:String, events:Dynamic):js.jquery.JQuery;
	/**
		Attach an event handler function for one or more events to the selected elements.
	**/
	@:overload(function(events:String, ?selector:String, ?data:Dynamic, handler:js.jquery.Event -> haxe.extern.Rest<Dynamic> -> Void):js.jquery.JQuery { })
	public function on(events:Dynamic, ?selector:String, ?data:Dynamic):js.jquery.JQuery;
	/**
		Set the CSS outer width of each element in the set of matched elements.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?includeMargin:Bool):Float { })
	public function outerWidth(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery;
	/**
		Bind an event handler to the "resize" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function resize():js.jquery.JQuery;
	/**
		Return the value at the named data store for the first element in the jQuery collection, as set by data(name, value) or by an HTML5 data-* attribute.
	**/
	@:overload(function(key:String):Dynamic { })
	@:overload(function(obj:Dynamic):js.jquery.JQuery { })
	@:overload(function(key:String, value:Dynamic):js.jquery.JQuery { })
	public function data():Dynamic;
	/**
		Get all preceding siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function prevAll(?selector:String):js.jquery.JQuery;
	/**
		Remove a property for the set of matched elements.
	**/
	public function removeProp(propertyName:String):js.jquery.JQuery;
	/**
		Get the current value of the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<String, haxe.extern.EitherType<Float, Array<String>>>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function val():haxe.extern.EitherType<String, haxe.extern.EitherType<Float, Array<Dynamic>>>;
	/**
		Register a handler to be called when Ajax requests complete with an error. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxError(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> String -> Void):js.jquery.JQuery;
	/**
		Bind an event handler to the "click" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function click():js.jquery.JQuery;
	/**
		Bind an event handler to the "error" JavaScript event.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function error(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Bind a single handler to the matched elements, to be executed when the mouse pointer enters or leaves the elements.
	**/
	@:overload(function(handlerIn:js.jquery.Event -> Void, handlerOut:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function hover(handlerInOut:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
	**/
	@:overload(function(?className:String):js.jquery.JQuery { })
	public function removeClass(_function:Int -> String -> String):js.jquery.JQuery;
	/**
		Bind an event handler to the "scroll" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function scroll():js.jquery.JQuery;
	/**
		The number of elements in the jQuery object.
	**/
	public var length : Int;
	/**
		For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree.
	**/
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(element:js.html.Element):js.jquery.JQuery { })
	@:overload(function(selector:String, ?context:js.html.Element):js.jquery.JQuery { })
	public function closest(selector:String):js.jquery.JQuery;
	/**
		Remove all child nodes of the set of matched elements from the DOM.
	**/
	public function empty():js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, after each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery { })
	@:overload(function(content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function after(_function:Int -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Get the closest ancestor element that is positioned.
	**/
	public function offsetParent():js.jquery.JQuery;
	/**
		Bind an event handler to the "load" JavaScript event.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(url:String, ?data:haxe.extern.EitherType<Dynamic, String>, ?complete:String -> String -> js.jquery.JqXHR -> Void):js.jquery.JQuery { })
	public function load(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Remove the set of matched elements from the DOM.
	**/
	public function detach(?selector:String):js.jquery.JQuery;
	/**
		Bind an event handler to the "unload" JavaScript event.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function unload(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Bind an event handler to the "mouseup" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseup():js.jquery.JQuery;
	/**
		Iterate over a jQuery object, executing a function for each matched element. 
	**/
	public function each(_function:Int -> js.html.Element -> Void):js.jquery.JQuery;
	/**
		 Return a Promise object to observe when all actions of a certain type bound to the collection, queued or not, have finished. 
	**/
	public function promise(?type:String, ?target:Dynamic):js.jquery.Promise;
	/**
		Get the current computed width for the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Int -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function width():Float;
	/**
		Get the current coordinates of the first element in the set of matched elements, relative to the document.
	**/
	@:overload(function(coordinates:{ var top : Float; var left : Float; }):js.jquery.JQuery { })
	@:overload(function(_function:Int -> { var top : Float; var left : Float; } -> Dynamic):js.jquery.JQuery { })
	public function offset():{ var top : Float; var left : Float; };
	/**
		Set the CSS outer Height of each element in the set of matched elements.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?includeMargin:Bool):Float { })
	public function outerHeight(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery;
	/**
		Stop the currently-running animation on the matched elements.
	**/
	@:overload(function(?queue:String, ?clearQueue:Bool, ?jumpToEnd:Bool):js.jquery.JQuery { })
	public function stop(?clearQueue:Bool, ?jumpToEnd:Bool):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to those that match the selector or pass the function's test. 
	**/
	@:overload(function(elements:js.html.Element):js.jquery.JQuery { })
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):js.jquery.JQuery { })
	public function filter(selector:String):js.jquery.JQuery;
	/**
		Get all following siblings of each element up to but not including the element matched by the selector, DOM node, or jQuery object passed.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function nextUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Check the current matched set of elements against a selector, element, or jQuery object and return <code>true</code> if at least one of these elements matches the given arguments.
	**/
	@:overload(function(selection:js.jquery.JQuery):Bool { })
	@:overload(function(elements:js.html.Element):Bool { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):Bool { })
	public function is(selector:String):Bool;
	/**
		Pass each element in the current matched set through a function, producing a new jQuery object containing the return values.
	**/
	public function map(callback:Int -> js.html.Element -> Dynamic):js.jquery.JQuery;
	/**
		Remove the set of matched elements from the DOM.
	**/
	public function remove(?selector:String):js.jquery.JQuery;
	/**
		Return the number of elements in the jQuery object.
	**/
	public function size():Int;
	/**
		Hide the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function hide():js.jquery.JQuery;
	/**
		Remove a previously-attached event handler from the elements.
	**/
	@:overload(function(event:js.jquery.Event):js.jquery.JQuery { })
	@:overload(function(eventType:String, _false:Bool):js.jquery.JQuery { })
	@:overload(function(eventType:String, ?handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function unbind():js.jquery.JQuery;
	/**
		Adds the specified class(es) to each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function addClass(className:String):js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, before each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery { })
	@:overload(function(content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function before(_function:Int -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Get the descendants of each element in the current set of matched elements, filtered by a selector, jQuery object, or element.
	**/
	@:overload(function(element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>):js.jquery.JQuery { })
	public function find(selector:String):js.jquery.JQuery;
	/**
		Attach a handler to an event for the elements.
	**/
	@:overload(function(eventType:String, ?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(eventType:String, ?eventData:Dynamic, ?preventBubble:Bool):js.jquery.JQuery { })
	public function bind(events:Dynamic):js.jquery.JQuery;
	/**
		Display the matched elements by fading them to opaque.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeIn(options:Dynamic):js.jquery.JQuery;
	/**
		Get the ancestors of each element in the current set of matched elements, up to but not including the element matched by the selector, DOM node, or jQuery object.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function parentsUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Wrap an HTML structure around all elements in the set of matched elements.
	**/
	@:overload(function(_function:Int -> haxe.extern.EitherType<String, js.jquery.JQuery>):js.jquery.JQuery { })
	public function wrapAll(wrappingElement:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Add a collection of DOM elements onto the jQuery stack.
	**/
	@:overload(function(elements:haxe.extern.EitherType<Array<js.html.Element>, js.html.NodeList>, name:String, arguments:Array<Dynamic>):js.jquery.JQuery { })
	public function pushStack(elements:haxe.extern.EitherType<Array<js.html.Element>, js.html.NodeList>):js.jquery.JQuery;
	/**
		Retrieve all the elements contained in the jQuery set, as an array.
	**/
	public function toArray():Array<js.html.Element>;
	/**
		Get the immediately preceding sibling of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function prev(?selector:String):js.jquery.JQuery;
	/**
		Execute the next function on the queue for the matched elements.
	**/
	public function dequeue(?queueName:String):js.jquery.JQuery;
	/**
		Determine whether any of the matched elements are assigned the given class.
	**/
	public function hasClass(className:String):Bool;
	/**
		Bind an event handler to the "keyup" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keyup():js.jquery.JQuery;
	/**
		Bind an event handler to the "mousemove" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mousemove():js.jquery.JQuery;
	/**
		Get the children of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function children(?selector:String):js.jquery.JQuery;
	/**
		Execute all handlers and behaviors attached to the matched elements for the given event type.
	**/
	@:overload(function(event:js.jquery.Event, ?extraParameters:haxe.extern.EitherType<Array<Dynamic>, Dynamic>):js.jquery.JQuery { })
	public function trigger(eventType:String, ?extraParameters:haxe.extern.EitherType<Array<Dynamic>, Dynamic>):js.jquery.JQuery;
	/**
		Wrap an HTML structure around the content of each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String):js.jquery.JQuery { })
	public function wrapInner(wrappingElement:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.jquery.JQuery, js.html.Element>>>):js.jquery.JQuery;
	/**
		Remove an attribute from each element in the set of matched elements.
	**/
	public function removeAttr(attributeName:String):js.jquery.JQuery;
	/**
		Bind an event handler to the "focusout" JavaScript event.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focusout(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to a subset specified by a range of indices.
	**/
	public function slice(start:Int, ?end:Int):js.jquery.JQuery;
	/**
		Add or remove one or more classes from each element in the set of matched elements, depending on either the class's presence or the value of the state argument.
	**/
	@:overload(function(?state:Bool):js.jquery.JQuery { })
	@:overload(function(className:String, state:Bool):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> Bool -> String, ?state:Bool):js.jquery.JQuery { })
	public function toggleClass(className:String):js.jquery.JQuery;
	/**
		The DOM node context originally passed to <code>jQuery()</code>; if none was passed then context will likely be the document.
	**/
	public var context : js.html.Element;
	/**
		Get the current coordinates of the first element in the set of matched elements, relative to the offset parent.
	**/
	public function position():{ var top : Float; var left : Float; };
	/**
		Bind an event handler to the "mouseover" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseover():js.jquery.JQuery;
	/**
		Show the queue of functions to be executed on the matched elements.
	**/
	@:overload(function(?queueName:String, newQueue:Array<Void -> Void>):js.jquery.JQuery { })
	@:overload(function(?queueName:String, callback:haxe.Constraints.Function -> Void):js.jquery.JQuery { })
	public function queue(?queueName:String):Array<Void -> Void>;
	/**
		Reduce the set of matched elements to the first in the set.
	**/
	public function first():js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements before the target.
	**/
	public function insertBefore(target:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, js.jquery.JQuery>>>>):js.jquery.JQuery;
	/**
		Replace each target element with the set of matched elements.
	**/
	public function replaceAll(target:haxe.extern.EitherType<String, haxe.extern.EitherType<js.jquery.JQuery, haxe.extern.EitherType<Array<js.html.Element>, js.html.Element>>>):js.jquery.JQuery;
	/**
		Register a handler to be called when all Ajax requests have completed. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxStop(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Hide the matched elements by fading them to transparent.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeOut(options:Dynamic):js.jquery.JQuery;
	/**
		Attach a handler to an event for the elements. The handler is executed at most once per element per event type.
	**/
	@:overload(function(events:Dynamic, ?selector:String, ?data:Dynamic):js.jquery.JQuery { })
	@:overload(function(events:String, ?selector:String, ?data:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function one(events:String, ?data:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Get the parent of each element in the current set of matched elements, optionally filtered by a selector.
	**/
	public function parent(?selector:String):js.jquery.JQuery;
	/**
		Add the previous set of elements on the stack to the current set, optionally filtered by a selector.
	**/
	public function addBack(?selector:String):js.jquery.JQuery;
	/**
		Bind an event handler to the "change" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function change():js.jquery.JQuery;
	/**
		Bind an event handler to the "select" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function select():js.jquery.JQuery;
	/**
		Register a handler to be called when the first Ajax request begins. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxStart(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Get all following siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function nextAll(?selector:String):js.jquery.JQuery;
	/**
		Remove a handler from the event for all elements which match the current selector, based upon a specific set of root elements.
	**/
	@:overload(function(namespace:String):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String):js.jquery.JQuery { })
	@:overload(function(selector:String, events:Dynamic):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function undelegate():js.jquery.JQuery;
	/**
		Create a new jQuery object with elements added to the set of matched elements.
	**/
	@:overload(function(elements:js.html.Element):js.jquery.JQuery { })
	@:overload(function(html:String):js.jquery.JQuery { })
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(selector:String, context:js.html.Element):js.jquery.JQuery { })
	public function add(selector:String):js.jquery.JQuery;
	/**
		Bind an event handler to be fired when the mouse enters an element, or trigger that handler on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseenter():js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements to the beginning of the target.
	**/
	public function prependTo(target:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, js.jquery.JQuery>>>>):js.jquery.JQuery;
	/**
		Display or hide the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideToggle(options:Dynamic):js.jquery.JQuery;
	/**
		Encode a set of form elements as a string for submission.
	**/
	public function serialize():String;
	/**
		Specify a function to execute when the DOM is fully loaded.
	**/
	public function ready(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Display the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function show():js.jquery.JQuery;
	/**
		Get the children of each element in the set of matched elements, including text and comment nodes.
	**/
	public function contents():js.jquery.JQuery;
	/**
		Create a deep copy of the set of matched elements.
	**/
	@:overload(function(?withDataAndEvents:Bool, ?deepWithDataAndEvents:Bool):js.jquery.JQuery { })
	public function clone(?withDataAndEvents:Bool):js.jquery.JQuery;
	/**
		Stop the currently-running animation, remove all queued animations, and complete all animations for the matched elements.
	**/
	public function finish(?queue:String):js.jquery.JQuery;
	/**
		Get the value of a property for the first element in the set of matched elements.
	**/
	@:overload(function(properties:Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, _function:Int -> Dynamic -> Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, value:Dynamic):js.jquery.JQuery { })
	public function prop(propertyName:String):Dynamic;
	/**
		Display or hide the matched elements by animating their opacity.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeToggle(options:Dynamic):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to the one at the specified index.
	**/
	@:overload(function(indexFromEnd:Int):js.jquery.JQuery { })
	public function eq(index:Int):js.jquery.JQuery;
	/**
		Get the current computed inner width for the first element in the set of matched elements, including padding but not border.
	**/
	@:overload(function(value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Float -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function innerWidth():Float;
	/**
		Get the current horizontal position of the scroll bar for the first element in the set of matched elements.
	**/
	@:overload(function(value:Float):js.jquery.JQuery { })
	public function scrollLeft():Int;
	/**
		Bind an event handler to the "submit" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function submit():js.jquery.JQuery;
	/**
		Display or hide the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function toggle(display:Bool):js.jquery.JQuery;
	/**
		Get the computed style properties for the first element in the set of matched elements.
	**/
	@:overload(function(propertyNames:Array<String>):String { })
	@:overload(function(properties:Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, value:haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	@:overload(function(propertyName:String, _function:Int -> String -> haxe.extern.EitherType<String, Float>):js.jquery.JQuery { })
	public function css(propertyName:String):String;
	/**
		Remove an event handler.
	**/
	@:overload(function(event:js.jquery.Event):js.jquery.JQuery { })
	@:overload(function(events:Dynamic, ?selector:String):js.jquery.JQuery { })
	@:overload(function(events:String, ?selector:String, ?handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function off():js.jquery.JQuery;
	/**
		Reduce the set of matched elements to those that have a descendant that matches the selector or DOM element.
	**/
	@:overload(function(contained:js.html.Element):js.jquery.JQuery { })
	public function has(selector:String):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to the final one in the set.
	**/
	public function last():js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.
	**/
	@:overload(function(content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function prepend(_function:Int -> String -> haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Remove a previously-stored piece of data.
	**/
	@:overload(function(?list:haxe.extern.EitherType<Array<String>, String>):js.jquery.JQuery { })
	public function removeData(?name:String):js.jquery.JQuery;
	/**
		Wrap an HTML structure around each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> haxe.extern.EitherType<String, js.jquery.JQuery>):js.jquery.JQuery { })
	public function wrap(wrappingElement:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Remove the parents of the set of matched elements from the DOM, leaving the matched elements in their place.
	**/
	public function unwrap():js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements after the target.
	**/
	public function insertAfter(target:haxe.extern.EitherType<String, haxe.extern.EitherType<String, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, js.jquery.JQuery>>>>):js.jquery.JQuery;
	/**
		Compile-time short cut to JQueryStatic.
	**/
	inline static public var _static = js.jquery.JQueryStatic;
}