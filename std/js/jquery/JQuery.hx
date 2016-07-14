/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:native("$") extern class JQuery implements ArrayAccess<js.html.Element> {
	/**
		A multi-purpose callbacks list object that provides a powerful way to manage callback lists.
	**/
	static public function Callbacks(flags:String):js.jquery.Callbacks;
	/**
		A factory function that returns a chainable utility object with methods to register multiple callbacks into callback queues, invoke callback queues, and relay the success or failure state of any synchronous or asynchronous function.
	**/
	static public function Deferred(?beforeStart:js.jquery.Deferred -> Void):js.jquery.Deferred;
	/**
		Perform an asynchronous HTTP (Ajax) request.
	**/
	@:overload(function(url:String, ?settings:Dynamic):js.jquery.JqXHR { })
	static public function ajax(?settings:Dynamic):js.jquery.JqXHR;
	/**
		Handle custom Ajax options or modify existing options before each request is sent and before they are processed by <code>$.ajax()</code>.
	**/
	static public function ajaxPrefilter(?dataTypes:String, handler:Dynamic -> Dynamic -> js.jquery.JqXHR -> Void):Void;
	/**
		Set default values for future Ajax requests. Its use is not recommended.
	**/
	static public function ajaxSetup(options:Dynamic):Void;
	/**
		Creates an object that handles the actual transmission of Ajax data.
	**/
	static public function ajaxTransport(dataType:String, handler:Dynamic -> Dynamic -> js.jquery.JqXHR -> Void):Void;
	/**
		States if the current page, in the user's browser, is being rendered using the <a href="http://www.w3.org/TR/REC-CSS2/box.html">W3C CSS Box Model</a>. <strong>This property was removed in jQuery 1.8</strong>. Please try to use feature detection instead.
	**/
	@:deprecated("Deprecated since jQuery 1.3")
	static public var boxModel : Bool;
	/**
		Contains flags for the useragent, read from navigator.userAgent. <strong>This property was removed in jQuery 1.9</strong> and is available only through the jQuery.migrate plugin. Please try to use feature detection instead.
	**/
	@:deprecated("Deprecated since jQuery 1.3")
	static public var browser : Dynamic;
	/**
		Check to see if a DOM element is a descendant of another DOM element.
	**/
	static public function contains(container:js.html.Element, contained:js.html.Element):Bool;
	/**
		Hook directly into jQuery to override how particular CSS properties are retrieved or set, normalize CSS property naming, or create custom properties.
	**/
	static public var cssHooks : Dynamic;
	/**
		An object containing all CSS properties that may be used without a unit. The <a href="/css/"><code>.css()</code></a> method uses this object to see if it may append <code>px</code> to unitless values.
	**/
	static public var cssNumber : Dynamic;
	/**
		Returns value at named data store for the element, as set by <code>jQuery.data(element, name, value)</code>, or the full data store for the element.
		OR
		Store arbitrary data associated with the specified element. Returns the value that was set.
	**/
	@:overload(function(element:js.html.Element, key:String):Dynamic { })
	@:overload(function(element:js.html.Element, key:String, value:Dynamic):Dynamic { })
	static public function data(element:js.html.Element):Dynamic;
	/**
		Execute the next function on the queue for the matched element.
	**/
	static public function dequeue(element:js.html.Element, ?queueName:String):Void;
	/**
		A generic iterator function, which can be used to seamlessly iterate over both objects and arrays. Arrays and array-like objects with a length property (such as a function's arguments object) are iterated by numeric index, from 0 to length-1. Other objects are iterated via their named properties.
	**/
	@:overload(function(object:Dynamic, callback:String -> Dynamic -> Void):Dynamic { })
	static public function each(array:Array<Dynamic>, callback:Int -> Dynamic -> Void):Dynamic;
	/**
		Takes a string and throws an exception containing it.
	**/
	static public function error(message:String):Void;
	/**
		Escapes any character that has a special meaning in a CSS selector.
	**/
	static public function escapeSelector(selector:String):String;
	/**
		Merge the contents of two or more objects together into the first object.
	**/
	@:overload(function(?deep:Bool, target:Dynamic, object1:Dynamic, ?objectN:Dynamic):Dynamic { })
	static public function extend(target:Dynamic, ?object1:Dynamic, ?objectN:Dynamic):Dynamic;
	/**
		An alias to `jQuery.prototype`.
	**/
	static public var fn : Dynamic;
	static public var fx : { /**
		The rate (in milliseconds) at which animations fire.
	**/
	var interval : Float; /**
		Globally disable all animations.
	**/
	var off : Bool; };
	/**
		Load data from the server using a HTTP GET request.
	**/
	@:overload(function(url:String, ?data:Dynamic, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void, ?dataType:String):js.jquery.JqXHR { })
	static public function get(settings:Dynamic):js.jquery.JqXHR;
	/**
		Load JSON-encoded data from the server using a GET HTTP request.
	**/
	static public function getJSON(url:String, ?data:Dynamic, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void):js.jquery.JqXHR;
	/**
		Load a JavaScript file from the server using a GET HTTP request, then execute it.
	**/
	static public function getScript(url:String, ?success:String -> String -> js.jquery.JqXHR -> Void):js.jquery.JqXHR;
	/**
		Execute some JavaScript code globally.
	**/
	static public function globalEval(code:String):Void;
	/**
		Finds the elements of an array which satisfy a filter function. The original array is not affected.
	**/
	static public function grep(array:haxe.extern.EitherType<Array<Dynamic>, js.html.NodeList>, _function:Dynamic -> Int -> Bool, ?invert:Bool):Array<Dynamic>;
	/**
		Determine whether an element has any jQuery data associated with it.
	**/
	static public function hasData(element:js.html.Element):Bool;
	/**
		Holds or releases the execution of jQuery's ready event.
	**/
	static public function holdReady(hold:Bool):Void;
	/**
		Modify and filter HTML strings passed through <a href="/category/manipulation/">jQuery manipulation methods</a>.
	**/
	static public function htmlPrefilter(html:String):String;
	/**
		Search for a specified value within an array and return its index (or -1 if not found).
	**/
	static public function inArray(value:Dynamic, array:Array<Dynamic>, ?fromIndex:Float):Float;
	/**
		Determine whether the argument is an array.
	**/
	static public function isArray(obj:Dynamic):Bool;
	/**
		Check to see if an object is empty (contains no enumerable properties).
	**/
	static public function isEmptyObject(object:Dynamic):Bool;
	/**
		Determine if the argument passed is a JavaScript function object.
	**/
	static public function isFunction(obj:Dynamic):Bool;
	/**
		Determines whether its argument represents a JavaScript number.
	**/
	static public function isNumeric(value:Dynamic):Bool;
	/**
		Check to see if an object is a plain object (created using "{}" or "new Object").
	**/
	static public function isPlainObject(object:Dynamic):Bool;
	/**
		Determine whether the argument is a window.
	**/
	static public function isWindow(obj:Dynamic):Bool;
	/**
		Check to see if a DOM node is within an XML document (or is an XML document).
	**/
	static public function isXMLDoc(node:js.html.Element):Bool;
	/**
		Convert an array-like object into a true JavaScript array.
	**/
	static public function makeArray(obj:Dynamic):Array<Dynamic>;
	/**
		Translate all items in an array or object to new array of items.
	**/
	@:overload(function(object:Dynamic, callback:Dynamic -> String -> Dynamic):Array<Dynamic> { })
	static public function map(array:Array<Dynamic>, callback:Dynamic -> Int -> Dynamic):Array<Dynamic>;
	/**
		Merge the contents of two arrays together into the first array.
	**/
	static public function merge(first:haxe.extern.EitherType<Array<Dynamic>, js.html.NodeList>, second:haxe.extern.EitherType<Array<Dynamic>, js.html.NodeList>):Array<Dynamic>;
	/**
		Relinquish jQuery's control of the <code>$</code> variable.
	**/
	static public function noConflict(?removeAll:Bool):Dynamic;
	/**
		An empty function.
	**/
	static public function noop():Void;
	/**
		Return a number representing the current time.
	**/
	static public function now():Float;
	/**
		Create a serialized representation of an array, a plain object, or a jQuery object suitable for use in a URL query string or Ajax request. In case a jQuery object is passed, it should contain input elements with name/value properties.
	**/
	@:overload(function(obj:Dynamic, traditional:Bool):String { })
	static public function param(obj:Dynamic):String;
	/**
		Parses a string into an array of DOM nodes.
	**/
	static public function parseHTML(data:String, ?context:js.html.Element, ?keepScripts:Bool):Array<js.html.Element>;
	/**
		Takes a well-formed JSON string and returns the resulting JavaScript value.
	**/
	@:deprecated("Deprecated since jQuery 3.0")
	static public function parseJSON(json:String):Dynamic;
	/**
		Parses a string into an XML document.
	**/
	static public function parseXML(data:String):js.html.Document;
	/**
		Load data from the server using a HTTP POST request.
	**/
	@:overload(function(url:String, ?data:Dynamic, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void, ?dataType:String):js.jquery.JqXHR { })
	static public function post(settings:Dynamic):js.jquery.JqXHR;
	/**
		Takes a function and returns a new one that will always have a particular context.
	**/
	@:overload(function(context:Dynamic, name:String):haxe.Constraints.Function { })
	@:overload(function(_function:haxe.Constraints.Function, context:Dynamic, ?additionalArguments:Dynamic):haxe.Constraints.Function { })
	@:overload(function(context:Dynamic, name:String, ?additionalArguments:Dynamic):haxe.Constraints.Function { })
	static public function proxy(_function:haxe.Constraints.Function, context:Dynamic):haxe.Constraints.Function;
	/**
		Show the queue of functions to be executed on the matched element.
		OR
		Manipulate the queue of functions to be executed on the matched element.
	**/
	@:overload(function(element:js.html.Element, queueName:String, newQueue:Array<Void -> Void>):js.jquery.JQuery { })
	@:overload(function(element:js.html.Element, queueName:String, callback:haxe.Constraints.Function):js.jquery.JQuery { })
	static public function queue(element:js.html.Element, ?queueName:String):Array<Void -> Void>;
	/**
		Handles errors thrown synchronously in functions wrapped in <code>jQuery()</code>.
	**/
	static public dynamic function readyException(error:js.Error):String;
	/**
		Remove a previously-stored piece of data.
	**/
	static public function removeData(element:js.html.Element, ?name:String):js.jquery.JQuery;
	/**
		Creates an object containing a set of properties ready to be used in the definition of custom animations.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?settings:Dynamic):Dynamic { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):Dynamic { })
	static public function speed(settings:Dynamic):Dynamic;
	/**
		Creates a new copy of jQuery whose properties and methods can be modified without affecting the original jQuery object.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	static public function sub():js.jquery.JQuery;
	/**
		A collection of properties that represent the presence of different browser features or bugs. Intended for jQuery's internal use; specific properties may be removed when they are no longer needed internally to improve page startup performance. For your own project's feature-detection needs, we strongly recommend the use of an external library such as <a href="http://modernizr.com">Modernizr</a> instead of dependency on properties in <code>jQuery.support</code>.
	**/
	@:deprecated("Deprecated since jQuery 1.9")
	static public var support : Dynamic;
	/**
		Remove the whitespace from the beginning and end of a string.
	**/
	static public function trim(str:String):String;
	/**
		Determine the internal JavaScript [[Class]] of an object.
	**/
	static public function type(obj:Dynamic):String;
	/**
		Sorts an array of DOM elements, in place, with the duplicates removed. Note that this only works on arrays of DOM elements, not strings or numbers.
	**/
	@:deprecated("Deprecated since jQuery 3.0")
	static public function unique(array:Array<js.html.Element>):Array<js.html.Element>;
	/**
		Sorts an array of DOM elements, in place, with the duplicates removed. Note that this only works on arrays of DOM elements, not strings or numbers.
	**/
	static public function uniqueSort(array:Array<js.html.Element>):Array<js.html.Element>;
	/**
		Provides a way to execute callback functions based on zero or more objects, usually <a href="/category/deferred-object/">Deferred</a> objects that represent asynchronous events.
	**/
	static public function when(deferreds:haxe.extern.Rest<js.jquery.Deferred>):js.jquery.Promise;
	/**
		Create a new jQuery object with elements added to the set of matched elements.
	**/
	@:overload(function(elements:js.html.Element):js.jquery.JQuery { })
	@:overload(function(html:String):js.jquery.JQuery { })
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(selector:String, context:js.html.Element):js.jquery.JQuery { })
	public function add(selector:String):js.jquery.JQuery;
	/**
		Add the previous set of elements on the stack to the current set, optionally filtered by a selector.
	**/
	public function addBack(?selector:String):js.jquery.JQuery;
	/**
		Adds the specified class(es) to each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function addClass(className:String):js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, after each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery { })
	@:overload(function(content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function after(_function:Int -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Register a handler to be called when Ajax requests complete. This is an <a href="/Ajax_Events/">AjaxEvent</a>.
	**/
	public function ajaxComplete(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Register a handler to be called when Ajax requests complete with an error. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxError(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> String -> Void):js.jquery.JQuery;
	/**
		Attach a function to be executed before an Ajax request is sent. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxSend(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Register a handler to be called when the first Ajax request begins. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxStart(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Register a handler to be called when all Ajax requests have completed. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxStop(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Attach a function to be executed whenever an Ajax request completes successfully. This is an <a href="/Ajax_Events/">Ajax Event</a>.
	**/
	public function ajaxSuccess(handler:js.jquery.Event -> js.jquery.JqXHR -> Dynamic -> Dynamic -> Void):js.jquery.JQuery;
	/**
		Add the previous set of elements on the stack to the current set.
	**/
	@:deprecated("Deprecated since jQuery 1.8")
	public function andSelf():js.jquery.JQuery;
	/**
		Perform a custom animation of a set of CSS properties.
	**/
	@:overload(function(properties:Dynamic, ?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function animate(properties:Dynamic, options:Dynamic):js.jquery.JQuery;
	/**
		Insert content, specified by the parameter, to the end of each element in the set of matched elements.
	**/
	@:overload(function(content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function append(_function:Int -> String -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements to the end of the target.
	**/
	public function appendTo(target:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Set one or more attributes for the set of matched elements.
		OR
		Get the value of an attribute for the first element in the set of matched elements.
	**/
	@:overload(function(attributes:Dynamic):js.jquery.JQuery { })
	@:overload(function(attributeName:String, value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(attributeName:String, _function:Int -> String -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function attr(attributeName:String):String;
	/**
		Insert content, specified by the parameter, before each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery { })
	@:overload(function(content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function before(_function:Int -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Attach a handler to an event for the elements.
	**/
	@:overload(function(eventType:String, ?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(eventType:String, ?eventData:Dynamic, ?preventBubble:Bool):js.jquery.JQuery { })
	public function bind(events:Dynamic):js.jquery.JQuery;
	/**
		Bind an event handler to the "blur" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function blur():js.jquery.JQuery;
	/**
		Bind an event handler to the "change" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function change():js.jquery.JQuery;
	/**
		Get the children of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function children(?selector:String):js.jquery.JQuery;
	/**
		Remove from the queue all items that have not yet been run.
	**/
	public function clearQueue(?queueName:String):js.jquery.JQuery;
	/**
		Bind an event handler to the "click" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function click():js.jquery.JQuery;
	/**
		Create a deep copy of the set of matched elements.
	**/
	@:overload(function(?withDataAndEvents:Bool, ?deepWithDataAndEvents:Bool):js.jquery.JQuery { })
	public function clone(?withDataAndEvents:Bool):js.jquery.JQuery;
	/**
		For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree.
		OR
		Get an array of all the elements and selectors matched against the current element up through the DOM tree.
	**/
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(element:js.html.Element):js.jquery.JQuery { })
	@:overload(function(selector:String, ?context:js.html.Element):js.jquery.JQuery { })
	@:overload(function(selectors:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<String, js.jquery.JQuery>>, ?context:js.html.Element):Array<Dynamic> { })
	public function closest(selector:String):js.jquery.JQuery;
	/**
		Get the children of each element in the set of matched elements, including text and comment nodes.
	**/
	public function contents():js.jquery.JQuery;
	/**
		The DOM node context originally passed to <code>jQuery()</code>; if none was passed then context will likely be the document.
	**/
	@:deprecated("Deprecated since jQuery 1.10")
	public var context : js.html.Element;
	/**
		Bind an event handler to the "contextmenu" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function contextmenu():js.jquery.JQuery;
	/**
		Set one or more CSS properties for the set of matched elements.
		OR
		Get the computed style properties for the first element in the set of matched elements.
	**/
	@:overload(function(propertyNames:Array<String>):String { })
	@:overload(function(properties:Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(propertyName:String, _function:Int -> String -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function css(propertyName:String):String;
	/**
		Return the value at the named data store for the first element in the jQuery collection, as set by data(name, value) or by an HTML5 data-* attribute.
		OR
		Store arbitrary data associated with the matched elements.
	**/
	@:overload(function(key:String):Dynamic { })
	@:overload(function(obj:Dynamic):js.jquery.JQuery { })
	@:overload(function(key:String, value:Dynamic):js.jquery.JQuery { })
	public function data():Dynamic;
	/**
		Bind an event handler to the "dblclick" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function dblclick():js.jquery.JQuery;
	/**
		Set a timer to delay execution of subsequent items in the queue.
	**/
	public function delay(duration:Int, ?queueName:String):js.jquery.JQuery;
	/**
		Attach a handler to one or more events for all elements that match the selector, now or in the future, based on a specific set of root elements.
	**/
	@:overload(function(selector:String, eventType:String, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String, eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function delegate(selector:String, events:Dynamic):js.jquery.JQuery;
	/**
		Execute the next function on the queue for the matched elements.
	**/
	public function dequeue(?queueName:String):js.jquery.JQuery;
	/**
		Remove the set of matched elements from the DOM.
	**/
	public function detach(?selector:String):js.jquery.JQuery;
	/**
		Remove event handlers previously attached using <code>.live()</code> from the elements.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	@:overload(function(events:Dynamic):js.jquery.JQuery { })
	@:overload(function(eventType:String, ?handler:String):js.jquery.JQuery { })
	public function die():js.jquery.JQuery;
	/**
		Iterate over a jQuery object, executing a function for each matched element.
	**/
	public function each(_function:Int -> js.html.Element -> Void):js.jquery.JQuery;
	/**
		Remove all child nodes of the set of matched elements from the DOM.
	**/
	public function empty():js.jquery.JQuery;
	/**
		End the most recent filtering operation in the current chain and return the set of matched elements to its previous state.
	**/
	public function end():js.jquery.JQuery;
	/**
		Reduce the set of matched elements to the one at the specified index.
	**/
	@:overload(function(indexFromEnd:Int):js.jquery.JQuery { })
	public function eq(index:Int):js.jquery.JQuery;
	/**
		Bind an event handler to the "error" JavaScript event.
	**/
	@:deprecated("Deprecated since jQuery 1.8")
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function error(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Display the matched elements by fading them to opaque.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeIn(options:Dynamic):js.jquery.JQuery;
	/**
		Hide the matched elements by fading them to transparent.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeOut(options:Dynamic):js.jquery.JQuery;
	/**
		Adjust the opacity of the matched elements.
	**/
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, opacity:Float, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeTo(duration:haxe.extern.EitherType<Float, String>, opacity:Float, ?complete:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Display or hide the matched elements by animating their opacity.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function fadeToggle(options:Dynamic):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to those that match the selector or pass the function's test.
	**/
	@:overload(function(elements:js.html.Element):js.jquery.JQuery { })
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):js.jquery.JQuery { })
	public function filter(selector:String):js.jquery.JQuery;
	/**
		Get the descendants of each element in the current set of matched elements, filtered by a selector, jQuery object, or element.
	**/
	@:overload(function(element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>):js.jquery.JQuery { })
	public function find(selector:String):js.jquery.JQuery;
	/**
		Stop the currently-running animation, remove all queued animations, and complete all animations for the matched elements.
	**/
	public function finish(?queue:String):js.jquery.JQuery;
	/**
		Reduce the set of matched elements to the first in the set.
	**/
	public function first():js.jquery.JQuery;
	/**
		Bind an event handler to the "focus" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focus():js.jquery.JQuery;
	/**
		Bind an event handler to the "focusin" event.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focusin():js.jquery.JQuery;
	/**
		Bind an event handler to the "focusout" JavaScript event.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function focusout():js.jquery.JQuery;
	/**
		Retrieve one of the elements matched by the jQuery object.
		OR
		Retrieve the elements matched by the jQuery object.
	**/
	@:overload(function(index:Int):js.html.Element { })
	public function get():Array<js.html.Element>;
	/**
		Reduce the set of matched elements to those that have a descendant that matches the selector or DOM element.
	**/
	@:overload(function(contained:js.html.Element):js.jquery.JQuery { })
	public function has(selector:String):js.jquery.JQuery;
	/**
		Determine whether any of the matched elements are assigned the given class.
	**/
	public function hasClass(className:String):Bool;
	/**
		Set the CSS height of every matched element.
		OR
		Get the current computed height for the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Int -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function height():Float;
	/**
		Hide the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function hide():js.jquery.JQuery;
	/**
		Bind two handlers to the matched elements, to be executed when the mouse pointer enters and leaves the elements.
		OR
		Bind a single handler to the matched elements, to be executed when the mouse pointer enters or leaves the elements.
	**/
	@:overload(function(handlerIn:js.jquery.Event -> Void, handlerOut:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function hover(handlerInOut:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Get the HTML contents of the first element in the set of matched elements.
		OR
		Set the HTML contents of each element in the set of matched elements.
	**/
	@:overload(function(htmlString:String):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function html():String;
	/**
		Search for a given element from among the matched elements.
	**/
	@:overload(function(selector:String):Int { })
	@:overload(function(element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>):Int { })
	public function index():Int;
	/**
		Set the CSS inner height of each element in the set of matched elements.
		OR
		Get the current computed height for the first element in the set of matched elements, including padding but not border.
	**/
	@:overload(function(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Float -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function innerHeight():Float;
	/**
		Get the current computed inner width for the first element in the set of matched elements, including padding but not border.
		OR
		Set the CSS inner width of each element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Float -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function innerWidth():Float;
	/**
		Insert every element in the set of matched elements after the target.
	**/
	public function insertAfter(target:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements before the target.
	**/
	public function insertBefore(target:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Check the current matched set of elements against a selector, element, or jQuery object and return <code>true</code> if at least one of these elements matches the given arguments.
	**/
	@:overload(function(selection:js.jquery.JQuery):Bool { })
	@:overload(function(elements:js.html.Element):Bool { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):Bool { })
	public function is(selector:String):Bool;
	/**
		A string containing the jQuery version number.
	**/
	public var jquery : String;
	/**
		Bind an event handler to the "keydown" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keydown():js.jquery.JQuery;
	/**
		Bind an event handler to the "keypress" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keypress():js.jquery.JQuery;
	/**
		Bind an event handler to the "keyup" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function keyup():js.jquery.JQuery;
	/**
		Reduce the set of matched elements to the final one in the set.
	**/
	public function last():js.jquery.JQuery;
	/**
		The number of elements in the jQuery object.
	**/
	public var length(default, null) : Int;
	/**
		Attach an event handler for all elements which match the current selector, now and in the future.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	@:overload(function(events:String, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(events:String, ?data:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function live(events:Dynamic):js.jquery.JQuery;
	/**
		Bind an event handler to the "load" JavaScript event.
		OR
		Load data from the server and place the returned HTML into the matched element.
	**/
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(url:String, ?data:Dynamic, ?complete:String -> String -> js.jquery.JqXHR -> Void):js.jquery.JQuery { })
	public function load(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Pass each element in the current matched set through a function, producing a new jQuery object containing the return values.
	**/
	public function map(callback:Int -> js.html.Element -> Dynamic):js.jquery.JQuery;
	/**
		Bind an event handler to the "mousedown" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mousedown():js.jquery.JQuery;
	/**
		Bind an event handler to be fired when the mouse enters an element, or trigger that handler on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseenter():js.jquery.JQuery;
	/**
		Bind an event handler to be fired when the mouse leaves an element, or trigger that handler on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseleave():js.jquery.JQuery;
	/**
		Bind an event handler to the "mousemove" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mousemove():js.jquery.JQuery;
	/**
		Bind an event handler to the "mouseout" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseout():js.jquery.JQuery;
	/**
		Bind an event handler to the "mouseover" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseover():js.jquery.JQuery;
	/**
		Bind an event handler to the "mouseup" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function mouseup():js.jquery.JQuery;
	/**
		Creates DOM elements on the fly from the provided string of raw HTML.
		OR
		Accepts a string containing a CSS selector which is then used to match a set of elements.
		OR
		Binds a function to be executed when the DOM has finished loading.
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
		Get the immediately following sibling of each element in the set of matched elements. If a selector is provided, it retrieves the next sibling only if it matches that selector.
	**/
	public function next(?selector:String):js.jquery.JQuery;
	/**
		Get all following siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function nextAll(?selector:String):js.jquery.JQuery;
	/**
		Get all following siblings of each element up to but not including the element matched by the selector, DOM node, or jQuery object passed.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function nextUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Remove elements from the set of matched elements.
	**/
	@:overload(function(selection:js.jquery.JQuery):js.jquery.JQuery { })
	@:overload(function(_function:Int -> js.html.Element -> Bool):js.jquery.JQuery { })
	public function not(selector:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, String>>):js.jquery.JQuery;
	/**
		Remove an event handler.
	**/
	@:overload(function(event:js.jquery.Event):js.jquery.JQuery { })
	@:overload(function(events:Dynamic, ?selector:String):js.jquery.JQuery { })
	@:overload(function(events:String, ?selector:String, ?handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function off():js.jquery.JQuery;
	/**
		Get the current coordinates of the first element in the set of matched elements, relative to the document.
		OR
		Set the current coordinates of every element in the set of matched elements, relative to the document.
	**/
	@:overload(function(coordinates:{ var top : Float; var left : Float; }):js.jquery.JQuery { })
	@:overload(function(_function:Int -> { var top : Float; var left : Float; } -> Dynamic):js.jquery.JQuery { })
	public function offset():{ var top : Float; var left : Float; };
	/**
		Get the closest ancestor element that is positioned.
	**/
	public function offsetParent():js.jquery.JQuery;
	/**
		Attach an event handler function for one or more events to the selected elements.
	**/
	@:overload(function(events:String, ?selector:String, ?data:Dynamic, handler:js.jquery.Event -> haxe.extern.Rest<Dynamic> -> Void):js.jquery.JQuery { })
	public function on(events:Dynamic, ?selector:String, ?data:Dynamic):js.jquery.JQuery;
	/**
		Attach a handler to an event for the elements. The handler is executed at most once per element per event type.
	**/
	@:overload(function(events:Dynamic, ?selector:String, ?data:Dynamic):js.jquery.JQuery { })
	@:overload(function(events:String, ?selector:String, ?data:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function one(events:String, ?data:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Get the current computed height for the first element in the set of matched elements, including padding, border, and optionally margin. Returns a number (without "px") representation of the value or null if called on an empty set of elements.
		OR
		Set the CSS outer Height of each element in the set of matched elements.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?includeMargin:Bool):Float { })
	public function outerHeight(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery;
	/**
		Get the current computed width for the first element in the set of matched elements, including padding and border.
		OR
		Set the CSS outer width of each element in the set of matched elements.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?includeMargin:Bool):Float { })
	public function outerWidth(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery;
	/**
		Get the parent of each element in the current set of matched elements, optionally filtered by a selector.
	**/
	public function parent(?selector:String):js.jquery.JQuery;
	/**
		Get the ancestors of each element in the current set of matched elements, optionally filtered by a selector.
	**/
	public function parents(?selector:String):js.jquery.JQuery;
	/**
		Get the ancestors of each element in the current set of matched elements, up to but not including the element matched by the selector, DOM node, or jQuery object.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function parentsUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Get the current coordinates of the first element in the set of matched elements, relative to the offset parent.
	**/
	public function position():{ var top : Float; var left : Float; };
	/**
		Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.
	**/
	@:overload(function(content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>, ?content:haxe.extern.EitherType<Array<String>, haxe.extern.EitherType<Array<js.jquery.JQuery>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.NodeList, haxe.extern.EitherType<String, js.jquery.JQuery>>>>>>):js.jquery.JQuery { })
	public function prepend(_function:Int -> String -> haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Insert every element in the set of matched elements to the beginning of the target.
	**/
	public function prependTo(target:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Get the immediately preceding sibling of each element in the set of matched elements. If a selector is provided, it retrieves the previous sibling only if it matches that selector.
	**/
	public function prev(?selector:String):js.jquery.JQuery;
	/**
		Get all preceding siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function prevAll(?selector:String):js.jquery.JQuery;
	/**
		Get all preceding siblings of each element up to but not including the element matched by the selector, DOM node, or jQuery object.
	**/
	@:overload(function(?element:haxe.extern.EitherType<js.html.Element, js.jquery.JQuery>, ?filter:String):js.jquery.JQuery { })
	public function prevUntil(?selector:String, ?filter:String):js.jquery.JQuery;
	/**
		Return a Promise object to observe when all actions of a certain type bound to the collection, queued or not, have finished.
	**/
	public function promise(?type:String, ?target:Dynamic):js.jquery.Promise;
	/**
		Set one or more properties for the set of matched elements.
		OR
		Get the value of a property for the first element in the set of matched elements.
	**/
	@:overload(function(properties:Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, _function:Int -> Dynamic -> Dynamic):js.jquery.JQuery { })
	@:overload(function(propertyName:String, value:Dynamic):js.jquery.JQuery { })
	public function prop(propertyName:String):Dynamic;
	/**
		Add a collection of DOM elements onto the jQuery stack.
	**/
	@:overload(function(elements:haxe.extern.EitherType<Array<js.html.Element>, js.html.NodeList>, name:String, arguments:Array<Dynamic>):js.jquery.JQuery { })
	public function pushStack(elements:haxe.extern.EitherType<Array<js.html.Element>, js.html.NodeList>):js.jquery.JQuery;
	/**
		Show the queue of functions to be executed on the matched elements.
		OR
		Manipulate the queue of functions to be executed, once for each matched element.
	**/
	@:overload(function(?queueName:String, newQueue:Array<Void -> Void>):js.jquery.JQuery { })
	@:overload(function(?queueName:String, callback:haxe.Constraints.Function -> Void):js.jquery.JQuery { })
	public function queue(?queueName:String):Array<Void -> Void>;
	/**
		Specify a function to execute when the DOM is fully loaded.
	**/
	public function ready(handler:haxe.Constraints.Function):js.jquery.JQuery;
	/**
		Remove the set of matched elements from the DOM.
	**/
	public function remove(?selector:String):js.jquery.JQuery;
	/**
		Remove an attribute from each element in the set of matched elements.
	**/
	public function removeAttr(attributeName:String):js.jquery.JQuery;
	/**
		Remove a single class, multiple classes, or all classes from each element in the set of matched elements.
	**/
	@:overload(function(?className:String):js.jquery.JQuery { })
	public function removeClass(_function:Int -> String -> String):js.jquery.JQuery;
	/**
		Remove a previously-stored piece of data.
	**/
	@:overload(function(?list:haxe.extern.EitherType<Array<String>, String>):js.jquery.JQuery { })
	public function removeData(?name:String):js.jquery.JQuery;
	/**
		Remove a property for the set of matched elements.
	**/
	public function removeProp(propertyName:String):js.jquery.JQuery;
	/**
		Replace each target element with the set of matched elements.
	**/
	public function replaceAll(target:haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Replace each element in the set of matched elements with the provided new content and return the set of elements that was removed.
	**/
	@:overload(function(_function:haxe.Constraints.Function):js.jquery.JQuery { })
	public function replaceWith(newContent:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<Array<js.html.Element>, haxe.extern.EitherType<String, js.jquery.JQuery>>>):js.jquery.JQuery;
	/**
		Bind an event handler to the "resize" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function resize():js.jquery.JQuery;
	/**
		Bind an event handler to the "scroll" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function scroll():js.jquery.JQuery;
	/**
		Get the current horizontal position of the scroll bar for the first element in the set of matched elements.
		OR
		Set the current horizontal position of the scroll bar for each of the set of matched elements.
	**/
	@:overload(function(value:Float):js.jquery.JQuery { })
	public function scrollLeft():Int;
	/**
		Set the current vertical position of the scroll bar for each of the set of matched elements.
		OR
		Get the current vertical position of the scroll bar for the first element in the set of matched elements or set the vertical position of the scroll bar for every matched element.
	**/
	@:overload(function(value:Float):js.jquery.JQuery { })
	public function scrollTop():Float;
	/**
		Bind an event handler to the "select" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function select():js.jquery.JQuery;
	/**
		A selector representing selector passed to jQuery(), if any, when creating the original set.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	public var selector : String;
	/**
		Encode a set of form elements as a string for submission.
	**/
	public function serialize():String;
	/**
		Encode a set of form elements as an array of names and values.
	**/
	public function serializeArray():Array<Dynamic>;
	/**
		Display the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function show():js.jquery.JQuery;
	/**
		Get the siblings of each element in the set of matched elements, optionally filtered by a selector.
	**/
	public function siblings(?selector:String):js.jquery.JQuery;
	/**
		Return the number of elements in the jQuery object.
	**/
	@:deprecated("Deprecated since jQuery 1.8")
	public function size():Int;
	/**
		Reduce the set of matched elements to a subset specified by a range of indices.
	**/
	public function slice(start:Int, ?end:Int):js.jquery.JQuery;
	/**
		Display the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideDown(options:Dynamic):js.jquery.JQuery;
	/**
		Display or hide the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideToggle(options:Dynamic):js.jquery.JQuery;
	/**
		Hide the matched elements with a sliding motion.
	**/
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	public function slideUp(options:Dynamic):js.jquery.JQuery;
	/**
		Stop the currently-running animation on the matched elements.
	**/
	@:overload(function(?queue:String, ?clearQueue:Bool, ?jumpToEnd:Bool):js.jquery.JQuery { })
	public function stop(?clearQueue:Bool, ?jumpToEnd:Bool):js.jquery.JQuery;
	/**
		Bind an event handler to the "submit" JavaScript event, or trigger that event on an element.
	**/
	@:overload(function(handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function submit():js.jquery.JQuery;
	/**
		Set the content of each element in the set of matched elements to the specified text.
		OR
		Get the combined text contents of each element in the set of matched elements, including their descendants.
	**/
	@:overload(function(text:haxe.extern.EitherType<Float, haxe.extern.EitherType<Bool, String>>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function text():String;
	/**
		Retrieve all the elements contained in the jQuery set, as an array.
	**/
	public function toArray():Array<js.html.Element>;
	/**
		Bind two or more handlers to the matched elements, to be executed on alternate clicks.
		OR
		Display or hide the matched elements.
	**/
	@:overload(function(options:Dynamic):js.jquery.JQuery { })
	@:overload(function(?duration:haxe.extern.EitherType<Float, String>, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(duration:haxe.extern.EitherType<Float, String>, ?easing:String, ?complete:haxe.Constraints.Function):js.jquery.JQuery { })
	@:overload(function(handler:js.jquery.Event -> Void, handler:js.jquery.Event -> Void, ?handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function toggle(display:Bool):js.jquery.JQuery;
	/**
		Add or remove one or more classes from each element in the set of matched elements, depending on either the class's presence or the value of the state argument.
	**/
	@:overload(function(?state:Bool):js.jquery.JQuery { })
	@:overload(function(className:String, state:Bool):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> Bool -> String, ?state:Bool):js.jquery.JQuery { })
	public function toggleClass(className:String):js.jquery.JQuery;
	/**
		Execute all handlers and behaviors attached to the matched elements for the given event type.
	**/
	@:overload(function(event:js.jquery.Event, ?extraParameters:Dynamic):js.jquery.JQuery { })
	public function trigger(eventType:String, ?extraParameters:Dynamic):js.jquery.JQuery;
	/**
		Execute all handlers attached to an element for an event.
	**/
	@:overload(function(event:js.jquery.Event, ?extraParameters:Dynamic):Dynamic { })
	public function triggerHandler(eventType:String, ?extraParameters:Dynamic):Dynamic;
	/**
		Remove a previously-attached event handler from the elements.
	**/
	@:overload(function(event:js.jquery.Event):js.jquery.JQuery { })
	@:overload(function(eventType:String, _false:Bool):js.jquery.JQuery { })
	@:overload(function(eventType:String, ?handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function unbind():js.jquery.JQuery;
	/**
		Remove a handler from the event for all elements which match the current selector, based upon a specific set of root elements.
	**/
	@:overload(function(namespace:String):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String):js.jquery.JQuery { })
	@:overload(function(selector:String, events:Dynamic):js.jquery.JQuery { })
	@:overload(function(selector:String, eventType:String, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function undelegate():js.jquery.JQuery;
	/**
		Bind an event handler to the "unload" JavaScript event.
	**/
	@:deprecated("Deprecated since jQuery 1.8")
	@:overload(function(?eventData:Dynamic, handler:js.jquery.Event -> Void):js.jquery.JQuery { })
	public function unload(handler:js.jquery.Event -> Void):js.jquery.JQuery;
	/**
		Remove the parents of the set of matched elements from the DOM, leaving the matched elements in their place.
	**/
	@:overload(function(?selector:String):js.jquery.JQuery { })
	public function unwrap():js.jquery.JQuery;
	/**
		Set the value of each element in the set of matched elements.
		OR
		Get the current value of the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<Float, haxe.extern.EitherType<Array<String>, String>>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> String -> String):js.jquery.JQuery { })
	public function val():Dynamic;
	/**
		Set the CSS width of each element in the set of matched elements.
		OR
		Get the current computed width for the first element in the set of matched elements.
	**/
	@:overload(function(value:haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	@:overload(function(_function:Int -> Int -> haxe.extern.EitherType<Float, String>):js.jquery.JQuery { })
	public function width():Float;
	/**
		Wrap an HTML structure around each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> haxe.extern.EitherType<String, js.jquery.JQuery>):js.jquery.JQuery { })
	public function wrap(wrappingElement:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Wrap an HTML structure around all elements in the set of matched elements.
	**/
	@:overload(function(_function:Void -> haxe.extern.EitherType<String, js.jquery.JQuery>):js.jquery.JQuery { })
	public function wrapAll(wrappingElement:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Wrap an HTML structure around the content of each element in the set of matched elements.
	**/
	@:overload(function(_function:Int -> String):js.jquery.JQuery { })
	public function wrapInner(wrappingElement:haxe.extern.EitherType<js.html.Element, haxe.extern.EitherType<String, js.jquery.JQuery>>):js.jquery.JQuery;
	/**
		Haxe iterator.
	**/
	@:runtime
	inline public function iterator():js.jquery.JqIterator return new js.jquery.JqIterator(this);
	/**
		Haxe iterator.
	**/
	@:runtime
	inline public function elements():js.jquery.JqEltsIterator return new js.jquery.JqEltsIterator(this);
}