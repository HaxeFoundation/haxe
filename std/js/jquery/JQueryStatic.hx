/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:final @:native("$") extern class JQueryStatic {
	/**
		Set default values for future Ajax requests. Its use is not recommended.
	**/
	static public function ajaxSetup(options:Dynamic):Void;
	/**
		Creates an object that handles the actual transmission of Ajax data.
	**/
	static public function ajaxTransport(dataType:String, handler:Dynamic -> Dynamic -> js.jquery.JqXHR -> Void):Void;
	/**
		Check to see if a DOM element is a descendant of another DOM element.
	**/
	static public function contains(container:js.html.Element, contained:js.html.Element):Bool;
	/**
		Check to see if a DOM node is within an XML document (or is an XML document).
	**/
	static public function isXMLDoc(node:js.html.Element):Bool;
	/**
		Returns value at named data store for the element, as set by <code>jQuery.data(element, name, value)</code>, or the full data store for the element.
	**/
	@:overload(function(element:js.html.Element, key:String):Dynamic { })
	@:overload(function(element:js.html.Element, key:String, value:Dynamic):Dynamic { })
	static public function data(element:js.html.Element):Dynamic;
	/**
		Determine whether the argument is a window.
	**/
	static public function isWindow(obj:Dynamic):Bool;
	/**
		Remove a previously-stored piece of data.
	**/
	static public function removeData(element:js.html.Element, ?name:String):js.jquery.JQuery;
	/**
		Provides a way to execute callback functions based on one or more objects, usually <a href="/category/deferred-object/">Deferred</a> objects that represent asynchronous events.
	**/
	static public function when(deferreds:haxe.extern.Rest<js.jquery.Deferred>):js.jquery.Promise;
	/**
		A generic iterator function, which can be used to seamlessly iterate over both objects and arrays. Arrays and array-like objects with a length property (such as a function's arguments object) are iterated by numeric index, from 0 to length-1. Other objects are iterated via their named properties.
	**/
	@:overload(function(object:Dynamic, callback:String -> Dynamic -> Void):Dynamic { })
	static public function each(array:Array<Dynamic>, callback:Int -> Dynamic -> Void):Dynamic;
	/**
		Execute the next function on the queue for the matched element.
	**/
	static public function dequeue(element:js.html.Element, ?queueName:String):Void;
	/**
		Search for a specified value within an array and return its index (or -1 if not found).
	**/
	static public function inArray(value:Dynamic, array:Array<Dynamic>, ?fromIndex:Float):Float;
	/**
		Load a JavaScript file from the server using a GET HTTP request, then execute it.
	**/
	static public function getScript(url:String, ?success:String -> String -> js.jquery.JqXHR -> Void):js.jquery.JqXHR;
	/**
		Relinquish jQuery's control of the <code>$</code> variable.
	**/
	static public function noConflict(?removeAll:Bool):Dynamic;
	/**
		Create a serialized representation of an array, a plain object, or a jQuery object suitable for use in a URL query string or Ajax request. In case a jQuery object is passed, it should contain input elements with name/value properties.
	**/
	@:overload(function(obj:haxe.extern.EitherType<Array<Dynamic>, haxe.extern.EitherType<Dynamic, js.jquery.JQuery>>, traditional:Bool):String { })
	static public function param(obj:haxe.extern.EitherType<Array<Dynamic>, haxe.extern.EitherType<Dynamic, js.jquery.JQuery>>):String;
	/**
		Determine if the argument passed is a JavaScript function object. 
	**/
	static public function isFunction(obj:Dynamic):Bool;
	/**
		Takes a function and returns a new one that will always have a particular context.
	**/
	@:overload(function(context:Dynamic, name:String):haxe.Constraints.Function { })
	@:overload(function(_function:haxe.Constraints.Function, context:Dynamic, ?additionalArguments:Dynamic):haxe.Constraints.Function { })
	@:overload(function(context:Dynamic, name:String, ?additionalArguments:Dynamic):haxe.Constraints.Function { })
	static public function proxy(_function:haxe.Constraints.Function, context:Dynamic):haxe.Constraints.Function;
	/**
		Remove the whitespace from the beginning and end of a string.
	**/
	static public function trim(str:String):String;
	/**
		Hook directly into jQuery to override how particular CSS properties are retrieved or set, normalize CSS property naming, or create custom properties.
	**/
	static public var cssHooks : Dynamic;
	/**
		Determine whether the argument is an array.
	**/
	static public function isArray(obj:Dynamic):Bool;
	/**
		Convert an array-like object into a true JavaScript array.
	**/
	static public function makeArray(obj:Dynamic):Array<Dynamic>;
	/**
		Holds or releases the execution of jQuery's ready event.
	**/
	static public function holdReady(hold:Bool):Void;
	/**
		Translate all items in an array or object to new array of items.
	**/
	@:overload(function(object:Dynamic, callback:Dynamic -> String -> Dynamic):Array<Dynamic> { })
	static public function map(array:Array<Dynamic>, callback:Dynamic -> Int -> Dynamic):Array<Dynamic>;
	/**
		Show the queue of functions to be executed on the matched element.
	**/
	@:overload(function(element:js.html.Element, queueName:String, newQueue:Array<Void -> Void>):js.jquery.JQuery { })
	@:overload(function(element:js.html.Element, queueName:String, callback:haxe.Constraints.Function):js.jquery.JQuery { })
	static public function queue(element:js.html.Element, ?queueName:String):Array<Void -> Void>;
	/**
		Parses a string into an array of DOM nodes.
	**/
	static public function parseHTML(data:String, ?context:js.html.Element, ?keepScripts:Bool):Array<js.html.Element>;
	/**
		Perform an asynchronous HTTP (Ajax) request.
	**/
	@:overload(function(url:String, ?settings:Dynamic):js.jquery.JqXHR { })
	static public function ajax(?settings:Dynamic):js.jquery.JqXHR;
	/**
		Takes a string and throws an exception containing it.
	**/
	static public function error(message:String):Void;
	/**
		Merge the contents of two arrays together into the first array. 
	**/
	static public function merge(first:haxe.extern.EitherType<Array<Dynamic>, js.html.NodeList>, second:haxe.extern.EitherType<Array<Dynamic>, js.html.NodeList>):Array<Dynamic>;
	/**
		Parses a string into an XML document.
	**/
	static public function parseXML(data:String):js.html.Document;
	/**
		Check to see if an object is empty (contains no enumerable properties).
	**/
	static public function isEmptyObject(object:Dynamic):Bool;
	/**
		Handle custom Ajax options or modify existing options before each request is sent and before they are processed by <code>$.ajax()</code>.
	**/
	static public function ajaxPrefilter(?dataTypes:String, handler:Dynamic -> Dynamic -> js.jquery.JqXHR -> Void):Void;
	/**
		Execute some JavaScript code globally.
	**/
	static public function globalEval(code:String):Void;
	/**
		Load JSON-encoded data from the server using a GET HTTP request.
	**/
	static public function getJSON(url:String, ?data:haxe.extern.EitherType<Dynamic, String>, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void):js.jquery.JqXHR;
	/**
		Finds the elements of an array which satisfy a filter function. The original array is not affected.
	**/
	static public function grep(array:Array<Dynamic>, _function:Dynamic -> Int -> Bool, ?invert:Bool):Array<Dynamic>;
	/**
		Merge the contents of an object onto the jQuery prototype to provide new jQuery instance methods.
	**/
	static public function fn(object:Dynamic):Dynamic;
	/**
		Load data from the server using a HTTP POST request.
	**/
	@:overload(function(url:String, ?data:haxe.extern.EitherType<Dynamic, String>, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void, ?dataType:String):js.jquery.JqXHR { })
	static public function post(settings:Dynamic):js.jquery.JqXHR;
	/**
		 A factory function that returns a chainable utility object with methods to register multiple callbacks into callback queues, invoke callback queues, and relay the success or failure state of any synchronous or asynchronous function.
	**/
	static public function Deferred(?beforeStart:js.jquery.Deferred -> Void):js.jquery.Deferred;
	/**
		A multi-purpose callbacks list object that provides a powerful way to manage callback lists.
	**/
	static public function Callbacks(flags:String):js.jquery.Callbacks;
	/**
		An empty function.
	**/
	static public function noop():Void;
	/**
		Determines whether its argument is a number.
	**/
	static public function isNumeric(value:Dynamic):Bool;
	/**
		Check to see if an object is a plain object (created using "{}" or "new Object").
	**/
	static public function isPlainObject(object:Dynamic):Bool;
	/**
		Takes a well-formed JSON string and returns the resulting JavaScript value.
	**/
	static public function parseJSON(json:String):haxe.extern.EitherType<String, haxe.extern.EitherType<Float, haxe.extern.EitherType<Dynamic, haxe.extern.EitherType<Array<Dynamic>, Bool>>>>;
	/**
		A collection of properties that represent the presence of different browser features or bugs. Intended for jQuery's internal use; specific properties may be removed when they are no longer needed internally to improve page startup performance. For your own project's feature-detection needs, we strongly recommend the use of an external library such as <a href="http://modernizr.com">Modernizr</a> instead of dependency on properties in <code>jQuery.support</code>.
	**/
	static public var support : Dynamic;
	/**
		Sorts an array of DOM elements, in place, with the duplicates removed. Note that this only works on arrays of DOM elements, not strings or numbers.
	**/
	static public function unique(array:Array<js.html.Element>):Array<js.html.Element>;
	/**
		An object containing all CSS properties that may be used without a unit. The <a href="/css/"><code>.css()</code></a> method uses this object to see if it may append <code>px</code> to unitless values.
	**/
	static public var cssNumber : Dynamic;
	/**
		Determine whether an element has any jQuery data associated with it.
	**/
	static public function hasData(element:js.html.Element):Bool;
	/**
		Merge the contents of two or more objects together into the first object.
	**/
	@:overload(function(?deep:Bool, target:Dynamic, object1:Dynamic, ?objectN:Dynamic):Dynamic { })
	static public function extend(target:Dynamic, ?object1:Dynamic, ?objectN:Dynamic):Dynamic;
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
	@:overload(function(url:String, ?data:haxe.extern.EitherType<Dynamic, String>, ?success:Dynamic -> String -> js.jquery.JqXHR -> Void, ?dataType:String):js.jquery.JqXHR { })
	static public function get(settings:Dynamic):js.jquery.JqXHR;
	/**
		Return a number representing the current time.
	**/
	static public function now():Float;
	/**
		Determine the internal JavaScript [[Class]] of an object.
	**/
	static public function type(obj:Dynamic):String;
}