package js;

typedef JqEvent = {
	var target : Dom.HtmlDom;
	var currentTarget : Dom.HtmlDom;
	var relatedTarget : Dom.HtmlDom;

	var type : String;
	var timeStamp : Int;

	//var data : Dynamic;
	//var namespace : String;
	//var result : Dynamic;

	// position
	var pageX : Int;
	var pageY : Int;

	var wheelDelta : Int;

	// keys
	var keyCode : Int;
	var charCode : Int;
	var shiftKey : Bool;
	var ctrlKey : Bool;
	var altKey : Bool;
	var metaKey : Bool;

	var which : Int;

	// propagation
	function isDefaultPrevented() : Bool;
	function isImmediatePropagationStopped() : Bool;
	function isPropationStopped() : Bool;
	function preventDefault() : Void;
	function stopImmediatePropagation() : Void;
	function stopPropagation() : Void;
}

extern class JQuery implements ArrayAccess<Dom.HtmlDom> {

	var context(default,null) : Dom.HtmlDom;
	var length(default, null) : Int;

	@:overload(function(j:js.JQuery):Void{})
	@:overload(function(j:js.Dom.HtmlDom):Void{})
	function new( html : String ) : Void;

	// attributes
	function addClass( className : String ) : JQuery;
	function removeClass( ?className : String ) : JQuery;
	function hasClass( className : String ) : Bool;
	function toggleClass( className : String, ?addRemove : Bool ) : JQuery;

	@:overload(function(name:String,value:String):js.JQuery{})
	function attr( name : String ) : String;

	function removeAttr( attr : String ) : JQuery;

	@:overload(function(prop:String,value:String):js.JQuery{})
	function css( prop : String ) : String;

	@:overload(function(html:String):js.JQuery{})
	function html() : String;

	@:overload(function(value:String):js.JQuery{})
	function val() : String;

	@:overload(function(text:String):js.JQuery{})
	function text() : String;

	// Size & Position
	@:overload(function(value:Int):js.JQuery{})
	function width() : Int;
	@:overload(function(value:Int):js.JQuery{})
	function height() : Int;
	@:overload(function(value:Int):js.JQuery{})
	function innerWidth() : Int;
	@:overload(function(value:Int):js.JQuery{})
	function innerHeight() : Int;

	function outerWidth( ?includeMargin : Bool ) : Int;
	function outerHeight( ?includeMargin : Bool ) : Int;

	@:overload(function(value:Int):js.JQuery{})
	function scrollLeft() : Int;

	@:overload(function(value:Int):js.JQuery{})
	function scrollTop() : Int;

	@:overload(function(value: { left : Int, top : Int }):js.JQuery{})
	function offset() : { left : Int, top : Int };

	function offsetParent() : JQuery;

	@:overload(function(value: { left : Int, top : Int }):js.JQuery{})
	function position() : { left : Int, top : Int };

	// current group manipulation
	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	@:overload(function(value:Array<js.Dom.HtmlDom>):js.JQuery{})
	function add( selector : String, ?context : JQuery ) : JQuery;
	function andSelf() : JQuery;
	function children( ?selector : String ) : JQuery;
	function clone( ?withDataAndEvents : Bool ) : JQuery;
	function closest( selector : String, ?context : JQuery ) : JQuery;
	function contents() : JQuery;
	function each( f : Void -> Void ) : JQuery;
	function end() : JQuery;
	function eq( index : Int ) : JQuery;
	function filter( selector : String ) : JQuery;
	function find( selector : String ) : JQuery;
	function first() : JQuery;
	function index( ?selector : String ) : Int;
	function last( ?selector : String ) : JQuery;
	function has( selector : String ) : JQuery;
	function next( ?selector : String ) : JQuery;
	function nextAll( ?selector : String ) : JQuery;
	function nextUntil( ?selector : String ) : JQuery;
	function parent( ?selector : String ) : JQuery;
	function parents( ?selector : String ) : JQuery;
	function parentsUntil( ?selector : String ) : JQuery;
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function not( selector : String ) : JQuery;
	function prev( ?selector : String ) : JQuery;
	function prevAll( ?selector : String ) : JQuery;
	function prevUntil( ?selector : String ) : JQuery;
	function pushStack( elements : Array<Dom.HtmlDom> ) : JQuery;
	function siblings( ?selector : String ) : JQuery;
	function size() : Int;
	function slice( start : Int, ?end : Int ) : JQuery;
	function toArray() : Array<Dom.HtmlDom>;

	// DOM changes
	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function before( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function after( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function append( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function appendTo( html : String ) : JQuery;

	function detach( ?selector : String ) : JQuery;
	function empty() : JQuery; // remove all texts

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function insertBefore( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function insertAfter( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function prepend( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function prependTo( html : String ) : JQuery;

	function remove( ?selector : String ) : JQuery;
	function replaceAll( selector : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function replaceWith( html : String ) : JQuery;

	function unwrap() : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function wrap( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function wrapAll( html : String ) : JQuery;

	@:overload(function(value:js.JQuery):js.JQuery{})
	@:overload(function(value:js.Dom.HtmlDom):js.JQuery{})
	function wrapInner( html : String ) : JQuery;

	// animation
	function animate( properties : { }, ?duration : Int, ?callb : Void -> Void ) : JQuery;
	function delay( duration : Int, ?queueName : String ) : JQuery;
	function hide( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function fadeIn( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function fadeOut( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function fadeTo( duration : Int, opacity : Float, ?call : Void -> Void ) : JQuery;
	function fadeToggle( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function show( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function slideDown( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function slideToggle( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function slideUp( ?duration : Int, ?call : Void -> Void ) : JQuery;
	function stop( ?clearQueue : Bool, ?jumpToEnd : Bool ) : JQuery;
	function toggle( ?duration : Int, ?call : Void -> Void ) : JQuery;

	// Events
	function blur( ?callb : JqEvent -> Void ) : JQuery;
	function change( ?callb : JqEvent -> Void ) : JQuery;
	
	@:overload(function(callb:Void->Void):js.JQuery { } )
	@:overload(function(callb:js.JQuery.JqEvent->Void):js.JQuery{})
	@:overload(function(callb:Void->Bool):js.JQuery{})
	function click( ?callb : JqEvent -> Void ) : JQuery;
	function dblclick( ?callb : JqEvent -> Void ) : JQuery;
	function error( ?callb : JqEvent -> Void ) : JQuery;
	function focus( ?callb : JqEvent -> Void ) : JQuery;
	function focusin( ?callb : JqEvent -> Void ) : JQuery;
	function focusout( ?callb : JqEvent -> Void ) : JQuery;
	function hover( onOver : JqEvent -> Void, ?onOut : Void -> Void ) : JQuery;

	@:overload(function( callb : js.JQuery.JqEvent -> Bool ) : js.JQuery {})
	function keydown( ?callb : JqEvent -> Void ) : JQuery;

	@:overload(function( callb : js.JQuery.JqEvent -> Bool ) : js.JQuery {})
	function keypress( ?callb : JqEvent -> Void ) : JQuery;

	@:overload(function( callb : js.JQuery.JqEvent -> Bool ) : js.JQuery {})
	function keyup( ?callb : JqEvent -> Void ) : JQuery;

	function mousedown( ?callb : JqEvent -> Void ) : JQuery;
	function mouseenter( ?callb : JqEvent -> Void ) : JQuery;
	function mouseleave( ?callb : JqEvent -> Void ) : JQuery;
	function mouseout( ?callb : JqEvent -> Void ) : JQuery;
	function mouseover( ?callb : JqEvent -> Void ) : JQuery;
	function mousemove( ?callb : JqEvent -> Void ) : JQuery;
	function mouseup( ?callb : JqEvent -> Void ) : JQuery;
	function load( ?callb : JqEvent -> Void ) : JQuery;
	function ready( callb : JqEvent -> Void ) : JQuery;
	function resize( ?callb : JqEvent -> Void ) : JQuery;
	function scroll( ?callb : JqEvent -> Void ) : JQuery;
	function select( ?callb : JqEvent -> Void ) : JQuery;
	function submit( ?callb : JqEvent -> Void ) : JQuery;
	function unload( ?callb : JqEvent -> Void ) : JQuery;

	function bind( events : String, callb : JqEvent -> Void ) : JQuery;
	function delegate( selector : String, events : String, callb : JqEvent -> Void ) : JQuery;
	function die( ?events : String, ?callb : JqEvent -> Void ) : JQuery;
	function one( events : String, callb : JqEvent -> Void ) : JQuery;
	function live( events : String, callb : JqEvent -> Void ) : JQuery;
	function trigger( events : String ) : JQuery;
	function triggerHandler( events : String ) : JQuery;
	function unbind( ?events : String, ?callb : JqEvent -> Void ) : JQuery;
	function undelegate( ?selector : String, ?events : String, ?callb : JqEvent -> Void ) : JQuery;

	// queue
	function clearQueue( ?queueName : String ) : JQuery;
	function dequeue( ?queueName : String ) : JQuery;
	function queue( ?queueName : String ) : { length : Int };

	// ajax
	// TODO

	// deferred
	// TODO

	// other tools
	function get() : Array<Dom.HtmlDom>;
	function is( selector : String ) : Bool;
	function data<T>( key : String, ?value : T ) : T;
	function removeData( ?key : String ) : JQuery;
	function serialize() : String;
	function serializeArray() : Array<{ name : String, value : String }>;
	//inline function map<T>( f : JQuery -> T ) : Array<T> {
	//	return untyped this["map"](function() return f(cur)).get();
	//}

	function iterator() : Iterator<JQuery>;


	// haxe-additions
	inline function noBubble( events : String ) : JQuery { return (cast this).bind(events, false); }
	inline function loadURL( url : String, ?callb : Void -> Void ) : JQuery { return (cast this).load(url,callb); }
	inline function toggleClick( ?first : Void -> Void, ?second : Void -> Void ) : JQuery { return (cast this).toggle(first, second); }

	inline static function of( d : Dom.HtmlDom ) : JQuery { return new js.JQuery(cast d); }

	/**
		Return the current JQuery element (in a callback), similar to $(this) in JS.
	**/
	static var cur(getCurrent, null) : JQuery;

	static var fx(default, null) : { off : Bool, interval : Int };
	static var browser(default, null) : { webkit : Bool, opera : Bool, msie : Bool, mozilla : Bool, version : String };

	static function contains( parent : Dom.HtmlDom, child : Dom.HtmlDom ) : Bool;
	static function noConflict( ?removeAll : Bool ) : Void;
	static function parseJSON( json : String ) : Dynamic;


	//static function parseXML
	//static function get, post
	//static function getJSON, getScript, globalEval, grep
	//static function is*, makeArray, map, merge, noop, now, param, proxy, sub, trim, type, unique

	private static inline function getCurrent() : JQuery {
		return untyped __js__("$(this)");
	}

	private static function __init__() : Void untyped {
		#if !noEmbedJS
		haxe.macro.Tools.includeFile("js/jquery-latest.min.js");
		#end
		var q : Dynamic = window.jQuery;
		js.JQuery = q;
		q.fn.noBubble = q.fn.bind;
		q.fn.loadURL = q.fn.load;
		q.fn.toggleClick = q.fn.toggle;
		q.of = q;
		q.fn.iterator = function() return { pos : 0, j : __this__, hasNext : function() return __this__.pos < __this__.j.length, next : function() return $(__this__.j[__this__.pos++]) };
	}
}