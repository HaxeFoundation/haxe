/* This file is generated, do not edit! Visit http://api.jquery.com/ for API documentation. */
package js.jquery;
@:native("$.Promise") typedef Promise = {
	/**
		Add handlers to be called when the Deferred object is either resolved or rejected.
	**/
	public function always(alwaysCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, ?alwaysCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>):js.jquery.Deferred;
	/**
		Add handlers to be called when the Deferred object is resolved.
	**/
	public function done(doneCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, ?doneCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>):js.jquery.Deferred;
	/**
		Add handlers to be called when the Deferred object is rejected.
	**/
	public function fail(failCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, ?failCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>):js.jquery.Deferred;
	/**
		Determine whether a Deferred object has been rejected.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	public function isRejected():Bool;
	/**
		Determine whether a Deferred object has been resolved.
	**/
	@:deprecated("Deprecated since jQuery 1.7")
	public function isResolved():Bool;
	/**
		Utility method to filter and/or chain Deferreds.
	**/
	@:deprecated("Deprecated since jQuery 1.8")
	@:overload(function(?doneFilter:haxe.Constraints.Function, ?failFilter:haxe.Constraints.Function, ?progressFilter:haxe.Constraints.Function):js.jquery.Promise { })
	public function pipe(?doneFilter:haxe.Constraints.Function, ?failFilter:haxe.Constraints.Function):js.jquery.Promise;
	/**
		Add handlers to be called when the Deferred object generates progress notifications.
	**/
	public function progress(progressCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, haxe.extern.EitherType<Array<Dynamic>, Array<haxe.Constraints.Function>>>, ?progressCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, haxe.extern.EitherType<Array<Dynamic>, Array<haxe.Constraints.Function>>>):js.jquery.Deferred;
	/**
		Determine the current state of a Deferred object.
	**/
	public function state():String;
	/**
		Add handlers to be called when the Deferred object is resolved, rejected, or still in progress.
	**/
	@:overload(function(doneFilter:haxe.Constraints.Function, ?failFilter:haxe.Constraints.Function, ?progressFilter:haxe.Constraints.Function):js.jquery.Promise { })
	@:overload(function(doneCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, failCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, ?progressCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>):js.jquery.Promise { })
	public function then(doneCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>, failCallbacks:haxe.extern.EitherType<haxe.Constraints.Function, Array<haxe.Constraints.Function>>):js.jquery.Promise;
}