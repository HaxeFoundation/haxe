package haxe.extern;

/**
	`Callback` is a type that unifies with functions with number of arguments less than or equals to `T`.
	Concretely, a function that accepts `Callback<A->B->C>` can be passed with `A->B->C`, `A->C`, or `Void->C`.
	It is implemented using `EitherType`.

	Some languages (e.g. JS), allow calling a function with extra arguments.
	When using such languages, it is a common practice to pass a callback function that
	accepts less arguments than the expected type. A JS example:

	```js
	var Process = {
		// `handler` will be called with a status message
		onComplete: function(handler) {
			handler("completed");
		}
	}

	var myhandler = function(){ // ignore the status message being passed in
		console.log("done");
	}
	Process.onComplete(myhandler);
	```

	To allow such usage in Haxe, we can create an extern class of `Process` as follows:

	```haxe
	extern class Process {
		// handler can be either `String->Void` or `Void->Void`
		static public function onComplete(handler:Callback<String->Void>):Void;
	}
	```
*/
@:genericBuild(haxe.macro.CallbackBuilder.build())
interface Callback<T:haxe.Constraints.Function> {}
