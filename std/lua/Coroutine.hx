package lua;

/**
  Externs for native Lua threads.
 **/
@:native("_G.coroutine")
extern class Coroutine {
	@:overload(function<A,B,C,D,E>(f : A->B->C->D->E->Void) : CoroutineInstance5<A,B,C,D,E> {})
	@:overload(function<A,B,C,D>(f   : A->B->C->D->Void)    : CoroutineInstance4<A,B,C,D>   {})
	@:overload(function<A,B,C>(f     : A->B->C->Void)       : CoroutineInstance3<A,B,C>     {})
	@:overload(function<A,B>(f       : A->B->Void)          : CoroutineInstance2<A,B>       {})
	@:overload(function<A>(f         : A->Void)             : CoroutineInstance1<A>         {})
	@:overload(function(f            : Void->Void)             : CoroutineInstance0         {})
	public static function create(f : haxe.Constraints.Function) : CoroutineInstance;

	@:overload(function<A,B,C,D,E>(c : CoroutineInstance5<A,B,C,D,E>, arg1 : A, arg2: B, arg3: C, arg4: C, arg5: C) : Void {})
	@:overload(function<A,B,C,D>(c   : CoroutineInstance4<A,B,C,D>, arg1: A, arg2: B, arg3: C, arg4: C) : Void             {})
	@:overload(function<A,B,C>(c     : CoroutineInstance3<A,B,C>, arg1: A, arg2: B, arg3: C) : Void                        {})
	@:overload(function<A,B>(c       : CoroutineInstance2<A,B>, arg1: A, arg2: B) : Void                                   {})
	@:overload(function<A>(c         : CoroutineInstance1<A>, arg1: A) : Void                                              {})
	@:overload(function(c            : CoroutineInstance0) : Void                                                          {})
	public static function resume(c : CoroutineInstance, ?arg1:Dynamic, ?arg2:Dynamic, ?arg3:Dynamic, ?arg4:Dynamic, ?arg5:Dynamic) : Void;

	@:overload(function<A,B,C,D,E>(c         : CoroutineInstance5<A,B,C,D,E>) : Void {})
	@:overload(function<A,B,C,D>(c           : CoroutineInstance4<A,B,C,D>)   : Void {})
	@:overload(function<A,B,C>(c             : CoroutineInstance3<A,B,C>)     : Void {})
	@:overload(function<A,B>(c               : CoroutineInstance2<A,B>)       : Void {})
	@:overload(function<A>(c                 : CoroutineInstance1<A>)         : Void {})
	@:overload(function(c                    : CoroutineInstance0)            : Void {})
	public static function yield(c : CoroutineInstance) : Void;
}

typedef CoroutineInstance             = {}
typedef CoroutineInstance0            = {}
typedef CoroutineInstance1<A>         = {}
typedef CoroutineInstance2<A,B>       = {}
typedef CoroutineInstance3<A,B,C>     = {}
typedef CoroutineInstance4<A,B,C,D>   = {}
typedef CoroutineInstance5<A,B,C,D,E> = {}

@:enum
abstract ThreadState(String) {
	var Suspended = "suspended";
	var Running   = "running";
	var Dead      = "dead";
}
