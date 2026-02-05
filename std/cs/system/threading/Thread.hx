package cs.system.threading;

/** Creates and controls a thread, sets its priority, and gets its status. */
@:native("System.Threading.Thread")
extern class Thread extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	/**
	 * Gets or sets the thread's current principal (for role-based security).
	 * @return An  value representing the security context.
	 */
	static var CurrentPrincipal(default, default):cs.system.security.principal.IPrincipal;
	/**
	 * Gets the currently running thread.
	 * @return A  that is the representation of the currently running thread.
	 */
	static var CurrentThread(default, never):cs.system.threading.Thread;
	/**
	 * Gets or sets the apartment state of this thread.
	 * @return One of the  values. The initial value is .
	 */
	var ApartmentState(default, default):cs.system.threading.ApartmentState;
	/**
	 * Gets or sets the culture for the current thread.
	 * @return An object that represents the culture for the current thread.
	 */
	var CurrentCulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the current culture used by the Resource Manager to look up
	 * culture-specific resources at run time.
	 * @return An object that represents the current culture.
	 */
	var CurrentUICulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets an  object that contains information about the various contexts of the
	 * current thread.
	 * @return An  object that consolidates context information for the current thread.
	 */
	var ExecutionContext(default, never):cs.system.threading.ExecutionContext;
	/**
	 * Gets a value indicating the execution status of the current thread.
	 * @return if this thread has been started and has not terminated normally or
	 * aborted; otherwise, .
	 */
	var IsAlive(default, never):Bool;
	/**
	 * Gets or sets a value indicating whether or not a thread is a background thread.
	 * @return if this thread is or is to become a background thread; otherwise, .
	 */
	var IsBackground(default, default):Bool;
	/**
	 * Gets a value indicating whether or not a thread belongs to the managed thread
	 * pool.
	 * @return if this thread belongs to the managed thread pool; otherwise, .
	 */
	var IsThreadPoolThread(default, never):Bool;
	/**
	 * Gets a unique identifier for the current managed thread.
	 * @return An integer that represents a unique identifier for this managed thread.
	 */
	var ManagedThreadId(default, never):Int;
	/**
	 * Gets or sets the name of the thread.
	 * @return A string containing the name of the thread, or  if no name was set.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets a value indicating the scheduling priority of a thread.
	 * @return One of the  values. The default value is .
	 */
	var Priority(default, default):cs.system.threading.ThreadPriority;
	/**
	 * Gets a value containing the states of the current thread.
	 * @return One of the  values indicating the state of the current thread. The
	 * initial value is .
	 */
	var ThreadState(default, never):cs.system.threading.ThreadState;
	@:overload(function(start:cs.system.threading.ParameterizedThreadStart):Void {})
	@:overload(function(start:cs.system.threading.ThreadStart):Void {})
	@:overload(function(start:cs.system.threading.ParameterizedThreadStart, maxStackSize:Int):Void {})
	function new(start:cs.system.threading.ThreadStart, maxStackSize:Int):Void;
	/**
	 * Allocates an unnamed data slot on all the threads. For better performance, use
	 * fields that are marked with the  attribute instead.
	 * @return The allocated named data slot on all threads.
	 */
	static function AllocateDataSlot():cs.system.LocalDataStoreSlot;
	/**
	 * Allocates a named data slot on all threads. For better performance, use fields
	 * that are marked with the  attribute instead.
	 * @param name The name of the data slot to be allocated.
	 * @return The allocated named data slot on all threads.
	 */
	static function AllocateNamedDataSlot(name:String):cs.system.LocalDataStoreSlot;
	/** Notifies a host that execution is about to enter a region of code in which the effects of a thread abort or unhandled exception might jeopardize other tasks in the application domain. */
	static function BeginCriticalRegion():Void;
	/** Notifies a host that managed code is about to execute instructions that depend on the identity of the current physical operating system thread. */
	static function BeginThreadAffinity():Void;
	/** Notifies a host that execution is about to enter a region of code in which the effects of a thread abort or unhandled exception are limited to the current task. */
	static function EndCriticalRegion():Void;
	/** Notifies a host that managed code has finished executing instructions that depend on the identity of the current physical operating system thread. */
	static function EndThreadAffinity():Void;
	/**
	 * Eliminates the association between a name and a slot, for all threads in the
	 * process. For better performance, use fields that are marked with the  attribute
	 * instead.
	 * @param name The name of the data slot to be freed.
	 */
	static function FreeNamedDataSlot(name:String):Void;
	static function GetCurrentProcessorId():Int;
	/**
	 * Retrieves the value from the specified slot on the current thread, within the
	 * current thread's current domain. For better performance, use fields that are
	 * marked with the  attribute instead.
	 * @param slot The  from which to get the value.
	 * @return The retrieved value.
	 */
	static function GetData(slot:cs.system.LocalDataStoreSlot):Dynamic;
	/**
	 * Returns the current domain in which the current thread is running.
	 * @return An  representing the current application domain of the running thread.
	 */
	static function GetDomain():cs.system.AppDomain;
	/**
	 * Returns a unique application domain identifier.
	 * @return A 32-bit signed integer uniquely identifying the application domain.
	 */
	static function GetDomainID():Int;
	/**
	 * Looks up a named data slot. For better performance, use fields that are marked
	 * with the  attribute instead.
	 * @param name The name of the local data slot.
	 * @return A  allocated for this thread.
	 */
	static function GetNamedDataSlot(name:String):cs.system.LocalDataStoreSlot;
	/** Synchronizes memory access as follows: The processor executing the current thread cannot reorder instructions in such a way that memory accesses prior to the call to  execute after memory accesses that follow the call to . */
	static function MemoryBarrier():Void;
	/** Cancels an  requested for the current thread. */
	static function ResetAbort():Void;
	/**
	 * Sets the data in the specified slot on the currently running thread, for that
	 * thread's current domain. For better performance, use fields marked with the 
	 * attribute instead.
	 * @param slot The  in which to set the value.
	 * @param data The value to be set.
	 */
	static function SetData(slot:cs.system.LocalDataStoreSlot, data:Dynamic):Void;
	@:overload(function(millisecondsTimeout:Int):Void {})
	/**
	 * Suspends the current thread for the specified number of milliseconds.
	 * @param millisecondsTimeout The number of milliseconds for which the thread is
	 * suspended. If the value of the  argument is zero, the thread relinquishes the
	 * remainder of its time slice to any thread of equal priority that is ready to
	 * run. If there are no other threads of equal priority that are ready to run,
	 * execution of the current thread is not suspended.
	 */
	static function Sleep(timeout:cs.system.TimeSpan):Void;
	/**
	 * Causes a thread to wait the number of times defined by the  parameter.
	 * @param iterations A 32-bit signed integer that defines how long a thread is to
	 * wait.
	 */
	static function SpinWait(iterations:Int):Void;
	@:overload(function(address:cs.Ref<cs.UInt8>):cs.UInt8 {})
	@:overload(function(address:cs.Ref<Float>):Float {})
	@:overload(function(address:cs.Ref<cs.Int16>):cs.Int16 {})
	@:overload(function(address:cs.Ref<Int>):Int {})
	@:overload(function(address:cs.Ref<haxe.Int64>):haxe.Int64 {})
	@:overload(function(address:cs.Ref<cs.system.IntPtr>):cs.system.IntPtr {})
	@:overload(function(address:cs.Ref<Dynamic>):Dynamic {})
	@:overload(function(address:cs.Ref<cs.Int8>):cs.Int8 {})
	@:overload(function(address:cs.Ref<Single>):Single {})
	@:overload(function(address:cs.Ref<cs.UInt16>):cs.UInt16 {})
	@:overload(function(address:cs.Ref<cs.UInt>):cs.UInt {})
	@:overload(function(address:cs.Ref<cs.UInt64>):cs.UInt64 {})
	/**
	 * Reads the value of a field. The value is the latest written by any processor in
	 * a computer, regardless of the number of processors or the state of processor
	 * cache.
	 * @param address The field to be read.
	 * @return The latest value written to the field by any processor.
	 */
	static function VolatileRead(address:cs.Ref<cs.system.UIntPtr>):cs.system.UIntPtr;
	@:overload(function(address:cs.Ref<cs.UInt8>, value:cs.UInt8):Void {})
	@:overload(function(address:cs.Ref<Float>, value:Float):Void {})
	@:overload(function(address:cs.Ref<cs.Int16>, value:cs.Int16):Void {})
	@:overload(function(address:cs.Ref<Int>, value:Int):Void {})
	@:overload(function(address:cs.Ref<haxe.Int64>, value:haxe.Int64):Void {})
	@:overload(function(address:cs.Ref<cs.system.IntPtr>, value:cs.system.IntPtr):Void {})
	@:overload(function(address:cs.Ref<Dynamic>, value:Dynamic):Void {})
	@:overload(function(address:cs.Ref<cs.Int8>, value:cs.Int8):Void {})
	@:overload(function(address:cs.Ref<Single>, value:Single):Void {})
	@:overload(function(address:cs.Ref<cs.UInt16>, value:cs.UInt16):Void {})
	@:overload(function(address:cs.Ref<cs.UInt>, value:cs.UInt):Void {})
	@:overload(function(address:cs.Ref<cs.UInt64>, value:cs.UInt64):Void {})
	/**
	 * Writes a value to a field immediately, so that the value is visible to all
	 * processors in the computer.
	 * @param address The field to which the value is to be written.
	 * @param value The value to be written.
	 */
	static function VolatileWrite(address:cs.Ref<cs.system.UIntPtr>, value:cs.system.UIntPtr):Void;
	/**
	 * Causes the calling thread to yield execution to another thread that is ready to
	 * run on the current processor. The operating system selects the thread to yield
	 * to.
	 * @return if the operating system switched execution to another thread; otherwise,
	 * .
	 */
	static function Yield():Bool;
	@:overload(function():Void {})
	/** Raises a  in the thread on which it is invoked, to begin the process of terminating the thread. Calling this method usually terminates the thread. */
	function Abort(stateInfo:Dynamic):Void;
	/** Turns off automatic cleanup of runtime callable wrappers (RCW) for the current thread. */
	function DisableComObjectEagerCleanup():Void;
	/**
	 * Returns an  value indicating the apartment state.
	 * @return One of the  values indicating the apartment state of the managed thread.
	 * The default is .
	 */
	function GetApartmentState():cs.system.threading.ApartmentState;
	/**
	 * Returns a  object that can be used to capture the stack for the current thread.
	 * @return None.
	 */
	function GetCompressedStack():cs.system.threading.CompressedStack;
	/**
	 * Returns a hash code for the current thread.
	 * @return An integer hash code value.
	 */
	function GetHashCode():Int;
	/** Interrupts a thread that is in the  thread state. */
	function Interrupt():Void;
	@:overload(function():Void {})
	@:overload(function(millisecondsTimeout:Int):Bool {})
	/** Blocks the calling thread until the thread represented by this instance terminates, while continuing to perform standard COM and  pumping. */
	function Join(timeout:cs.system.TimeSpan):Bool;
	/** Resumes a thread that has been suspended. */
	function Resume():Void;
	/**
	 * Sets the apartment state of a thread before it is started.
	 * @param state The new apartment state.
	 */
	function SetApartmentState(state:cs.system.threading.ApartmentState):Void;
	/**
	 * Applies a captured  to the current thread.
	 * @param stack The  object to be applied to the current thread.
	 */
	function SetCompressedStack(stack:cs.system.threading.CompressedStack):Void;
	@:overload(function():Void {})
	/** Causes the operating system to change the state of the current instance to . */
	function Start(parameter:Dynamic):Void;
	/** Either suspends the thread, or if the thread is already suspended, has no effect. */
	function Suspend():Void;
	/**
	 * Sets the apartment state of a thread before it is started.
	 * @param state The new apartment state.
	 * @return if the apartment state is set; otherwise, .
	 */
	function TrySetApartmentState(state:cs.system.threading.ApartmentState):Bool;
}
