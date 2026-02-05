package cs.system;

/** Controls the system garbage collector, a service that automatically reclaims unused memory. */
@:native("System.GC")
extern class GC {
	/**
	 * Gets the maximum number of generations that the system currently supports.
	 * @return A value that ranges from zero to the maximum number of supported
	 * generations.
	 */
	static var MaxGeneration(default, never):Int;
	/**
	 * Informs the runtime of a large allocation of unmanaged memory that should be
	 * taken into account when scheduling garbage collection.
	 * @param bytesAllocated The incremental amount of unmanaged memory that has been
	 * allocated.
	 */
	static function AddMemoryPressure(bytesAllocated:haxe.Int64):Void;
	/** Cancels the registration of a garbage collection notification. */
	static function CancelFullGCNotification():Void;
	@:overload(function():Void {})
	@:overload(function(generation:Int):Void {})
	@:overload(function(generation:Int, mode:cs.system.GCCollectionMode):Void {})
	@:overload(function(generation:Int, mode:cs.system.GCCollectionMode, blocking:Bool):Void {})
	/** Forces an immediate garbage collection of all generations. */
	static function Collect(generation:Int, mode:cs.system.GCCollectionMode, blocking:Bool, compacting:Bool):Void;
	/**
	 * Returns the number of times garbage collection has occurred for the specified
	 * generation of objects.
	 * @param generation The generation of objects for which the garbage collection
	 * count is to be determined.
	 * @return The number of times garbage collection has occurred for the specified
	 * generation since the process was started.
	 */
	static function CollectionCount(generation:Int):Int;
	/** Ends the no GC region latency mode. */
	static function EndNoGCRegion():Void;
	/**
	 * Gets the total number of bytes allocated to the current thread since the
	 * beginning of its lifetime.
	 * @return The total number of bytes allocated to the current thread since the
	 * beginning of its lifetime.
	 */
	static function GetAllocatedBytesForCurrentThread():haxe.Int64;
	@:overload(function(obj:Dynamic):Int {})
	/**
	 * Returns the current generation number of the specified object.
	 * @param obj The object that generation information is retrieved for.
	 * @return The current generation number of .
	 */
	static function GetGeneration(wo:cs.system.WeakReference):Int;
	/**
	 * Retrieves the number of bytes currently thought to be allocated. A parameter
	 * indicates whether this method can wait a short interval before returning, to
	 * allow the system to collect garbage and finalize objects.
	 * @param forceFullCollection to indicate that this method can wait for garbage
	 * collection to occur before returning; otherwise, .
	 * @return A number that is the best available approximation of the number of bytes
	 * currently allocated in managed memory.
	 */
	static function GetTotalMemory(forceFullCollection:Bool):haxe.Int64;
	/**
	 * References the specified object, which makes it ineligible for garbage
	 * collection from the start of the current routine to the point where this method
	 * is called.
	 * @param obj The object to reference.
	 */
	static function KeepAlive(obj:Dynamic):Void;
	/**
	 * Specifies that a garbage collection notification should be raised when
	 * conditions favor full garbage collection and when the collection has been
	 * completed.
	 * @param maxGenerationThreshold A number between 1 and 99 that specifies when the
	 * notification should be raised based on the objects allocated in generation 2.
	 * @param largeObjectHeapThreshold A number between 1 and 99 that specifies when
	 * the notification should be raised based on objects allocated in the large object
	 * heap.
	 */
	static function RegisterForFullGCNotification(maxGenerationThreshold:Int, largeObjectHeapThreshold:Int):Void;
	/**
	 * Informs the runtime that unmanaged memory has been released and no longer needs
	 * to be taken into account when scheduling garbage collection.
	 * @param bytesAllocated The amount of unmanaged memory that has been released.
	 */
	static function RemoveMemoryPressure(bytesAllocated:haxe.Int64):Void;
	/**
	 * Requests that the system call the finalizer for the specified object for which 
	 * has previously been called.
	 * @param obj The object that a finalizer must be called for.
	 */
	static function ReRegisterForFinalize(obj:Dynamic):Void;
	/**
	 * Requests that the common language runtime not call the finalizer for the
	 * specified object.
	 * @param obj The object whose finalizer must not be executed.
	 */
	static function SuppressFinalize(obj:Dynamic):Void;
	@:overload(function(totalSize:haxe.Int64):Bool {})
	@:overload(function(totalSize:haxe.Int64, disallowFullBlockingGC:Bool):Bool {})
	@:overload(function(totalSize:haxe.Int64, lohSize:haxe.Int64):Bool {})
	/**
	 * Attempts to disallow garbage collection during the execution of a critical path
	 * if a specified amount of memory is available.
	 * @param totalSize The amount of memory in bytes to allocate without triggering a
	 * garbage collection. It must be less than or equal to the size of an ephemeral
	 * segment. For information on the size of an ephemeral segment, see the "Ephemeral
	 * generations and segments" section in the Fundamentals of Garbage Collection
	 * article.
	 * @return if the runtime was able to commit the required amount of memory and the
	 * garbage collector is able to enter no GC region latency mode; otherwise, .
	 */
	static function TryStartNoGCRegion(totalSize:haxe.Int64, lohSize:haxe.Int64, disallowFullBlockingGC:Bool):Bool;
	@:overload(function():cs.system.GCNotificationStatus {})
	/**
	 * Returns the status of a registered notification for determining whether a full,
	 * blocking garbage collection by the common language runtime is imminent.
	 * @return The status of the registered garbage collection notification.
	 */
	static function WaitForFullGCApproach(millisecondsTimeout:Int):cs.system.GCNotificationStatus;
	@:overload(function():cs.system.GCNotificationStatus {})
	/**
	 * Returns the status of a registered notification for determining whether a full,
	 * blocking garbage collection by the common language runtime has completed.
	 * @return The status of the registered garbage collection notification.
	 */
	static function WaitForFullGCComplete(millisecondsTimeout:Int):cs.system.GCNotificationStatus;
	/** Suspends the current thread until the thread that is processing the queue of finalizers has emptied that queue. */
	static function WaitForPendingFinalizers():Void;
}
