/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package eval.vm;

/**
	The memory management counters are returned in a stat record.
	The total amount of memory allocated by the program since it was started is (in words) minor_words + major_words - promoted_words. Multiply by the word size (4 on a 32-bit machine, 8 on a 64-bit machine) to get the number of bytes.
**/
typedef Stat = {
	/**
		Number of words allocated in the minor heap since the program was started. This number is accurate in byte-code programs, but only an approximation in programs compiled to native code.
	**/
	var minor_words:Float;

	/**
		Number of words allocated in the minor heap that survived a minor collection and were moved to the major heap since the program was started.
	**/
	var promoted_words:Float;

	/**
		Number of words allocated in the major heap, including the promoted words, since the program was started.
	**/
	var major_words:Float;

	/**
		Number of minor collections since the program was started.
	**/
	var minor_collections:Float;

	/**
		Number of major collection cycles completed since the program was started.
	**/
	var major_collections:Float;

	/**
		Total size of the major heap, in words.
	**/
	var heap_words:Int;

	/**
		Number of contiguous pieces of memory that make up the major heap.
	**/
	var heap_chunks:Int;

	/**
		Number of words of live data in the major heap, including the header words.
	**/
	var live_words:Int;

	/**
		Number of live blocks in the major heap.
	**/
	var live_blocks:Int;

	/**
		Number of words in the free list.
	**/
	var free_words:Int;

	/**
		Number of blocks in the free list.
	**/
	var free_blocks:Int;

	/**
		Size (in words) of the largest block in the free list.
	**/
	var largest_free:Int;

	/**
		Number of wasted words due to fragmentation. These are 1-words free blocks placed between two live blocks. They are not available for allocation.
	**/
	var fragments:Int;

	/**
		Number of heap compactions since the program was started.
	**/
	var compactions:Int;

	/**
		Maximum size reached by the major heap, in words.
	**/
	var top_heap_words:Int;

	/**
		Current size of the stack, in words.
	**/
	var stack_size:Int;
}

/**
	The GC parameters are given as a control record. Note that these parameters can also be initialised by setting the OCAMLRUNPARAM environment variable. See the documentation of ocamlrun.
**/
typedef Control = {
	/**
		The size (in words) of the minor heap. Changing this parameter will trigger a minor collection. Default: 256k.
	**/
	var minor_heap_size:Int;

	/**
		How much to add to the major heap when increasing it. If this number is less than or equal to 1000, it is a percentage of the current heap size (i.e. setting it to 100 will double the heap size at each increase). If it is more than 1000, it is a fixed number of words that will be added to the heap. Default: 15.
	**/
	var major_heap_increment:Int;

	/**
		The major GC speed is computed from this parameter. This is the memory that will be "wasted" because the GC does not immediatly collect unreachable blocks. It is expressed as a percentage of the memory used for live data. The GC will work more (use more CPU time and collect blocks more eagerly) if space_overhead is smaller. Default: 80.
	**/
	var space_overhead:Int;

	/**
		This value controls the GC messages on standard error output. It is a sum of some of the following flags, to print messages on the corresponding events:
			* 0x001 Start of major GC cycle.
			* 0x002 Minor collection and major GC slice.
			* 0x004 Growing and shrinking of the heap.
			* 0x008 Resizing of stacks and memory manager tables.
			* 0x010 Heap compaction.
			* 0x020 Change of GC parameters.
			* 0x040 Computation of major GC slice size.
			* 0x080 Calling of finalisation functions.
			* 0x100 Bytecode executable and shared library search at start-up.
			* 0x200 Computation of compaction-triggering condition.
			* 0x400 Output GC statistics at program exit. Default: 0.
	**/
	var verbose:Int;

	/**
		Heap compaction is triggered when the estimated amount of "wasted" memory is more than max_overhead percent of the amount of live data. If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle (this setting is intended for testing purposes only). If max_overhead >= 1000000, compaction is never triggered. If compaction is permanently disabled, it is strongly suggested to set allocation_policy to 1. Default: 500.
	**/
	var max_overhead:Int;

	/**
		The maximum size of the stack (in words). This is only relevant to the byte-code runtime, as the native code runtime uses the operating system's stack. Default: 1024k.
	**/
	var stack_limit:Int;

	/**
		The policy used for allocating in the heap. Possible values are 0 and 1. 0 is the next-fit policy, which is quite fast but can result in fragmentation. 1 is the first-fit policy, which can be slower in some cases but can be better for programs with fragmentation problems. Default: 0.
	**/
	var allocation_policy:Int;
}

/**
	Memory management control and statistics; finalised values.
**/
extern class Gc {
	/**
		Return the total number of bytes allocated since the program was started. It is returned as a float to avoid overflow problems with int on 32-bit machines.
	**/
	static function allocated_bytes():Float;

	/**
		Perform a full major collection and compact the heap. Note that heap compaction is a lengthy operation.
	**/
	static function compact():Void;

	/**
		Return (minor_words, promoted_words, major_words). This function is as fast as quick_stat.
	**/
	static function counters():{minor_words:Float, promoted_words:Float, major_words:Float};

	/**
		Registers f as a finalisation function for v. v must be heap-allocated. f will be called with v as argument at some point between the first time v becomes unreachable (including through weak pointers) and the time v is collected by the GC. Several functions can be registered for the same value, or even several instances of the same function. Each instance will be called once (or never, if the program terminates before v becomes unreachable).
		The GC will call the finalisation functions in the order of deallocation. When several values become unreachable at the same time (i.e. during the same GC cycle), the finalisation functions will be called in the reverse order of the corresponding calls to finalise. If finalise is called in the same order as the values are allocated, that means each value is finalised before the values it depends upon. Of course, this becomes false if additional dependencies are introduced by assignments.

		In the presence of multiple OCaml threads it should be assumed that any particular finaliser may be executed in any of the threads.
	**/
	static function finalise<T>(f:T->Void, v:T):Void;

	/**
		Do a minor collection, finish the current major collection cycle, and perform a complete new cycle. This will collect all currently unreachable blocks.
	**/
	static function full_major():Void;

	/**
		Return the current values of the GC parameters in a control record.
	**/
	static function get():Control;

	/**
		Do a minor collection and finish the current major collection cycle.
	**/
	static function major():Void;

	/**
		Do a minor collection and a slice of major collection. n is the size of the slice: the GC will do enough work to free (on average) n words of memory. If n = 0, the GC will try to do enough work to ensure that the next automatic slice has no work to do. This function returns an unspecified integer (currently: 0).
	**/
	static function major_slice():Void;

	/**
		Trigger a minor collection.
	**/
	static function minor():Void;

	/**
		Print the current values of the memory management counters (in human-readable form) into the channel argument.
	**/
	static function print_stat(out_channel:haxe.io.Output):Void;

	/**
		Same as stat except that live_words, live_blocks, free_words, free_blocks, largest_free, and fragments are set to 0. This function is much faster than stat because it does not need to go through the heap.
	**/
	static function quick_stat():Stat;

	/**
		Changes the GC parameters according to the control record r.
	**/
	static function set(r:Control):Void;

	/**
		Return the current values of the memory management counters in a stat record. This function examines every heap block to get the statistics.
	**/
	static function stat():Stat;
}
