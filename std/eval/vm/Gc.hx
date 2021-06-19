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
			* 0x400 Output GC statistics at program exit. Default