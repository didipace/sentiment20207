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

package haxe.ds;

/**
	A linked-list of elements. The list is composed of element container objects
	that are chained together. It is optimized so that adding or removing an
	element does not imply copying the whole list content every time.

	@see https://haxe.org/manual/std-List.html
**/
class List<T> {
	private var h:ListNode<T>;
	private var q:ListNode<T>;

	/**
		The length of `this` List.
	**/
	public var length(default, null):Int;

	/**
		Creates a new empty list.
	**/
	public function new() {
		length = 0;
	}

	/**
		Adds element `item` at the end of `this` List.

		`this.length` increases by 1.
	**/
	public function add(item:T) {
		var x = ListNode.create(item, null);
		if (h == null)
			h = x;
		else
			q.next = x;
		q = x;
		length++;
	}

	/**
		Adds element `item` at the beginning of `this` List.

		`this.length` increases by 1.
	**/
	public function push(item:T) {
		var x = ListNode.create(item, h);
		h = x;
		if (q == null)
			q = x;
		length++;
	}

	/**
		Returns the first element of `this` List, or null if no elements exist.

		This function does not modify `this` List.
	**/
	public function first():Null<T> {
		return if (h == null) null else h.item;
	}

	/**
		Returns the last element of `this` List, or null if no elements exist.

		This function does not modify `this` List.
	**/
	public function last():Null<T> {
		return if (q == null) null else q.item;
	}

	/**
		Returns the first element of `this` List, or null if no elements exist.

		The element is removed from `this` List.
	**/
	public function pop():Null<T> {
		if (h == null)
			return null;
		var x = h.item;
		h = h.next;
		if (h == null)
			q = null;
		length--;
		return x;
	}

	/**
		Tells if `this` List is empty.
	**/
	public function isEmpty():Bool {
		return (h == null);
	}

	/**
		Empties `this` List.

		This function does not traverse the elements, but simply sets the
		internal references to null and `this.length` to 0.
	**/
	public function clear():Void {
		h = null;
		q = null;
		length = 0;
	}

	/**
		Removes the first occurrence of `v` in `this` List.

		If `v` is found by checking standard equality, it is removed from `this`
		List and the function returns true.

		Otherwise, false is returned.
	**/
	public function remove(v:T):Bool {
		var prev:ListNode<T> = null;
		var l = h;
		while (l != null) {
			if (l.item == v) {
				if (prev == null)
					h = l.next;
				else
					prev.next = l.next;
				if (q == l)
					q = prev;
				length--;
				return true;
			}
			prev = l;
			l = l.next;
		}
		return false;
	}

	/**
		Returns an iterator on the elements of the list.
	**/
	public inline function iterator():ListIterator<T> {
		return new ListIterator<T>(h);
	}

	/**
		Returns an iterator of the List indices and values.
	**/
	@:pure @:runtime public inline function keyValueIterator():ListKeyValueIterator<T> {
		return new ListKeyValueIterator(h);
	}

	/**
		Returns a string representation of `this` List.

		The result is enclosed in { } with the individual elements being
		separated by a comma.
	**/
	public function toString() {
		var s = new StringBuf();
		var first = true;
		var l = h;
		s.add("{");
		while (l != null) {
			if (first)
				first = false;
			else
				s.add(", ");
			s.add(Std.string(l.item));
			l = l.next;
		}
		s.add("}");
		return s.toString();
	}

	/**
		Returns a string representation of `this` List, with `sep` separating
		each element.
	**/
	public function join(sep:String) {
		var s = new StringBuf();
		var first = true;
		var l = h;
		while (l != null) {
			if (first)
				first = false;
			else
				s.add(sep);
			s.ad