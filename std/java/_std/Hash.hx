/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

@:core_api class Hash<T>  
{
	//private var hashes:Array<Int>;
	private var keysArr:Array<String>;
	private var valuesArr:Array<T>;
	
	public function new() : Void 
	{
		//hashes = [];
		keysArr = [];
		valuesArr = [];
	}

	public function set( key : String, value : T ) : Void 
	{
		keysArr.push(key);
		valuesArr.push(value);
	}

	public function get( key : String ) : Null<T> 
	{
		var i = 0;
		for (k in keysArr)
		{
			if (k == key) return valuesArr[i];
			i++;
		}
		return null;
	}

	public function exists( key : String ) : Bool 
	{
		for (k in keysArr)
		{
			if (k == key) return true;
		}
		return false;
	}

	public function remove( key : String ) : Bool 
	{
		var i = 0;
		for (k in keysArr)
		{
			if (k == key)
			{
				keysArr.splice(i, 1);
				valuesArr.splice(i, 1);
				return true;
			}
			i++;
		}
		return false;
	}

	/**
		Returns an iterator of all keys in the hashtable.
	**/
	public function keys() : Iterator<String> 
	{
		return keysArr.iterator();
	}

	/**
		Returns an iterator of all values in the hashtable.
	**/
	public function iterator() : Iterator<T> 
	{
		return valuesArr.iterator();
	}

	/**
		Returns an displayable representation of the hashtable content.
	**/

	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
}
