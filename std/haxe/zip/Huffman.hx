/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package haxe.zip;

enum Huffman {
	Found( i : Int );
	NeedBit( left : Huffman, right : Huffman );
	NeedBits( n : Int, table : Array<Huffman> );
}

class HuffTools {

	public function new() {
	}

	function treeDepth(t) {
		return switch(t) {
		case Found(_): 0;
		case NeedBits(_,_): throw "assert";
		case NeedBit(a,b):
			var da = treeDepth(a);
			var db = treeDepth(b);
			1 + ((da < db) ? da : db);
		}
	}

	function treeCompress(t) {
		var d = treeDepth(t);
		if( d == 0 )
			return t;
		if( d == 1 )
			return switch( t ) {
			case NeedBit(a,b): NeedBit(treeCompress(a),treeCompress(b));
			default: throw "assert";
			}
		var size = 1 << d;
		var table = new Array();
		for( i in 0...size )
			table.push(Found(-1));
		treeWalk(table,0,0,d,t);
		return NeedBits(d,table);
	}

	function treeWalk(table,p,cd,d,t) {
		switch( t ) {
		case NeedBit(a,b):
			if( d > 0 ) {
				treeWalk(table,p,cd+1,d-1,a);
				treeWalk(table,p|(1<<cd),cd+1,d-1,b);
			} else
				table[p] = treeCompress(t);
		default:
			table[p] = treeCompress(t);
		}
	}

	function treeMake( bits : haxe.ds.IntMap<Int>, maxbits : Int, v : Int, len : Int ) {
		if( len > maxbits ) throw "Invalid huffman";
		var idx = (v << 5) | len;
		if( bits.exists(idx) )
			return Found(bits.get(idx));
		v <<= 1;
		len += 1;
		return NeedBit(treeMake(bits,maxbits,v,len),treeMake(bits,maxbits,v|1,len));
	}

	public function make(lengths,pos,nlengths,maxbits) {
		var counts = new Array();
		var tmp = new Array();
		if( maxbits > 32 ) throw "Invalid huffman";
		for( i in 0...maxbits ) {
			counts.push(0);
			tmp.push(0);
		}
		for( i in 0...nlengths ) {
			var p = lengths[i+pos];
			if( p >= maxbits ) throw "Invalid huffman";
			counts[p]++;
		}
		var code = 0;
		for( i in 1...maxbits-1 ) {
			code = (code + counts[i]) << 1;
			tmp[i] = code;
		}
		var bits = new haxe.ds.IntMap();
		for( i in 0...nlengths ) {
			var l = lengths[i + pos];
			if( l != 0 ) {
				var n = tmp[l-1];
				tmp[l-1] = n + 1;
				bits.set((n << 5) | l,i);
			}
		}
		return treeCompress(NeedBit(treeMake(bits,maxbits,0,1),treeMake(bits,maxbits,1,1)));
	}
}
