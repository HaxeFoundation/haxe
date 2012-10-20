/*
 * format - haXe File Formats
 *
 *  inflate format decompression algorithm
 *  Copyright (C) 2004-2008 Nicolas Cannasse
 *  Compliant with RFC 1950 and 1951
 *
 * Copyright (c) 2008, The haXe Project Contributors
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

	function treeMake( bits : IntHash<Int>, maxbits : Int, v : Int, len : Int ) {
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
		var bits = new IntHash();
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
