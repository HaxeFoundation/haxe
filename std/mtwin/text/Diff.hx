/*
 * Copyright (c) 2006, Motion-Twin
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
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
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

package mtwin.text;

typedef LcsMatrix = Array<Array<Int>>

/**
	An haxe implementation of Diff/Patch/Unpatch.

	The diff text format is the same as the gnu diff utility format.

	Usage:

	var myDiff = Diff.diff(sourceString, modifiedString);
	var modified = Diff.patch(sourceString, myDiff); // modified == modifiedString
	var origin = Diff.unpatch(modified, myDiff); // origin == sourceString
**/
class Diff {

	static var END = 0;
	static var DOWN = 1;
	static var RIGHT = 2;
	static var DOWN_RIGHT = 3;
	static var NO_NEW_LINE = "\n\\ No newline at end of file\n";

	/**
		This class uses a longest common subsequence algorithm which computes the distance between 
		two array of strings and creates an optimal modification cursor.

		The cursor is stored in a int matrix and starts at index 0:0.

		The matrix is filled of END, DOWN, DOWN_RIGHT, RIGHT movements.

		DOWN => the src line must be removed
		DOWN_RIGHT => lines are equals, no operation required
		RIGHT => the dst line must be inserted
		END => end of files reached, if END is reached before [m,n], some lines must be removed or inserted 
	**/
	static function longestCommonSubsequence( src:Array<String>, dst:Array<String> ) : LcsMatrix {
		var m = src.length;
		var n = dst.length;
		var cursor = new Array();
		var c = new Array();
		for (i in 0...m+1) {
			cursor[i] = new Array(); 
			cursor[i][n] = 0;
			c[i] = new Array(); 
			c[i][n] = 0; 
		}
		for (j in 0...n+1) {
			cursor[m][j] = 0; 
			c[m][j] = 0;
		}
		var i = m-1;
		while (i >= 0){
			var j = n-1;
			while (j >= 0){
				if (src[i] == dst[j]){
					c[i][j] = c[i+1][j+1]+1;
					cursor[i][j] = DOWN_RIGHT;
				}
				else if (c[i+1][j] >= c[i][j+1]){
					c[i][j] = c[i+1][j];
					cursor[i][j] = DOWN;
				}
				else {
					c[i][j] = c[i][j+1];
					cursor[i][j] = RIGHT;
				}
				j--;
			}
			i--;
		}
		return  cursor;
	}

	/**
		Returns an optimized cursor.

		Compacts the cursor and transform it from a sequential operation cursor into a vector of operations.
	**/
	static function lcsToStack( c:LcsMatrix ) : List<Array<Int>> {
		var stack = new List();
		var i = 0;
		var w = c.length-1;
		var j = 0;
		var h = c[0].length-1;
		var prev = -1;
		while (true){
			// direction changes, store this position
			if (c[i][j] != prev)
				stack.add([i,j]); 
			prev = c[i][j];
			switch (c[i][j]){
				case DOWN: i++;
				case RIGHT: j++;
				case DOWN_RIGHT: i++; j++; 
				case END: break;
				default: throw "Unknown "+c[i][j]+" at "+i+":"+j; i=0; j=0;
			}
		}
		return stack;
	}

	static function split( txt:String ){
		var res = new Array();
		var old = 0;
		var pos = txt.indexOf("\n", 0);
		while (pos != -1){
			res.push(txt.substr(old, pos+1-old));
			old = pos+1;
			pos = txt.indexOf("\n", old);
		}
		res.push(txt.substr(old, txt.length));
		return res;
	}

	/**
		Returns the difference between two strings.

		The returned string may be used by the gnu diff/patch utilities.
	**/
	public static function diff( source:String, dest:String ) : String {
		var srcNoNewLine = false;		
		var dstNoNewLine = false;

		var sourceLines = split(source); // source.split("\n");
		if (sourceLines[sourceLines.length-1].length > 0){
			// no newline at end of file
			sourceLines.push("");
			srcNoNewLine = true;
		}
		
		var destLines = split(dest); // dest.split("\n");
		if (destLines[destLines.length-1].length > 0){
			// no newline at end of file
			destLines.push("");
			dstNoNewLine = true;
		}

		var m = sourceLines.length;
		var n = destLines.length;
		
		var lcs = longestCommonSubsequence(sourceLines, destLines);
		var stack = lcsToStack(lcs);

		var cursorKind = function(p){
			return lcs[p[0]][p[1]]; 
		}

		var operationAdd = function(after:Int, from:Int, to:Int){
			var op = new StringBuf();
			op.add(after); 
			op.add("a"); 
			if (from == to) op.add(from) else { op.add(from); op.add(","); op.add(to); }
			op.add("\n");
			for (i in (from-1)...to){
				op.add("> "); 
				op.add(destLines[i]);
				if (dstNoNewLine && i == (destLines.length-2)){
					op.add(NO_NEW_LINE);
					break;
				}
			}
			return op.toString();
		}

		var operationDel = function(from:Int, to:Int, dest:Int){
			var op = new StringBuf();
			if (from == to) op.add(from) else { op.add(from); op.add(","); op.add(to); }
			op.add("d"); op.add(dest); op.add("\n");
			for (i in (from-1)...to){
				op.add("< ");
				op.add(sourceLines[i]);
				if (srcNoNewLine && i == sourceLines.length-2){
					op.add(NO_NEW_LINE);
					break;
				}
			}
			return op.toString();
		}

		var operationUpd = function(from:Int, to:Int, byFrom:Int, byTo:Int){
			var op = new StringBuf();
			if (from == to) op.add(from) else { op.add(from); op.add(","); op.add(to); }
			op.add("c");
			if (byFrom == byTo) op.add(byFrom) else { op.add(byFrom); op.add(","); op.add(byTo); }
			op.add("\n");
			for (i in (from-1)...to){
				op.add("< ");
				op.add(sourceLines[i]);
				if (srcNoNewLine && i == sourceLines.length-2){
					op.add(NO_NEW_LINE);
					break;
				}
			}
			op.add("---\n");
			for (i in (byFrom-1)...byTo){
				op.add("> ");
				op.add(destLines[i]);
				if (dstNoNewLine && i == destLines.length-2){
					op.add(NO_NEW_LINE);
					break;
				}
			}
			return op.toString();
		}

		var result = new StringBuf();
		while (stack.length > 0){
			var pos = stack.pop();
			switch (cursorKind(pos)){
				case DOWN_RIGHT:

				case DOWN:
					var end = stack.pop(); 
					if (cursorKind(end) == RIGHT){	
						var del = stack.pop(); stack.push(del);
						result.add(operationUpd(pos[0]+1, end[0], pos[1]+1, del[1]));
					}
					else {
						stack.push(end);
						result.add(operationDel(pos[0]+1, end[0], end[1]));
					}

				case RIGHT:
					var end = stack.pop(); stack.push(end);
					//result.add(operationAdd(pos[0], end[1], pos[1]+1));
					result.add(operationAdd(pos[0], pos[1]+1, end[1]));

				case END:
					if (pos[0] < m){
						result.add(operationDel(pos[0], m-1, n-1));
					}
					if (pos[1] < n){
						result.add(operationAdd(m-1, pos[1], n-1));
					}
			}
		}
		return result.toString();
	}

	/**
		Creates a patch operation structure.
	**/
	static function parsePatchOp( left:String, op:String, right:String ): {op:String, left:Array<Int>, right:Array<Int>, data:Array<String>}{
		var l = Lambda.amap(left.split(","), function(v){ return Std.parseInt(v); });
		if (l.length == 1) l.push(l[0]);
		var r = Lambda.amap(right.split(","), function(v){ return Std.parseInt(v); });
		if (r.length == 1) r.push(r[0]);
		return { op:op, left:l, right:r, data:[] };
	}

	/**
		Returns the patched string resulting of the application of the patch data on src.
	**/
	public static function patch( src:String, patch:String ) : String {
		var opRegexp = ~/^([0-9,]+)([adc])([0-9,]+)$/;
		var lines = src.split("\n");
		var counter = 0;
		var result = new StringBuf();
		var patchLines = patch.split("\n");
		while (patchLines.length > 0){
			var patchLine = patchLines.shift();
			if (opRegexp.match(patchLine)){
				var op = parsePatchOp(opRegexp.matched(1), opRegexp.matched(2), opRegexp.matched(3));
				var tmp = patchLines.shift();
				while (tmp != "" && tmp != null && !opRegexp.match(tmp)){
					op.data.push(tmp);
					tmp = patchLines.shift();
				}
				if (tmp != null){
					patchLines.unshift(tmp);
				}
				switch (op.op){
					case "a":
						while (counter < op.left[0]){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						for (i in 0...op.data.length){
							var line = op.data[i];
							if (line.substr(0,2) == "> "){
								result.add(line.substr(2, line.length));
								if (i == op.data.length-1 || op.data[i+1].substr(0,2) != "\\ ") result.add("\n");
							}
						}

					case "d":
						while (counter < op.left[0]-1){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						counter = op.left[1];

					case "c":
						while (counter < op.left[0]-1){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						for (i in 0...op.data.length){
							var line = op.data[i];
							if (line.substr(0,2) == "> "){
								result.add(line.substr(2, line.length));
								if (i == op.data.length-1 || op.data[i+1].substr(0,2) != "\\ ") result.add("\n");
							}
						}
						counter = op.left[1];
				}
			}
		}
		for (i in counter...lines.length-1){
			result.add(lines[i]); result.add("\n");
		}
		return result.toString();
	}

	/**
		Returns the unpatched string resulting of the cancelation of the patch applied to src.
	**/
	public static function unpatch( src:String, patch:String ){
		var opRegexp = ~/^([0-9,]+)([adc])([0-9,]+)$/;
		var lines = src.split("\n");
		var counter = 0;
		var result = new StringBuf();
		var patchLines = patch.split("\n");
		while (patchLines.length > 0){
			var patchLine = patchLines.shift();
			if (opRegexp.match(patchLine)){
				var op = parsePatchOp(opRegexp.matched(1), opRegexp.matched(2), opRegexp.matched(3));
				var tmp = patchLines.shift();
				while (tmp != "" && tmp != null && !opRegexp.match(tmp)){
					op.data.push(tmp);
					tmp = patchLines.shift();
				}
				if (tmp != null){
					patchLines.unshift(tmp);
				}
				switch (op.op){
					case "a": // unpatch => delete
						while (counter < op.right[0]-1){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						counter = op.right[1];

					case "d": // unpatch => add
						while (counter < op.right[0]){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						for (i in 0...op.data.length){
							var line = op.data[i];
							if (line.substr(0,2) == "< "){
								result.add(line.substr(2, line.length));
								if (i == op.data.length-1 || op.data[i+1].substr(0,2) != "\\ ") result.add("\n");
							}
						}

					case "c": // unpatch => change reverse
						while (counter < op.right[0]-1){
							result.add(lines[counter]); result.add("\n");
							counter++;
						}
						for (i in 0...op.data.length){
							var line = op.data[i];
							if (line.substr(0,2) == "< "){
								result.add(line.substr(2, line.length));
								if (i == op.data.length-1 || op.data[i+1].substr(0,2) != "\\ ") result.add("\n");
							}
						}
						counter = op.right[1];
				}
			}
		}
		for (i in counter...lines.length-1){
			result.add(lines[i]); result.add("\n");
		}
		return result.toString();
	}

	public static function main(){
		var src = neko.File.getContent("src.txt");
		var dst = neko.File.getContent("dst.txt");
		var diff = diff(src,dst);
		neko.Lib.print(diff);
		var patched = patch(src, diff);
		if (patched != dst) throw "Patch failed";
		var unpatched = unpatch(patched, diff);
		if (unpatched != src) throw "Unpatch failed";
		neko.Lib.print("diff/patch/unpatch success");
	}
}
