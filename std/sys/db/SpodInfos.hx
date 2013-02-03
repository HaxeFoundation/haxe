/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package sys.db;

enum SpodType {
	DId;
	DInt;
	DUId;
	DUInt;
	DBigId;
	DBigInt;
	DSingle;
	DFloat;
	DBool;
	DString( n : Int );
	DDate;
	DDateTime;
	DTimeStamp;
	DTinyText;
	DSmallText;
	DText;
	DSmallBinary;
	DLongBinary;
	DBinary;
	DBytes( n : Int );
	DEncoded;
	DSerialized;
	DNekoSerialized;
	DFlags( flags : Array<String>, autoSize : Bool );
	DTinyInt;
	DTinyUInt;
	DSmallInt;
	DSmallUInt;
	DMediumInt;
	DMediumUInt;
	DData;
	DEnum( name : String );
	// specific for intermediate calculus
	DInterval;
	DNull;
}

typedef SpodField = {
	var name : String;
	var t : SpodType;
	var isNull : Bool;
}

typedef SpodRelation = {
	var prop : String;
	var key : String;
	var type : String;
	var cascade : Bool;
	var lock : Bool;
	var isNull : Bool;
}

typedef SpodInfos = {
	var name : String;
	var key : Array<String>;
	var fields : Array<SpodField>;
	var hfields : haxe.ds.StringMap<SpodField>;
	var relations : Array<SpodRelation>;
	var indexes : Array<{ keys : Array<String>, unique : Bool }>;
}
