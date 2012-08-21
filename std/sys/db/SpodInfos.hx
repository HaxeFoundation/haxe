/*
 * Copyright (c) 2005-2011, The haXe Project Contributors
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
	var hfields : Hash<SpodField>;
	var relations : Array<SpodRelation>;
	var indexes : Array<{ keys : Array<String>, unique : Bool }>;
}
