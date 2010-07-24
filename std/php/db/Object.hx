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
package php.db;

/**
	SPOD Object : the persistent object base type. See the tutorial on haXe
	website to learn how to use SPOD.
**/
class Object #if spod_rtti implements haxe.rtti.Infos #end {

/*
	(optional)
	static var TABLE_NAME = "TableName";
	static var TABLE_IDS = ["id"];
	static var PRIVATE_FIELDS = ["my_priv_field"];
	static function RELATIONS() {
		return [{ key : "uid", prop : "user", manager : User.manager }];
	}

	static var manager = new php.db.Manager();
*/

	var __cache__ : Object;
	var __noupdate__ : Bool;
	var __manager__ : {
		private function doUpdate( o : Object ) : Void;
		private function doInsert( o : Object ) : Void;
		private function doSync( o : Object ) : Void;
		private function doDelete( o : Object ) : Void;
		private function objectToString( o : Object ) : String;
	};

	public function new() {
		__init_object();
	}

	private function __init_object() {
		__noupdate__ = false;
		__manager__ = Manager.managers.get(Type.getClassName(Type.getClass(this)));
		var rl : Array<Dynamic>;
		try {
			rl = untyped __manager__.cls.RELATIONS();
		} catch(e : Dynamic) { return; }
		for(r in rl)
			untyped __manager__.initRelation(this, r);
	}

	public function insert() {
		__manager__.doInsert(this);
	}

	public function update() {
		if( __noupdate__ ) throw "Cannot update not locked object";
		__manager__.doUpdate(this);
	}

	public function sync() {
		__manager__.doSync(this);
	}

	public function delete() {
		__manager__.doDelete(this);
	}

	public function toString() {
		return __manager__.objectToString(this);
	}

}
