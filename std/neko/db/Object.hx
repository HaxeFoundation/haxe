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
package neko.db;

/**
	Record Object : the persistent object base type. See the tutorial on haXe
	website to learn how to use Record.
**/
#if old_spod

#if spod-rtti
@:rtti
#end
class Object {

/*
	(optional)
	static var TABLE_NAME = "TableName";
	static var TABLE_IDS = ["id"];
	static var PRIVATE_FIELDS = ["my_priv_field"];
	static function RELATIONS() {
		return [{ key : "uid", prop : "user", manager : User.manager }];
	}

	static var manager = new neko.db.Manager();
*/

	var local_manager : {
		private function doUpdate( o : Object ) : Void;
		private function doInsert( o : Object ) : Void;
		private function doSync( o : Object ) : Void;
		private function doDelete( o : Object ) : Void;
		private function objectToString( o : Object ) : String;
	};


	public function new() {
	}

	public function insert() {
		local_manager.doInsert(this);
	}

	public dynamic function update() {
		local_manager.doUpdate(this);
	}

	public function sync() {
		local_manager.doSync(this);
	}

	public function delete() {
		local_manager.doDelete(this);
	}

	public function toString() {
		return local_manager.objectToString(this);
	}

}

#end