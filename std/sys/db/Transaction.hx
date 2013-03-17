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

class Transaction {

	public static function isDeadlock(e : Dynamic) {
		return Std.is(e,String) && ~/try restarting transaction/.match(e);
	}

	private static function runMainLoop(mainFun,logError,count) {
		try {
			mainFun();
		} catch( e : Dynamic ) {
			if( count > 0 && isDeadlock(e) ) {
				Manager.cleanup();
				Manager.cnx.rollback(); // should be already done, but in case...
				Manager.cnx.startTransaction();
				runMainLoop(mainFun,logError,count-1);
				return;
			}
			if( logError == null ) {
				Manager.cnx.rollback();
				#if neko
				neko.Lib.rethrow(e);
				#else
				throw e;
				#end
			}
			logError(e); // should ROLLBACK if needed
		}
	}

	public static function main( cnx, mainFun : Void -> Void, ?logError : Dynamic -> Void ) {
		Manager.initialize();
		Manager.cnx = cnx;
		Manager.cnx.startTransaction();
		runMainLoop(mainFun,logError,3);
		try {
			Manager.cnx.commit();
		} catch( e : String ) {
			// sqlite can have errors on commit
			if( ~/Database is busy/.match(e) )
				logError(e);
		}
		Manager.cnx.close();
		Manager.cnx = null;
		Manager.cleanup();
	}

}
