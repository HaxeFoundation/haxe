package unit.issues;

import php.*;

class Issue5270 extends unit.Test {
#if php
	function test() {
		SuperGlobal._SERVER['CONTENT_TYPE'] = 'type';
		SuperGlobal._SERVER['HTTP_USER_AGENT'] = 'browser';
		SuperGlobal._SERVER['REDIRECT_HTTP_AUTHORIZATION'] = 'auth1';

		eq(Web.getClientHeader('Content-Type'), 'type');
		eq(Web.getClientHeader('User-Agent'), 'browser');
		eq(Web.getClientHeader('Authorization'), 'auth1');
	}
#end
}