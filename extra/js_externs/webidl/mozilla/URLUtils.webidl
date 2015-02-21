/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://url.spec.whatwg.org/#urlutils
 *
 * To the extent possible under law, the editors have waived all copyright
 * and related or neighboring rights to this work. In addition, as of 17
 * February 2013, the editors have made this specification available under
 * the Open Web Foundation Agreement Version 1.0, which is available at
 * http://www.openwebfoundation.org/legal/the-owf-1-0-agreements/owfa-1-0.
 */

[NoInterfaceObject,
 Exposed=(Window, Worker)]
interface URLUtils {
  // Bug 824857: no support for stringifier attributes yet.
  //  stringifier attribute USVString href;
  [Throws, CrossOriginWritable=Location]
           attribute USVString href;
  [Throws]
  readonly attribute USVString origin;

  [Throws]
           attribute USVString protocol;
  [Throws]
           attribute USVString username;
  [Throws]
           attribute USVString password;
  [Throws]
           attribute USVString host;
  [Throws]
           attribute USVString hostname;
  [Throws]
           attribute USVString port;
  [Throws]
           attribute USVString pathname;
  [Throws]
           attribute USVString search;

  [Throws]
           attribute USVString hash;

  // Bug 824857 should remove this.
  [Throws]
  stringifier;
};

[NoInterfaceObject,
 Exposed=(Window, Worker)]
interface URLUtilsSearchParams {
           attribute URLSearchParams searchParams;
};
