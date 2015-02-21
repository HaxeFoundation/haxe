/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://encoding.spec.whatwg.org/#interface-textencoder
 *
 * Any copyright is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
 */

[Constructor(optional DOMString utfLabel = "utf-8"),
 Exposed=(Window,Worker,System)]
interface TextEncoder {
  [Constant]
  readonly attribute DOMString encoding;
  [NewObject]
  Uint8Array encode(optional USVString input = "");
};
