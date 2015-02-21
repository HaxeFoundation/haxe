/* -*- Mode: IDL; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=8 sts=2 et sw=2 tw=80: */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://fetch.spec.whatwg.org/#headers-class
 */

typedef (Headers or sequence<sequence<ByteString>> or MozMap<ByteString>) HeadersInit;

enum HeadersGuardEnum {
  "none",
  "request",
  "request-no-cors",
  "response",
  "immutable"
};

[Constructor(optional HeadersInit init),
 Exposed=(Window,Worker),
 Func="mozilla::dom::Headers::PrefEnabled"]
interface Headers {
  [Throws] void append(ByteString name, ByteString value);
  [Throws] void delete(ByteString name);
  [Throws] ByteString? get(ByteString name);
  [Throws] sequence<ByteString> getAll(ByteString name);
  [Throws] boolean has(ByteString name);
  [Throws] void set(ByteString name, ByteString value);

  // Used to test different guard states from mochitest.
  // Note: Must be set prior to populating headers or will throw.
  [ChromeOnly, SetterThrows] attribute HeadersGuardEnum guard;
};
