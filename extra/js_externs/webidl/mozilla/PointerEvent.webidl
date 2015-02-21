/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information see nsIPointerEvent.idl.
 *
 * Portions Copyright 2013 Microsoft Open Technologies, Inc. */

interface WindowProxy;

[Pref="dom.w3c_pointer_events.enabled",
 Constructor(DOMString type, optional PointerEventInit eventInitDict)]
interface PointerEvent : MouseEvent
{
  readonly attribute long pointerId;
  readonly attribute long width;
  readonly attribute long height;
  readonly attribute float pressure;
  readonly attribute long tiltX;
  readonly attribute long tiltY;
  readonly attribute DOMString pointerType;
  readonly attribute boolean isPrimary;
};

dictionary PointerEventInit : MouseEventInit
{
  long pointerId = 0;
  long width = 0;
  long height = 0;
  float pressure = 0;
  long tiltX = 0;
  long tiltY = 0;
  DOMString pointerType = "";
  boolean isPrimary = false;
};

