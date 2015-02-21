/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[CheckPermissions="bluetooth",
 Constructor(DOMString type, optional BluetoothAdapterEventInit eventInitDict)]
interface BluetoothAdapterEvent : Event
{
  readonly attribute BluetoothAdapter? adapter;
  readonly attribute DOMString?        address;
};

dictionary BluetoothAdapterEventInit : EventInit
{
  BluetoothAdapter? adapter = null;
  DOMString?        address = "";
};
