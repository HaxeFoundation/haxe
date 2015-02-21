/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Constructor(DOMString type, optional MozSettingsEventInit eventInitDict)]
interface MozSettingsEvent : Event
{
  readonly attribute DOMString? settingName;
  readonly attribute any settingValue;
};

dictionary MozSettingsEventInit : EventInit
{
  DOMString settingName = "";
  any settingValue = null;
};
