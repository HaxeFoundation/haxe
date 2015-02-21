/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[JSImplementation="@mozilla.org/phoneNumberService;1",
 NavigatorProperty="mozPhoneNumberService",
 CheckPermissions="phonenumberservice"]
interface PhoneNumberService {
  DOMRequest fuzzyMatch([TreatNullAs=EmptyString] optional DOMString number1 = "",
                        [TreatNullAs=EmptyString] optional DOMString number2= "");

  DOMString normalize(DOMString number);
};
