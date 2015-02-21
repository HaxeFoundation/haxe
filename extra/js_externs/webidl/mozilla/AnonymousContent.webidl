/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file declares the AnonymousContent interface which is used to
 * manipulate content that has been inserted into the document's canvasFrame
 * anonymous container.
 * See Document.insertAnonymousContent.
 *
 * This API never returns a reference to the actual inserted DOM node on
 * purpose. This is to make sure the content cannot be randomly changed and the
 * DOM cannot be traversed from the node, so that Gecko can remain in control of
 * the inserted content.
 */

[ChromeOnly]
interface AnonymousContent {
  /**
   * Get the text content of an element inside this custom anonymous content.
   */
  [Throws]
  DOMString getTextContentForElement(DOMString elementId);

  /**
   * Set the text content of an element inside this custom anonymous content.
   */
  [Throws]
  void setTextContentForElement(DOMString elementId, DOMString text);

  /**
   * Get the value of an attribute of an element inside this custom anonymous
   * content.
   */
  [Throws]
  DOMString? getAttributeForElement(DOMString elementId,
                                    DOMString attributeName);

  /**
   * Set the value of an attribute of an element inside this custom anonymous
   * content.
   */
  [Throws]
  void setAttributeForElement(DOMString elementId,
                              DOMString attributeName,
                              DOMString value);

  /**
   * Remove an attribute from an element inside this custom anonymous content.
   */
  [Throws]
  void removeAttributeForElement(DOMString elementId,
                                 DOMString attributeName);
};
