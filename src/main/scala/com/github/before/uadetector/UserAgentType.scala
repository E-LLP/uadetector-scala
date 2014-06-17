/**
 * Copyright 2014 André Rouél
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.before.uadetector

sealed trait UserAgentType {
  def name: String
}
object UserAgentType {
  def classify(name: String): UserAgentType = {
    if (BrowserType.name == name)
      BrowserType
    else if (EmailClient.name == name)
      EmailClient
    else if (FeedReader.name == name)
      FeedReader
    else if (Library.name == name)
      Library
    else if (MobileBrowser.name == name)
      MobileBrowser
    else if (MultimediaPlayer.name == name)
      MultimediaPlayer
    else if (OfflineBrowser.name == name)
      OfflineBrowser
    else if (OtherType.name == name)
      OtherType
    else if (UserAgentAnonymizer.name == name)
      UserAgentAnonymizer
    else if (Validator.name == name)
      Validator
    else if (WapBrowser.name == name)
      WapBrowser
    else
      UnknownType(name)
  }
}
sealed abstract class KnownUserAgentType(override val name: String) extends UserAgentType
case object BrowserType extends KnownUserAgentType("Browser")
case object EmailClient extends KnownUserAgentType("Email client")
case object FeedReader extends KnownUserAgentType("Feed Reader")
case object Library extends KnownUserAgentType("Library")
case object MobileBrowser extends KnownUserAgentType("Mobile Browser")
case object MultimediaPlayer extends KnownUserAgentType("Multimedia Player")
case object OfflineBrowser extends KnownUserAgentType("Offline Browser")
case object OtherType extends KnownUserAgentType("Other")
case object RobotType extends KnownUserAgentType("robot")
case object UserAgentAnonymizer extends KnownUserAgentType("Useragent Anonymizer")
case object Validator extends KnownUserAgentType("Validator")
case object WapBrowser extends KnownUserAgentType("Wap Browser")
case class UnknownType(override val name: String) extends UserAgentType
