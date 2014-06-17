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

import com.github.before.uadetector.datasource.Device

case class UserAgent(
    deviceCategory: Option[DeviceClass],
    iconName: String,
    name: String,
    operatingSystem: Option[OperatingSystem],
    producer: String,
    producerUrl: String,
    uaType: Option[UserAgentType],
    url: String,
    version: Option[Version])
