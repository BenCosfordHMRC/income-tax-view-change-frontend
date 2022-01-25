/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package helpers.servicemocks

import helpers.WiremockHelper
import models.liabilitycalculation.LiabilityCalculationResponse
import play.api.libs.json.{Json}

object IncomeTaxCalculationStub {

  def getCalculationResponseUrl(nino: String, taxYear: String): String = s"/income-tax-calculation/income-tax/nino/$nino?taxYear=$taxYear"

  def stubGetCalculationResponse(nino: String, taxYear: String)(status: Int, body: LiabilityCalculationResponse): Unit = {
    WiremockHelper.stubGet(getCalculationResponseUrl(nino, taxYear), status, Json.toJson(body).toString())
  }

  def verifyGetCalculationResponse(nino: String, taxYear: String, noOfCalls: Int = 1): Unit = {
    WiremockHelper.verifyGet(getCalculationResponseUrl(nino, taxYear), noOfCalls)
  }

}
