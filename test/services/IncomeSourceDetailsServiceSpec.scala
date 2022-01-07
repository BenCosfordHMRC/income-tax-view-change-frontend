/*
 * Copyright 2022 HM Revenue & Customs
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

package services

import testConstants.BaseTestConstants._
import testConstants.IncomeSourceDetailsTestConstants._
import audit.mocks.MockAuditingService
import mocks.connectors.MockIncomeTaxViewChangeConnector
import mocks.services.{MockAsyncCacheApi, MockNextUpdatesService}
import testUtils.TestSupport
import play.api.cache.AsyncCacheApi

//scalastyle:off
class IncomeSourceDetailsServiceSpec extends TestSupport with MockIncomeTaxViewChangeConnector with MockNextUpdatesService
  with MockAuditingService with MockAsyncCacheApi {
  val cache = app.injector.instanceOf[AsyncCacheApi]
  object TestIncomeSourceDetailsService extends IncomeSourceDetailsService(mockIncomeTaxViewChangeConnector, cache)

  override def beforeEach() {
    super.beforeEach()
    cache.removeAll()
  }

  "The IncomeSourceDetailsService.getIncomeSourceDetails method" when {

    "a result with both business and property details is returned" should {

      "return an IncomeSourceDetailsModel with business and property options" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(businessesAndPropertyIncome)
        TestIncomeSourceDetailsService.getIncomeSourceDetails().futureValue shouldBe businessesAndPropertyIncome
      }
    }

    "a result with just business details is returned" should {
      "return an IncomeSourceDetailsModel with just a business option" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(singleBusinessIncome)
        TestIncomeSourceDetailsService.getIncomeSourceDetails().futureValue shouldBe singleBusinessIncome
      }
    }

    "a result with just property details is returned" should {
      "return an IncomeSourceDetailsModel with just a property option" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(propertyIncomeOnly)
        TestIncomeSourceDetailsService.getIncomeSourceDetails().futureValue shouldBe propertyIncomeOnly
      }
    }

    "a result with no income source details is returned" should {
      "return an IncomeSourceDetailsModel with no options" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(noIncomeDetails)
        TestIncomeSourceDetailsService.getIncomeSourceDetails().futureValue shouldBe noIncomeDetails
      }
    }

    "a result where the Income Source Details are error" should {
      "return an IncomeSourceError" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(errorResponse)
        TestIncomeSourceDetailsService.getIncomeSourceDetails().futureValue shouldBe errorResponse
      }
    }
    "caching" when {
      "should cache" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(noIncomeDetails)
        TestIncomeSourceDetailsService.getIncomeSourceDetails(Some("key")).futureValue shouldBe noIncomeDetails
        TestIncomeSourceDetailsService.getIncomeSourceDetails(Some("key")).futureValue shouldBe noIncomeDetails
        verifyMockIncomeSourceDetailsResponse(1)
      }

      "should NOT cache" in {
        setupMockIncomeSourceDetailsResponse(testMtditid, testNino, Some(testSaUtr), Some(testCredId), Some(testUserType))(noIncomeDetails)
        TestIncomeSourceDetailsService.getIncomeSourceDetails(Some("key2")).futureValue shouldBe noIncomeDetails
        TestIncomeSourceDetailsService.getIncomeSourceDetails(Some("someotherkey")).futureValue shouldBe noIncomeDetails
        verifyMockIncomeSourceDetailsResponse(2)
      }
    }
  }
}
