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

package controllers

import testConstants.BaseTestConstants.{testCredId, testMtditid, testNino, testRetrievedUserName, testUserTypeIndividual}
import testConstants.CalcBreakdownTestConstants.calculationDataSuccessModel
import testConstants.EstimatesTestConstants.testYear
import audit.mocks.MockAuditingService
import audit.models.AllowanceAndDeductionsResponseAuditModel
import auth.MtdItUserWithNino
import config.featureswitch.{FeatureSwitching, NewTaxCalcProxy, TxmEventsApproved}
import config.{ItvcErrorHandler, ItvcHeaderCarrierForPartialsConverter}
import controllers.predicates.{NinoPredicate, SessionTimeoutPredicate}
import mocks.controllers.predicates.MockAuthenticationPredicate
import mocks.services.MockCalculationService
import play.api.http.Status
import play.api.mvc.MessagesControllerComponents
import play.api.test.FakeRequest
import play.api.test.Helpers.{charset, contentType, _}
import testUtils.TestSupport

class DeductionsSummaryControllerSpec extends TestSupport with MockCalculationService
  with MockAuthenticationPredicate with FeatureSwitching with MockAuditingService {

  object TestDeductionsSummaryController extends DeductionsSummaryController(
    app.injector.instanceOf[SessionTimeoutPredicate],
    MockAuthenticationPredicate,
    app.injector.instanceOf[NinoPredicate],
    mockCalculationService,
    app.injector.instanceOf[ItvcHeaderCarrierForPartialsConverter],
    mockAuditingService,
    app.injector.instanceOf[views.html.DeductionBreakdown],
    app.injector.instanceOf[views.html.DeductionBreakdownNew],
    app.injector.instanceOf[ItvcErrorHandler]
  )(
    appConfig,
    app.injector.instanceOf[MessagesControllerComponents],
    ec,
    languageUtils
  )
  override def beforeEach(): Unit = {
    super.beforeEach()
    disable(NewTaxCalcProxy)
  }

  "showDeductionsSummary" when {

    "NewTaxCalcProxy FS is enabled, all calc data available" should {

      lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)
      lazy val document = result.toHtmlDocument

      "render the Allowances and Deductions page" in {
        enable(NewTaxCalcProxy)
        mockCalculationSuccessFullNew(testMtditid)
        status(result) shouldBe Status.OK
        document.title() shouldBe "Allowances and deductions - Business Tax account - GOV.UK"
        document.getElementById("total-value").text() shouldBe "£17,500.99"
      }
    }

    "NewTaxCalcProxy FS is enabled, no calc data available" should {

      lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)
      lazy val document = result.toHtmlDocument

      "render the Allowances and Deductions page" in {
        enable(NewTaxCalcProxy)
        mockCalculationSuccessMinimalNew(testMtditid)
        status(result) shouldBe Status.OK
        document.title() shouldBe "Allowances and deductions - Business Tax account - GOV.UK"
        document.getElementById("total-value").text() shouldBe "£0.00"
      }
    }
    "NewTaxCalcProxy FS is enabled, calc returns NOT_FOUND" should {

      lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)

      "render error page" in {
        enable(NewTaxCalcProxy)
        mockCalculationNotFoundNew(testMtditid)
        status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      }
    }

    "given a tax year which can be found in ETMP with TxmApproved FS enabled" should {

        lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)
        lazy val document = result.toHtmlDocument

        "return Status OK (200) with TxmApproved FS enabled" in {
          enable(TxmEventsApproved)
          mockCalculationSuccess()
          status(result) shouldBe Status.OK

          val expectedMtdItUser = MtdItUserWithNino(mtditid = testMtditid, nino = testNino, userName = Some(testRetrievedUserName),
            saUtr = None, credId = Some(testCredId), userType = Some(testUserTypeIndividual), arn = None)(FakeRequest())

          verifyExtendedAudit(AllowanceAndDeductionsResponseAuditModel(expectedMtdItUser,
            calculationDataSuccessModel.allowancesAndDeductions, true))
        }

        "return HTML" in {
          contentType(result) shouldBe Some("text/html")
          charset(result) shouldBe Some("utf-8")
        }

        "render the Allowances and deductions page" in {
          document.title() shouldBe "Allowances and deductions - Business Tax account - GOV.UK"
        }
      }
      "given a tax year which can be found in ETMP with TxmApproved FS disabled" should {
        lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)

        "return Status OK (200) with TxmApproved FS false" in {
          disable(TxmEventsApproved)
          mockCalculationSuccess()
          status(result) shouldBe Status.OK

          val expectedMtdItUser = MtdItUserWithNino(mtditid = testMtditid, nino = testNino, userName = Some(testRetrievedUserName),
            saUtr = None, credId = Some(testCredId), userType = Some(testUserTypeIndividual), arn = None)(FakeRequest())
          verifyExtendedAudit(AllowanceAndDeductionsResponseAuditModel(expectedMtdItUser,
            calculationDataSuccessModel.allowancesAndDeductions, false))
        }
      }
      "given a tax year which can not be found in ETMP" should {

        lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)

        "return Status Internal Server Error (500)" in {
          mockCalculationNotFound()
          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        }

      }

      "there is a downstream error" should {

        lazy val result = TestDeductionsSummaryController.showDeductionsSummary(testYear)(fakeRequestWithActiveSession)

        "return Status Internal Server Error (500)" in {
          mockCalculationError()
          status(result) shouldBe Status.INTERNAL_SERVER_ERROR
        }
      }
    }
  }


